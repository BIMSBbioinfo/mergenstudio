#' Chat UI
#'
#' @param id id of the module
#' @param translator A Translator from `shiny.i18n::Translator`
#' @param bslib tooltip
#' @noRd
mod_chat_ui <- function(id, translator = create_translator()) {
  ns <- NS(id)

  bslib::card(
    class = "h-100",
    bslib::card_body(
      class = "py-2 h-100",
      div(
        class = "d-flex flex-column h-100",
        div(
          class = "p-2 mh-100 overflow-auto",
          welcomeMessageOutput(ns("welcome")),
          uiOutput(ns("history"))
        ),
        div(
          class = "mt-auto",
          style = css(
            "margin-left" = "40px",
            "margin-right" = "40px"
          ),
          htmltools::div(
            class = "position-relative",
            style = css(
              "width" = "100%"
            ),
            div(
              text_area_input_wrapper(
                inputId = ns("chat_input"),
                label = NULL,
                width = "100%",
                placeholder = translator$t("Write your prompt here"),
                value = "",
                resize = "none",
                textarea_class = "chat-prompt"
              )
            ),
            div(
              class = "position-absolute top-50 end-0 translate-middle",

              # send prompt button
              actionButton(
                inputId = ns("chat"),
                label = icon("fas fa-paper-plane"),
                class = "w-45 btn-primary p-1 chat-send-btn"
              ) %>%
                bslib::tooltip("Send (click or Enter)"),

              # execute code button
              actionButton(
                inputId = ns("execute"),
                label = icon("fas fa-play"),
                class = "w-45 btn-primary p-1 chat-send-btn"
              ) %>%
                bslib::tooltip("Execute Code")
            )
          )
        )
      )
    )
  )
}

#' Chat server
#'
#' @param id id of the module
#' @param translator Translator from `shiny.i18n::Translator`
#' @param settings,history Reactive values from the settings and history module
#' @inheritParams run_chat_app
#'
#' @importFrom utils tail
#' @importFrom mergen clean_code_blocks executeCode extractCode
#' @importFrom waiter waiter_show waiter_hide spin_ring
#' @noRd
mod_chat_server <- function(id,
                            ide_colors = get_ide_theme_info(),
                            translator = create_translator(),
                            settings,
                            history) {
  # This is where changes will focus
  moduleServer(id, function(input, output, session) {

    # Session data ----
    ns <- session$ns

    # reactive values
    rv <- reactiveValues()
    rv$reset_welcome_message <- 0L
    rv$reset_streaming_message <- 0L
    rv$code_of_last_response <- NULL

    # UI outputs ----

    output$welcome <- renderWelcomeMessage({
      welcomeMessage(ide_colors)
    }) %>%
      bindEvent(rv$reset_welcome_message)


    output$history <- renderUI({
      history$chat_history %>%
        style_chat_history(ide_colors = ide_colors)
    })

    # Observers ----

    observe({
      rv$reset_welcome_message <- rv$reset_welcome_message + 1L
    }) %>%
      bindEvent(history$create_new_chat)


    # chat event
    observe({

      # update chat history with prompt
      new_history <- c(
        history$chat_history,
        list(
          list(role = "user", content = input$chat_input)
        )
      )
      history$chat_history <- new_history
      updateTextAreaInput(session, "chat_input", value = "")

      # get response
      skeleton <- mergenstudio_skeleton(
          api_key = settings$api_key,
          service = settings$service,
          model = settings$model,
          prompt = input$chat_input,
          history = history$chat_history,
          # stream = settings$stream,
          selfcorrect = settings$selfcorrect
      )

      waiter::waiter_show(html = waiter::spin_ring(), color = paste0("rgba(128,128,128,", 0.5, ")"))
      response <- mergenstudio_request(skeleton = skeleton)
      waiter::waiter_hide() # hide the waiter

      # update history with response
      history$chat_history <- response$history
      rv$code_of_last_response <- response$response
      # if (settings$stream) {
      #   rv$reset_streaming_message <- rv$reset_streaming_message + 1L
      # }
      updateTextAreaInput(session, "chat_input", value = "")

    }) %>%
      bindEvent(input$chat)

    # execute code event
    observe({
      # cleaning and parsing the code from response
      if(is.null(rv$code_of_last_response)) {
        showNotification(ui = "You have to get a response with code first!", duration = 3, type = "message", session = session)
      } else {

        # check history
        if(grepl("^Here are the results once the code is executed", utils::tail(history$chat_history,1)[[1]]$content)){
          showNotification(ui = "You have to get another response to execute code again!", duration = 3, type = "message", session = session)
        } else {

          # get code
          code_cleaned <- mergen::clean_code_blocks(rv$code_of_last_response)
          final_code <- mergen::extractCode(code_cleaned,delimiter = "```")
          final_code <- final_code$code



          # execute code
          setwd(settings$directory)
          code_result<-mergen::executeCode(final_code,output="html",output.file=paste0(getwd(),"output_mergen_studio.html"))

          # if html file is created (code did not return any error)
          if (grepl("html",code_result[1])){
            # add result to chat history
            if(length(rvest::read_html(code_result))>1){
              history$chat_history <- c(history$chat_history,
                                        list(list(role = "assistant",
                                                  content = shiny::includeHTML(code_result))
                                        ))
            }else{
              history$chat_history <- c(history$chat_history,
                                        list(list(role = "assistant",
                                                  content = "The code returns no output.")
                                        ))
            }
            # remove html file
            file.remove(code_result)
            # append code to already generated code in chat:
            mergenstudio_env$code <- paste(mergenstudio_env$code,final_code,sep="\n")

            # send via port
            if ("port" %in% names(mergenstudio_env)){
              con = socketConnection(port=mergenstudio_env$port)
              if (exists("con")){
                svSocket::evalServer(con,mergen_code,mergenstudio_env$code)
                close(con)
              }
            }

          }else{
            history$chat_history <- c(history$chat_history,
                                      list(list(role = "assistant",
                                                content = paste0("The code resulted in the following errors/warnings:\n```\n",
                                                                 code_result,"\n```\n\n"))
                                           ))
          }

          updateTextAreaInput(session, "chat_input", value = "")
        }
      }
    }) %>%
      bindEvent(input$execute)

  })
}
