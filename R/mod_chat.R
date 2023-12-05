#' Chat UI
#'
#' @param id id of the module
#' @param translator A Translator from `shiny.i18n::Translator`
#'
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
          uiOutput(ns("history")),
          streamingMessageOutput(ns("streaming")),
          # uiOutput(ns("streaming"))
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
#'
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


    output$streaming <- renderStreamingMessage({
      # This has display: none by default. It is only shown when receiving a stream
      # After the stream is completed, it will reset.
      streamingMessage(ide_colors)
    }) %>%
      bindEvent(rv$reset_streaming_message)


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
      response <- mergenstudio_request(skeleton = skeleton)

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
          code_cleaned <- mergen::clean_code_blocks(rv$code_of_last_response)
          final_code <- mergen::extractCode(code_cleaned,delimiter = "```")
          final_code <- final_code$code
          code_result <- mergen::executeCode(final_code)

          # update history
          history$chat_history <- c(history$chat_history,
                                    list(list(role = "assistant",
                                              content = paste0("Here are the results once the code is executed:\n\n```\n", paste(code_result, collapse = "\n"), "\n```\n\n"))
                                    ))
          updateTextAreaInput(session, "chat_input", value = "")
        }
      }
    }) %>%
      bindEvent(input$execute)

  })
}
