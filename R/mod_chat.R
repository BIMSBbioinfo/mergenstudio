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
          htmltools::div(
            class = "d-flex p-3",
            div(
              class = "flex-grow-1 pe-3",
              # text_area_input_wrapper(
              #   inputId = ns("chat_input"),
              #   label = NULL,
              #   width = "100%",
              #   placeholder = translator$t("Write your prompt here"),
              #   value = "",
              #   resize = "vertical",
              #   rows = 5,
              #   textarea_class = "chat-prompt"
              # )
              text_area_input_wrapper(
                inputId = ns("chat_input"),
                label = NULL,
                width = "100%",
                placeholder = translator$t("Write your prompt here"),
                value = "",
                rows = 4,
                resize = "none",
                textarea_class = "chat-prompt"
              )
            ),
            div(
              style = htmltools::css(width = "30px"),
              # actionButton(
              #   inputId = ns("chat"),
              #   label = icon("fas fa-paper-plane"),
              #   class = "w-100 btn-primary p-1 chat-send-btn"
              # ),
              # actionButton(
              #   inputId = ns("clear_history"),
              #   label = icon("eraser"),
              #   class = "w-100 btn-primary mt-2 p-1"
              # ),
              # actionButton(
              #   inputId = ns("settings"),
              #   label = icon("gear"),
              #   class = "w-100 btn-primary mt-2 p-1"),

              # send prompt button
              actionButton(
                inputId = ns("chat"),
                label = icon("fas fa-paper-plane"),
                class = "w-100 btn-primary p-1 mt-2 chat-send-btn"
              ) %>%
                bslib::tooltip("Send (click or Enter)"),
              # clear history button
              actionButton(
                inputId = ns("clear_history"),
                label = icon("fas fa-trash"),
                class = "w-100 h-10 btn-primary p-1 mt-2",
                style="background-color: red; border-color: red"
              ) %>%
                bslib::tooltip("Remove History"),
              actionButton(
                inputId = ns("save_conversation"),
                label = fontawesome::fa("comments"),
                class='w-100 h-10 btn-primary p-1 mt-2',
                style="border-color: white; background-color: white;"
              ) %>%
                bslib::tooltip("Save conversation")
            ),
            #codejs and responsejs are textareas which are hidden,
            #which can only be manipulated by CopyToClipboard.js and will change
            # in value once an execution click event is observed.
            textAreaInput(
              inputId=ns("codejs"),
              label = NULL,
              value=""
            ),
            textAreaInput(
              inputId=ns("responsejs"),
              label = NULL,
              value=""
            )
          )
        )
        # div(
        #   class = "mt-auto",
        #   style = css(
        #     "margin-left" = "40px",
        #     "margin-right" = "40px"
        #   ),
        #   htmltools::div(
        #     class = "position-relative",
        #     style = css(
        #       "width" = "100%"
        #     ),
        #     div(
        #       text_area_input_wrapper(
        #         inputId = ns("chat_input"),
        #         label = NULL,
        #         width = "100%",
        #         placeholder = translator$t("Write your prompt here"),
        #         value = "",
        #         resize = "none",
        #         textarea_class = "chat-prompt"
        #       )
        #     ),
        #     div(
        #       class = "position-absolute top-50 end-0 translate-middle",
        #
        #       # send prompt button
        #       actionButton(
        #         inputId = ns("chat"),
        #         label = icon("fas fa-paper-plane"),
        #         class = "w-40 btn-primary p-1 chat-send-btn"
        #       ) %>%
        #         bslib::tooltip("Send (click or Enter)"),
        #
        #       # execute code button
        #       actionButton(
        #         inputId = ns("execute"),
        #         label = icon("fas fa-play"),
        #         class = "w-55 btn-primary p-1",
        #         style="background-color: green; border-color: green"
        #       ) %>%
        #         bslib::tooltip("Execute Code")
        #     )
        #   )
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
#' @importFrom rvest read_html
#'
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
    rv$last_response <- NULL

    hide("responsejs")
    hide("codejs")


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

    # execution event: when button is clicked. Input controlled by JavaScript
    # in CopyToClipboard.js
    observe(
      {
        if (input$responsejs != ""){
          waiter::waiter_show(html =  tagList(waiter::spin_ring(),"Running code and appending result...."), color = paste0("rgba(128,128,128,", 0.15, ")"))
          fileVal <- check_data(input$codejs)
          mergenstudio_execute(rv, history, settings, session,code=input$codejs,rep=input$responsejs)

          if (is.list(fileVal) && !is_empty(fileVal)){
            empty_obs <- which(fileVal[[1]] == TRUE)
            history$chat_history[length(history$chat_history)+1]<- list(list(role = "user",
                                                                            content = paste("the following objects which
                                                                                            were created are empty: ", names(empty_obs))))
          }



          waiter::waiter_hide()
          }
        }) %>%
      bindEvent(input$codejs)


    # chat event
    observe({

      # save prompt as variable
      chat_input <- paste0("<query>",input$chat_input,"</query>")
      fileheaders<-NULL

      # if adding fileheaders is set to TRUE:
      if (settings$fileheader == TRUE){
        filenames<-mergen::extractFilenames(input$chat_input)
        if (!is.na(filenames[1])){
          filenames<-unique(filenames)
          for (file in filenames){
            final_path <- paste0(settings$directory,"/",file)
            result <- tryCatch({
              mergen::fileHeaderPrompt(final_path)
              }, warning = function(w) {
                paste0("WARNING: ",w)
              }, error = function(e) {
                paste0("ERROR: ",e)
              }
            )
            if (grepl("^WARNING:",result) | grepl("^ERROR:",result)){

              resp <- paste0("Header of file ",file,
                             " could not be added. Are you in the correct directory?
                             Will proceed without addition.")

              showNotification(ui = resp, duration = 4, type = "message", session = session)

            } else{
            fileheaders<- paste(fileheaders,result,sep='\n')
            #chat_input <- paste(chat_input,"<fileinfo>", result, "</fileinfo>",sep="\n")
            showNotification(ui = paste("Header of file ", file, "added."), duration = 3, type = "message", session = session)
            }
          }
        }
      }

      if (!is.null(fileheaders)){
        chat_input<-paste(chat_input,'<fileinfo>',fileheaders,"</fileinfo>",sep='\n')
      }

      #calculate how much of the history to send with
      # 4 characters of text is ~1 token
      #delete all messages which hold output of code:
      history_to_send<-history$chat_history
      del<-c()
      chat_length<-length(history_to_send)
      if (chat_length>0){
        for (i in 1:chat_length){
          curr_chat<-history_to_send[i][[1]]
          if (grepl("<pre><code>##", curr_chat$content) | grepl("<p><img",curr_chat$content)){
            del <- c(del,i)
          }
        }
      }
      if (length(del)>0){
        history_to_send<-history_to_send[-del]
      }

      #of history without output code,
      # calculate max messages to send with
      tokens<-as.numeric(settings$nr_tokens)
      char_amnt<-tokens * 4 - nchar(chat_input)
      msg_amnt<-length(history_to_send)
      count<-0

      if (msg_amnt!=0 & char_amnt>0){
        while(count<char_amnt & msg_amnt>1){
          curr_chat<-history_to_send[msg_amnt][[1]]
          extra<-nchar(curr_chat$content)
          if (extra+count>char_amnt){
            count=char_amnt
          }else{
            count=extra+count
            msg_amnt<-msg_amnt-1
          }
        }
      }

      #get last bit of history which is under the max amnt of tokens
      history_to_send<-history_to_send[msg_amnt:length(history_to_send)]

      # set directory for potential code running with selfcorrect and get response
      setwd(settings$directory)
      skeleton <- mergenstudio_skeleton(
          api_key = settings$api_key,
          service = settings$service,
          url = settings$api_url,
          model = settings$model,
          prompt = chat_input,
          custom_context = settings$custom_context,
          history = history_to_send,
          # stream = settings$stream,
          selfcorrect = settings$selfcorrect
      )


        waiter::waiter_show(html = tagList(waiter::spin_ring(),"Receiving response...."), color = paste0("rgba(128,128,128,", 0.15, ")"))
        response <- mergenstudio_request(skeleton = skeleton)
        waiter::waiter_hide() # hide the waiter


      # update history with prompt, potential selfcorrect message and response
      #prompt
      history$chat_history[length(history$chat_history)+1]<- list(list(role = "user",
                                                                       content = input$chat_input
      ))
      #self correct message
      if (settings$selfcorrect){

        #last response
        last_response <- response$history[length(response$history)]
        history$chat_history[length(history$chat_history)+1]<- last_response

        waiter::waiter_show(html = tagList(waiter::spin_ring(),"Self-correct activated...."), color = paste0("rgba(128,128,128,", 0.15, ")"))
        Sys.sleep(1)
        waiter::waiter_hide()


        # messsage selfcorrect is getting activated
        history$chat_history <- c(
          history$chat_history,
          list(
            list(role = "assistant", content = "Self Correct is activated: trying to correct potential errors...")
          )
        )
      }else{
        #last response
        last_response <- response$history[length(response$history)]
        history$chat_history[length(history$chat_history)+1]<- last_response
      }

      rv$last_response <- response$response

      # if auto execution is on:
      if (settings$autoexecution==TRUE){
          waiter::waiter_show(html = tagList(waiter::spin_ring(), "Running code and appending results...."), color = paste0("rgba(128,128,128,", 0.15, ")"))
          mergenstudio_execute(rv,history,settings,session)
          waiter::waiter_hide()
      }

      # update text area
      updateTextAreaInput(session, "chat_input", value = "")


    }) %>%
      bindEvent(input$chat)


    # remove history
    observe({
      if(length(history$chat_history) > 0){
        # history$chat_history <- history$chat_history[1]
        history$chat_history <- list()
        updateTextAreaInput(session, "chat_input", value = "")
      }
    }) %>%
      bindEvent(input$clear_history)



    observe({
      showModal(modalDialog(
        tags$p("You are about to save this chat conversation"),
        tags$p("You can find your saved conversation in your current directory under conversation.Rmd and conversation.html"),

        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_save"), "Ok")
        )
        ))

    }) %>%
      bindEvent(input$save_conversation)

    observe({
      filename <- "conversation.Rmd"
      counter <- 1
      while (file.exists(filename)){
        filename <- paste0("conversation_", counter,".Rmd")
        counter <- counter + 1
      }
      unlisted <- paste(unlist(unlist(history$chat_history)),collapse=" \n\n ")
      unlisted <- gsub ("user", "__user__",unlisted)
      unlisted <- gsub ("assistant", "__assistant__",unlisted)

      writeLines(unlisted, filename)
      suppressWarnings(rmarkdown::render(filename))
      removeModal(session)

    }) %>%
      bindEvent(input$confirm_save)



    #observe a chat event again for selfcorrect purposes
    observeEvent(input$chat,
     {
       if (settings$selfcorrect &
           history$chat_history[length(history$chat_history)][[1]]$role=="assistant"){

         # save response:
         resp_before_correct<- history$chat_history[(length(history$chat_history)-1)][[1]]$content

         # build skeleton
         skeleton <- mergenstudio_skeleton(
         api_key = settings$api_key,
         service = settings$service,
         url = settings$api_url,
         model = settings$model,
         prompt = input$chat_input,
         custom_context = settings$custom_context,
         history = list(list()),
         # stream = settings$stream,
         selfcorrect = settings$selfcorrect
       )

      # send to recieve response
         waiter::waiter_show(html = tagList(waiter::spin_ring(),"Correcting potential errors...."), color = paste0("rgba(128,128,128,", 0.15, ")"))
         response <- mergenstudio_request(skeleton, resp=resp_before_correct)$response
         waiter::waiter_hide()

         # update history
         if (response == resp_before_correct){
           showNotification(ui = "No errors found. Selfcorrect not activated", duration = 4, type = "message", session = session)
           history$chat_history <- c(history$chat_history[1:(length(history$chat_history)-1)])
         } else {
           history$chat_history <- c(history$chat_history,
                                   list(list(role='assistant',
                                             content='The corrected answer is:')),
                                   list(list(role='assistant',
                                             content=response))
                                    )
         }

         # if auto execution is on:
         if (settings$autoexecution==TRUE){
           waiter::waiter_show(html = tagList(waiter::spin_ring(), "Running code and appending results...."), color = paste0("rgba(128,128,128,", 0.15, ")"))
           mergenstudio_execute(rv,history,settings,session)
           waiter::waiter_hide()
         }


       }
     })
  })



}
