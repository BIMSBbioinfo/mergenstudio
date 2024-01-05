#' @importFrom bslib accordion accordion_panel tooltip
#' @importFrom fontawesome fa
#' @importFrom shinyjs useShinyjs hidden
#' @noRd
mod_settings_ui <- function(id, translator = create_translator()) {
  ns <- NS(id)

  shinyjs::useShinyjs()

  # api_services <- c("openai-chat", "openai-completion", "replicate")
  api_services <- c("openai", "replicate", "generic")

  preferences <- bslib::accordion(
    open = FALSE,
    multiple = FALSE,

    bslib::accordion(open = TRUE,
      bslib::accordion_panel(
        title = "Chat Options",
        icon = fontawesome::fa("sliders"),

        # selectInput(
        #   inputId = ns("promptcontext"),
        #   label = getIconLabel(translator$t("Select Context"),
        #                        message="Optional context to provide alongside with the prompt to help LLM model to help user in different ways."
        #   ),
        #   choices = context_choices,
        #   #selected = getOption("mergenstudio.service"),
        #   selected = NULL,
        #   width = "200px"
        # ),

        selectizeInput(
          inputId = ns("custom_context"),
          label = getIconLabel(translator$t("Select Context"),
                               message="Optional context to provide alongside with the prompt to help LLM model to help user in different ways."
          ),
          choices = get_available_context(),
          width = "200px",
          selected = NULL,
          options = list(create = TRUE)
        ),


        directoryInput(ns('directory'),
                       label = getIconLabel("Select Directory",
                                            message="Selecting the working directory for code execution. Once the execute code button is clicked, this directory will be used for reading and saving files."
                       ),
                       value = getwd()),
        radioButtons(
          inputId = ns("selfcorrect"),
          label = getIconLabel("Activate Self Correct",
                               message = "Activating Self Correct will attempt to correct code that is returned by the agent if it results in errors, by resending the prompt together with additional information about the error message."
          ),
          choiceNames = c("Yes", "No"),
          choiceValues = c(TRUE, FALSE),
          selected = FALSE,
          inline = TRUE,
          width = "200px",
        ),
        radioButtons(
          inputId = ns("fileheader"),
          label = getIconLabel("Activate file header addition",
                               message="Activating file header addition will attempt to add the first few lines of files mentioned in your prompt to your prompt. The directory that will be searched can be set in 'Execute options'"
          ),
          choiceNames = c("Yes","No"),
          choiceValues = c(TRUE, FALSE),
          selected = FALSE,
          inline = TRUE,
          width = "200px",
        )
      )
    ),

    bslib::accordion(open = FALSE,
      bslib::accordion_panel(
        title = "API Options",
        icon = fontawesome::fa("server"),

        selectInput(
          inputId = ns("service"),
          label = translator$t("Select API Service"),
          choices = api_services,
          selected = getOption("mergenstudio.service"),
          width = "200px"
        ),
        textInput(
          inputId = ns("api_key"),
          label = translator$t("API Key"),
          value = "",
          width = "200px"
        ),
        textInput(
          inputId = ns("api_url"),
          label = getIconLabel(translator$t("API URL"),
                               message="Provide an API URL. This is only needed when service is set to generic."
                               ),
          value = "",
          width = "200px"
        ),
        selectizeInput(
          inputId = ns("model"),
          label = translator$t("Chat Model"),
          choices = get_available_models(getOption("mergenstudio.service"))[1],
          width = "200px",
          selected = getOption("mergenstudio.model"),
          options = list(create = TRUE)
        )
      )
    ),

    bslib::accordion_panel(
      title = "UI options",
      icon = fontawesome::fa("sliders"),

      selectInput(
        inputId = ns("language"),
        # label = translator$t("Language"), # TODO: update translator
        label = "Language",
        choices = c("en", "es", "de"),
        width = "200px",
        selected = getOption("mergenstudio.language")
      )
    )
  )

  tagList(
    br(),
    br(),
    preferences

  )
}

#' @importFrom glue glue
#' @importFrom shinyjs show hide
#' @noRd
mod_settings_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    rv <- reactiveValues()
    rv$selected_history <- 0L # originally 0L
    rv$modify_session_settings <- 0L
    rv$create_new_chat <- 0L
    rv$directory <- 0L
    # api_services <- c("openai-chat", "openai-completion", "replicate")
    api_services <- c("openai", "replicate", "generic")

    # # hide api_url
    # observe({
    #   print("observe")
    #   shinyjs::hide(ns('api_url'))
    # })

    # choose directory
    observeEvent(
      ignoreNULL = TRUE,
      eventExpr = {
        input$directory
      },
      handlerExpr = {
        if (input$directory > 0) {
          path = choose.dir(default = readDirectoryInput(session, ns('directory')),
                            caption="Choose a directory...")
          updateDirectoryInput(session, ns('directory'), value = path)
        } else {
          path <- getwd()
        }
        rv$directory <- path
      }
    )
    output$directory = renderText({
      readDirectoryInput(session, ns('directory'))
    })

    # main observe
    observe({

      # if(isolate(input$service) == "generic"){
      #   # print("generic")
      #   shinyjs::show(ns('api_url'), asis = TRUE)
      # } else {
      #   # print("hide")
      #   shinyjs::hide(ns('api_url'), asis = TRUE)
      # }
      if(isolate(input$service) != "generic"){
        updateTextInput(session, "api_url", value = "")
      }

      msg <- glue::glue("Fetching models for {input$service} service...")
      showNotification(ui = msg, type = "message", duration = 3, session = session)

      models <- get_available_models(input$service)

      if (length(models) > 0) {
        showNotification(ui = "Got models!", duration = 3, type = "message", session = session)

        updateSelectizeInput(
          session = session,
          inputId = "model",
          choices = models,
          selected = models[1]
        )

      } else {
        showNotification(ui = "No models available", duration = 3, type = "error", session = session)

        updateSelectizeInput(
          session = session,
          inputId = "model",
          choices = character(),
          selected = NULL
        )
      }
    }) %>%
      bindEvent(input$service)

    # # self correct cannot be used with completion models
    # observe({
    #   if(input$service == "openai-completion" && input$selfcorrect == TRUE){
    #     showNotification(ui = "selfcorrect cannot be used with type completion. Can only be used with type chat", duration = 3, type = "error", session = session)
    #     updateRadioButtons(
    #       session = session,
    #       inputId = "selfcorrect",
    #       choiceNames = c("Yes", "No"),
    #       choiceValues = c(TRUE, FALSE),
    #       selected = FALSE,
    #     )
    #   }
    # }) %>%
    #   bindEvent(input$service, input$selfcorrect)


    observe({
      rv$model <- input$model %||% getOption("mergenstudio.model")
      rv$service <- input$service %||% getOption("mergenstudio.service")
      rv$api_key <- input$api_key
      rv$api_url <- input$api_url
      rv$custom_context <- input$custom_context %||% getOption("mergenstudio.custom_context")
      rv$selfcorrect <- as.logical(input$selfcorrect %||% getOption("mergenstudio.selfcorrect"))
      rv$fileheader <- as.logical(input$fileheader %||% getOption("mergenstudio.fileheader"))
      print(rv$custom_context)
    })

    ## Module output ----
    rv

  })
}

get_available_context <- function(){
  c("actAs", "rbionfoExp", "CoT", "simple", "No Context")
}
