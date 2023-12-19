#' @importFrom bslib accordion accordion_panel tooltip
#' @importFrom fontawesome fa
#' @noRd
mod_settings_ui <- function(id, translator = create_translator()) {
  ns <- NS(id)

  # api_services <- c("openai-chat", "openai-completion", "replicate")
  api_services <- c("openai", "replicate")

  preferences <- bslib::accordion(
    open = FALSE,
    multiple = FALSE,

    bslib::accordion(open = TRUE,
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
        selectInput(
          inputId = ns("model"),
          label = translator$t("Chat Model"),
          choices = get_available_models(getOption("mergenstudio.service"))[1],
          width = "200px",
          selected = getOption("mergenstudio.model")
        ),
        radioButtons(
          inputId = ns("selfcorrect"),
          label = "Activate Self Correct",
          choiceNames = c("Yes", "No"),
          choiceValues = c(TRUE, FALSE),
          selected = FALSE,
          inline = TRUE,
          width = "200px",
        )
      )
    ),

    bslib::accordion_panel(
      title = "Execute Options",
      icon = fontawesome::fa("sliders"),

      directoryInput(ns('directory'), label = 'Select Directory', value = getwd()),
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
    api_services <- c("openai", "replicate")

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

      msg <- glue::glue("Fetching models for {input$service} service...")
      showNotification(ui = msg, type = "message", duration = 3, session = session)

      models <- get_available_models(input$service)

      if (length(models) > 0) {
        showNotification(ui = "Got models!", duration = 3, type = "message", session = session)

        updateSelectInput(
          session = session,
          inputId = "model",
          choices = models,
          selected = models[1]
        )

      } else {
        showNotification(ui = "No models available", duration = 3, type = "error", session = session)

        updateSelectInput(
          session = session,
          inputId = "model",
          choices = character(),
          selected = NULL
        )
      }
    }) %>%
      bindEvent(input$service)

    # self correct cannot be used with completion models
    observe({
      if(input$service == "openai-completion" && input$selfcorrect == TRUE){
        showNotification(ui = "selfcorrect cannot be used with type completion. Can only be used with type chat", duration = 3, type = "error", session = session)
        updateRadioButtons(
          session = session,
          inputId = "selfcorrect",
          choiceNames = c("Yes", "No"),
          choiceValues = c(TRUE, FALSE),
          selected = FALSE,
        )
      }
    }) %>%
      bindEvent(input$service, input$selfcorrect)


    observe({
      rv$model <- input$model %||% getOption("mergenstudio.model")
      rv$service <- input$service %||% getOption("mergenstudio.service")
      rv$api_key <- input$api_key
      rv$custom_prompt <- input$custom_prompt %||% getOption("mergenstudio.custom_prompt")
      rv$selfcorrect <- as.logical(input$selfcorrect %||% getOption("mergenstudio.selfcorrect"))
    })

    ## Module output ----
    rv

  })
}
