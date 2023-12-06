#' @importFrom bslib accordion accordion_panel tooltip
#' @importFrom fontawesome fa
mod_settings_ui <- function(id, translator = create_translator()) {
  ns <- NS(id)

  api_services <- c("openai-chat", "openai-completion", "replicate")

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
          choices = NULL,
          width = "200px",
          selected = getOption("mergenstudio.model")
        ),
        # radioButtons(
        #   inputId = ns("stream"),
        #   label = "Stream Response",
        #   choiceNames = c("Yes", "No"),
        #   choiceValues = c(TRUE, FALSE),
        #   inline = TRUE,
        #   width = "200px",
        # ),
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

  btn_to_history <- actionButton(
    inputId = ns("to_history"),
    label = fontawesome::fa("arrow-left-long"),
    class = "mb-3"
  ) %>%
    bslib::tooltip("Back to history")

  btn_save_as_default <- actionButton(
    inputId = ns("save_default"),
    label = fontawesome::fa("floppy-disk"),
    class = "mb-3"
  ) %>%
    bslib::tooltip("Save as default")

  btn_save_in_session <- actionButton(
    inputId = ns("save_session"),
    label = fontawesome::fa("bookmark"),
    class = "mb-3"
  ) %>%
    bslib::tooltip("Save for this session")

  tagList(
    btn_to_history,
    btn_save_in_session,
    btn_save_as_default,

    preferences

  )
}

#' @importFrom glue glue
mod_settings_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    rv <- reactiveValues()
    rv$selected_history <- 0L
    rv$modify_session_settings <- 0L
    rv$create_new_chat <- 0L
    rv$directory <- 0L
    api_services <- c("openai-chat", "openai-completion", "replicate")

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


    observe({
      rv$selected_history <- rv$selected_history + 1L
    }) %>%
      bindEvent(input$to_history)


    observe({
      showModal(modalDialog(
        tags$p("These settings will persist for all your future chat sessions."),
        tags$p("After changing the settings a new chat will be created."),

        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_default"), "Ok")
        )
      ))
    }) %>%
      bindEvent(input$save_default)


    observe({
      if (!isTruthy(input$confirm_default)) return()

      save_user_config(
        language = input$language,
        service = input$service,
        model = input$model,
        custom_prompt = input$custom_prompt,
        # stream = input$stream,
        selfcorrect = input$selfcorrect
      )

      rv$modify_session_settings <- rv$modify_session_settings + 1L

      removeModal(session)

      showNotification("Defaults updated", duration = 3, type = "message", session = session)
    }) %>% bindEvent(input$confirm_default)


    observe({
      showModal(modalDialog(

        tags$p("After changing the settings a new chat will be created."),

        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_session"), "Ok")
        )
      ))
    }) %>%
      bindEvent(input$save_session)

    observe({
      if (!isTruthy(input$confirm_session)) return()

      rv$modify_session_settings <- rv$modify_session_settings + 1L
      removeModal(session)
    }) %>%
      bindEvent(input$confirm_session)

    observe({
      rv$model <- input$model %||% getOption("mergenstudio.model")
      rv$service <- input$service %||% getOption("mergenstudio.service")
      rv$api_key <- input$api_key
      # rv$stream <- as.logical(input$stream %||% getOption("mergenstudio.stream"))
      rv$custom_prompt <- input$custom_prompt %||% getOption("mergenstudio.custom_prompt")
      rv$selfcorrect <- as.logical(input$selfcorrect %||% getOption("mergenstudio.selfcorrect"))
      rv$create_new_chat <- rv$create_new_chat + 1L
    }) %>%
      bindEvent(rv$modify_session_settings)

    ## Module output ----
    rv

  })
}
