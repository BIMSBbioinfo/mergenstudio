#' @importFrom bslib accordion accordion_panel tooltip
#' @importFrom fontawesome fa
#' @importFrom shinyjs useShinyjs hidden disabled
#' @importFrom shinyFiles shinyDirButton
#' @noRd
mod_settings_ui <- function(id, translator = create_translator(), dir = NULL) {
  ns <- NS(id)

  preferences <- bslib::accordion(
    open = FALSE,
    multiple = FALSE,

    bslib::accordion(open = TRUE,
      bslib::accordion_panel(
        title = "Chat Options",
        icon = fontawesome::fa("sliders"),

        ## custom context ####
        selectizeInput(
          inputId = ns("custom_context"),
          label = getIconLabel(translator$t("Select Context"),
                               message="Optional context to provide alongside with the prompt to help LLM model to help user in different ways. If 'No Context' is selected, no context will be provided. You can also enter a custom context by typing on this box"
          ),
          choices = get_available_context(),
          width = "200px",
          selected = getOption("mergenstudio.custom_context"),
          options = list(create = TRUE)
        ),

        ## directory selection ####
        textInput(inputId = ns("directorytext"),
                  label = getIconLabel("Select Directory",
                                       message="Selecting the working directory for code execution. Once the execute code button is clicked, this directory will be used for reading and saving files."
                  ),
                  value = ifelse(is.null(dir), getwd(), dir)
        ),

        column(4,
               shinyFiles::shinyDirButton(id = ns('directory'),
                                          label = "...", title = "Select Directory",
                                          class = "btn action-button btn-primary",
                                          style = "padding:2px; font-size:90%; width: 100%; margin-bottom: 20px; margin-top: -14px; margin-left: 0px", buttonType = "default")
                                          # style = "width: 60%; height: 50%; margin-bottom: 20px; margin-top: -14px; margin-left: 0px", buttonType = "blue")
        ),

        ## auto execution ####
        radioButtons(
          inputId = ns("autoexecution"),
          label = getIconLabel("Activate Auto execution",
                               message = "Activating auto execution will automatically try to run any code that is generated by the agent"
          ),
          choiceNames = c("Yes", "No"),
          choiceValues = c(TRUE, FALSE),
          selected = getOption("mergenstudio.autoexecution"),
          inline = TRUE,
          width = "200px",
        ),

        ## selfcorrect ####
        radioButtons(
          inputId = ns("selfcorrect"),
          label = getIconLabel("Activate Self Correct",
                               message = "Activating Self Correct will attempt to correct code that is returned by the agent if it results in errors, by resending the prompt together with additional information about the error message."
          ),
          choiceNames = c("Yes", "No"),
          choiceValues = c(TRUE, FALSE),
          selected = getOption("mergenstudio.selfcorrect"),
          inline = TRUE,
          width = "200px",
        ),

        ## fileheader ####
        radioButtons(
          inputId = ns("fileheader"),
          label = getIconLabel("Activate file header addition",
                               message="Activating file header addition will attempt to add the first few lines of files mentioned in your prompt to your prompt. The directory that will be searched can be set in 'select directory'"
          ),
          choiceNames = c("Yes","No"),
          choiceValues = c(TRUE, FALSE),
          selected = getOption("mergenstudio.fileheader"),
          inline = TRUE,
          width = "200px",
        )
      )
    ),

    bslib::accordion(open = FALSE,
      bslib::accordion_panel(
        title = "API Options",
        icon = fontawesome::fa("server"),

        ## service ####
        selectInput(
          inputId = ns("service"),
          label = translator$t("Select API Service"),
          choices = get_api_services(),
          selected = getOption("mergenstudio.service"),
          width = "200px"
        ),

        ## api_key ####
        textInput(
          inputId = ns("api_key"),
          label = translator$t("API Key"),
          value = getOption("mergenstudio.api_key"),
          width = "200px"
        ),

        ## api_url ####
        textInput(
          inputId = ns("api_url"),
          label = getIconLabel(translator$t("API URL (only needed for 'generic')"),
                               message="Provide an API URL. This is only needed when service is set to generic."
                               ),
          value = getOption("mergenstudio.api_url"),
          width = "200px"
        ),

        ## model ####
        selectizeInput(
          inputId = ns("model"),
          label = translator$t("Chat Model"),
          choices = get_available_models(getOption("mergenstudio.service"))[1],
          width = "200px",
          selected = getOption("mergenstudio.model"),
          options = list(create = TRUE)
        ),

        ## nr_tokens ####
        selectizeInput(
          inputId = ns("nr_tokens"),
          label = getIconLabel(translator$t("Nr of tokens"),
                               message="Maximum amount of tokens to send to LLM."),
          choices = seq(3000,10000,by=1000),
          selected = getOption("mergenstudio.nr_tokens")
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

  # buttons
  btn_to_history <- actionButton(
    inputId = ns("to_history"),
    label = fontawesome::fa("arrow-left-long"),
    # class = "mb-3"
    # class = "flex-grow-1 me-2"
    class = "me-2"
    # class = "flex-grow-1"
  ) %>%
    bslib::tooltip("Go to conversations")

  btn_save_as_default <- actionButton(
    inputId = ns("save_default"),
    label = fontawesome::fa("floppy-disk"),
    # class = "mb-3"
    # class = "me-2"
  ) %>%
    bslib::tooltip("Save settings as default")



  # btn_save_in_session <- actionButton(
  #   inputId = ns("save_session"),
  #   label = fontawesome::fa("bookmark"),
  #   # class = "mb-3"
  #   class = "me-2"
  # ) %>%
  #   bslib::tooltip("Save for this session")

  # tag lists, html elements
  tagList(
    shinyjs::useShinyjs(),
    tags$div(
      class = "d-flex mb-1",
      btn_to_history,
      # btn_save_in_session,
      btn_save_as_default
    ),
    br(),
    br(),
    preferences
  )
}

#' @importFrom glue glue
#' @importFrom shinyjs show hide
#' @importFrom shinyFiles shinyDirChoose parseDirPath
#' @importFrom fs path_home
#' @noRd
mod_settings_server <- function(id, dir = NULL) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    rv <- reactiveValues()
    rv$selected_history <- 0L # originally 0L
    rv$modify_session_settings <- 0L
    rv$create_new_chat <- 0L

    ## choose directory ####
    rv$directory <- ifelse(is.null(dir), getwd(), dir)
    volumes <- c(ifelse(is.null(dir), getwd(), dir), fs::path_home())
    names(volumes) <- lapply(volumes, function(x){
      names_volumes <- strsplit(x, split = "\\/")[[1]]
      return(names_volumes[length(names_volumes)])
    })
    if(volumes[[1]]==volumes[[2]]) volumes <- volumes[1]
    shinyFiles::shinyDirChoose(input = input, id = "directory", session = session, roots = volumes)

    observeEvent(input$directory, {
      updateTextInput(session, "directorytext", value = parseDirPath(roots=volumes, selection=input$directory))
      path <- shinyFiles::parseDirPath(roots = volumes, selection = input$directory)
      rv$directory <- as.character(path)
    })
    observeEvent(input$directorytext, {
      if(dir.exists(input$directorytext)){
        showNotification("Directory is set correctly!", type = "message")
        rv$directory <- input$directorytext
      } else {
        showNotification("Directory doesnt exist! abort ...", type = "error")
        rv$directory <- getwd()
      }
    })

    ## hide api_url ####
    observe({
      # if api_url != generic, hide the api_url
      if(input$service != "generic"){
        updateTextInput(session, "api_url", value = "")
        shinyjs::hideElement("api_url")
      } else {
        shinyjs::showElement("api_url")
      }
    }) %>%
      bindEvent(input$service)

    ## go back to history ####
    observe({
      rv$selected_history <- rv$selected_history + 1L
    }) %>%
      bindEvent(input$to_history)

    ## save_default ####
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

      # save config
      save_user_config(
        model = input$model,
        service = input$service,
        api_url = input$api_url,
        api_key = input$api_key,
        custom_context = input$custom_context,
        selfcorrect = input$selfcorrect,
        fileheader = input$fileheader,
        autoexecution = input$autoexecution,
        nr_tokens = input$nr_tokens,
        language = input$language
      )

      rv$modify_session_settings <- rv$modify_session_settings + 1L

      removeModal(session)

      showNotification("Defaults updated", duration = 3, type = "message", session = session)
    }) %>% bindEvent(input$confirm_default)




    # main observe
    observe({

      msg <- glue::glue("Fetching models for {input$service} service...")
      showNotification(ui = msg, type = "message", duration = 3, session = session)

      models <- get_available_models(input$service, token = input$api_key)

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
      bindEvent(input$service, input$api_key)

    observe({
      rv$model <- input$model
      rv$service <- input$service
      rv$api_key <- input$api_key
      rv$api_url <- input$api_url
      rv$custom_context <- input$custom_context
      rv$selfcorrect <- as.logical(input$selfcorrect)
      rv$fileheader <- as.logical(input$fileheader)
      rv$autoexecution <- as.logical(input$autoexecution)
      rv$nr_tokens <- input$nr_tokens
    })
    ## Module output ----
    rv
  })
}

get_available_context <- function(){
  c("actAs", "rbionfoExp", "CoT", "simple", "No Context")
}

get_api_services <- function(){
  c("openai", "replicate", "generic")
}
