.onLoad <- function(lib, pkg) {
  user_config <-
    file.path(tools::R_user_dir("mergenstudio", which = "config"), "config.yml")

  if (file.exists(user_config)) {
    config <- yaml::read_yaml(user_config)
  } else {
    config <- yaml::read_yaml(system.file("rstudio/config.yml",
                                          package = "mergenstudio"))
  }

  op <- options()

  op_mergenstudio <- list(
    mergenstudio.valid_api     = FALSE,
    mergenstudio.max_tokens    = 500,
    mergenstudio.openai_url    = "https://api.openai.com/v1",
    # mergenstudio.code_style    = config$code_style,
    # mergenstudio.skill         = config$skill,
    # mergenstudio.task          = config$task,
    mergenstudio.language      = config$language,
    mergenstudio.service       = config$service,
    mergenstudio.model         = config$model,
    mergenstudio.custom_context = config$custom_context,
    # mergenstudio.stream        = config$stream,
    mergenstudio.selfcorrect   = config$selfcorrect
  )
  toset <- !(names(op_mergenstudio) %in% names(op))
  if (any(toset)) options(op_mergenstudio[toset])
  invisible()
}

utils::globalVariables(".rs.invokeShinyPaneViewer")
