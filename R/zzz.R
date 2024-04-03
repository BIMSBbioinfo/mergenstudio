#' @importFrom tools R_user_dir
#' @importFrom yaml read_yaml
.onLoad <- function(lib, pkg) {
  user_config <-
    file.path(tools::R_user_dir("mergenstudio", which = "config"), "config.yml")

  if (file.exists(user_config)) {
    config <- yaml::read_yaml(user_config)
  } else {
    config <- yaml::read_yaml(system.file("rstudio/config.yml", package = "mergenstudio"))
  }

  op <- options()
  op_mergenstudio <- list(
    mergenstudio.valid_api      = FALSE,
    mergenstudio.max_tokens     = 500,
    mergenstudio.openai_url     = "https://api.openai.com/v1",
    mergenstudio.model          = config$model,
    mergenstudio.service        = config$service,
    mergenstudio.api_url        = config$api_url,
    mergenstudio.api_key        = ifelse(config$api_key == "", Sys.getenv("AI_API_KEY"), config$api_key),
    mergenstudio.custom_context = config$custom_context,
    mergenstudio.selfcorrect    = config$selfcorrect,
    mergenstudio.fileheader     = config$fileheader,
    mergenstudio.autoexecution  = config$autoexecution,
    mergenstudio.nr_tokens      = config$nr_tokens,
    mergenstudio.language       = config$language
  )
  toset <- !(names(op_mergenstudio) %in% names(op))
  if (any(toset)) options(op_mergenstudio[toset])
  invisible()
}

utils::globalVariables(".rs.invokeShinyPaneViewer")
