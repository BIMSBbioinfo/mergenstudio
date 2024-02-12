#' @importFrom tools R_user_dir
#' @importFrom yaml write_yaml
#' @noRd
save_user_config <- function(model,
                             service,
                             api_url,
                             custom_context,
                             selfcorrect,
                             fileheader,
                             autoexecution,
                             nr_tokens) {
  if (is.null(custom_context)) custom_context <- ""
  config <-
    data.frame(
      model,
      service,
      api_url,
      custom_context,
      selfcorrect,
      fileheader,
      autoexecution,
      nr_tokens
    )
  user_config_path <- tools::R_user_dir("mergenstudio", which = "config")
  user_config <- file.path(user_config_path, "config.yml")
  if (!dir.exists(user_config_path)) {
    dir.create(user_config_path, recursive = TRUE)
  }
  yaml::write_yaml(config, user_config)
  set_user_options(config)
}

set_user_options <- function(config) {
  op <- options()

  op_mergenstudio <- list(
    mergenstudio.model          = config$model,
    mergenstudio.service        = config$service,
    mergenstudio.api_url        = config$api_url,
    mergenstudio.custom_context        = config$custom_context,
    mergenstudio.selfcorrect    = config$selfcorrect,
    mergenstudio.fileheader     = config$fileheader,
    mergenstudio.autoexecution        = config$autoexecution,
    mergenstudio.nr_tokens        = config$nr_tokens
  )
  options(op_mergenstudio)
  invisible()
}
