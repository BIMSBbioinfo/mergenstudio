#' @importFrom tools R_user_dir
#' @importFrom yaml write_yaml
#' @noRd
save_user_config <- function(code_style,
                             skill,
                             task,
                             language,
                             service,
                             model,
                             custom_context,
                             # stream,
                             selfcorrect,
                             fileheader) {
  if (is.null(custom_context)) custom_context <- ""
  config <-
    data.frame(
      language,
      service,
      model,
      custom_context,
      # stream,
      selfcorrect,
      fileheader
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
    mergenstudio.language      = config$language,
    mergenstudio.service       = config$service,
    mergenstudio.model         = config$model,
    mergenstudio.custom_context = config$custom_context,
    # mergenstudio.stream        = config$stream,
    mergenstudio.selfcorrect   = config$selfcorrect,
    mergenstudio.selfcorrect   = config$fileheader
  )
  options(op_mergenstudio)
  invisible()
}
