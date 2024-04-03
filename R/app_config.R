#' @importFrom tools R_user_dir
#' @importFrom yaml write_yaml
#' @noRd
save_user_config <- function(model,
                             service,
                             api_url,
                             api_key,
                             custom_context,
                             selfcorrect,
                             fileheader,
                             autoexecution,
                             nr_tokens,
                             language) {

  # context
  if (is.null(custom_context)) custom_context <- ""

  # API KEY
  # Sys.setenv(AI_API_KEY = api_key)
  # add_variable_r_environ("AI_API_KEY", api_key, )

  # other options
  config <-
    data.frame(
      model,
      service,
      api_url,
      api_key,
      custom_context,
      selfcorrect,
      fileheader,
      autoexecution,
      nr_tokens,
      language
    )
  user_config_path <- tools::R_user_dir("mergenstudio", which = "config")
  user_config <- file.path(user_config_path, "config.yml")
  if (!dir.exists(user_config_path)) {
    dir.create(user_config_path, recursive = TRUE)
  }
  yaml::write_yaml(config, user_config)

  # save options
  set_user_options(config, api_key)
}

set_user_options <- function(config, api_key) {
  op <- options()
  op_mergenstudio <- list(
    mergenstudio.model          = config$model,
    mergenstudio.service        = config$service,
    mergenstudio.api_url        = config$api_url,
    mergenstudio.api_key        = config$api_key,
    mergenstudio.custom_context = config$custom_context,
    mergenstudio.selfcorrect    = config$selfcorrect,
    mergenstudio.fileheader     = config$fileheader,
    mergenstudio.autoexecution  = config$autoexecution,
    mergenstudio.nr_tokens      = config$nr_tokens,
    mergenstudio.language       = config$language
  )
  options(op_mergenstudio)
  invisible()
}

# # from https://github.com/r-lib/usethis/issues/1448
# r_environ_path = function(scope = c("user", "project")) {
#   path <- usethis:::scoped_path_r(scope, ".Renviron", envvar = "R_ENVIRON_USER")
# }
#
# # from https://github.com/r-lib/usethis/issues/1448
# add_variable_r_environ = function(envvar, value) {
#
#   # get environmental variables
#   path <- r_environ_path(scope = "user")
#   values = readLines(path, warn = FALSE)
#
#   # environmental variables
#   env = Sys.getenv()
#   if (envvar %in% names(env)) {
#
#   } else {
#     out_value = paste0(envvar, ' = "', value, '"')
#     values = c(values, out_value)
#     writeLines(values, path, sep = "\n")
#   }
# }
