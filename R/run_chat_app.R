mergenstudio_env <- new.env()
mergenstudio_env$code <- ""
mergenstudio_env$port <- 8888

#' Run the Mergen Chat app
#'
#' This functions starts the mergen chat as a regular shiny application. The same app could be called using the RStudio Add-in and the \code{mergenchat()} function.
#'
#' @param ide_colors List containing the colors of the IDE theme.
#'
#' @inheritParams shiny::runApp
#'
#' @export
#'
#' @return This function has no return value.
#'
#' @examples
#' \dontrun{
#' run_chat_app()
#' }
run_chat_app <- function(ide_colors = get_ide_theme_info(),
                         host = getOption("shiny.host", "127.0.0.1"),
                         port = getOption("shiny.port")){

  ui <- mod_app_ui("app", ide_colors)

  server <- function(input, output, session) {
    mod_app_server("app", ide_colors)
    session$onSessionEnded(function() {
      stopApp()
    })
  }

  shiny::shinyApp(ui, server, options = list(host = host, port = port),
                  onStart = function() {
                    cat("Doing application setup\n")
                    onStop(function() {
                      cat("Doing application cleanup\n")
                    })
                  })

}
