#' @importFrom bslib navset_hidden nav_panel_hidden
#' @noRd
mod_sidebar_ui <- function(id, translator = create_translator(), dir = NULL) {
  ns <- NS(id)
  tagList(
    bslib::navset_hidden(
      id = ns("panel"),
      # selected = "history",
      selected = "settings",

      bslib::nav_panel_hidden(
        value = "history",
        class = "px-0 py-2",
        mod_history_ui(id = ns("history"))

      ),

      bslib::nav_panel_hidden(
        value = "settings",
        class = "px-0 py-2",
        mod_settings_ui(id = ns("settings"), translator = translator, dir = dir)
      )
    )
  )
}

#' @importFrom bslib nav_select
#' @noRd
mod_sidebar_server <- function(id, dir = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      settings <- mod_settings_server("settings", dir = dir)
      history <- mod_history_server("history", settings)
      # bslib::nav_select("panel", selected = "settings", session = session)

      observe({
        bslib::nav_select("panel", selected = "history", session = session)
      }) %>%
        bindEvent(settings$selected_history, ignoreInit = TRUE)

      observe({
        bslib::nav_select("panel", selected = "settings", session = session)
      }) %>%
        bindEvent(history$selected_settings, ignoreInit = TRUE)

      list(
        settings = settings,
        history = history
      )
    }
  )
}
