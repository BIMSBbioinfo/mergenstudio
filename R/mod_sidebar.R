#' @importFrom bslib navset_hidden nav_panel_hidden
#' @noRd
mod_sidebar_ui <- function(id, translator = create_translator()) {
  ns <- NS(id)
  tagList(
    bslib::navset_hidden(
      id = ns("panel"),
      selected = "history",

      bslib::nav_panel_hidden(
        value = "settings",
        class = "px-0 py-2",
        mod_settings_ui(id = ns("settings"), translator = translator)
      )
    )
  )
}

#' @importFrom bslib nav_select
#' @noRd
mod_sidebar_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      settings <- mod_settings_server("settings")
      history <- mod_history_server("history", settings)
      bslib::nav_select("panel", selected = "settings", session = session)

      list(
        settings = settings,
        history = history
      )
    }
  )
}
