#' App UI
#'
#' @param id id of the module
#' @inheritParams run_chat_app
#'
#' @import htmltools
#' @import shiny
#' @import bslib
#' @importFrom waiter use_waiter
#' @noRd
mod_app_ui <- function(id, ide_colors = get_ide_theme_info(), dir = NULL) {
  ns <- NS(id)
  translator <- create_translator(language = getOption("mergenstudio.language"))
  tagList(
    waiter::use_waiter(),
    bslib::page_fluid(
      theme = create_chat_app_theme(ide_colors),
      title = "ChatGPT from mergenstudio",
      class = "vh-100 p-0 m-0",
      html_dependencies(),
      prismDependencies,
      prismLanguageDependencies(c("r")),
      bslib::layout_sidebar(
        class = "vh-100",
        sidebar = bslib::sidebar(
          open = "closed",
          width = 300,
          class = "p-0",
          padding = "0.5rem",

          mod_sidebar_ui(ns("sidebar"), translator, dir = dir)
        ),
        div(
          class = "row justify-content-center h-100",
          div(
            class = "col h-100",
            style = htmltools::css(`max-width` = "800px"),
            mod_chat_ui(ns("chat"), translator)
          )
        )
      )
    )
  )
}

#' App Server
#'
#' @param id id of the module
#' @inheritParams run_chat_app
#' @noRd
mod_app_server <- function(id, ide_colors = get_ide_theme_info(), dir = NULL) {
  moduleServer(id, function(input, output, session) {
    sidebar <- mod_sidebar_server("sidebar", dir = dir)
    mod_chat_server(
      id = "chat",
      ide_colors = ide_colors,
      translator = NULL,
      settings = sidebar$settings,
      history = sidebar$history
    )
  })
}


#' RGB str to hex
#'
#' @param rgb_string The RGB string as returned by `rstudioapi::getThemeInfo()`
#' @noRd
#'
#' @return hex color
rgb_str_to_hex <- function(rgb_string) {
  rgb_vec <- unlist(strsplit(gsub("[rgba() ]", "", rgb_string), ","))
  grDevices::rgb(
    red = as.numeric(rgb_vec[1]),
    green = as.numeric(rgb_vec[2]),
    blue = as.numeric(rgb_vec[3]),
    names = FALSE,
    maxColorValue = 255
  ) %>%
    unname()
}

#' Chat App Theme
#'
#' Create a bslib theme that matches the user's RStudio IDE theme.
#'
#' @inheritParams run_chat_app
#' @noRd
#'
#' @return A bslib theme
create_chat_app_theme <- function(ide_colors = get_ide_theme_info()) {
  bslib::bs_theme(
    version = 5,
    bg = ide_colors$bg,
    fg = ide_colors$fg,
    font_scale = 0.9,
  )
}


#' Get IDE theme information.
#'
#' This function returns a list with the current IDE theme's information.
#'
#' @importFrom rstudioapi isAvailable getThemeInfo
#' @import cli
#' @noRd
#'
#' @return A list with three components:
#' \item{is_dark}{A boolean indicating whether the current IDE theme is dark.}
#' \item{bg}{The current IDE theme's background color.}
#' \item{fg}{The current IDE theme's foreground color.}
#'
get_ide_theme_info <- function() {
  if (rstudioapi::isAvailable()) {
    rstudio_theme_info <- rstudioapi::getThemeInfo()

    # create a list with three components
    list(
      is_dark = rstudio_theme_info$dark,
      bg = rgb_str_to_hex(rstudio_theme_info$background),
      fg = rgb_str_to_hex(rstudio_theme_info$foreground)
    )
  } else {
    if (interactive()) cli::cli_inform("Using fallback ide theme")

    # create a list with three components with fallback values
    list(
      is_dark = TRUE,
      bg = "#002B36",
      fg = "#93A1A1"
    )
  }
}

#' @importFrom htmltools htmlDependency
#' @noRd
html_dependencies <- function() {
  htmltools::htmlDependency(
    name = "mergenstudio-assets", version = "1.0.0",
    package = "mergenstudio",
    src = "assets",
    script = c("js/copyToClipboard.js", "js/shiftEnter.js", "js/conversation.js", "js/directory_input_binding.js", "js/DynamicScroll.js"),
    stylesheet = c("css/mod_app.css")
  )
}

#' Internationalization for the ChatGPT addin
#'
#' The language can be set via `options("mergenstudio.language" = "<language>")`
#' (defaults to "en").
#'
#' @import cli
#' @importFrom shiny.i18n Translator
#' @noRd
#'
#' @param language The language to be found in the translation JSON file.
#'
create_translator <- function(language = getOption("mergenstudio.language")) {
  translator  <- shiny.i18n::Translator$new(translation_json_path = system.file("translations/translation.json", package = "mergenstudio"))
  supported_languages <- translator$get_languages()

  if (!language %in% supported_languages) {
    cli::cli_abort("Language {.val {language}} is not supported. Must be one of {.val {supported_languages}}")
  }

  translator$set_translation_language(language)

  translator
}
