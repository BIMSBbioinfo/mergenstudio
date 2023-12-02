#' @importFrom rstudioapi verifyAvailable selectionGet
get_selection <- function() {
  rstudioapi::verifyAvailable()
  rstudioapi::selectionGet()
}

#' @importFrom rstudioapi verifyAvailable insertText
insert_text <- function(improved_text) {
  rstudioapi::verifyAvailable()
  rstudioapi::insertText(improved_text)
}
