#' @importFrom rstudioapi verifyAvailable selectionGet
#' @noRd
get_selection <- function() {
  rstudioapi::verifyAvailable()
  rstudioapi::selectionGet()
}

#' @importFrom rstudioapi verifyAvailable insertText
#' @noRd
insert_text <- function(improved_text) {
  rstudioapi::verifyAvailable()
  rstudioapi::insertText(improved_text)
}
