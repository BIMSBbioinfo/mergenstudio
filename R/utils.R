#' AND infix operator
#'
#' Given x and y, return y only if both x and y are set
#'
#' @param x left operand
#' @param y right operand
#' @noRd
`%AND%` <- function(x, y) {
  if (!is.null(x) && !isTRUE(is.na(x)))
    if (!is.null(y) && !isTRUE(is.na(y)))
      return(y)
  return(NULL)
}

#' getIconLabel
#'
#' creates a label with information icon
#'
#' @param label label
#' @param message message
#'
#' @noRd
getIconLabel <- function(label = NULL, message = NULL){

  icon_label <- tags$span(label,
                          tags$i(
                            class = "glyphicon glyphicon-info-sign",
                            style = "color:#0072B2;",
                            title = message
                          )
  )
  return(icon_label)
}
