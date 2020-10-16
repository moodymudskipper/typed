#' Define Constants
#'
#' `const()` is meant to be used using the syntax `? x <- const(3)` or `? x <- const(3)`.
#' The variable then cannot be modified anymore.
#'
#' @param x value
#'
#' @export
const <- function(x) {
  warning(
    "const should be used with question mark syntax, e.g. ",
    "`? x <- const(3)` or ? `x <- const(3)`")
  x
}
