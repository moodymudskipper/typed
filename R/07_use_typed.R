#' Use the 'typed' package
#'
#' This sets up your package so you can use type, by editing the DESCRIPTION file
#' and editing or creating 'R/your.pkg-package.R'
#'
#' @return Returns `NULL` invisibly, called for side effects
#' @export
use_typed <- function() {
  usethis::use_package("typed")
  usethis::use_package_doc(open = FALSE)
  usethis::use_import_from("typed", getNamespaceExports("typed"), load = FALSE)
  invisible(NULL)
}
