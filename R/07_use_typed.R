#' Use the 'typed' package
#'
#' This sets up your package so you can use 'typed', by editing the DESCRIPTION file
#' and editing or creating 'R/your.pkg-package.R'
#'
#' @return Returns `NULL` invisibly, called for side effects
#' @export
use_typed <- function() {
  usethis::use_package("typed")
  pkg_doc_path <- sprintf(
    "R/%s-package.R",
    usethis:::proj_desc()$get_field("Package")
  )
  if (!file.exists(pkg_doc_path)) usethis::use_package_doc(open = FALSE)
  usethis::use_import_from("typed", getNamespaceExports("typed"), load = FALSE)
  invisible(NULL)
}
