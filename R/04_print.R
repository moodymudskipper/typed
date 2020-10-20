#' @export
print.typed <- function(x, ...) {
  x2 <- x
  attributes(x2) <- NULL
  writeLines(text = "# typed function")
  print(x2)
  if(!identical(attr(x, "return_type"), NA)) {
    writeLines(text = paste0(
      "# Return type: ", deparse1(attr(x ,"return_type"))))
  }
  if(length(attr(x, "arg_types"))) {
    writeLines(text = "# Arg types:")
    for(arg in names(attr(x, "arg_types"))) {
      writeLines(text = paste0("# ",
        arg, ": ", deparse1(attr(x ,"arg_types")[[arg]])))
    }
  }
  invisible(x)
}
