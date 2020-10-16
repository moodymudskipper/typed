#' @export
print.typed <- function(x, ...) {
  x2 <- x
  attributes(x2) <- NULL
  print(x2)
  if(!identical(attr(x, "output_ptype"), NA)) {
    writeLines(text = paste0(
      "# Return type: ", deparse1(attr(x ,"output_ptype"))))
  }
  if(length(attr(x, "arg_ptypes"))) {
    writeLines(text = "# Arg types:")
    for(arg in names(attr(x, "arg_ptypes"))) {
      writeLines(text = paste0(
        arg, ": ", deparse1(attr(x ,"arg_ptypes")[[arg]])))
    }
  }
  invisible(x)
}
