# deal with the conflicting `?` in the  devtools_shims env
# follow up in https://github.com/r-lib/pkgload/issues/265

.onAttach <- function(...) {
  if (isNamespaceLoaded("pkgload")) {
    attr(`?`, "original") <- getFromNamespace("shim_question", "pkgload")
    getFromNamespace("assignInNamespace", "utils")("shim_question", `?`, "pkgload")
  }
}

.onDetach <- function(...) {
  if (isNamespaceLoaded("pkgload")) {
    original <- attr(getFromNamespace("shim_question", "pkgload"), "original")
    if (!is.null(original)) {
      getFromNamespace("assignInNamespace", "utils")(original, `?`, "pkgload")
    }
  }
}
