#' @export
assert_types <- function() {
  f <- eval.parent(eval.parent(quote(match.call()))[[1]], 2)
  pf <- parent.frame()
  ptypes <- attr(f, "arg_ptypes")
  output_ptype <- attr(f, "output_ptype")
  if(!identical(output_ptype, NA)) {
    eval.parent(substitute(
      {
        makeActiveBinding(
          "res",
          env = environment(),
          local({
            val <- output_ptype
            function(v) {
              if (!missing(v)) {
                if(!identical(vctrs::vec_ptype(val), vctrs::vec_ptype(v)))
                  stop("assigned value should have same prototype as `res`: ",
                       deparse1(output_ptype), call. = FALSE)
                val <<- v
              }
              val
            }
          }))
      }, environment()))
  }
  for(arg in names(ptypes)) {
    ptype <- ptypes[[arg]]
    eval.parent(substitute(
      {
        makeActiveBinding(
          arg,
          env = environment(),
          local({
            val <- get(arg)
            rm(list = arg, envir = pf)
            if(!identical(vctrs::vec_ptype(val), ptype))
              stop("`", arg, "`'s prototype should be ", deparse1(ptype), call. = FALSE)
            function(v) {
              if (!missing(v)) {
                if(!identical(vctrs::vec_ptype(val), vctrs::vec_ptype(v)))
                  stop("assigned value should have same prototype as `",
                       arg, "`: ", deparse1(ptype), call. = FALSE)
                val <<- v
              }
              val
            }
          }))
      }, environment()))
  }
}





