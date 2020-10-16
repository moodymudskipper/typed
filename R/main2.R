#' `?` <- function(lhs, rhs) {
#'   fun_nm <- as.character(substitute(lhs))
#'   rhs <- substitute(rhs)
#'   rhs_is_fun <-
#'     is.call(rhs) &&
#'     identical(rhs[[1]], quote(`<-`)) &&
#'     is.call(rhs[[3]]) &&
#'     identical(rhs[[c(3,1)]], quote(`function`))
#'   if(!rhs_is_fun) stop("wrong syntax!")
#'
#'   ## handle fun name and type
#'   output_ptype <- eval.parent(rhs[[2]])
#'   if(is.function(output_ptype))
#'     output_ptype <- output_ptype()
#'
#'   ## handle formals and types
#'   value <- eval.parent(rhs[[3]])
#'   fmls <- formals(value)
#'   nms <- names(fmls)
#'   tmp_lgl <- sapply(fmls, function(x) is.call(x) && identical(x[[1]], quote(`?`)))
#'   tmp <- lapply(fmls[tmp_lgl], function(x)
#'     if(length(x) == 2)
#'       list(quote(expr=), x[[2]])
#'     else
#'       list(x[[2]], x[[2]]))
#'   fmls[tmp_lgl] <- lapply(tmp, `[[`, 1)
#'   env <- parent.frame()
#'   arg_ptypes <- sapply(tmp, function(x) {
#'     pt <- eval(x[[2]], env)
#'     if(is.function(pt)) pt() else pt})
#'
#'   ## make sure that last call is `res` and that all return calls return `res`
#'   # TODO
#'
#'
#'   ## edit body to add checks
#'   body <- body(value)
#'   if(is.call(body) && identical(body[[1]], quote(`{`)))
#'     body <- as.list(body)[-1]
#'   body <- as.call(c(quote(`{`), quote(assert_types2()), body))
#'
#'   f <- as.function(c(fmls, body), envir =  parent.frame())
#'   attr(f, "arg_ptypes")   <- arg_ptypes
#'   attr(f, "output_ptype") <- output_ptype
#'   assign(fun_nm, f, envir = parent.frame())
#'   invisible(f)
#' }
#'
#'
# #' @export
# `:=` <- function(var, value) {
#   var_nm <- as.character(substitute(var))
#   eval.parent(substitute(
#     makeActiveBinding(
#       var_nm,
#       env = environment(),
#       local({
#         val <- value
#         function(v) {
#           if (!missing(v)) {
#             if(!identical(vctrs::vec_ptype(val), vctrs::vec_ptype(v)))
#               stop("assigned value should have same prototype as `",
#                    var_nm, "`: ",  deparse1(vctrs::vec_ptype(val)),
#                    call. = FALSE)
#             val <<- v
#           }
#           val
#         }
#       })), environment()))
# }
#
#
#
# assert_types2 <- function() {
#   f <- eval.parent(eval.parent(quote(match.call()))[[1]], 2)
#   ptypes <- attr(f, "arg_ptypes")
#   for(arg in names(ptypes)) {
#     ptype <- ptypes[[arg]]
#     envir <- parent.frame()
#     eval.parent(substitute(
#       {
#         makeActiveBinding(
#           arg,
#           env = environment(),
#           local({
#             val <- get(arg)
#             rm(list = arg, envir = envir)
#             if(!identical(vctrs::vec_ptype(val), ptype))
#               stop("`", arg, "`'s prototype should be ", deparse1(ptype), call. = FALSE)
#             function(v) {
#               if (!missing(v)) {
#                 if(!identical(vctrs::vec_ptype(val), vctrs::vec_ptype(v)))
#                   stop("assigned value should have same prototype as `",
#                        arg, "`: ", deparse1(ptype), call. = FALSE)
#                 val <<- v
#               }
#               val
#             }
#           }))
#       }, environment()))
#
#   }
# }
