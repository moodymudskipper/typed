# #' @export
# `:=` <- function(var, value) {
#   var_nm <- as.character(substitute(var))
#   if(is.function(value)) {
#     ## handle fun name and type
#     output_ptype <- NA
#     if(grepl("..", var_nm, fixed = TRUE)) {
#       tmp <- strsplit(var_nm, "\\.\\.")[[1]]
#       var_nm <- tmp[[1]]
#       output_ptype  <- tmp[[2]]
#       output_length <- as.numeric(sub("^\\D+(\\d*)$", "\\1", output_ptype))
#       if(!is.na(output_length)) {
#         output_ptype <- sub("^(\\D+)\\d*$", "\\1", output_ptype)
#       }
#     }
#
#     ## handle formals and types
#     fmls <- formals(value)
#     nms <- names(fmls)
#     arg_ptypes <- vector("list", length(fmls))
#     tmp_lgl <- grepl("..", nms, fixed = TRUE)
#     tmp <- strsplit(nms[tmp_lgl], "\\.\\.")
#     names(arg_ptypes)[tmp_lgl] <- sapply(tmp, `[[`, 1)
#     names(fmls)[tmp_lgl] <- names(arg_ptypes)[tmp_lgl]
#     arg_ptypes[tmp_lgl] <- sapply(tmp, `[[`, 2)
#
#     ## make sure that last call is `res` and that all return calls return `res`
#     # TODO
#
#
#     ## edit body to add checks
#     body <- body(value)
#     if(is.call(body) && identical(body[[1]], quote(`{`)))
#       body <- as.list(body)[-1]
#     body <- as.call(c(quote(`{`), quote(assert_types()), body))
#
#     f <- as.function(c(fmls, body), envir =  parent.frame())
#     attr(f, "arg_ptypes")   <- unlist(arg_ptypes)
#     attr(f, "output_ptype") <- output_ptype
#     assign(var_nm, f, envir = parent.frame())
#     return(invisible(f))
#   }
#
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
# #' @export
# assert_types <- function() {
#   f <- eval.parent(eval.parent(quote(match.call()))[[1]], 2)
#   args <- attr(f, "arg_ptypes")
#   for(arg in names(args)) {
#     if(args[[arg]] %in% c(
#       "list", "logical", "integer", "numeric", "double", "complex",
#       "character", "raw")) {
#       call <- call(args[[arg]])
#     } else  {
#       call <- str2lang(paste0("vctrs::new_", args[[arg]], "()"))
#     }
#
#     envir <- parent.frame()
#     eval.parent(substitute(
#       {
#         makeActiveBinding(
#           arg,
#           env = environment(),
#           local({
#             val <- get(arg)
#             rm(list = arg, envir = envir)
#             if(!identical(vctrs::vec_ptype(val), call))
#               stop("`", arg, "`'s prototype should be ", deparse1(call), call. = FALSE)
#             function(v) {
#               if (!missing(v)) {
#                 if(!identical(vctrs::vec_ptype(val), vctrs::vec_ptype(v)))
#                   stop("assigned value should have same prototype as `",
#                        arg, "`: ", deparse1(call), call. = FALSE)
#                 val <<- v
#               }
#               val
#             }
#           }))
#       }, environment()))
#
#   }
# }
