#' Build a new type
#'
#' @param f a function
#'
#' @export
new_type_checker <- function(f) {
  # create a function with arguments being the additional args to f and dots
  f_call <- as.call(c(quote(f), quote(x), sapply(names(formals(f)[-1]), as.name)))

  res <- as.function(c(formals(f)[-1],alist(...=), bquote({
    f_call <- substitute(.(f_call))
    # remove if empty
    f_call <- Filter(function(x) !identical(x, quote(expr=)), f_call)

    header <- call("{",
      quote(f <- .(f)), # so the substituted definition is readable
      substitute(x <- F_CALL, list(F_CALL = f_call))
      )

    # the footer is made of additional assertions derived from `...`
    footer <- process_type_checker_dots(...)
    if(is.null(footer)) {
      body <- call("{", header, quote(x))
    } else {
      body <- call("{", header, footer, quote(x))
    }
    as.function(c(alist(x=), body), envir = parent.frame())
  })))
  class(res) <- "type_checker"
  environment(res) <- parent.frame()
  res
}

#' Process type checker dots
#'
#' This needs to be exported, but shouldn't be called by the user
#'
#' @param ... dots
#' @export
process_type_checker_dots <- function(...) {
  args <- list(...)
  if(!length(args)) return(NULL)
  nms <- allNames(args)
  exprs <- vector("list", length(args))
  for (i in seq_along(args)) {
    if(nms[[i]] != "") {
      exprs[[i]] <- bquote(
        if(!identical(.(as.name(nms[[i]]))(x), .(args[[i]]))) {
          print(waldo::compare(
            .(args[[i]]),
            .(as.name(nms[[i]]))(x),
            x_arg = "expected",
            y_arg = .(paste0(nms[[i]], "(value)"))
          ))
          stop(.(paste0("`", nms[[i]], "` mismatch")), call. = FALSE)
        })
    } else {
      if(!is.call(args[[i]]) || !identical(args[[i]][[1]], as.name("~"))) {
        stop("assertions should be either named function, or unnamed formulas")
      }
      assertion <- do.call(substitute, list(args[[i]][[3]], list(. = quote(x))))
      error <- args[[i]][[2]]
      exprs[[i]] <- bquote(if(!.(assertion)) stop(.(error), call. = FALSE))
    }
  }
  exprs
  as.call(c(quote(`{`), exprs))
}

infer_implicit_assignment_call <- function(value) {
  # note : attr(, "class") is different from class()
  if(is.atomic(value) && is.null(attr(value, "class"))) {
    assertion_call <- switch(
      typeof(value),
      "logical" = quote(Logical()),
      "integer" = quote(Integer()),
      "double" = quote(Double()),
      "complex" = bquote(Any(typeof = "complex")),
      "character" = quote(Character()),
      "raw" = quote(Raw())
    )
    return(assertion_call)
  }
  cl <- class(value)
  if(length(cl) == 1) {
    assertion_call <- switch(
      cl,
      "list" = quote(List()),
      "NULL" = quote(Null()),
      "function" = quote(Function()),
      "environment" = quote(Environment()),
      "name" = quote(Symbol()),
      "pairlist" = quote(Pairlist()),
      "language" = quote(Language()),
      "expression" = quote(Expression()),
      "factor" = quote(Factor()),
      "data.frame" = quote(Data.frame()),
      "matrix" = quote(Matrix()),
      "array" = quote(Array()),
      "date" = quote(Date()),
      "matrix" = quote(Matrix()),
      call("Any", class = cl)
    )
    return(assertion_call)
  }
  if (identical(cl, c("POSIXct", "POSIXt"))) {
    return(quote(Time()))
  }
  call("Any", class = cl)
}


#' Fetch assertion from active binding function
#'
#' @param x obect
get_assertion <- function(x) {
  x <- as.character(substitute(x))
  fun <- activeBindingFunction(x, parent.frame())
  body(fun)[[c(2, 3, 2, 3, 1)]]
}
