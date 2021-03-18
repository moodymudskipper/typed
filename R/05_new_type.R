#' Build a new type
#'
#' @param f a function
#' @return a function with class `assertion_factory`
#'
#' @export
as_assertion_factory <- function(f) {
  # create a function with arguments being the additional args to f and dots
  f_call <- as.call(c(quote(f), quote(value), sapply(names(formals(f)[-1]), as.name)))

  res <- as.function(c(formals(f)[-1],alist(...=), bquote({
    f_call <- substitute(.(f_call))
    # remove if empty
    f_call <- Filter(function(value) !identical(value, quote(expr=)), f_call)

    header <- call("{",
      quote(f <- .(f)), # so the substituted definition is readable
      substitute(value <- F_CALL, list(F_CALL = f_call))
      )

    # the footer is made of additional assertions derived from `...`
    footer <- process_assertion_factory_dots(...)
    if(is.null(footer)) {
      body <- call("{", header, quote(value))
    } else {
      body <- call("{", header, footer, quote(value))
    }
    as.function(c(alist(value=), body), envir = parent.frame())
  })))
  class(res) <- "assertion_factory"
  environment(res) <- parent.frame()
  res
}


#' Process assertion factory dots
#'
#' This needs to be exported, but shouldn't be called by the user
#'
#' @param ... dots
#' @return a `{` expression
#' @export
process_assertion_factory_dots <- function(...) {
  args <- list(...)
  if(!length(args)) return(NULL)
  nms <- allNames(args)
  exprs <- vector("list", length(args))
  for (i in seq_along(args)) {
    ## is the ith argument named ?
    if(!nms[[i]] %in% c("", "...")) {
      exprs[[i]] <- bquote(
        if(!identical(.(as.name(nms[[i]]))(value), .(args[[i]]))) {
          stop(sprintf(
            "%s\n%s",
            .(paste0("`", nms[[i]], "` mismatch")),
            waldo::compare(
              .(as.name(nms[[i]]))(value),
              .(args[[i]]),
              x_arg = .(paste0(nms[[i]], "(value)")),
              y_arg = "expected"))
            , call. = FALSE)
        })
    } else {
      ## is it not a formula ?
      if(!is.call(args[[i]]) || !identical(args[[i]][[1]], as.name("~"))) {
        stop("assertions should be either named function, or unnamed formulas")
      }
      ## is it a 2 sided formula ?
      if (length(args[[i]]) == 3) {
        error <- args[[i]][[2]]
        assertion <- do.call(substitute, list(args[[i]][[3]], list(. = quote(value))))
      } else  {
        error <- "mismatch"
        assertion <- do.call(substitute, list(args[[i]][[2]], list(. = quote(value))))
      }

      exprs[[i]] <- bquote(if(!.(assertion)) stop(
        sprintf(
          "%s\n%s",
          .(error),
          waldo::compare(
            FALSE,
            TRUE,
            x_arg = .(deparse1(assertion)),
            y_arg = "expected"))
      , call. = FALSE))
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

get_assertion <- function(x) {
  x <- as.character(substitute(x))
  fun <- activeBindingFunction(x, parent.frame())
  body(fun)[[c(2, 3, 2, 3, 2, 1)]]
}
