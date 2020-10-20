#' Build a new type
#'
#' @param f a function
#'
#' @export
new_type_checker <- function(f) {
  # create a function with arguments being the additional args to f and dots
  f_call <- as.call(c(quote(f), quote(x), sapply(names(formals(f)[-1]), as.name)))

  res <- as.function(c(formals(f)[-1],alist(...=), bquote({
    #browser()
    f_call <- substitute(.(f_call))
    # remove if empty
    f_call <- Filter(function(x) !identical(x, quote(expr=)), f_call)

    header <- call("{",
      quote(f <- .(f)), # so the substituted definition is readable
      substitute(x <- F_CALL, list(F_CALL = f_call))
      )

    # the footer is made of additional assertions derived from `...`
    footer <- process_assertions(...)
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

#' Process assertions
#'
#' This needs to be exported, but shouldn't be called by the user
#'
#' @param ... additional assertions
#' @export
process_assertions <- function(...) {
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
      error <-args[[i]][[2]]
      exprs[[i]] <- bquote(if(!.(assertion)) stop(.(error)))
    }
  }
  exprs
  as.call(c(quote(`{`), exprs))
}

is_promise2 <- getFromNamespace("is_promise2", "pryr")

#' @param x variable name as a string
#' @param assertion_fun a function
#' @param value an optional value
#'
#' @export
#' @rdname static_typing
declare <- function(x, assertion_fun, value) {
  pf<- parent.frame()
  promise_lgl <- is_promise2(as.name(x), pf)
  if(promise_lgl && missing(value)) {
    value <- get(x, envir = pf, inherits = FALSE)
    rm(list = x, envir = pf)
  }

  if(missing(assertion_fun)) {
    assertion_fun_call <- bquote(Any(class = .(class(value))))
    assertion_fun <- eval(assertion_fun_call)
  } else {
    ## is assertion_fun a type_checker ?
    assertion_fun_call <- substitute(assertion_fun)
    if("type_checker" %in% class(assertion_fun)) {
      ## overwrite it with a call to itself with no arg
      # this is so we can use `Integer` in place of `Integer()` for instance
      assertion_fun <- assertion_fun()
      assertion_fun_call <- as.call(list(assertion_fun_call))
    }
  }

  ## is value provided ?
  if(!missing(value) || promise_lgl) {
    value <- assertion_fun(value)
  } else {
    # NULL is accepted as a first value even if it doesn't pass the check
    # this is because we're very flexible with types, the alternative would be
    # to force the user to define a default object when value is missing in the type checker
    value <- NULL
  }

  f <- eval(substitute(local({
    val <- value
    function(v) {
      if (!missing(v)) {
        val <<- assertion_fun(v)
      }
      val
    }
  }), list (assertion_fun = assertion_fun_call)))

  attr(f, "srcref") <- NULL # so it's not set to old definition
  makeActiveBinding(x, f, pf)

  return(invisible(value))
}


#' Fetch assertion function from active binding function
#'
#' @param x obect
get_assertion_fun <- function(x) {
  x <- as.character(substitute(x))
  fun <- activeBindingFunction(x, parent.frame())
  body(fun)[[c(2, 3, 2, 3, 1)]]
}
