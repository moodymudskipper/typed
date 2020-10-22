#' @param x variable name as a string
#' @param assertion a function
#' @param value an optional value
#'
#' @export
#' @rdname static_typing
declare <- function(x, assertion, value, const = FALSE) {
  pf<- parent.frame()
  promise_lgl <- is_promise2(as.name(x), pf)
  if(promise_lgl && missing(value)) {
    value <- get(x, envir = pf, inherits = FALSE)
    rm(list = x, envir = pf)
  }

  if(missing(assertion)) {
    assertion_call <- infer_implicit_assignment_call(value)
    assertion <- eval(assertion_call)
  } else {
    ## is assertion a type_checker ?
    assertion_call <- substitute(assertion)
    if("type_checker" %in% class(assertion)) {
      ## overwrite it with a call to itself with no arg
      # this is so we can use `Integer` in place of `Integer()` for instance
      assertion <- assertion()
      assertion_call <- as.call(list(assertion_call))
    }
  }

  ## is value provided ?
  if(!missing(value) || promise_lgl) {
    val <<- try(assertion(value), silent = TRUE)
    if(inherits(val, "try-error")) {
      e <- attr(val, "condition")$message
      declare_call <- sys.call()

      sc4 <- if(length(sys.calls()) >=4) sys.call(-4)    # the potential `?` call
      if (is.call(sc4) && identical(sc4[[1]], quote(`?`))) {
        declare_call <- sc4
        fun_call <- sys.call(-5)
        if(length(declare_call) == 3) {
          declare_call <- paste(deparse1(declare_call[[2]]),"?", deparse1(declare_call[[3]]))
        } else {
          # not sure if we ever go there, that's an error on a `? x <- value` call
          declare_call <- paste("?", deparse1(declare_call[[3]]))
        }
      } else {
        fun_call <- sys.call(-1)
        declare_call <- deparse1(declare_call,"srcref")
      }
      fun_call <- deparse1(fun_call)

      if (promise_lgl) {
        e <- sprintf("In `%s` at `%s`: wrong argument to function, %s", fun_call, declare_call, e)
      } else {
        e <- sprintf("In `%s` at `%s`: %s", fun_call, declare_call, e)
      }
      stop(e, call. = FALSE)
    }
  } else {
    # NULL is accepted as a first value even if it doesn't pass the check
    # this is because we're very flexible with types, the alternative would be
    # to force the user to define a default object when value is missing in the type checker
    value <- NULL
  }

  if(const) {
    # we could use `lockBinding()` but we'd lose flexibility on the error message
    # so we use an active binding here as well
    f <- eval(substitute(local({
      nm <- x
      val <- value
      function(assigned_value) {
        if (!missing(assigned_value)) {
          e <- paste0("Can't assign to a constant")
          fun_call <- sys.call(-1)
          if(!is.null(fun_call)) {
            fun_call <- deparse1(fun_call)
            e <- sprintf("In `%s` at `%s <- ...`: %s", fun_call, var_nm, e)
          }
          stop(e, call. = FALSE)
        }
        val
      }
    }), list (var_nm = x)))
  } else {
    f <- eval(substitute(local({
      val <- value
      # use long name`assigned_value` so trace is more intuitive
      function(assigned_value) {
        # browser()
        if (!missing(assigned_value)) {
          # we should catch this error and use `sys.call()` to enrich it
          val <<- try(assertion(assigned_value), silent = TRUE)
          if(inherits(val, "try-error")) {
            e <- attr(val, "condition")$message
            fun_call <- sys.call(-1)
            if(!is.null(fun_call)) {
              fun_call <- deparse1(fun_call)
              e <- sprintf("In `%s` at `%s <- ...`: %s", fun_call, var_nm, e)
            }
            stop(e, call. = FALSE)
          }
        }
        val
      }
    }), list (assertion = assertion_call, var_nm = x)))
  }

  attr(f, "srcref") <- NULL # so it's not set to old definition
  makeActiveBinding(x, f, pf)

  return(invisible(value))
}
