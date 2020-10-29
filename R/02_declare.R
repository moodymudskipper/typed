#' @param .output function output
#' @param .assertion an assertion
#' @param ... additional arguments passed to assertion
#'
#' @export
#' @rdname static_typing
check_output <- function(.output, .assertion, ...) {
  val <- try(.assertion(.output, ...), silent = TRUE)
  if(inherits(val, "try-error")) {
    e <- attr(val, "condition")$message
    check_arg_call <- deparse1(sys.call())
    fun_call <- deparse1(sys.call(-1))
    e <- sprintf("In `%s` at `%s`:\nwrong return value, %s", fun_call, check_arg_call, e)
    stop(e, call. = FALSE)
  }
  return(invisible(.output))
}

#' @param .arg function argument
#' @param .assertion an assertion
#' @param ... additional arguments passed to assertion
#' @param .bind whether to actively bind the argument so it cannot be modified
#'   unless it satisfies the assertion
#'
#' @export
#' @rdname static_typing
check_arg <- function(.arg, .assertion, ..., .bind = FALSE) {
  if(.bind) {
    var_nm <- as.character(substitute(.arg))
    assertion_quoted <- substitute(.assertion)
    pf <- parent.frame()
    dots <- eval(substitute(alist(...)))
    f <- eval(substitute(local({
      val <- .arg
      # use long name`assigned_value` so trace is more intuitive
      function(assigned_value) {
        # browser()
        if (!missing(assigned_value)) {
          # we should catch this error and use `sys.call()` to enrich it
          val <<- try(assertion_call, silent = TRUE)
          if(inherits(val, "try-error")) {
            e <- attr(val, "condition")$message
            fun_call <- sys.call(-1)
            if(!is.null(fun_call)) {
              fun_call <- deparse1(fun_call)
              e <- sprintf("In `%s` at `%s <- ...`:\n%s", fun_call, var_nm, e)
            }
            stop(e, call. = FALSE)
          }
        }
        val
      }
    }), list (
      assertion_call = as.call(c(assertion_quoted, quote(assigned_value), dots)),
      var_nm = var_nm)))
    attr(f, "srcref") <- NULL # so it's not set to old definition*
    rm(list = var_nm, envir = pf)
    makeActiveBinding(var_nm, f, pf)
  }

  val <- tryCatch(.assertion(.arg, ...), error = function(e) e)
  if(class(val)[[1]]  == "simpleError") { # faster than inherits
    e <- val$message
    check_arg_call <- deparse1(sys.call())
    fun_call <- deparse1(sys.call(-1))
    e <- sprintf("In `%s` at `%s`:\nwrong argument to function, %s", fun_call, check_arg_call, e)
    stop(e, call. = FALSE)
  }

  invisible(NULL)
}

#' @param x variable name as a string
#' @param assertion a function
#' @param value an optional value
#' @param const whether to declare `x` as a constant
#'
#' @export
#' @rdname static_typing
declare <- function(x, assertion, value, const = FALSE) {
  pf<- parent.frame()

  if(missing(assertion)) {
    assertion_quoted <- infer_implicit_assignment_call(value)
    assertion <- eval(assertion_quoted)
  } else {
    ## is assertion a assertion_ factory ?
    assertion_quoted <- substitute(assertion)
    if("assertion_ factory" %in% class(assertion)) {
      ## overwrite it with a call to itself with no arg
      # this is so we can use `Integer` in place of `Integer()` for instance
      assertion <- assertion()
      assertion_quoted <- as.call(list(assertion_quoted))
    }
  }

  ## is value provided ?
  if(!missing(value)) {
    val <- try(assertion(value), silent = TRUE)
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
        declare_call <- deparse1(declare_call)
      }
      fun_call <- deparse1(fun_call)

      e <- sprintf("In `%s` at `%s`:\n%s", fun_call, declare_call, e)
      stop(e, call. = FALSE)
    }
  } else {
    # NULL is accepted as a first value even if it doesn't pass the check
    # this is because we're very flexible with types, the alternative would be
    # to force the user to define a default object when value is missing in the assertion factory
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
            e <- sprintf("In `%s` at `%s <- ...`:\n%s", fun_call, var_nm, e)
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
              e <- sprintf("In `%s` at `%s <- ...`:\n%s", fun_call, var_nm, e)
            }
            stop(e, call. = FALSE)
          }
        }
        val
      }
    }), list (assertion = assertion_quoted, var_nm = x)))
  }

  attr(f, "srcref") <- NULL # so it's not set to old definition
  makeActiveBinding(x, f, pf)

  return(invisible(value))
}
