#' @param .output function output
#' @param .assertion an assertion
#' @param ... additional arguments passed to assertion
#'
#' @export
#' @return `.output`if it satisfies the assertion, fails otherwise.
#' @rdname check_arg
check_output <- function(.output, .assertion, ...) {
  val <- try(.assertion(.output, ...), silent = TRUE)
  if(inherits(val, "try-error")) {
    e <- attr(val, "condition")$message
    check_arg_call <- deparse1(sys.call())
    fun_call <- deparse1(sys.call(-1))
    e <- sprintf("In `%s` at `%s`:\nwrong return value, %s", fun_call, check_arg_call, e)
    stop(e, call. = FALSE)
  }
  .output
}

#' Check Argument Types and Return Type
#'
#' These functions are not designed to be used directly, we advise to use the
#' syntaxes described in `?declare` instead. `check_arg` checks that arguments
#' satisfy an assertion, and if relevant make them into active bindings to make sure they
#' always satisy it. `check_output` checks that the value, presumably a return
#' value, satisfies an assertion,
#'
#' @param .arg function argument
#' @param .assertion an assertion
#' @param ... additional arguments passed to assertion
#' @param .bind whether to actively bind the argument so it cannot be modified
#'   unless it satisfies the assertion
#' @return returns `NULL` invisibly, called for side effects.
#' @export
check_arg <- function(.arg, .assertion, ..., .bind = FALSE) {
  if(.bind) {
    var_nm <- as.character(substitute(.arg))
    assertion_quoted <- substitute(.assertion)
    pf <- parent.frame()
    dots <- eval(substitute(alist(...)))
    f <- eval(substitute(
      local({
        val <- ARG
        # use long name`assigned_value` so trace is more intuitive
        function(assigned_value) {
          # browser()
          if (!missing(assigned_value)) {
            # we should catch this error and use `sys.call()` to enrich it
            val <<- try(ASSERTION_CALL, silent = TRUE)
            if (inherits(val, "try-error")) {
              e <- attr(val, "condition")$message
              fun_call <- sys.call(-1)
              if(!is.null(fun_call)) {
                fun_call <- deparse1(fun_call)
                e <- sprintf("In `%s` at `%s <- ...`:\n%s", fun_call, VAR_NM, e)
              }
              stop(e, call. = FALSE)
            }
          }
          val
        }
      }, envir = pf),
      list(
        ASSERTION_CALL = as.call(c(assertion_quoted, quote(assigned_value), dots)),
        VAR_NM = var_nm,
        ARG = .arg
      )
    ))
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
declare <- function(x, assertion, value, const = FALSE) {
  pf <- parent.frame()
  f_env <- new.env(parent = pf)

  if(missing(assertion)) {
    assertion_quoted <- infer_implicit_assignment_call(value)
    assertion <- eval(assertion_quoted)
  } else {
    ## is assertion a assertion_ factory ?
    assertion_quoted <- substitute(assertion)
    if(inherits(assertion, "assertion_factory")) {
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
        declare_call <- paste(deparse1(declare_call[[2]]),"?", deparse1(declare_call[[3]]))
      } else {
        fun_call <- sys.call(-1)
        declare_call <- deparse1(declare_call)
      }

      # if we use reprex or knitr the call stack is edited, we account for it
      is_eval_call <-
        is.call(fun_call) && identical(fun_call[[1]], quote(eval)) # nocov

      if(!is.null(fun_call) && !is_eval_call) {
        fun_call <- deparse1(fun_call)
        e <- sprintf("In `%s` at `%s`:\n%s", fun_call, declare_call, e)
      }
      stop(e, call. = FALSE)
    }
    value <- val
  } else {
    # NULL is accepted as a first value even if it doesn't pass the check
    # this is because we're very flexible with types, the alternative would be
    # to force the user to define a default object when value is missing in the assertion factory
    value <- NULL
  }

  if(const) {
    # we could use `lockBinding()` but we'd lose flexibility on the error message
    # so we use an active binding here as well
    f <- eval(substitute(
      local(
        {
          val <- VALUE
          function(assigned_value) {
            if (!missing(assigned_value)) {
              e <- paste0("Can't assign to a constant")
              fun_call <- sys.call(-1)
              is_eval_call <-
                is.call(fun_call) && identical(fun_call[[1]], quote(eval)) # nocov
              if(!is.null(fun_call) && !is_eval_call) {
                fun_call <- deparse1(fun_call)
                e <- sprintf("In `%s` at `%s <- ...`:\n%s", fun_call, VAR_NM, e)
              }
              stop(e, call. = FALSE)
            }
            val
          }
        },
        envir = f_env),
      list(VAR_NM = x, VALUE = value)
    ))
  } else {
    f <- eval(
      substitute(
        local({
          val <- VALUE
          # use long name`assigned_value` so trace is more intuitive
          function(assigned_value) {
            if (!missing(assigned_value)) {
              # we should catch this error and use `sys.call()` to enrich it
              val <<- try(ASSERTION(assigned_value), silent = TRUE)
              if(inherits(val, "try-error")) {
                e <- attr(val, "condition")$message
                fun_call <- sys.call(-1)
                is_eval_call <-
                  is.call(fun_call) && identical(fun_call[[1]], quote(eval)) # nocov
                if(!is.null(fun_call) && !is_eval_call) {
                  fun_call <- deparse1(fun_call)
                  e <- sprintf("In `%s` at `%s <- ...`:\n%s", fun_call, VAR_NM, e)
                }
                stop(e, call. = FALSE)
              }
            }
            val
          }
        }, envir = f_env),
        list (ASSERTION = assertion_quoted, VAR_NM = x, VALUE = value)
      )
    )
  }

  attr(f, "srcref") <- NULL # so it's not set to old definition
  if (exists(x, pf)) {
    rm(list =x, envir = pf)
  }
  makeActiveBinding(x, f, pf)

  return(invisible(value))
}
