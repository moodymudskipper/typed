


allNames <- function (x) {
  value <- names(x)
  if (is.null(value))
    character(length(x))
  else value
}

#' Static typing
#'
#' Use `?` to set a function's return type, argument types, or variable types
#' in the body of the function.
#'
#' @param lhs lhs
#' @param rhs rhs
#'
#' @export
#' @importFrom utils help
#' @value
#' `assertion ? function(<args>) {<body>}` returns a typed function.
#' `fun <- assertion ? function(<args>) {<body>}` returns a typed function and
#' binds it to `fun` in the local environment.
#' `assertion ? var <- val` returns `val` invisibly
#' and defines an active binding for `var` in the local environment so that the return
#' value of `var` is `val`, this return value can be modified  by `var <- new_val` if the assertion is verified.
#' `assertion ? var` is similar to the latter but the default return value for `var` will be `NULL`.
#' `assertion ? (var) <- val` returns `val` invisibly
#' and defines an active binding for `var` in the local environment so that the return
#' value of `var` is `val` and can't be modified by assigning to `var`, making
#' `var` act as a constant.
#'
#' As a side effect The assigned value, returned invisibly.
#' @examples
#' numeric ? function (x= ?numeric) {
#'   numeric ? y <- 2
#'   res <- x + y
#'   res
#' }
#' @rdname static_typing
`?` <- function(lhs, rhs) {
  pf <- parent.frame()

  ## did we use unary `?` ?
  if(unary_qm_lgl <- missing(rhs)) {
    ## assign the value to quoted rhs and define lhs as `NA`
    rhs <- substitute(lhs)
    lhs <- NA
  } else {
    ## quote lhs and rhs
    lhs <- substitute(lhs)
    rhs <- substitute(rhs)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # `?` TO REFACTOR FUNCTION CODE

  ## is rhs a function definition ?
  if(is.call(rhs) && identical(rhs[[1]], quote(`function`))) {
    ## fetch formals and formal types
    value <- eval.parent(rhs)
    body <- body(value)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # EDIT BODY TO REPLACE CALLS TO `?` USING DECLARE

    modify_qm_calls <- function(x) {
      if(!is.call(x)) return(x)
      if(identical(x[[1]], quote(`?`))) {
        ## do we set the variable type implicitly (no lhs to `?`)
        unary_qm_lgl <- length(x) == 2
        if(unary_qm_lgl) {
          rhs <- x[[2]]
          ## is the rhs an assignment ?
          if(is_assign_stmt(rhs)) {
            ## is it a constant, using syntax `? (x) <- value` ?
            if(is.call(rhs[[2]]) && identical(rhs[[c(2,1)]], quote(`(`))) {
              call <- call(
                "declare", as.character(rhs[[c(2,2)]]), value = rhs[[3]], const = TRUE)
              return(call)
            }

            call <- call("declare", as.character(rhs[[2]]), value = rhs[[3]])
            return(call)
          }

          # use regular help
          call <- call("help", as.character(rhs))
          return(call)
        }

        lhs <- x[[2]]
        rhs <- x[[3]]

        ## is the rhs an assignment ?
        if(is_assign_stmt(rhs)) {
          ## is it a constant, using syntax `assertion ? (x) <- value` ?
          if(is.call(rhs[[2]]) && identical(rhs[[c(2,1)]], quote(`(`))) {
            call <- call(
              "declare", as.character(rhs[[c(2,2)]]), lhs, value = rhs[[3]], const = TRUE)
            return(call)
          }

          call <- call("declare", as.character(rhs[[2]]), lhs, value = rhs[[3]])
          return(call)
        }

        call <- call("declare", as.character(rhs), lhs)
        return(call)
      }
      x[] <- lapply(x, modify_qm_calls)
      x
    }

    body <- modify_qm_calls(body)


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # EDIT BODY TO INSERT CALLS TO `check_arg` MATCHING ANNOTATED ARGS


    fmls <- formals(value)
    nms <- names(fmls)
    annotated_fmls_lgl <-
      sapply(fmls, function(x) is.call(x) && identical(x[[1]], quote(`?`)))

    args_are_annotated <- any(annotated_fmls_lgl)
    if(args_are_annotated) {

      annotations <- annotations_attr <-
        lapply(fmls[annotated_fmls_lgl], function(x) x[[length(x)]])

      bind_lgl <- sapply(annotations, function(x) {
        is.call(x) && identical(x[[1]], quote(`+`))
      })

      lazy_lgl <- sapply(annotations, function(x) {
        is.call(x) && identical(x[[1]], quote(`~`))
      })


      annotations_attr[bind_lgl] <- lapply(annotations_attr[bind_lgl], `[[`, 2)
      annotations[bind_lgl | lazy_lgl] <- lapply(annotations[bind_lgl | lazy_lgl], `[[`, 2)

      arg_assertion_factory_calls <- Map(function(x, y, bind, lazy) {
        if(x == "...") {
          if (bind) {
            stop("Can't bind the `...`")
          }
          if (lazy) {
            if(is.call(y) && identical(y[[1]], quote(Dots))) {
              y[[1]] <- quote(Dots)
              call <- bquote(check_arg(substitute(...()), .(y)))
              return(call)
            }
            call <- bquote(check_arg(substitute(...()), Pairlist(each = .(y))))
            return(call)
          }
          if(is.call(y) && identical(y[[1]], quote(Dots))) {
            y[[1]] <- quote(Dots)
            call <- bquote(check_arg(list(...), .(y)))
            return(call)
          }
          call <- bquote(check_arg(list(...), List(each = .(y))))
          return(call)
        }
        if (bind) {
          call <- bquote(check_arg(.(as.symbol(x)), .(y), .bind = TRUE))
          return(call)
        }
        if (lazy) {
          call <- bquote(check_arg(substitute(.(as.symbol(x))), .(y)))
          return(call)
        }
        bquote(check_arg(.(as.symbol(x)), .(y)))
      }, nms[annotated_fmls_lgl], annotations, bind_lgl, lazy_lgl)


      fmls[annotated_fmls_lgl] <- lapply(fmls[annotated_fmls_lgl], function(x)
        if(length(x) == 2) quote(expr=) else x[[2]])

      if(is.call(body) && identical(body[[1]], quote(`{`)))
        body <- as.list(body)[-1]
      body <- as.call(c(quote(`{`), arg_assertion_factory_calls, body))
      # from there on, body starts with `{` in any case

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # EDIT BODY TO INSERT CALLS TO `check_output` MATCHING ANNOTATED FUNCTION

    ## is the lhs a call to `<-`
    if(lhs_is_assignment <- is_assign_stmt(lhs)) {
      return_assertion_factory <- lhs[[3]]
    } else if(lhs_is_qm <- is.call(lhs) && identical(lhs[[1]], quote(`?`))) {
      return_assertion_factory <- lhs[[c(3, 3)]]
    } else {
      return_assertion_factory <- lhs
    }

    ## did we set a return type ?
    if(!unary_qm_lgl) {
      modify_return_calls <- function(x) {
        if(!is.call(x)) return(x)
        if(identical(x[[1]], quote(`return`))) {
          x[[2]] <- bquote(check_output(.(x[[2]]), .(return_assertion_factory)))
          return(x)
        }
        x[] <- lapply(x, modify_return_calls)
        x
      }

      body <- modify_return_calls(body)

      ## modify last call if it's not a return call
      last_call <- body[[length(body)]]
      if(!is.call(last_call) || !identical(last_call[[1]], quote(return)))
        body[[length(body)]] <-
          bquote(check_output(.(last_call), .(return_assertion_factory)))

    }


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # BUILD FUNCTION

    ## build function from formals, body, and type attributes
    f <- as.function(c(fmls, body), envir =  pf)
    if(args_are_annotated)
      attr(f, "arg_types")   <- annotations_attr
    attr(f, "return_type") <- return_assertion_factory
    class(f) <- c("typed", "function")
    if(lhs_is_assignment) {
      var_nm <- as.character(lhs[[2]])
      assign(x = var_nm, value = f, envir = pf)
      return(invisible(f))
    } else if (lhs_is_qm){
      var_nm <- as.character(lhs[[c(3,2)]])
      fun_checker <- lhs[[2]]
      eval.parent(substitute(declare(var_nm, fun_checker, f), environment()))
      return(invisible(f))
    } else {
      return(f)
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # `?` TO MAP TO `declare`

  ## do we set the variable type implicitly (no lhs to `?`)
  if(unary_qm_lgl) {
    ## is the rhs an assignment ?
    if(is_assign_stmt(rhs)) {
      ## is it a constant, using syntax `? (x) <- value` ?
      if(is.call(rhs[[2]]) && identical(rhs[[c(2,1)]], quote(`(`))) {
        call <- call(
          "declare", as.character(rhs[[c(2,2)]]), value = rhs[[3]], const = TRUE)
        return(eval.parent(call))
      }

      call <- call("declare", as.character(rhs[[2]]), value = rhs[[3]])
      return(eval.parent(call))
    }

    # use regular help
    # we might tweak this to use devtools help if it's in the search path
    call <- call("help", as.character(rhs))
    return(eval.parent(call))
  }

  ## is the rhs an assignment ?
  if(is_assign_stmt(rhs)) {
    ## is it a constant, using syntax `assertion ? (x) <- value` ?
    if(is.call(rhs[[2]]) && identical(rhs[[c(2,1)]], quote(`(`))) {
      call <- call(
        "declare", as.character(rhs[[c(2,2)]]), lhs, value = rhs[[3]], const = TRUE)
      return(eval.parent(call))
    }

    call <- call("declare", as.character(rhs[[2]]), lhs, value = rhs[[3]])
    return(eval.parent(call))
  }

  call <- call("declare", as.character(rhs), lhs)
  return(eval.parent(call))
}

is_assign_stmt <- function (expr) {
  is.call(expr) && (identical(expr[[1]], quote(`<-`)) || identical(expr[[1]], quote(`=`)))
}
