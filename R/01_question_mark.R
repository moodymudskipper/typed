


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
#' @examples
#' numeric ? function (x= ?numeric) {
#'   numeric ? y <- 2
#'   res <- x + y
#'   res
#' }
#' @rdname static_typing
`?` <- function(lhs, rhs) {
  # commented so can be viewed with
  # flow::flow_view(`?`, prefix = "##", code = NA, out = "png")
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
          if(is.call(rhs) && identical(rhs[[1]], quote(`<-`))) {
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
        if(is.call(rhs) && identical(rhs[[1]], quote(`<-`))) {
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

      annotations <-
        lapply(fmls[annotated_fmls_lgl], function(x) x[[length(x)]])

      bind_lgl <- sapply(annotations, function(x) {
        is.call(x) && identical(x[[1]], quote(`+`))
      })

      lazy_lgl <- sapply(annotations, function(x) {
        is.call(x) && identical(x[[1]], quote(`~`))
      })

      annotations[bind_lgl | lazy_lgl] <- lapply(annotations[bind_lgl | lazy_lgl], `[[`, 2)

      arg_assertion_factory_calls <- Map(function(x, y, bind, lazy) {
        if (bind) {
          bquote(check_arg(.(as.symbol(x)), .(y), .bind = TRUE))
        } else if (lazy) {
          bquote(check_arg(substitute(.(as.symbol(x))), .(y)))
        } else {
          bquote(check_arg(.(as.symbol(x)), .(y)))
        }
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
    if(lhs_is_assignment <- is.call(lhs) && identical(lhs[[1]], quote(`<-`))) {
      return_assertion_factory <- lhs[[3]]
    } else if(lhs_is_qm <- is.call(lhs) && identical(lhs[[1]], quote(`?`))) {
      if(!is.call(lhs[[3]]) || !identical(lhs[[c(3,1)]], quote(`<-`)))
        stop("wrong syntax")
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
      attr(f, "arg_types")   <- annotations
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

  ## is rhs a return call ?
  # if(is.call(rhs) && identical(rhs[[1]], quote(`return`))) {
  #   assertion_call <- as.call(c(lhs, rhs[[2]]))
  #   value <- try(eval.parent(assertion_call), silent = TRUE)
  #   if(inherits(value, "try-error")) {
  #     e <- attr(value, "condition")$message
  #     fun_call <- sys.call(-1)
  #     if(!is.null(fun_call)) {
  #       fun_call <- deparse1(fun_call)
  #       return_call <- sys.call()
  #       return_call <- paste(
  #         deparse1(return_call[[2]]), "?", deparse1(return_call[[3]]))
  #       e <- sprintf("In `%s` at `%s`:\nwrong return value, %s", fun_call, return_call, e)
  #     }
  #     stop(e, call. = FALSE)
  #   }
  #   eval_bare(call("return", assertion_call), pf)
  # }

  ## do we set the variable type implicitly (no lhs to `?`)
  if(unary_qm_lgl) {
    ## is the rhs an assignment ?
    if(is.call(rhs) && identical(rhs[[1]], quote(`<-`))) {
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
  if(is.call(rhs) && identical(rhs[[1]], quote(`<-`))) {
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
