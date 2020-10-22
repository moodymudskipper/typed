


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

  ## is rhs a function definition ?
  if(is.call(rhs) && identical(rhs[[1]], quote(`function`))) {
    ## fetch formals and formal types
    value <- eval.parent(rhs)
    body <- body(value)
    fmls <- formals(value)
    nms <- names(fmls)
    tmp_lgl <- sapply(fmls, function(x) is.call(x) && identical(x[[1]], quote(`?`)))

    arg_type_checkers <- lapply(fmls[tmp_lgl], function(x) x[[length(x)]])
    arg_type_checker_calls <- Map(function(x, y) {
      bquote(.(y) ? .(as.symbol(x)))
    }, nms[tmp_lgl], arg_type_checkers)


    fmls[tmp_lgl] <- lapply(fmls[tmp_lgl], function(x)
            if(length(x) == 2) quote(expr=) else x[[2]])

    if(is.call(body) && identical(body[[1]], quote(`{`)))
      body <- as.list(body)[-1]
    body <- as.call(c(quote(`{`), arg_type_checker_calls, body))
    # from there on, body starts with `{` in any case


    ## is the lhs a call to `<-`
    if(lhs_is_assignment <- is.call(lhs) && identical(lhs[[1]], quote(`<-`))) {
      return_type_checker <- lhs[[3]]
    } else if(lhs_is_qm <- is.call(lhs) && identical(lhs[[1]], quote(`?`))) {
      if(!is.call(lhs[[3]]) || !identical(lhs[[c(3,1)]], quote(`<-`)))
        stop("wrong syntax")
      return_type_checker <- lhs[[c(3, 3)]]
    } else {
      return_type_checker <- lhs
    }

    ## did we set a return type ?
    if(!unary_qm_lgl) {
      modify_return_calls <- function(x) {
        if(!is.call(x)) return(x)
        if(identical(x[[1]], quote(`return`))) {
          x <- call("?", return_type_checker, x)
          #x[[2]] <- as.call(c(return_type_checker, x[[2]]))
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
        call("?", return_type_checker, call("return", last_call))
        #body[[length(body)]] <- as.call(c(return_type_checker, last_call))

    }

    ## build function from formals, body, and type attributes
    f <- as.function(c(fmls, body), envir =  pf)
    attr(f, "arg_types")   <- arg_type_checkers
    attr(f, "return_type") <- return_type_checker
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

  ## is rhs a return call ?
  if(is.call(rhs) && identical(rhs[[1]], quote(`return`))) {
    assertion_call <- as.call(c(lhs, rhs[[2]]))
    value <- eval.parent(assertion_call)
    eval_bare(call("return", assertion_call), pf)
  }

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
        "declare", as.character(rhs[[c(2,2)]]), lhs, rhs[[3]], const = TRUE)
      return(eval.parent(call))
    }

    call <- call("declare", as.character(rhs[[2]]), lhs, rhs[[3]])
    return(eval.parent(call))
  }

  # use regular help
  # we might tweak this to use devtools help if it's in the search path
  call <- call("declare", as.character(rhs), lhs)
  return(eval.parent(call))
}
