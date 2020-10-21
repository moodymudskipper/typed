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
  if(missing(rhs)) {
    ## assign the value to quoted rhs and define lhs as `NA`
    rhs <- substitute(lhs)
    lhs <- NA
  } else {
    ## quote lhs and rhs
    lhs <- substitute(lhs)
    rhs <- substitute(rhs)
  }

  ## is rhs is a function definition ?
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

    ## edit body to add call to `assert_types()`
    if(is.call(body) && identical(body[[1]], quote(`{`)))
      body <- as.list(body)[-1]
    body <- as.call(c(quote(`{`), arg_type_checker_calls, body))


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
    if(!identical(return_type_checker, NA)) {
      # rather than stopping, we could just make sure than function ends with
      # `(res <- last_value)`
      # and that we return `return((res <- returned_value))`

      ## does the body end with other than `res` ?
      if(!identical(body[[length(body)]], quote(res))) {
        ## fail explicitly
        stop("The last call of the function should be `res`", call. = FALSE)
      }

      ## fail if we use `return` in other way than `return(res)`
      fail_on_wrong_return <- function(x) {
        if(!is.call(x)) return(x)
        if(identical(x[[1]], quote(`return`))) {
          if(!identical(x[[2]], quote(`res`))) {
            stop("The function should only return `res`", call. = FALSE)
          }
          return(x)
        }
        x[] <- lapply(x, fail_on_wrong_return)
        x
      }
      fail_on_wrong_return(body)
      return_type_checker_call <- bquote(.(return_type_checker) ? res)
      if(is.call(body) && identical(body[[1]], quote(`{`)))
        body <- as.list(body)[-1]
      body <- as.call(c(quote(`{`), return_type_checker_call, body))
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

  ## do we set the variable type implicitly (no lhs to `?`)
  if(identical(lhs, NA)) {
    ## is the rhs an assignment ?
    if(is.call(rhs) && identical(rhs[[1]], quote(`<-`))) {
      call <- call("declare", as.character(rhs[[2]]), value = rhs[[3]])
      return(eval.parent(call))
    } else {
      # use regular help
      # we might tweak this to use devtools help if it's in the search path
      call <- call("help", as.character(rhs))
      return(eval.parent(call))
    }
  } else {
    ## is the rhs an assignment ?
    if(is.call(rhs) && identical(rhs[[1]], quote(`<-`))) {
      call <- call("declare", as.character(rhs[[2]]), lhs, rhs[[3]])
      return(eval.parent(call))
    } else {
      # use regular help
      # we might tweak this to use devtools help if it's in the search path
      call <- call("declare", as.character(rhs), lhs)
      return(eval.parent(call))
    }
  }
}
