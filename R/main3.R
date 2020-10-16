#' Static typing
#'
#' Use `?` to set a function's return type, argument types, or variable types
#' in the body of the function.
#'
#' @param lhs lhs
#' @param rhs rhs
#'
#' @export
#' @examples
#' numeric ? add2 <- function (x= ?numeric) {
#'   numeric ? y <- 2
#'   res <- x + y
#'   res
#' }
#' add2
#' add2(3)
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
    ## quote rhs
    rhs <- substitute(rhs)
  }

  ## find out if rhs is a function definition
  rhs_is_fun <-
    is.call(rhs) &&
    identical(rhs[[1]], quote(`<-`)) &&
    is.call(rhs[[3]]) &&
    identical(rhs[[c(3,1)]], quote(`function`))

  ## is it ?
  if(rhs_is_fun) {
    ## fetch function name and output type
    fun_nm <- as.character(rhs[[2]])
    output_ptype <- lhs

    ## is the output type a function ?
    if(is.function(output_ptype)) {
      ## overwrite it with a call to itself with no argument
      output_ptype <- output_ptype()
    }

    ## fetch formals and formal types
    value <- eval.parent(rhs[[3]])
    fmls <- formals(value)
    nms <- names(fmls)
    tmp_lgl <- sapply(fmls, function(x) is.call(x) && identical(x[[1]], quote(`?`)))
    tmp <- lapply(fmls[tmp_lgl], function(x)
      if(length(x) == 2)
        list(quote(expr=), x[[2]])
      else
        list(x[[2]], x[[3]]))
    fmls[tmp_lgl] <- lapply(tmp, `[[`, 1)
    arg_ptypes <- sapply(tmp, function(x) {
      pt <- eval(x[[2]], pf)
      if(is.function(pt)) pt() else pt})

    ## edit body to add call to `assert_types()`
    body <- body(value)
    if(is.call(body) && identical(body[[1]], quote(`{`)))
      body <- as.list(body)[-1]
    body <- as.call(c(quote(`{`), quote(assert_types()), body))

    ## did we set an output type ?
    if(!identical(output_ptype, NA)) {
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
    }

    ## build function from formals, body, and type attributes
    f <- as.function(c(fmls, body), envir =  pf)
    attr(f, "arg_ptypes")   <- arg_ptypes
    attr(f, "output_ptype") <- output_ptype
    assign(fun_nm, f, envir = pf)
    return(invisible(f))
  }

  ## do we set the variable type implicitly (no lhs to `?`)
  if(identical(lhs, NA)) {
    ## is the rhs an assignment ?
    if(is.call(rhs) && identical(rhs[[1]], quote(`<-`))) {
      ## fetch variable name
      var_nm <- as.character(rhs[[2]])
      ## is the rhs of the assignment a call to const ?
      if(is.call(rhs[[3]]) && identical(rhs[[c(3,1)]], quote(const))) {
        ## fetch value, assign, lock and return
        value <- eval.parent(rhs[[c(3,2)]])
        assign(var_nm, value, pf)
        lockBinding(var_nm, pf)
        return(invisible(value))
      }

      ## fetch value, define ptype based on value
      value <- eval.parent(rhs[[3]])
      ptype <- vctrs::vec_ptype(value)
    } else {
      ## behave as utils::`?` e.g. `?mean`
      return(help(deparse1(rhs)))
    }
  } else {
    ## define ptype as the lhs
    ptype <- lhs
    ## is it a function ?
    if(is.function(ptype)) {
      ## overwrite it with a call to itself with no arg
      ptype <- ptype()
    }
    ## is the rhs an assignment ?
    if(is.call(rhs) && identical(rhs[[1]], quote(`<-`))) {
      ## fetch variable name
      var_nm <- as.character(rhs[[2]])

      ## is the rhs of the assignment a call to const ?
      if(is.call(rhs[[3]]) && identical(rhs[[c(3,1)]], quote(const))) {
        ## fetch value
        value <- eval.parent(rhs[[c(3,2)]])
        ## is the value incompatible with ptype ?
        if(!identical(vctrs::vec_ptype(value), ptype)) {
          ## fail explicitly
          stop("assigned value should have same prototype as `",
               var_nm, "`: ",  deparse1(ptype),
               call. = FALSE)
        }
        ## assign, lock and return
        assign(var_nm, value, pf)
        lockBinding(var_nm, pf)
        return(invisible(value))
      }

      ## fetch value
      value <- eval.parent(rhs[[3]])
      ## is the value incompatible with ptype ?
      if(!identical(vctrs::vec_ptype(value), ptype)) {
        ## fail explicitly
        stop("assigned value should have same prototype as `",
             var_nm, "`: ",  deparse1(ptype),
             call. = FALSE)
      }
    } else {
      ## fetch variable name define value as ptype
      var_nm <- as.character(rhs)
      value <- ptype
    }
  }

  ## create active binding, allowing only assignments of same ptype
  eval.parent(substitute(
    makeActiveBinding(
      var_nm,
      env = environment(),
      local({
        val <- value
        function(v) {
          if (!missing(v)) {
            if(!identical(vctrs::vec_ptype(val), vctrs::vec_ptype(v)))
              stop("assigned value should have same prototype as `",
                   var_nm, "`: ",  deparse1(ptype),
                   call. = FALSE)
            val <<- v
          }
          val
        }
      })), environment()))
  return(invisible(value))
}

#' @export
assert_types <- function() {
  f <- eval.parent(eval.parent(quote(match.call()))[[1]], 2)
  pf <- parent.frame()
  ptypes <- attr(f, "arg_ptypes")
  output_ptype <- attr(f, "output_ptype")
  if(!identical(output_ptype, NA)) {
    eval.parent(substitute(
      {
        makeActiveBinding(
          "res",
          env = environment(),
          local({
            val <- output_ptype
            function(v) {
              if (!missing(v)) {
                if(!identical(vctrs::vec_ptype(val), vctrs::vec_ptype(v)))
                  stop("assigned value should have same prototype as `res`: ",
                       deparse1(output_ptype), call. = FALSE)
                val <<- v
              }
              val
            }
          }))
      }, environment()))
  }
  for(arg in names(ptypes)) {
    ptype <- ptypes[[arg]]
    eval.parent(substitute(
      {
        makeActiveBinding(
          arg,
          env = environment(),
          local({
            val <- get(arg)
            rm(list = arg, envir = pf)
            if(!identical(vctrs::vec_ptype(val), ptype))
              stop("`", arg, "`'s prototype should be ", deparse1(ptype), call. = FALSE)
            function(v) {
              if (!missing(v)) {
                if(!identical(vctrs::vec_ptype(val), vctrs::vec_ptype(v)))
                  stop("assigned value should have same prototype as `",
                       arg, "`: ", deparse1(ptype), call. = FALSE)
                val <<- v
              }
              val
            }
          }))
      }, environment()))
  }
}


#' Define Constants
#'
#' `const()` is meant to be used using the syntax `? x <- const(3)` or `? x <- const(3)`.
#' The variable then cannot be modified anymore.
#'
#' @param x value
#'
#' @export
const <- function(x) {
  warning(
    "const should be used with question mark syntax, e.g. ",
    "`? x <- const(3)` or ? `x <- const(3)`")
  x
}



