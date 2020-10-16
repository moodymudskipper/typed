#' @export
`?` <- function(lhs, rhs) {
  #browser()
  if(missing(rhs)) {
    rhs <- substitute(lhs)
    lhs <- NA
  } else {
    rhs <- substitute(rhs)
  }

  rhs_is_fun <-
    is.call(rhs) &&
    identical(rhs[[1]], quote(`<-`)) &&
    is.call(rhs[[3]]) &&
    identical(rhs[[c(3,1)]], quote(`function`))
  if(rhs_is_fun) {
    fun_nm <- as.character(rhs[[2]])
    ## handle fun name and type
    output_ptype <- lhs
    if(is.function(output_ptype))
      output_ptype <- output_ptype()

    ## handle formals and types
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
    env <- parent.frame()
    arg_ptypes <- sapply(tmp, function(x) {
      pt <- eval(x[[2]], env)
      if(is.function(pt)) pt() else pt})

    ## edit body to add checks
    body <- body(value)
    if(is.call(body) && identical(body[[1]], quote(`{`)))
      body <- as.list(body)[-1]
    body <- as.call(c(quote(`{`), quote(assert_types()), body))

    ## make sure that last call is `res` and that all return calls return `res`
    if(!identical(output_ptype, NA)) {
      if(!identical(body[[length(body)]], quote(res))) {
        stop("The last call of the function should be `res`", call. = FALSE)
      }
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

    f <- as.function(c(fmls, body), envir =  parent.frame())
    attr(f, "arg_ptypes")   <- arg_ptypes
    attr(f, "output_ptype") <- output_ptype
    assign(fun_nm, f, envir = parent.frame())
    return(invisible(f))
  }

  # `?` with no lhs
  if(identical(lhs, NA)) {
    if(is.call(rhs) && identical(rhs[[1]], quote(`<-`))) {
      # e.g. `?foo <- 1` or `NA?foo <- 1` : implicit prototype
      var_nm <- as.character(rhs[[2]])
      value <- eval.parent(rhs[[3]])
      ptype <- vctrs::vec_ptype(value)
    } else {
      # e.g. `?foo` or `NA?foo` : not interesting
      # we could fail but we decide to ignore
      return(invisible(NULL))
    }
  } else {

    # if has a lhs
    ptype <- lhs
    if(is.function(ptype))
      ptype <- ptype()
    # if assignment
    has_assignment <- is.call(rhs) && identical(rhs[[1]], quote(`<-`))
    if(has_assignment) {
      var_nm <- as.character(rhs[[2]])
      value <- eval.parent(rhs[[3]])
      if(!identical(vctrs::vec_ptype(value), ptype)) {
        stop("assigned value should have same prototype as `",
             var_nm, "`: ",  deparse1(ptype),
             call. = FALSE)
      }
    } else {
      var_nm <- as.character(rhs)
      value <- ptype
    }
  }

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
    envir <- parent.frame()
    eval.parent(substitute(
      {
        makeActiveBinding(
          arg,
          env = environment(),
          local({
            val <- get(arg)
            rm(list = arg, envir = envir)
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



