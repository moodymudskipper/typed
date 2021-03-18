#' Assertion factories of package 'typed'
#'
#' These functions are assertion factories, they produce assertions,
#' which take an object, check conditions, and
#' returns the input, usually unmodified (never modified with the functions
#' documented on this page).
#'
#' Additional conditions can be provided :
#'
#' * If they are named, the name should be the name of a function to use on our
#'   object, and the value should be the expected value.
#' * If they are unnamed, they should be formulas, the right hand side should
#'   be a condition, using `value` or `.` as a placeholder for the latter, and
#'   the optional `lhs` an error message.
#'
#' `Any` is the most general assertion factory, it doesn't check anything unless
#' provided additional conditions through `...`. Others use the base `is.<type>` function
#' if available, or check that the object is of the relevant type with `typeof`
#' for atomic types, or check that the class of the checked value contains
#' the relevant class.
#'
#' `Dots` should only be used to check the dots using `check_arg` on `list(...)`
#' or `substitute(...())`, which will
#' be the case when it's called respectively with `function(... = ? Dots())`
#' and `function(... = ?~ Dots())`
#'
#' @param length length of the object
#' @param nrow number of rows
#' @param ncol number of columns
#' @param each assertion that every item must satisfy
#' @param dim dimensions
#' @param levels factor levels
#' @param data_frame_ok whether data frames are to be considered as lists
#' @param null_ok whether `NULL` values should be accepted, and not subjected to
#'   any further check.
#' @param ... additional conditions, see details.
#'
#' @usage Any(length, ...)
#' @usage Logical(length, null_ok = FALSE, ...)
#' @usage Integer(length, null_ok = FALSE, ...)
#' @usage Double(length, null_ok = FALSE, ...)
#' @usage Character(length, null_ok = FALSE, ...)
#' @usage Raw(length, null_ok = FALSE, ...)
#' @usage List(length, each, data_frame_ok, null_ok = FALSE, ...)
#' @usage Null(...)
#' @usage Closure(null_ok = FALSE, ...)
#' @usage Special(null_ok = FALSE, ...)
#' @usage Builtin(null_ok = FALSE, ...)
#' @usage Environment(null_ok = FALSE, ...)
#' @usage Symbol(null_ok = FALSE, ...)
#' @usage Pairlist(length, each, null_ok = TRUE, ...)
#' @usage Language(null_ok = FALSE, ...)
#' @usage Expression(length, null_ok = FALSE, ...)
#' @usage Function(null_ok = FALSE, ...)
#' @usage Factor(length, levels, null_ok = FALSE, ...)
#' @usage Matrix(nrow, ncol, null_ok = FALSE, ...)
#' @usage Array(dim, null_ok = FALSE, ...)
#' @usage Data.frame(nrow, ncol, each, null_ok = FALSE, ...)
#' @usage Date(length, null_ok = FALSE, ...)
#' @usage Time(length, null_ok = FALSE, ...)
#' @usage Dots(length, each, ...)
#'
#' @export
#' @return A function, and more specifically, an assertion as defined above.
#' @rdname assertion_factories
#' @examples
#'
#' \dontrun{
#' # fails
#' Integer() ? x <- 1
#' # equivalent to
#' declare("x", Integer(), value = 1)
#'
#' Integer(2) ? x <- 1L
#'
#' # we can use additional conditions in `...`
#' Integer(anyNA = FALSE) ? x <- c(1L, NA, 1L)
#' Integer(anyDuplicated = FALSE) ? x <- c(1L, NA, 1L)
#' }
#'
#' Integer(2) ? x <- 11:12
#'
#' \dontrun{
#' # We can also use it directly to test assertions
#' Integer() ? x <- 1
#' # equivalent to
#' declare("x", Integer(), value = 1)
#'
#' Integer(2) ? x <- 1L
#' }
#'
Any <- as_assertion_factory(function(value, length) {
  if(!missing(length) && length(value) != length) {
    length <- as.integer(length)
    e <- sprintf(
      "%s\n%s",
      "length mismatch",
      waldo::compare(
        length(value),
        length,
        x_arg = "length(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  value
})

#' @export
#' @rdname assertion_factories
Logical <- as_assertion_factory(function(value, length, null_ok = FALSE) {
  if(null_ok && is.null(value)) return(NULL)
  if(!is.logical(value)) {
    e <- sprintf(
      "%s\n%s",
      "type mismatch",
      waldo::compare(
        typeof(value),
        "logical",
        x_arg = "typeof(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  if(!missing(length) && length(value) != length) {
    length <- as.integer(length)
    e <- sprintf(
      "%s\n%s",
      "type mismatch",
      waldo::compare(
        length(value),
        length,
        x_arg = "length(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  value
})

#' @export
#' @rdname assertion_factories
Integer <- as_assertion_factory(function(value, length, null_ok = FALSE) {
  if(null_ok && is.null(value)) return(NULL)
  if(!is.integer(value)) {
    e <- sprintf(
      "%s\n%s",
      "type mismatch",
      waldo::compare(
        typeof(value),
        "integer",
        x_arg = "typeof(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  if(!missing(length) && length(value) != length) {
    length <- as.integer(length)
    e <- sprintf(
      "%s\n%s",
      "length mismatch",
      waldo::compare(
        length(value),
        length,
        x_arg = "length(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  value
})

#' @export
#' @rdname assertion_factories
Double <- as_assertion_factory(function(value, length, null_ok = FALSE) {
  if(null_ok && is.null(value)) return(NULL)
  if(!is.double(value)) {
    e <- sprintf(
      "%s\n%s",
      "type mismatch",
      waldo::compare(
        typeof(value),
        "double",
        x_arg = "typeof(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  if(!missing(length) && length(value) != length) {
    length <- as.integer(length)
    e <- sprintf(
      "%s\n%s",
      "length mismatch",
      waldo::compare(
        length(value),
        length,
        x_arg = "length(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  value
})

# would override methods::Complex, of no consequence probably by annoying red ink

# #' @export
# #' @rdname assertion_factories
# Complex <- as_assertion_factory(function(value, length) {
#   if(!is.complex(value)) {
#     print(waldo::compare(
#       "complex",
#       typeof(value),
#       x_arg = "expected",
#       y_arg = "typeof(value)"))
#     stop("type mismatch", call. = FALSE)
#   }
#   if(!missing(length) && length(value) != length) {
#     length <- as.integer(length)
#     print(waldo::compare(
#       length,
#       length(value),
#       x_arg = "expected",
#       y_arg = "length(value)"))
#     stop("length mismatch", call. = FALSE)
#   }
#   value
# })

#' @export
#' @rdname assertion_factories
Character <- as_assertion_factory(function(value, length, null_ok = FALSE) {
  if(null_ok && is.null(value)) return(NULL)
  if(!is.character(value)) {
    e <- sprintf(
      "%s\n%s",
      "type mismatch",
      waldo::compare(
        typeof(value),
        "character",
        x_arg = "typeof(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  if(!missing(length) && length(value) != length) {
    length <- as.integer(length)
    e <- sprintf(
      "%s\n%s",
      "length mismatch",
      waldo::compare(
        length(value),
        length,
        x_arg = "length(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  value
})

#' @export
#' @rdname assertion_factories
Raw <- as_assertion_factory(function(value, length, null_ok = FALSE) {
  if(null_ok && is.null(value)) return(NULL)
  if(!is.raw(value)) {
    e <- sprintf(
      "%s\n%s",
      "type mismatch",
      waldo::compare(
        typeof(value),
        "raw",
        x_arg = "typeof(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  if(!missing(length) && length(value) != length) {
    length <- as.integer(length)
    e <- sprintf(
      "%s\n%s",
      "length mismatch",
      waldo::compare(
        length(value),
        length,
        x_arg = "length(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  value
})

#' @export
#' @rdname assertion_factories
List <- as_assertion_factory(function(value, length, each, data_frame_ok = TRUE, null_ok = FALSE) {
  if(null_ok && is.null(value)) return(NULL)
  if(!is.list(value)) {
    e <- sprintf(
      "%s\n%s",
      "type mismatch",
      waldo::compare(
        typeof(value),
        "list",
        x_arg = "typeof(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  if(!missing(length) && length(value) != length) {
    length <- as.integer(length)
    e <- sprintf(
      "%s\n%s",
      "length mismatch",
      waldo::compare(
        length(value),
        length,
        x_arg = "length(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }

  if(!missing(each)) {
    nms <- allNames(value)
    for (i in seq_along(value)) {
      tryCatch(each(value[[i]]), error = function(e) {
        if(nms[[i]] == "") {
          stop(sprintf("element %s %s",i , e$message), call. = FALSE)
        } else {
          stop(sprintf('element %s ("%s") %s',i , nms[[i]], e$message), call. = FALSE)
        }
      })
    }
  }

  if(!data_frame_ok && is.data.frame(value)) {
    e <- sprintf(
      "%s\n%s",
      "type mismatch",
      waldo::compare(
        is.data.frame(value),
        FALSE,
        x_arg = "is.data.frame(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  value
})

#' @export
#' @rdname assertion_factories
Null <- as_assertion_factory(function(value) {
  if(!is.null(value)) {
    e <- sprintf(
      "%s\n%s",
      "type mismatch",
      waldo::compare(
        typeof(value),
        "NULL",
        x_arg = "typeof(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  value
})

#' @export
#' @rdname assertion_factories
Closure <- as_assertion_factory(function(value, null_ok = FALSE) {
  if(null_ok && is.null(value)) return(NULL)
  if(typeof(value) != "closure") {
    e <- sprintf(
      "%s\n%s",
      "type mismatch",
      waldo::compare(
        typeof(value),
        "closure",
        x_arg = "typeof(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  value
})

#' @export
#' @rdname assertion_factories
Special <- as_assertion_factory(function(value, null_ok = FALSE) {
  if(null_ok && is.null(value)) return(NULL)
  if(typeof(value) != "special") {
    e <- sprintf(
      "%s\n%s",
      "type mismatch",
      waldo::compare(
        typeof(value),
        "special",
        x_arg = "typeof(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  value
})

#' @export
#' @rdname assertion_factories
Builtin <- as_assertion_factory(function(value, null_ok = FALSE) {
  if(null_ok && is.null(value)) return(NULL)
  if(typeof(value) != "builtin") {
    e <- sprintf(
      "%s\n%s",
      "type mismatch",
      waldo::compare(
        typeof(value),
        "builtin",
        x_arg = "typeof(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  value
})

#' @export
#' @rdname assertion_factories
Environment <- as_assertion_factory(function(value, null_ok = FALSE) {
  if(null_ok && is.null(value)) return(NULL)
  if(!is.environment(value)) {
    e <- sprintf(
      "%s\n%s",
      "type mismatch",
      waldo::compare(
        typeof(value),
        "environment",
        x_arg = "typeof(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  value
})

#' @export
#' @rdname assertion_factories
Symbol <- as_assertion_factory(function(value, null_ok = FALSE) {
  if(null_ok && is.null(value)) return(NULL)
  if(!is.symbol(value)) {
    e <- sprintf(
      "%s\n%s",
      "type mismatch",
      waldo::compare(
        typeof(value),
        "symbol",
        x_arg = "typeof(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  value
})

#' @export
#' @rdname assertion_factories
Pairlist <- as_assertion_factory(function(value, length, each, null_ok = TRUE) {
  if(is.null(value)) {
    if (null_ok) return(NULL) else stop("`value` can't be NULL", call. = FALSE)
  }
  if(!is.pairlist(value)) {
    e <- sprintf(
      "%s\n%s",
      "type mismatch",
      waldo::compare(
        typeof(value),
        "pairlist",
        x_arg = "typeof(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  if(!missing(each)) {
    nms <- allNames(value)
    for (i in seq_along(value)) {
      tryCatch(each(value[[i]]), error = function(e) {
        if(nms[[i]] == "") {
          stop(sprintf("element %s %s",i , e$message), call. = FALSE)
        } else {
          stop(sprintf('element %s ("%s") %s',i , nms[[i]], e$message), call. = FALSE)
        }
      })
    }
  }
  if(!missing(length) && length(value) != length) {
    length <- as.integer(length)
    e <- sprintf(
      "%s\n%s",
      "length mismatch",
      waldo::compare(
        length(value),
        length,
        x_arg = "length(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  value
})

#' @export
#' @rdname assertion_factories
Language <- as_assertion_factory(function(value, null_ok = FALSE) {
  if(null_ok && is.null(value)) return(NULL)
  if(typeof(value) != "language") {
    e <- sprintf(
      "%s\n%s",
      "type mismatch",
      waldo::compare(
        typeof(value),
        "language",
        x_arg = "typeof(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  value
})

#' @export
#' @rdname assertion_factories
Expression <- as_assertion_factory(function(value, length, null_ok = FALSE) {
  if(null_ok && is.null(value)) return(NULL)
  if(typeof(value) != "expression") {
    e <- sprintf(
      "%s\n%s",
      "type mismatch",
      waldo::compare(
        typeof(value),
        "expression",
        x_arg = "typeof(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  if(!missing(length) && length(value) != length) {
    length <- as.integer(length)
    e <- sprintf(
      "%s\n%s",
      "length mismatch",
      waldo::compare(
        length(value),
        length,
        x_arg = "length(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  value
})

# function, factor, matrix, array, data.frame, date, time

#' @export
#' @rdname assertion_factories
Function <- as_assertion_factory(function(value, null_ok = FALSE) {
  if(null_ok && is.null(value)) return(NULL)
  if(!is.function(value)) {
    e <- sprintf(
      "%s\n%s",
      "type mismatch",
      waldo::compare(
        is.function(value),
        TRUE,
        x_arg = "is.function(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  value
})

#' @export
#' @rdname assertion_factories
Factor <- as_assertion_factory(function(value, length, levels, null_ok = FALSE) {
  if(null_ok && is.null(value)) return(NULL)
  if(!is.factor(value)) {
    e <- sprintf(
      "%s\n%s",
      "type mismatch",
      waldo::compare(
        is.factor(value),
        TRUE,
        x_arg = "is.factor(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  if(!missing(length) && length(value) != length) {
    length <- as.integer(length)
    e <- sprintf(
      "%s\n%s",
      "length mismatch",
      waldo::compare(
        length(value),
        length,
        x_arg = "length(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  if(!missing(levels) && !identical(levels(value), levels)) {
    e <- sprintf(
      "%s\n%s",
      "type mismatch",
      waldo::compare(
        levels(value),
        levels,
        x_arg = "levels(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  value
})

#' @export
#' @rdname assertion_factories
Data.frame <- as_assertion_factory(function(value, nrow, ncol, each, null_ok = FALSE) {
  if(null_ok && is.null(value)) return(NULL)
  if(!is.data.frame(value)) {
    e <- sprintf(
      "%s\n%s",
      "type mismatch",
      waldo::compare(
        class(value),
        "data.frame",
        x_arg = "class(value)",
        y_arg = "expected to contain"))
    stop(e, call. = FALSE)
  }
  if(!missing(nrow) && nrow(value) != nrow) {
    nrow <- as.integer(nrow)
    e <- sprintf(
      "%s\n%s",
      "Row number mismatch",
      waldo::compare(
        nrow(value),
        nrow,
        x_arg = "nrow(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  if(!missing(ncol) && ncol(value) != ncol) {
    ncol <- as.integer(ncol)
    e <- sprintf(
      "%s\n%s",
      "Column number mismatch",
      waldo::compare(
        ncol(value),
        ncol,
        x_arg = "ncol(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }

  if(!missing(each)) {
    nms <- allNames(value)
    for (i in seq_along(value)) {
      tryCatch(each(value[[i]]), error = function(e) {
          stop(sprintf('column %s ("%s") %s',i , nms[[i]], e$message), call. = FALSE)
        })
    }
  }
  value
})

#' @export
#' @rdname assertion_factories
Matrix <- as_assertion_factory(function(value, nrow, ncol, null_ok = FALSE) {
  if(null_ok && is.null(value)) return(NULL)
  if(!is.matrix(value)) {
    e <- sprintf(
      "%s\n%s",
      "type mismatch",
      waldo::compare(
        class(value),
        "matrix",
        x_arg = "class(value)",
        y_arg = "expected to contain"))
    stop(e, call. = FALSE)
  }
  if(!missing(nrow) && nrow(value) != nrow) {
    nrow <- as.integer(nrow)
    e <- sprintf(
      "%s\n%s",
      "Row number mismatch",
      waldo::compare(
        nrow(value),
        nrow,
        x_arg = "nrow(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  if(!missing(ncol) && ncol(value) != ncol) {
    ncol <- as.integer(ncol)
    e <- sprintf(
      "%s\n%s",
      "Column number mismatch",
      waldo::compare(
        ncol(value),
        ncol,
        x_arg = "ncol(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  value
})

#' @export
#' @rdname assertion_factories
Array <- as_assertion_factory(function(value, dim, null_ok = FALSE) {
  if(null_ok && is.null(value)) return(NULL)
  if(!is.array(value)) {
    e <- sprintf(
      "%s\n%s",
      "type mismatch",
      waldo::compare(
        is.array(value),
        TRUE,
        x_arg = "is.array(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  if(!missing(dim) && !identical(dim(value), as.integer(dim))) {
    dim <- as.integer(dim)
    e <- sprintf(
      "%s\n%s",
      "dimension mismatch",
      waldo::compare(
        dim(value),
        dim,
        x_arg = "dim(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  value
})

#' @export
#' @rdname assertion_factories
Date <- as_assertion_factory(function(value, length, null_ok = FALSE) {
  if(null_ok && is.null(value)) return(NULL)
  if(!"Date" %in% class(value)) {
    e <- sprintf(
      "%s\n%s",
      "type mismatch",
      waldo::compare(
        class(value),
        "Date",
        x_arg = "class(value)",
        y_arg = "expected to contain"))
    stop(e, call. = FALSE)
  }

  if(!missing(length) && length(value) != length) {
    length <- as.integer(length)
    e <- sprintf(
      "%s\n%s",
      "length mismatch",
      waldo::compare(
        length(value),
        length,
        x_arg = "length(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  value
})

#' @export
#' @rdname assertion_factories
Time <- as_assertion_factory(function(value, length, null_ok = FALSE) {
  if(null_ok && is.null(value)) return(NULL)
  if(!"POSIXct" %in% class(value)) {
    e <- sprintf(
      "%s\n%s",
      "type mismatch",
      waldo::compare(
        class(value),
        "POSIXct",
        x_arg = "class(value)",
        y_arg = "expected to contain"))
    stop(e, call. = FALSE)
  }

  if(!missing(length) && length(value) != length) {
    length <- as.integer(length)
    e <- sprintf(
      "%s\n%s",
      "length mismatch",
      waldo::compare(
        length(value),
        length,
        x_arg = "length(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  value
})

#' @export
#' @rdname assertion_factories
Dots <- as_assertion_factory(function(value, length, each) {
  if(!missing(length) && length(value) != length) {
    length <- as.integer(length)
    e <- sprintf(
      "%s\n%s",
      "length mismatch",
      waldo::compare(
        length(value),
        length,
        x_arg = "length(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }

  if(!missing(each)) {
    nms <- allNames(value)
    for (i in seq_along(value)) {
      tryCatch(each(value[[i]]), error = function(e) {
        if(nms[[i]] == "") {
          stop(sprintf("element %s %s",i , e$message), call. = FALSE)
        } else {
          stop(sprintf('element %s ("%s") %s',i , nms[[i]], e$message), call. = FALSE)
        }
      })
    }
  }
  value
})
