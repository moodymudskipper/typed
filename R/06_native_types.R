#' Native types of package 'typed'
#'
#' These functions are type checkers, they produce assertions,
#' which take an object, verify assertions, and
#' returns the input, possibly modified (though never modified with native types).
#'
#' Additional conditions can be provided :
#'
#' * If they are named, the name should be the name of a function to use on our
#'   object, and the value should be the expected value.
#' * If they are unnamed, they should be formulas, the right hand side should
#'   be a condition, using `value` or `.` as a placeholder for the latter, and
#'   the optional `lhs` an error message.
#'
#' Any is the most general type checker, it doesn't check anything unless
#' provided additional conditions through `...`. Others use the base `is.<type>` function
#' if available, or check that the object is of the relevant type with `typeof`
#' for atomic types, or check that the class of the checked value contains
#' the relevant class.
#'
#' @param length length of the object
#' @param nrow number of rows
#' @param ncol number of columns
#' @param dim dimensions
#' @param levels factor levels
#' @param data_frame_ok whether data frames are to be considered as lists
#' @param ... additional conditions, see details.
#'
#' @usage Any(length, ...)
#' @usage Logical(length, ...)
#' @usage Integer(length, ...)
#' @usage Double(length, ...)
#' @usage Character(length, ...)
#' @usage List(length, data_frame_ok, ...)
#' @usage Null(...)
#' @usage Closure(...)
#' @usage Special(...)
#' @usage Builtin(...)
#' @usage Environment(...)
#' @usage Symbol(...)
#' @usage Pairlist(length, ...)
#' @usage Language(...)
#' @usage Expression(length, ...)
#' @usage Function(...)
#' @usage Factor(length, levels, ...)
#' @usage Matrix(nrow, ncol, ...)
#' @usage Array(dim, ...)
#' @usage Data.frame(nrow, ncol, ...)
#' @usage Date(length, ...)
#' @usage Time(length, ...)
#'
#' @export
#' @rdname native_types
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
Any <- new_type_checker(function(value, length) {
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
#' @rdname native_types
Logical <- new_type_checker(function(value, length) {
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
#' @rdname native_types
Integer <- new_type_checker(function(value, length) {
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
#' @rdname native_types
Double <- new_type_checker(function(value, length) {
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

# would override methods::Complex, of no consequence probably by annoying red ink

# #' @export
# #' @rdname native_types
# Complex <- new_type_checker(function(value, length) {
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
#' @rdname native_types
Character <- new_type_checker(function(value, length) {
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
#' @rdname native_types
Raw <- new_type_checker(function(value, length) {
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
#' @rdname native_types
List <- new_type_checker(function(value, length, data_frame_ok = TRUE) {
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
      "type mismatch",
      waldo::compare(
        length(value),
        length,
        x_arg = "length(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
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
#' @rdname native_types
Null <- new_type_checker(function(value) {
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
#' @rdname native_types
Closure <- new_type_checker(function(value) {
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
#' @rdname native_types
Special <- new_type_checker(function(value) {
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
#' @rdname native_types
Builtin <- new_type_checker(function(value) {
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
#' @rdname native_types
Environment <- new_type_checker(function(value) {
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
#' @rdname native_types
Symbol <- new_type_checker(function(value) {
  if(is.symbol(value)) {
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
#' @rdname native_types
Pairlist <- new_type_checker(function(value, length) {
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
#' @rdname native_types
Language <- new_type_checker(function(value) {
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
#' @rdname native_types
Expression <- new_type_checker(function(value, length) {
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

# function, factor, matrix, array, data.frame, date, time

#' @export
#' @rdname native_types
Function <- new_type_checker(function(value) {
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
#' @rdname native_types
Factor <- new_type_checker(function(value, length, levels) {
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
      "type mismatch",
      waldo::compare(
        length(value),
        length,
        x_arg = "length(value)",
        y_arg = "expected"))
    stop(e, call. = FALSE)
  }
  if(!missing(levels) && identical(levels(value), levels)) {
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
#' @rdname native_types
Data.frame <- new_type_checker(function(value, nrow, ncol) {
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
  value
})

#' @export
#' @rdname native_types
Matrix <- new_type_checker(function(value, nrow, ncol) {
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
#' @rdname native_types
Array <- new_type_checker(function(value, dim) {
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
#' @rdname native_types
Date <- new_type_checker(function(value, length) {
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
#' @rdname native_types
Time <- new_type_checker(function(value, length) {
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
