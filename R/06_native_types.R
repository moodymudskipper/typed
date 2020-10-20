#' Native types of package 'typed'
#'
#' These functions are type checkers, they produce assertion functions,
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
    print(waldo::compare(
      length,
      length(value),
      x_arg = "expected",
      y_arg = "length(value)"))
    stop("length mismatch", call. = FALSE)
  }
  value
})

#' @export
#' @rdname native_types
Logical <- new_type_checker(function(value, length) {
  if(!is.logical(value)) {
    print(waldo::compare(
      "logical",
      typeof(value),
      x_arg = "expected",
      y_arg = "typeof(value)"))
    stop("type mismatch", call. = FALSE)
  }
  if(!missing(length) && length(value) != length) {
    length <- as.integer(length)
    print(waldo::compare(
      length,
      length(value),
      x_arg = "expected",
      y_arg = "length(value)"))
    stop("length mismatch", call. = FALSE)
  }
  value
})

#' @export
#' @rdname native_types
Integer <- new_type_checker(function(value, length) {
  if(!is.integer(value)) {
    print(waldo::compare(
      "integer",
      typeof(value),
      x_arg = "expected",
      y_arg = "typeof(value)"))
    stop("type mismatch", call. = FALSE)
  }
  if(!missing(length) && length(value) != length) {
    length <- as.integer(length)
    print(waldo::compare(
      length,
      length(value),
      x_arg = "expected",
      y_arg = "length(value)"))
    stop("length mismatch", call. = FALSE)
  }
  value
})

#' @export
#' @rdname native_types
Double <- new_type_checker(function(value, length) {
  if(!is.double(value)) {
    print(waldo::compare(
      "double",
      typeof(value),
      x_arg = "expected",
      y_arg = "typeof(value)"))
    stop("type mismatch", call. = FALSE)
  }
  if(!missing(length) && length(value) != length) {
    length <- as.integer(length)
    print(waldo::compare(
      length,
      length(value),
      x_arg = "expected",
      y_arg = "length(value)"))
    stop("length mismatch", call. = FALSE)
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
    print(waldo::compare(
      "character",
      typeof(value),
      x_arg = "expected",
      y_arg = "typeof(value)"))
    stop("type mismatch", call. = FALSE)
  }
  if(!missing(length) && length(value) != length) {
    length <- as.integer(length)
    print(waldo::compare(
      length,
      length(value),
      x_arg = "expected",
      y_arg = "length(value)"))
    stop("length mismatch", call. = FALSE)
  }
  value
})

#' @export
#' @rdname native_types
Raw <- new_type_checker(function(value, length) {
  if(!is.raw(value)) {
    print(waldo::compare(
      "raw",
      typeof(value),
      x_arg = "expected",
      y_arg = "typeof(value)"))
    stop("type mismatch", call. = FALSE)
  }
  if(!missing(length) && length(value) != length) {
    length <- as.integer(length)
    print(waldo::compare(
      length,
      length(value),
      x_arg = "expected",
      y_arg = "length(value)"))
    stop("length mismatch", call. = FALSE)
  }
  value
})

#' @export
#' @rdname native_types
List <- new_type_checker(function(value, length, data_frame_ok = TRUE) {
  if(!is.list(value)) {
    print(waldo::compare(
      "list",
      typeof(value),
      x_arg = "expected",
      y_arg = "typeof(value)"))
    stop("type mismatch", call. = FALSE)
  }
  if(!missing(length) && length(value) != length) {
    length <- as.integer(length)
    print(waldo::compare(
      length,
      length(value),
      x_arg = "expected",
      y_arg = "length(value)"))
    stop("length mismatch", call. = FALSE)
  }
  if(!data_frame_ok && is.data.frame(value)) {
    print(waldo::compare(
      FALSE,
      is.data.frame(value),
      x_arg = "expected",
      y_arg = "is.data.frame(value)"))
    stop("`value` can't be a data frame.", call. = FALSE)
  }
  value
})

#' @export
#' @rdname native_types
Null <- new_type_checker(function(value) {
  if(!is.null(value)) {
    print(waldo::compare(
      "NULL",
      typeof(value),
      x_arg = "expected",
      y_arg = "typeof(value)"))
    stop("type mismatch", call. = FALSE)
  }
  value
})

#' @export
#' @rdname native_types
Closure <- new_type_checker(function(value) {
  if(typeof(value) == "closure") {
    print(waldo::compare(
      "closure",
      typeof(value),
      x_arg = "expected",
      y_arg = "typeof(value)"))
    stop("type mismatch", call. = FALSE)
  }
  value
})

#' @export
#' @rdname native_types
Special <- new_type_checker(function(value) {
  if(typeof(value) != "special") {
    print(waldo::compare(
      "special",
      typeof(value),
      x_arg = "expected",
      y_arg = "typeof(value)"))
    stop("type mismatch", call. = FALSE)
  }
  value
})

#' @export
#' @rdname native_types
Builtin <- new_type_checker(function(value) {
  if(typeof(value) != "builtin") {
    print(waldo::compare(
      "builtin",
      typeof(value),
      x_arg = "expected",
      y_arg = "typeof(value)"))
    stop("type mismatch", call. = FALSE)
  }
  value
})

#' @export
#' @rdname native_types
Environment <- new_type_checker(function(value) {
  if(!is.environment(value)) {
    print(waldo::compare(
      "environment",
      typeof(value),
      x_arg = "expected",
      y_arg = "typeof(value)"))
    stop("type mismatch", call. = FALSE)
  }
  value
})

#' @export
#' @rdname native_types
Symbol <- new_type_checker(function(value) {
  if(is.symbol(value)) {
    print(waldo::compare(
      "symbol",
      typeof(value),
      x_arg = "expected",
      y_arg = "typeof(value)"))
    stop("type mismatch", call. = FALSE)
  }
  value
})

#' @export
#' @rdname native_types
Pairlist <- new_type_checker(function(value, length) {
  if(!is.pairlist(value)) {
    print(waldo::compare(
      "pairlist",
      typeof(value),
      x_arg = "expected",
      y_arg = "typeof(value)"))
    stop("type mismatch", call. = FALSE)
  }
  if(!missing(length) && length(value) != length) {
    length <- as.integer(length)
    print(waldo::compare(
      length,
      length(value),
      x_arg = "expected",
      y_arg = "length(value)"))
    stop("length mismatch", call. = FALSE)
  }
  value
})

#' @export
#' @rdname native_types
Language <- new_type_checker(function(value) {
  if(typeof(value) != "language") {
    print(waldo::compare(
      "language",
      typeof(value),
      x_arg = "expected",
      y_arg = "typeof(value)"))
    stop("type mismatch", call. = FALSE)
  }
  value
})

#' @export
#' @rdname native_types
Expression <- new_type_checker(function(value, length) {
  if(typeof(value) != "expression") {
    print(waldo::compare(
      "expression",
      typeof(value),
      x_arg = "expected",
      y_arg = "typeof(value)"))
    stop("type mismatch", call. = FALSE)
  }
  if(!missing(length) && length(value) != length) {
    length <- as.integer(length)
    print(waldo::compare(
      length,
      length(value),
      x_arg = "expected",
      y_arg = "length(value)"))
    stop("length mismatch", call. = FALSE)
  }
  value
})

# function, factor, matrix, array, data.frame, date, time

#' @export
#' @rdname native_types
Function <- new_type_checker(function(value) {
  if(!is.function(value)) {
    print(waldo::compare(
      TRUE,
      is.function(value),
      x_arg = "expected",
      y_arg = "is.function(value)"))
    stop("type mismatch", call. = FALSE)
  }
  value
})

#' @export
#' @rdname native_types
Factor <- new_type_checker(function(value, length, levels) {
  if(!is.factor(value)) {
    print(waldo::compare(
      TRUE,
      is.function(value),
      x_arg = "expected",
      y_arg = "is.factor(value)"))
    stop("type mismatch", call. = FALSE)
  }
  if(!missing(length) && length(value) != length) {
    length <- as.integer(length)
    print(waldo::compare(
      length,
      length(value),
      x_arg = "expected",
      y_arg = "length(value)"))
    stop("length mismatch", call. = FALSE)
  }
  if(!missing(levels) && identical(levels(value), levels)) {
    print(waldo::compare(
      levels,
      levels(value),
      x_arg = "expected",
      y_arg = "levels(value)"))
    stop("levels mismatch", call. = FALSE)
  }
  value
})

#' @export
#' @rdname native_types
Data.frame <- new_type_checker(function(value, nrow, ncol) {
  if(!is.data.frame(value)) {
    print(waldo::compare(
      "data.frame",
      class(value),
      x_arg = "expected to contain",
      y_arg = "class(value)"))
    stop("class mismatch", call. = FALSE)
  }
  if(!missing(nrow) && nrow(value) != nrow) {
    nrow <- as.integer(nrow)
    print(waldo::compare(
      nrow,
      nrow(value),
      x_arg = "expected",
      y_arg = "nrow(value)"))
    stop("Row number mismatch", call. = FALSE)
  }
  if(!missing(ncol) && ncol(value) != ncol) {
    ncol <- as.integer(ncol)
    print(waldo::compare(
      ncol,
      ncol(value),
      x_arg = "expected",
      y_arg = "ncol(value)"))
    stop("Column number mismatch", call. = FALSE)
  }
  value
})

#' @export
#' @rdname native_types
Matrix <- new_type_checker(function(value, nrow, ncol) {
  if(!is.matrix(value)) {
    print(waldo::compare(
      "matrix",
      class(value),
      x_arg = "expected to contain",
      y_arg = "class(value)"))
    stop("class mismatch", call. = FALSE)
  }
  if(!missing(nrow) && nrow(value) != nrow) {
    nrow <- as.integer(nrow)
    print(waldo::compare(
      nrow,
      nrow(value),
      x_arg = "expected",
      y_arg = "nrow(value)"))
    stop("Row number mismatch", call. = FALSE)
  }
  if(!missing(ncol) && ncol(value) != ncol) {
    ncol <- as.integer(ncol)
    print(waldo::compare(
      ncol,
      ncol(value),
      x_arg = "expected",
      y_arg = "ncol(value)"))
    stop("Column number mismatch", call. = FALSE)
  }
  value
})

#' @export
#' @rdname native_types
Array <- new_type_checker(function(value, dim) {
  if(!is.array(value)) {
    print(waldo::compare(
      TRUE,
      is.array(value),
      x_arg = "expected",
      y_arg = "is.array(value)"))
    stop("class mismatch", call. = FALSE)
  }
  if(!missing(dim) && !identical(dim(value), as.integer(dim))) {
    dim <- as.integer(dim)
    print(waldo::compare(
      dim,
      dim(value),
      x_arg = "expected",
      y_arg = "dim(value)"))
    stop("Dimension mismatch", call. = FALSE)
  }
  value
})

#' @export
#' @rdname native_types
Date <- new_type_checker(function(value, length) {
  if(!"Date" %in% class(value)) {
    print(waldo::compare(
      "Date",
      class(value),
      x_arg = "expected to contain",
      y_arg = "class(value)"))
    stop("class mismatch", call. = FALSE)
  }

  if(!missing(length) && length(value) != length) {
    length <- as.integer(length)
    print(waldo::compare(
      length,
      length(value),
      x_arg = "expected",
      y_arg = "length(value)"))
    stop("length mismatch", call. = FALSE)
  }
  value
})

#' @export
#' @rdname native_types
Time <- new_type_checker(function(value, length) {
  if(!"POSIXct" %in% class(value)) {
    print(waldo::compare(
      c("POSIXct", "POSIXt"),
      class(value),
      x_arg = "expected to contain",
      y_arg = "class(value)"))
    stop("class mismatch", call. = FALSE)
  }

  if(!missing(length) && length(value) != length) {
    length <- as.integer(length)
    print(waldo::compare(
      length,
      length(value),
      x_arg = "expected",
      y_arg = "length(value)"))
    stop("length mismatch", call. = FALSE)
  }
  value
})
