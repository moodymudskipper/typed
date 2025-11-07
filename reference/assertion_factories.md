# Assertion factories of package 'typed'

These functions are assertion factories, they produce assertions, which
take an object, check conditions, and returns the input, usually
unmodified (never modified with the functions documented on this page).

Additional conditions can be provided :

- If they are named, the name should be the name of a function to use on
  our object, and the value should be the expected value.

- If they are unnamed, they should be formulas, the right hand side
  should be a condition, using `value` or `.` as a placeholder for the
  latter, and the optional `lhs` an error message.

`Any` is the most general assertion factory, it doesn't check anything
unless provided additional conditions through `...`. Others use the base
`is.<type>` function if available, or check that the object is of the
relevant type with `typeof` for atomic types, or check that the class of
the checked value contains the relevant class.

See advanced examples at the bottom, including uses of `Symbol()` and
`Dots()`.

## Usage

``` r
Any(length = NULL, ...)

Logical(length = NULL, null_ok = FALSE, ...)

Integer(length = NULL, null_ok = FALSE, ...)

Double(length = NULL, null_ok = FALSE, ...)

Character(length = NULL, null_ok = FALSE, ...)

Raw(length = NULL, null_ok = FALSE, ...)

List(length = NULL, each, data_frame_ok, null_ok = FALSE, ...)

Null(...)

Closure(null_ok = FALSE, ...)

Special(null_ok = FALSE, ...)

Builtin(null_ok = FALSE, ...)

Environment(null_ok = FALSE, ...)

Symbol(null_ok = FALSE, ...)

Pairlist(length = NULL, each, null_ok = TRUE, ...)

Language(null_ok = FALSE, ...)

Expression(length = NULL, null_ok = FALSE, ...)

Function(null_ok = FALSE, ...)

Factor(length = NULL, levels, null_ok = FALSE, ...)

Matrix(nrow, ncol, null_ok = FALSE, ...)

Array(dim, null_ok = FALSE, ...)

Data.frame(nrow, ncol, each, null_ok = FALSE, ...)

Date(length = NULL, null_ok = FALSE, ...)

Time(length = NULL, null_ok = FALSE, ...)

Dots(length = NULL, each, ...)

Logical(length = NULL, null_ok = FALSE, ...)

Integer(length = NULL, null_ok = FALSE, ...)

Double(length = NULL, null_ok = FALSE, ...)

Character(length = NULL, null_ok = FALSE, ...)

Raw(length = NULL, null_ok = FALSE, ...)

List(length = NULL, each, data_frame_ok = TRUE, null_ok = FALSE, ...)

Null(...)

Closure(null_ok = FALSE, ...)

Special(null_ok = FALSE, ...)

Builtin(null_ok = FALSE, ...)

Environment(null_ok = FALSE, ...)

Symbol(null_ok = FALSE, ...)

Pairlist(length = NULL, each, null_ok = TRUE, ...)

Language(null_ok = FALSE, ...)

Expression(length = NULL, null_ok = FALSE, ...)

Function(null_ok = FALSE, ...)

Factor(length = NULL, levels, null_ok = FALSE, ...)

Data.frame(nrow, ncol, each, null_ok = FALSE, ...)

Matrix(nrow, ncol, null_ok = FALSE, ...)

Array(dim, null_ok = FALSE, ...)

Date(length = NULL, null_ok = FALSE, ...)

Time(length = NULL, null_ok = FALSE, ...)

Dots(length = NULL, each, ...)
```

## Arguments

- length:

  length of the object

- ...:

  additional conditions, see details.

- null_ok:

  whether `NULL` values should be accepted, and not subjected to any
  further check.

- each:

  assertion that every item must satisfy

- data_frame_ok:

  whether data frames are to be considered as lists

- levels:

  factor levels

- nrow:

  number of rows

- ncol:

  number of columns

- dim:

  dimensions

## Value

A function, and more specifically, an assertion as defined above.

## Examples

``` r
if (FALSE) { # \dontrun{
# fails
Integer() ? x <- 1
# equivalent to
declare("x", Integer(), value = 1)

Integer(2) ? x <- 1L

# we can use additional conditions in `...`
Integer(anyNA = FALSE) ? x <- c(1L, NA, 1L)
Integer(anyDuplicated = 0L) ? x <- c(1L, NA, 1L)
} # }

Integer(2) ? x <- 11:12

if (FALSE) { # \dontrun{
# We can also use it directly to test assertions
Integer() ? x <- 1
# equivalent to
declare("x", Integer(), value = 1)

Integer(2) ? x <- 1L
} # }

if (FALSE) { # \dontrun{
# I we want to restrict the quoted expression rather than the value of an
# argument, we can use `?~` :
identity_sym_only <- ? function (x= ?~ Symbol()) {
  x
}

a <- 1
identity_sym_only(a)
identity_sym_only(a + a)

identity_sym_only
} # }

if (FALSE) { # \dontrun{
integer_list <- ? function (...= ? Integer()) {
  list(...)
}

integer_list(1L, 2L, "a")

integer_pair <- ? function (...= ? Dots(2, each = Integer())) {
  list(...)
}

integer_pair(1L, 2L, 3L)
integer_pair(1L, "a", "a")

x <- 1
y <- 2
symbol_list1 <- ? function (...= ? Dots(2, Symbol())) {
  list(...)
}
symbol_list1(quote(x), quote(y))
symbol_list1(x, y)

symbol_list2 <- ? function (...= ?~ Dots(2, Symbol())) {
  list(...)
}
symbol_list2(x, x + y)
symbol_list2(x, y)
} # }
```
