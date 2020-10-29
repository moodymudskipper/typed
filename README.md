
<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/moodymudskipper/typed.svg?branch=master)](https://travis-ci.com/moodymudskipper/typed)
<!-- badges: end -->

# typed

*{typed}* implements static typing in R, it has 3 main features:

  - set variable types in a script or the body of a function
  - set argument types in a function definition
  - set return type of a function

The user can define their own types, or leverage assertions from other
packages.

Under the hood we use active bindings, so once a variable is restricted
to a type, or by any other condition, it cannot be modified in a way
that would not respect these restrictions.

## Installation

Install with:

``` r
remotes::install_github("moodymudskipper/typed@iteration2")
```

And attach with :

``` r
# masking warning about overriding `?`
library(typed, warn.conflicts = FALSE) 
```

## Set variable type

### Question mark notation and `declare`

Here are examples on how we would set types

``` r
Character() ? x # restrict x to "character" type
x <- "a"
#> [1] "entering"
#> [1] "if"
x
#> [1] "entering"
#> [1] "a"
Integer(3) ? y <- 1:3 # restrict y to "integer" type of length 3
y
#> [1] "entering"
#> [1] 1 2 3
```

We cannot assign values of the wrong type to `x` and `y` anymore.

``` r
x <- 2
#> [1] "entering"
#> [1] "if"
#> `expected`:      "character"
#> `typeof(value)`: "double"   
#> [1] "hello"
#> [1] "world"
#> Error: type mismatch in `eval(expr, envir, enclos)` at ``
y <- 4:5
#> [1] "entering"
#> [1] "if"
#>      `expected`: 3
#> `length(value)`: 2
#> [1] "hello"
#> [1] "world"
#> Error: length mismatch in `eval(expr, envir, enclos)` at ``
y[1] <- 10
#> [1] "entering"
#> [1] "entering"
#> [1] "entering"
#> [1] "if"
#> `expected`:      "integer"  
#> `typeof(value)`: "character"
#> [1] "hello"
#> [1] "world"
#> Error: type mismatch in `eval(expr, envir, enclos)` at ``
```

But the right type will work.

``` r
x <- c("b", "c")
#> [1] "entering"
#> [1] "if"
y <- c(1L, 10L, 100L)
#> [1] "entering"
#> [1] "if"
```

If you dislike the `?` operator you can use `declare`, it’s a strict
equivalent, slightly more efficient, which looks like `base::assign`.

``` r
declare("x", Character())
#> [1] "entering"
x <- "a"
#> [1] "entering"
#> [1] "if"
x
#> [1] "entering"
#> [1] "a"
declare("y", Integer(3), 1:3)
#> [1] "entering"
y
#> [1] "entering"
#> [1] 1 2 3
```

### Type checkers and assertions

`Integer` and `Character` are called type checkers.

The package contains many of those (see `?native_types`), the main ones
are:

  - `Any` (No default restriction)
  - `Logical`
  - `Integer`
  - `Double`
  - `Character`
  - `List`
  - `Environment`
  - `Factor`
  - `Matrix`
  - `Data.frame`
  - `Date`
  - `Time` (POSIXct)

They are function factories (functions that return functions), thus
`Integer(3)` and `Character()` are functions.

In particular these functions operate checks on a value and in case of
success return this value, generally unmodified. For instance :

``` r
Data.frame()(letters)
#> `expected to contain`: "data.frame"
#> `class(value)`:        "character"
#> Error: class mismatch
Data.frame(ncol = 5)(cars)
#>    `expected`: 5
#> `ncol(value)`: 2
#> Error: Column number mismatch
```

These functions are called assertions.

### Custom types

As we’ve seen with `Integer(3)`, `Data.frame(ncol = 5)`, passing
arguments to a type checker restricts the type. For instance `Integer`
has arguments `length` and `...`, in the dots we can use arguments named
as functions and with the value of the expected result.

``` r
Integer(anyNA = FALSE) ? x <- c(1L, 2L, NA)
#> [1] "entering"
#> `expected`:     FALSE
#> `anyNA(value)`: TRUE
#> Error: `anyNA` mismatch
```

That makes type checkers very flexible\! If it is still not flexible
enough, one can provide conditions using formulas.

``` r
fruit <- Character(1, "`value` is not a fruit!" ~ . %in% c("apple", "pear", "cherry"))
fruit ? x <- "potatoe"
#> [1] "entering"
#> Error: `value` is not a fruit!
```

### Leverage assertions from other packages

Some great packages provide assertions, and they can be used with
`typed` provided that they take the object as a first input and return
the object if no failure. Richie Cotton’s *{assertive}* and Michel
Lang’s *{checkmate}* both qualify and we’ll use them as examples.

``` r
library(assertive)

# used without type definition
assert_is_monotonic_increasing(1:3)
assert_is_monotonic_increasing(3:1)
#> Error in eval(expr, envir, enclos): is_monotonic_increasing : The values of 3:1 are not monotonic increasing.
#>   Position ValueBefore ValueAfter
#> 1      1/2           3          2
#> 2      2/3           2          1

# used with type definition
assert_is_monotonic_increasing ? z
z <- 1:3 # works
#> [1] "entering"
#> [1] "if"
z <- 3:1 # fails
#> [1] "entering"
#> [1] "if"
#> [1] "hello"
#> [1] "world"
#> Error: is_monotonic_increasing : The values of assigned_value are not monotonic increasing.
#>   Position ValueBefore ValueAfter
#> 1      1/2           3          2
#> 2      2/3           2          1 in `eval(expr, envir, enclos)` at ``
```

If we want to use more than the first argument, we can use
`purrr::partial()` :

``` r
library(checkmate)
#> Warning: package 'checkmate' was built under R version 4.0.3
library(purrr)
#> 
#> Attaching package: 'purrr'
#> The following objects are masked from 'package:assertive':
#> 
#>     is_atomic, is_character, is_double, is_empty, is_formula,
#>     is_function, is_integer, is_list, is_logical, is_null, is_numeric,
#>     is_vector
partial(assert_string, pattern ="^t") ? x <- "tomato"
#> [1] "entering"
partial(assert_string, pattern ="^t") ? x <- "potato"
#> [1] "entering"
#> Error in eval_tidy(call, mask): Assertion on 'value' failed: Must comply to pattern '^t'.
```

### Custom type checkers

Finally, custom type checkers can be defined using the function
`new_type_checker`, [check my code on github to understand how to create
them](https://github.com/moodymudskipper/typed/blob/iteration2/R/06_native_types.R)

### Constants

To define a constant, we just surround the variable by parentheses
(think of them as a protection)

``` r
Double() ? (x) <- 1
#> [1] "entering"
x <- 2
#> Error: `x` is a constant, can't assign a new value. in `eval(expr, envir, enclos)` at ``
? (y) <- 1
#> [1] "entering"
y <- 2
#> Error: `y` is a constant, can't assign a new value. in `eval(expr, envir, enclos)` at ``
```

## Set argument type

We can set argument types this way :

``` r
add <- ? function (x= ? Double(), y= 1 ? Double()) {
  x + y
}
```

Note that we started the definition with a `?`, and that we gave a
default to `y`, but not `x`

This created the following function, by adding declarations at the top
of the function.

``` r
add
#> # typed function
#> function (x, y = 1) 
#> {
#>     `?`(Double(), x)
#>     `?`(Double(), y)
#>     x + y
#> }
#> # Arg types:
#> # x: Double()
#> # y: Double()
```

Note that a declaration will fail if the variable already exists in the
environment, but an exception is made for unevaluated function
arguments.

Let’s test it by assigning a right and wrong type

``` r
add(2, 3)
#> [1] "entering"
#> [1] "entering"
#> [1] 5
add(2, 3L)
#> `expected`:      "double" 
#> `typeof(value)`: "integer"
#> Error: type mismatch
```

Arguments `x` and `y` will also be restricted to the type “integer” in
the rest of the body of the function.

## Set function return type

To set a return type one way is to manually use the `type_checker() ?
return(value)` syntax with every return value, as we do here (note that
we need the `?` or the precedence of `?` will cause issues) :

``` r
add_or_substract <- function (x, y, substract = FALSE) {
  if(substract) {Double() ? return(x - y)}
  Double() ? return(x + y)
}
add_or_substract(1, 2)
#> [1] 3
add_or_substract(1L, 2L)
#> `expected`:      "double" 
#> `typeof(value)`: "integer"
#> Error: type mismatch
```

A simpler way is to use `?` before the function definition as in the
previous section, but to give it a type checker on the left hand side.
It will ultimately create the same function code, but the source code is
more readable to read, we don’t need to worry about precedence, and the
function will get a class “type” and attributes.

``` r
add_or_substract <- Double() ? function (x, y, substract = FALSE) {
  if(substract) return(x - y)
  x + y
}
add_or_substract
#> # typed function
#> function (x, y, substract = FALSE) 
#> {
#>     if (substract) 
#>         `?`(Double(), return(x - y))
#>     `?`(Double(), return(x + y))
#> }
#> # Return type: Double()
```

These can be defined in a package and documented with *{roxygen2}* like
regular functions, except that you’ll need to make sure to add the
`@name` tag.

``` r
#' add
#'
#' @param x double
#' @param y double
#' @export
#' @name add
add <- Double() ? function (x = ? Double(), y = 1 ? Double()) {
  x + y
}
```

## Notes

  - This is inspired in good part by Jim Hester and Gabor Csardi’s work.
  - The magic comes from the fact that apparent variables are made into
    active bindings, using a variation of the last example of `?bindenv`
  - Your package would import *{typed}* but `?` won’t be exposed to the
    user, they will see it in the code but will be able to use `?` just
    as before. In fact the most common standard use `?mean` still works
    even when *{typed}* is attached.
