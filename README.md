
<!-- badges: start -->

<!--[![Travis build status](https://travis-ci.com/moodymudskipper/typed.svg?branch=iteration2)](https://travis-ci.com/moodymudskipper/typed)
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
x
#> [1] "a"

Integer(3) ? y <- 1:3 # restrict y to "integer" type of length 3
y
#> [1] 1 2 3
```

We cannot assign values of the wrong type to `x` and `y` anymore.

``` r
x <- 2
#> Error: In `eval(expr, envir, enclos)` at `x <- ...`:
#> type mismatch
#> `typeof(value)`: "double"   
#> `expected`:      "character"

y <- 4:5
#> Error: In `eval(expr, envir, enclos)` at `y <- ...`:
#> type mismatch
#> `length(value)`: 2
#>      `expected`: 3

y[1] <- 10
#> Error: In `eval(expr, envir, enclos)` at `y <- ...`:
#> type mismatch
#> `typeof(value)`: "character"
#> `expected`:      "integer"
```

But the right type will work.

``` r
x <- c("b", "c")

y <- c(1L, 10L, 100L)
```

If you dislike the `?` operator you can use `declare`, it’s a strict
equivalent, slightly more efficient, which looks like `base::assign`.

``` r
declare("x", Character())
x <- "a"
x
#> [1] "a"

declare("y", Integer(3), 1:3)
y
#> [1] 1 2 3
```

### assertion factories and assertions

`Integer` and `Character` are function factories (functions that return
functions), thus `Integer(3)` and `Character()` are functions.

The latter functions operate checks on a value and in case of success
return this value, generally unmodified. For instance :

``` r
Integer(3)(1:2)
#> Error: type mismatch
#> `length(value)`: 2
#>      `expected`: 3

Character()(3)
#> Error: type mismatch
#> `typeof(value)`: "double"   
#> `expected`:      "character"
```

We call `Integer(3)` and `Character()` assertions, and we call `Integer`
and `Character` assertion factories.

The package contains many assertion factories (see `?native_types`), the
main ones are:

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

### Custom types

As we’ve seen with `Integer(3)`, `Data.frame(ncol = 5)`, passing
arguments to a assertion factory restricts the type. For instance
`Integer` has arguments `length` and `...`, in the dots we can use
arguments named as functions and with the value of the expected result.

``` r
Integer(anyNA = FALSE) ? x <- c(1L, 2L, NA)
#> Error: In `eval(expr, envir, enclos)` at `Integer(anyNA = FALSE) ? x <- c(1L, 2L, NA)`:
#> `anyNA` mismatch
#> `anyNA(value)`: TRUE 
#> `expected`:     FALSE
```

Useful arguments might be for instance, `anyDuplicated = 0L`, `names =
NULL`, `attributes = NULL`… Any available function can be used.

That makes assertion factories very flexible\! If it is still not
flexible enough, one can provide conditions using formulas.

``` r
fruit <- Character(1, "`value` is not a fruit!" ~ . %in% c("apple", "pear", "cherry"))

fruit ? x <- "potatoe"
#> Error: In `eval(expr, envir, enclos)` at `fruit ? x <- "potatoe"`:
#> `value` is not a fruit!
#> `value %in% c("apple", "pear", "cherry")`: FALSE
#> `expected`:                                TRUE
```

### Leverage assertions from other packages, build your own assertion factories

Some great packages provide assertions, and they can be used with
`typed` provided that they take the object as a first input and return
the object if no failure. Richie Cotton’s *{assertive}* and Michel
Lang’s *{checkmate}* both qualify and we’ll use them as examples.

``` r
library(assertive)
assert_is_monotonic_increasing ? z
z <- 3:1
#> Error: In `eval(expr, envir, enclos)` at `z <- ...`:
#> is_monotonic_increasing : The values of assigned_value are not monotonic increasing.
#>   Position ValueBefore ValueAfter
#> 1      1/2           3          2
#> 2      2/3           2          1
```

If we want to use more than the first argument, we should create an
assertion factory :

``` r
Monotonic_incr <- as_assertion_factory(assert_is_monotonic_increasing)
Monotonic_incr(strictly = TRUE) ? z
z <- c(1, 1, 2)
#> Error: In `eval(expr, envir, enclos)` at `z <- ...`:
#> is_monotonic_increasing : The values of value are not strictly monotonic increasing.
#>   Position ValueBefore ValueAfter
#> 1      1/2           1          1
library(checkmate)
#> Warning: package 'checkmate' was built under R version 4.0.3
```

`as_assertion_factory` can be used to create your own assertion
factories, in fact [it’s used to build the native assertion factories of
this
package](https://github.com/moodymudskipper/typed/blob/iteration2/R/06_native_types.R)
.

### Constants

To define a constant, we just surround the variable by parentheses
(think of them as a protection)

``` r
Double() ? (x) <- 1
x <- 2
#> Error: In `eval(expr, envir, enclos)` at `x <- ...`:
#> Can't assign to a constant

? (y) <- 1
y <- 2
#> Error: In `eval(expr, envir, enclos)` at `y <- ...`:
#> Can't assign to a constant
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
#> [1] 5
add(2, 3L)
#> Error: In `add(2, 3L)` at `Double() ? y`:
#> wrong argument to function, type mismatch
#> `typeof(value)`: "integer"
#> `expected`:      "double"
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
#> Error: In `add_or_substract(1L, 2L)` at `Double() ? return(x + y)`:
#> wrong return value, type mismatch
#> `typeof(value)`: "integer"
#> `expected`:      "double"
```

A simpler way is to use `?` before the function definition as in the
previous section, but to give it a assertion factory on the left hand
side. It will ultimately create the same function code, but the source
code is more readable to read, we don’t need to worry about precedence,
and the function will get a class “type” and attributes.

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
