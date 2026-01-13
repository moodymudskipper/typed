# Using {typed} in packages

``` r
library(typed, warn.conflicts = FALSE)
```

Here are some recommendations to use {typed} in your own package and
define custom types..

## Use {typed} in your package

Call this to set up your package so you can use ‘typed’, by editing the
DESCRIPTION file and editing or creating ‘R/your.pkg-package.R’

``` r
typed::use_typed()
devtools::document() # to actually import from the generated roxygen2 comments
```

## Use existing types to define your functions

Here’s an example where we define types for the return value, for all
arguments, and for a `msg` variable in the body.

The use of roxygen2 tags is standard, except that you’ll need to make
sure to add the `@name` tag.

``` r
#' add_or_subtract
#'
#' @param x double of length 1
#' @param y double of length 1
#' @param subtract whether to subtract instead of adding
#' @export
#' @name add_or_subtract
add_or_subtract <- Double(1) ? function (
    x = ? Double(1), 
    y = ? Double(1), 
    subtract = FALSE ? Logical(1, anyNA = FALSE)
  ) {
  Character(1) ? msg
  if(subtract) {
    msg <- "subtracting"
    message(msg)
    return(x - y)
  }
    msg <- "adding"
    message(msg)
  x + y
}
```

The created function will be the following:

``` r
add_or_subtract
#> # typed function
#> function (x, y, subtract = FALSE) 
#> {
#>     check_arg(x, Double(1))
#>     check_arg(y, Double(1))
#>     check_arg(subtract, Logical(1, anyNA = FALSE))
#>     declare("msg", Character(1))
#>     if (subtract) {
#>         msg <- "subtracting"
#>         message(msg)
#>         return(check_output(x - y, Double(1)))
#>     }
#>     msg <- "adding"
#>     message(msg)
#>     check_output(x + y, Double(1))
#> }
#> # Return type: Double(1)
#> # Arg types:
#> # x: Double(1)
#> # y: Double(1)
#> # subtract: Logical(1, anyNA = FALSE)
```

We see that `?` is not present in the generated code, instead we have
[`check_arg()`](https://moodymudskipper.github.io/typed/reference/check_arg.md),
[`declare()`](https://moodymudskipper.github.io/typed/reference/declare.md),
[`check_output()`](https://moodymudskipper.github.io/typed/reference/check_arg.md).
The substitution is done by the first `?`, it is both for efficiency and
readability, unfamiliar users might be intimidated by `?` and calls to
`?` don’t print nicely in base R.

Note that you’re free to use these functions directly in your code if
you don’t like the `?` Syntax.

## Define your own types from existing types

A way to define custom types is to wrap existing ones and add
constraints

``` r
Fruit <- function() {
  Character(
    length = 1, 
    ... = "`value` is not a fruit!" ~ . %in% c("apple", "pear", "cherry")
    # we could have several args named ... to apply multiple checks
  )
}

Fruit() ? x <- "potatoe"
#> Error:
#> ! `value` is not a fruit!
#> `value %in% c("apple", "pear", "cherry")`: FALSE
#> `expected`:                                TRUE
```

The type
[`Any()`](https://moodymudskipper.github.io/typed/reference/assertion_factories.md)
is the most general, but in many case it’s a good idea to start from a
more restricted type so you get its own checks for free.

``` r
Fruit() ? x <- 1L
#> Error:
#> ! type mismatch
#> `typeof(value)`: "integer"  
#> `expected`:      "character"
```

Here’s a case where starting from
[`Any()`](https://moodymudskipper.github.io/typed/reference/assertion_factories.md)
makes sense :

``` r
Ggplot <- function() {
  Any(... = "Expected a ggplot object" ~ ggplot2::is.ggplot(value))
}

Ggplot() ? x <- 1
#> Warning: `is.ggplot()` was deprecated in ggplot2 3.5.2.
#> ℹ Please use `is_ggplot()` instead.
#> This warning is displayed once per session.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
#> Error:
#> ! Expected a ggplot object
#> `ggplot2::is.ggplot(value)`: FALSE
#> `expected`:                  TRUE
```

A custom type might also just be a restriction on an existing type using
existing arguments.

In the following example we apply a simple restriction to an existing
type, using the existing argument `length`.

Here we restrict the length but keep other args flexible by forwarding
them:

``` r
ScalarInteger1 <- function(null_ok = FALSE, ...) {
  Integer(length = 1, null_ok = null_ok, ...)
}
ScalarInteger1() ? x <- c(1L, 2L)
#> Error:
#> ! length mismatch
#> `length(value)`: 2
#>      `expected`: 1
```

Here we remove all flexibility:

``` r
ScalarInteger2 <- function() {
  Integer(length = 1)
}
ScalarInteger2() ? x <- c(1L, 2L)
#> Error:
#> ! length mismatch
#> `length(value)`: 2
#>      `expected`: 1
```

## Define your own types from scratch

We can define a check function and use
[`as_assertion_factory()`](https://moodymudskipper.github.io/typed/reference/as_assertion_factory.md)
on it.

``` r
check_is_ggplot <- function(x) {
  if(!ggplot2::is.ggplot(x)) {
    msg <- "Class mismatch"
    info1 <- "Expected a ggplot object"
    info2 <- sprintf("Got an object of class <%s>", paste(class(x), collapse = "/"))
   rlang::abort(
     c(msg, i = info1, x = info2)
   )
  }
}
Ggplot <- as_assertion_factory(check_is_ggplot)

Ggplot() ? x <- 1
#> Error:
#> ! Class mismatch
#> ℹ Expected a ggplot object
#> ✖ Got an object of class <numeric>
```

Another example, to impose a data type based on a prototype:

``` r
check_cars <- function(x) vctrs::vec_assert(x, cars)
Cars <- as_assertion_factory(check_cars)
Cars() ? x <- iris
#> Error:
#> ! `x` must be a vector with type:
#> 
#>   <data.frame<
#>     speed: double
#>     dist : double
#>   >>
#> 
#> Instead, it has type:
#> 
#>   <data.frame<
#>     Sepal.Length: double
#>     Sepal.Width : double
#>     Petal.Length: double
#>     Petal.Width : double
#>     Species     : factor<fb977>
#>   >>
```

Or if we want to be more general:

``` r
check_valid_ptype <- function(x, ptype) vctrs::vec_assert(x, ptype)
Ptype <- as_assertion_factory(check_valid_ptype)
Ptype(cars) ? x <- iris
#> Error:
#> ! `x` must be a vector with type:
#> 
#>   <data.frame<
#>     speed: double
#>     dist : double
#>   >>
#> 
#> Instead, it has type:
#> 
#>   <data.frame<
#>     Sepal.Length: double
#>     Sepal.Width : double
#>     Petal.Length: double
#>     Petal.Width : double
#>     Species     : factor<fb977>
#>   >>
```

[`as_assertion_factory()`](https://moodymudskipper.github.io/typed/reference/as_assertion_factory.md)
[is actually used to build the native assertion factories of this
package](https://github.com/moodymudskipper/typed/blob/master/R/06_native_types.R)
