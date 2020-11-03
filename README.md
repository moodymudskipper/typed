
<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/moodymudskipper/typed.svg?branch=iteration2)](https://travis-ci.com/moodymudskipper/typed)
[![Codecov test
coverage](https://codecov.io/gh/moodymudskipper/typed/branch/master/graph/badge.svg)](https://codecov.io/gh/moodymudskipper/typed?branch=master)
<!-- badges: end -->

# typed <img src='man/figures/logo.png' align="right" height="139" />

*{typed}* implements a type system for R, it has 3 main features:

  - set variable types in a script or the body of a function, so they
    can’t be assigned illegal values
  - set argument types in a function definition
  - set return type of a function

The user can define their own types, or leverage assertions from other
packages.

Under the hood variable types use active bindings, so once a variable is
restricted by an assertion, it cannot be modified in a way that would
not satisfy it.

## Installation

Install with:

``` r
remotes::install_github("moodymudskipper/typed")
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
#> Error: type mismatch
#> `typeof(value)`: "double"   
#> `expected`:      "character"

y <- 4:5
#> Error: length mismatch
#> `length(value)`: 2
#>      `expected`: 3

y[1] <- 10
#> Error: type mismatch
#> `typeof(value)`: "character"
#> `expected`:      "integer"
```

But the right type will work.

``` r
x <- c("b", "c")

y <- c(1L, 10L, 100L)
```

`declare` is a strict equivalent, slightly more efficient, which looks
like `base::assign`.

``` r
declare("x", Character())
x <- "a"
x
#> [1] "a"

declare("y", Integer(3), 1:3)
y
#> [1] 1 2 3
```

### Assertion factories and assertions

`Integer` and `Character` are function factories (functions that return
functions), thus `Integer(3)` and `Character()` are functions.

The latter functions operate checks on a value and in case of success
return this value, generally unmodified. For instance :

``` r
Integer(3)(1:2)
#> Error: length mismatch
#> `length(value)`: 2
#>      `expected`: 3

Character()(3)
#> Error: type mismatch
#> `typeof(value)`: "double"   
#> `expected`:      "character"
```

We call `Integer(3)` and `Character()` assertions, and we call `Integer`
and `Character` assertion factories.

The package contains many assertion factories (see
`?assertion_factories`), the main ones are:

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

### Custom assertions

As we’ve seen with `Integer(3)`, passing arguments to a assertion
factory restricts the type.

For instance `Integer` has arguments `length` `null_ok` and `...`, we
already used `length`, `null_ok` is convenient to allow a default `NULL`
value in addition to the `"integer"` type. In the dots we can use
arguments named as functions and with the value of the expected result.

``` r
Integer(anyNA = FALSE) ? x <- c(1L, 2L, NA)
#> Error: `anyNA` mismatch
#> `anyNA(value)`: TRUE 
#> `expected`:     FALSE
```

Useful arguments might be for instance, `anyDuplicated = 0L`, `names =
NULL`, `attributes = NULL`… Any available function can be used.

That makes assertion factories very flexible\! If it is still not
flexible enough, one can provide conditions using formulas in the `...`.
Be careful to skip all named arguments by adding comas, or name the
formula args `...`.

``` r
fruit <- Character(1, ... = "`value` is not a fruit!" ~ . %in% c("apple", "pear", "cherry"))

fruit ? x <- "potatoe"
#> Error: `value` is not a fruit!
#> `value %in% c("apple", "pear", "cherry")`: FALSE
#> `expected`:                                TRUE
```

The arguments can differ between assertion factories, for instance
`Data.frame` has `nrow`, `ncol`, `each`, `null_ok` and `...`

``` r
Data.frame() ? x <- iris
Data.frame(ncol = 2) ? x <- iris
#> Error: Column number mismatch
#> `ncol(value)`: 5
#>    `expected`: 2
Data.frame(each = Double()) ? x <- iris
#> Error: column 5 ("Species") type mismatch
#> `typeof(value)`: "integer"
#> `expected`:      "double"
```

### Leverage assertions from other packages, build your own assertion factories

Some great packages provide assertions, and they can be used with
`typed` provided that they take the object as a first input and return
the object if no failure. Richie Cotton’s *{assertive}* and Michel
Lang’s *{checkmate}* both qualify.

``` r
library(assertive)
assert_is_monotonic_increasing ? z
z <- 3:1
#> Error: is_monotonic_increasing : The values of assigned_value are not monotonic increasing.
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
#> Error: is_monotonic_increasing : The values of value are not strictly monotonic increasing.
#>   Position ValueBefore ValueAfter
#> 1      1/2           1          1
```

`as_assertion_factory` can be used to create your own assertion
factories from scratch too, in fact [it’s used to build the native
assertion factories of this
package](https://github.com/moodymudskipper/typed/blob/iteration2/R/06_assertion_factories.R)
.

### Constants

To define a constant, we just surround the variable by parentheses
(think of them as a protection)

``` r
Double() ? (x) <- 1
x <- 2
#> Error: Can't assign to a constant

? (y) <- 1
y <- 2
#> Error: Can't assign to a constant
```

## Set argument type

We can set argument types this way :

``` r
add <- ? function (x= ? Double(), y= 1 ? Double()) {
  x + y
}
```

Note that we started the definition with a `?`, and that we gave a
default to `y`, but not `x`. Note also the `=` sign next to `x`,
necessary even when we have no default value. If you forget it you’ll
have an error “unexpected `?` in …”.

This created the following function, by adding checks at the top of the
body

``` r
add
#> # typed function
#> function (x, y = 1) 
#> {
#>     check_arg(x, Double())
#>     check_arg(y, Double())
#>     x + y
#> }
#> # Arg types:
#> # x: Double()
#> # y: Double()
```

Let’s test it by providing a right and wrong type.

``` r
add(2, 3)
#> [1] 5
add(2, 3L)
#> Error: In `add(2, 3L)` at `check_arg(y, Double())`:
#> wrong argument to function, type mismatch
#> `typeof(value)`: "integer"
#> `expected`:      "double"
```

If we want to restrict `x` and `y` to the type “integer” in the rest of
the body of the function we can use the `?+` notation :

``` r
add <- ? function (x= ?+ Double(), y= 1 ?+ Double()) {
  x + y
}

add
#> # typed function
#> function (x, y = 1) 
#> {
#>     check_arg(x, Double(), .bind = TRUE)
#>     check_arg(y, Double(), .bind = TRUE)
#>     x + y
#> }
#> # Arg types:
#> # x: Double()
#> # y: Double()
```

We see that it is translated into a `check_arg` call containing a `.bind
= TRUE` argument.

I we want to restrict the quoted expression rather than the value of an
argument, we can use `?~` :

``` r
identity_sym_only <- ? function (x= ?~ Symbol()) {
  x
}

a <- 1
identity_sym_only(a)
#> [1] 1
identity_sym_only(a + a)
#> Error: In `identity_sym_only(a + a)` at `check_arg(substitute(x), Symbol())`:
#> wrong argument to function, type mismatch
#> `typeof(value)`: "language"
#> `expected`:      "symbol"

identity_sym_only
#> # typed function
#> function (x) 
#> {
#>     check_arg(substitute(x), Symbol())
#>     x
#> }
#> <bytecode: 0x000000001d1a98a8>
#> # Arg types:
#> # x: ~Symbol()
```

We see that it is translated into a `check_arg` call containing a call
to `substitute` as the first argument. The `~` is kept in the attributes
of the function.

We can also check the `...`, for instance use `function(... = ?
Integer())` to check that only integers are passed to the dots, and use
`function(... = ?~ Symbol())` to check that all quoted values passed to
`...` are symbols.

The special assertion factory `Dots` can also be used, in that case the
checks will apply to `list(...)` rather than to each element
individually, for instance `function(... = ? Dots(2))` makes sure the
dots were fed 2 values. In a similar fashion `function(... = ?~
Dots(2))` can be used to apply checks to the list of quoted argument
passed to `...`.

## Set function return type

To set a return type we use `?` before the function definition as in the
previous section, but we type an assertion on the left hand side.

``` r
add_or_subtract <- Double() ? function (x, y, subtract = FALSE) {
  if(subtract) return(x - y)
  x + y
}
add_or_subtract
#> # typed function
#> function (x, y, subtract = FALSE) 
#> {
#>     if (subtract) 
#>         return(check_output(x - y, Double()))
#>     check_output(x + y, Double())
#> }
#> # Return type: Double()
```

We see that the returned values have been wrapped inside `check_output`
calls.

## Putting it all together, write packages using {typed}

Let’s define our function for our package and document it with
*{roxygen2}*. It is documented as usual,except that you’ll need to make
sure to add the `@name` tag.

We declare types for the return value, for all arguments, and we declare
a string `msg`.

``` r
#' add_or_subtract
#'
#' @param x double of length 1
#' @param y double of length 1
#' @param subtract whether to subtract instead of adding
#' @export
#' @name add_or_subtract
add_or_subtract <- 
  Double(1) ? function (
    x= ? Double(1), 
    y= ? Double(1), 
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

The created function will be the following, we see that `Character(1) ?
msg was changed into a`declare`call too, this is both for efficiency and
readability. Unfamiliar users might be intimidated by`?`and calls to`?\`
don’t print nicely.

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

Note that your package would import *{typed}* but `?` won’t be exposed
to the user, they will see it in the code but will be able to use `?`
just as before. In fact the most common standard use `?mean` still works
even when *{typed}* is attached.

## Aknowledgements

This is inspired in good part by Jim Hester and Gabor Csardi’s work and
many great efforts on static typing, assertions, or annotations in R, in
particular:

  - Gabor Csardy’s *{argufy}*
  - Richie Cotton’s *{assertive}*
  - Tony Fishettti’s *{assertr*}
  - Hadley Wickham’s *{assertthat}*
  - Michel Lang’s *{checkmate}*
  - Joe Thorley’s *{checkr}*
  - Joe Thorley’s *{chk}*
  - Aviral Goel’s *{contractr}*
  - Stefan Bache’s *{ensurer}*
  - Brian Lee Yung Rowe’s *{lambda.r}*
  - Kun Ren’s *{rtype}*
  - Jim Hester’s *{types}*
