
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

Install CRAN version with:

``` r
install.packages("typed")
```

or development version with :

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
and `Character` assertion factories (or just types, with then we must be
careful not to confuse them with atomic types returned by the `typeof`
function).

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

### Advanced type restriction using arguments

As we’ve seen with `Integer(3)`, passing arguments to a assertion
factory restricts the type.

For instance `Integer` has arguments `length` `null_ok` and `...`. We
already used `length`, `null_ok` is convenient to allow a default `NULL`
value in addition to the `"integer"` type.

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

In the dots we can use arguments named as functions and with the value
of the expected result.

``` r
# Integer has no anyNA arg but we can still use it because a function named
# this way exists
Integer(anyNA = FALSE) ? x <- c(1L, 2L, NA)
#> Error: `anyNA` mismatch
#> `anyNA(value)`: TRUE 
#> `expected`:     FALSE
```

Useful arguments might be for instance, `anyDuplicated = 0L`,
`names = NULL`, `attributes = NULL`… Any available function can be used.

That makes assertion factories very flexible! If it is still not
flexible enough, we can provide arguments arguments named `...` to
functional factories to add a custom restriction, this is usually better
done by defining a wrapper.

``` r
Character(1, ... = "`value` is not a fruit!" ~ . %in% c("apple", "pear", "cherry")) ? 
  x <- "potatoe"
#> Error: `value` is not a fruit!
#> `value %in% c("apple", "pear", "cherry")`: FALSE
#> `expected`:                                TRUE
```

This is often better done by defining a wrapper as shown below.

### Constants

To define a constant, we just surround the variable by parentheses
(think of them as a protection)

``` r
Double() ? (x) <- 1
x <- 2
#> Error: Can't assign to a constant

# defining a type is optional
? (y) <- 1
y <- 2
#> Error: Can't assign to a constant
```

### Set a function’s argument type

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
the body, so they cannot be overwritten by character for instance,we can
use the `?+` notation :

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

We see that it is translated into a `check_arg` call containing a
`.bind = TRUE` argument.

## Set a function’s return type

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

# Use type in a package and define your own types

See `vignette("typed-in-packaged", "typed")` or the Article section if
you’re browsing the pkgdown website.

## Acknowledgements

This is inspired in good part by Jim Hester and Gabor Csardi’s work and
many great efforts on static typing, assertions, or annotations in R, in
particular:

- Gabor Csardy’s [*argufy*](https://github.com/gaborcsardi/argufy)
- Richie Cotton’s
  [*assertive*](https://bitbucket.org/richierocks/assertive/)
- Tony Fishettti’s [*assertr*](https://github.com/tonyfischetti/assertr)
- Hadley Wickham’s [*assertthat*](https://github.com/hadley/assertthat)
- Michel Lang’s [*checkmate*](https://github.com/mllg/checkmate)
- Joe Thorley’s [*checkr*](https://github.com/poissonconsulting/checkr)
- Joe Thorley’s [*chk*](https://github.com/poissonconsulting/chk/)
- Aviral Goel’s [*contractr*](https://github.com/aviralg/contractr)
- Stefan Bache’s [*ensurer*](https://github.com/smbache/ensurer)
- Brian Lee Yung Rowe’s
  [*lambda.r*](https://github.com/zatonovo/lambda.r)
- Kun Ren’s [*rtype*](https://github.com/renkun-ken/rtype)
- Duncan Temple Lang’s
  [*TypeInfo*](https://bioconductor.org/packages/TypeInfo/)
- Jim Hester’s [*types*](https://github.com/jimhester/types)
