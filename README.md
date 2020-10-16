
<!-- README.md is generated from README.Rmd. Please edit that file -->

# typed

Experiment on static typing in R. Largely untested\!

## Installation

Install with:

``` r
remotes::install_github("moodymudskipper/typed")
```

## Example

We define a function below, the question mark is used to introduce
static typing, `numeric` here is a prototyping function as understood by
*{vctrs}*, (we could have written `numeric()` with the same effect, and
we could use *{vctrs}* functions such as `new_date()` too).

We use it for 3 different things here :

  - set a return type to the function `add2` : if the first `?` has a
    lhs, the function should only return a variable `res`, and *{typed}*
    will make sure that `res` is always of the right type.
  - set argument type : the arguments for which we want to set a type
    are suffixed with `?`, in the case below we don’t have a default
    value so `?` has no lhs
  - set variable type : we initiate the variable `y` as numeric in this
    example and assigning any non numeric to it will fail.

<!-- end list -->

``` r
library(typed)
#> 
#> Attaching package: 'typed'
#> The following object is masked from 'package:utils':
#> 
#>     ?
numeric ? add2 <- function (x = ?numeric) {
  numeric ? y
  y <- 2
  res <- x + y
  res
}
```

This creates a function `add2()`, with attributes and a call
`assert_types()` at the start of the body.

``` r
add2
#> function (x) 
#> {
#>     assert_types()
#>     `?`(numeric, y)
#>     y <- 2
#>     res <- x + y
#>     res
#> }
#> attr(,"arg_ptypes")
#> attr(,"arg_ptypes")$x
#> numeric(0)
#> 
#> attr(,"output_ptype")
#> numeric(0)
```

The call `assert_types()` will check that all parameters are right, and
will make sure they can’t be overridden by a value having another
prototype.

We can call it :

``` r
add2(3)
#> [1] 5
```

It fails early if fed the wrong arg type :

``` r
add2("a")
#> Error: `x`'s prototype should be numeric(0)
```

if we had changed the value of x, by assigning a char in the body it
would have failed too, right when we’d try to assign it:

``` r
numeric ? add2 <- function (x = ?numeric) {
  message("we get up to here")
  x <- as.character(x)
  message("but not there")
  numeric ? y
  y <- 2
  res <- x + y
  res
}

add2(3)
#> we get up to here
#> Error: assigned value should have same prototype as `x`: numeric(0)
```

Same thing if we try to modify `y` using an illegal type:

``` r
numeric ? add2 <- function (x = ?numeric) {
  numeric ? y
  y <- 2
  message("we get up to here")
  y <- "2"
  message("but not there")
  res <- x + y
  res
}

add2(3)
#> we get up to here
#> Error: assigned value should have same prototype as `y`: numeric(0)
```

instead of :

``` r
  numeric ? y
  y <- 2
```

We can write on one line :

``` r
  numeric ? y <- 2
```

It will test the value of y just like with the former syntax.

We can also set the prototype implicitly by simply typing :

``` r
  ? y <- 2
```

Let’s write a variant, we might decide not to set a return type, not to
declare our y variable, and just set argument types, setting a default
this time.

``` r
? add2 <- function (x = 1 ?numeric) {
  y <- 2
  x + y
}

add2()
#> [1] 3
```

It fails if we try to override the argument with a different type:

``` r
? add2 <- function (x = 1 ?numeric) {
  y <- 2
  x <- as.character(x)
  x + y
}

add2()
#> Error: assigned value should have same prototype as `x`: numeric(0)
```

## Notes

  - This is inspired in good part by Jim Hester and Gabor Csardi’s work.
  - The magic comes from the fact that apparent variables are made into
    active bindings, using a variation of the last example of `?bindenv`
  - Your package would import *{typed}* but `?` won’t be exposed to the
    user, they will see it in the code but will be able to use `?` just
    as before.
  - It might be slow
  - I’d like to be more flexible than just asserting prototypes,
    supporting at least length would be nice, but more complex
    conditions too.
  - A class and printing method for these functions would be nice, to
    print the attributes in a prettier way, and to print the `?` better,
    maybe some fancy *{crayon}* stuff to highlight types.
