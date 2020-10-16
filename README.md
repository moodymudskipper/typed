
<!-- README.md is generated from README.Rmd. Please edit that file -->

# typed

Experiment on static typing in R. Largely untested\!

We use the question mark operator to support static typing in R.

3 features are proposed :

We use it for 3 different things here :

  - set variable types in a script or the body of a function
  - set argument types in a function definition
  - set return type of a function

We detail those below.

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

## set variable type

We can set a variable type explicitly or implicitly. The following are
equivalent.

``` r
# implicit
? x <- 1
```

``` r
# explicit
numeric ? x <- 1
```

``` r
# explicit without assignment on declaration
numeric ? x
x <- 1
```

Let’s test it

``` r
numeric ? x
x
#> numeric(0)
x <- 1
x
#> [1] 1
x <- "a"
#> Error: assigned value should have same prototype as `x`: numeric(0)
```

To assess if assignments are allowed we compare
`vctrs::vec_ptype(assigne_value)` to the prototype given on the left
hand side, which we can give as a function or value. in the latter for
instance `vctrs::vec_ptype(1)` and `numeric()` return the same thing so
the assignment is allowed.

Using *{vctrs}* allows us more flexibility than working with `class()`
or `type_of()`.

``` r
library(vctrs)
new_date ? my_date <- "2020-10-16"
#> Error: assigned value should have same prototype as `my_date`: structure(numeric(0), class = "Date")
new_date ? my_date <- as.Date("2020-10-16")

val <- new_list_of(list(1, 2), numeric())
val
#> <list_of<double>[2]>
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
new_list_of(list(), numeric()) ? new_val <- list(1, 2)
#> Error: assigned value should have same prototype as `new_val`: structure(list(), ptype = numeric(0), class = c("vctrs_list_of", "vctrs_vctr", "list"))
new_list_of(list(), numeric()) ? new_val <- val
```

We can also define constants:

``` r
character ? chr1 <- const(1) 
#> Error: assigned value should have same prototype as `chr1`: character(0)
character ? chr1 <- const("a") 
chr1
#> [1] "a"
chr1 <- "b"
#> Error in eval(expr, envir, enclos): impossible de changer la valeur d'un lien verrouillé pour 'chr1'
```

## set argument type

We can set argument types this way :

``` r
? add <- function (x = ?numeric, y = 1 ?numeric) {
  x + y
}
```

Note that we started the definition with a `?`, and that we gave a
default to `y`, but not `x`

This created the following function :

``` r
add
#> function (x, y = 1) 
#> {
#>     assert_types()
#>     x + y
#> }
#> # Arg types:
#> x: numeric(0)
#> y: numeric(0)
```

You can see a call to `assert_types()`, it ensures arguments passed have
the right type, and that they won’t be overridden by a different type.

Let’s test it.

``` r
add(2, 3)
#> [1] 5
add(2)
#> [1] 3
add("a")
#> Error: `x`'s prototype should be numeric(0)
```

Let’s create a function that tried to modify `y` and assign the wrong
type

``` r
? add_wrong <- function (x = ?numeric, y = 1 ?numeric) {
  y <- as.character(y)
  x + y
}
add_wrong(2)
#> Error: assigned value should have same prototype as `y`: numeric(0)
```

## set function return type

Setting a function return type is similar to setting variable types, but
the function should only return an object named `res`, which cannot ever
have another type than the one we set.

Putting it all together we have :

``` r
numeric ? add10 <- function (x = ?numeric) {
  ? y <- 10
  res <- x + y
  res
}
add10(20)
#> [1] 30
```

## Notes

  - This is inspired in good part by Jim Hester and Gabor Csardi’s work.
  - The magic comes from the fact that apparent variables are made into
    active bindings, using a variation of the last example of `?bindenv`
  - Your package would import *{typed}* but `?` won’t be exposed to the
    user, they will see it in the code but will be able to use `?` just
    as before.
  - It might be slow
  - We use `vctrs::vec_ptype` on the given prototype and the object to
    ensure their compatible, it’s not always satisfactory. expressions
    or functions ar not supported, the prototype of a factor necessarily
    contains the levels, the prototype of a dataframe necessarily
    contains the column names. I’d like to be more flexible (suggestions
    welcome).
  - In particular, setting length as part of the type would be great, so
    we could define scalars for instance.
  - A class and printing method for these functions would be nice, to
    print the attributes in a prettier way, and to print the `?` better,
    maybe some fancy *{crayon}* stuff to highlight types.
