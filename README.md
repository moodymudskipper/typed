
<!-- README.md is generated from README.Rmd. Please edit that file -->

# typed

Experiment on static typing in R. Not robust\!

I realize the syntax is not quite right, but publishing a draft already.
Output type of the function is not enforced yet, only arguments and
variables defined in the function for now.

## Installation

Install with:

``` r
remotes::install_github("moodymudskipper/typed")
```

## Example

We define a function below, the suffix after `..` describes the expected
prototype of the output or arguments. These can be any of “list”,
“logical”, “integer”, “numeric”, “double”, “complex”, “character”
“raw”, with the addition of default prototypes proposed by
*{vctrs}*, such as “date”, “datetime” etc.

`:=` must be used to define the function. Inside the body `y := 1` makes
sure `y` returns 1, and that it can only be assigned doubles (the
prototype of `1`).

``` r
library(typed)
add1..numeric := function (x..numeric) {
  y := 1
  x + y
}
```

This creates a function `add1()`, with attributes and a call
`assert_types()`.

``` r
add1
#> function (x) 
#> {
#>     assert_types()
#>     `:=`(y, 1)
#>     x + y
#> }
#> attr(,"arg_ptypes")
#>         x 
#> "numeric" 
#> attr(,"output_ptype")
#> [1] "numeric"
```

The call to assert\_types will check that all parameters are right, and
will make sure they can’t be overriden by a value having another
prototype.

We can call it :

``` r
add1(3)
#> [1] 4
```

Fails early if fed the wrong arg type :

``` r
add1("a")
#> Error: `x`'s prototype should be numeric(0)
```

if we had changed the value of x, by assigning a char in the body it
would have failed too, right when we’d try to assign it:

``` r
add1..numeric := function (x..numeric) {
  message("we get up to here")
  x <- as.character(x)
  message("but not there")
  y := 1
  x + y
}

add1(3)
#> we get up to here
#> Error: assigned value should have same prototype as `x`: numeric(0)
```

Same thing if we try to modify `y` using an illegal type:

``` r
add1..numeric := function (x..numeric) {
  y := 1 # this forces y keep the same prototype
  message("we get up to here")
  y <- "2"
  message("but not there")
  x + y
}

add1(3)
#> we get up to here
#> Error: assigned value should have same prototype as `y`: numeric(0)
```
