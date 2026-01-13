# Set Variable Types, Argument Types and Return Types.

Use `?` to set a function's return type, argument types, or variable
types in the body of the function. `declare` is an alternative to set a
variable's type.

## Usage

``` r
`?`(lhs, rhs)

declare(x, assertion, value, const = FALSE)
```

## Arguments

- lhs:

  lhs

- rhs:

  rhs

- x:

  variable name as a string

- assertion:

  a function

- value:

  an optional value

- const:

  whether to declare `x` as a constant

## Value

`declare` (and `?` when it maps to `declare`) returns `value` invisibly,
it is called for side effects. `assertion ? function(<args>) {<body>}`
returns a typed function, of class `c("typed", "function")`.
`fun <- assertion ? function(<args>) {<body>}` returns a typed function
and binds it to `fun` in the local environment.

## Set A Variable's Type

When used to set a variable's type, `?` maps to `declare` so that
`assertion ? var` calls `declare("var", assertion)`,
`assertion ? var <- value` calls `declare("var", assertion, value)`, and
`assertion ? (var) <- value` calls
`declare("var", assertion, value, const = TRUE)`

In those cases an active binding is defined so `var` returns `value` (or
`NULL` if none was provided). If `const` is `FALSE` (the default), the
returned value can then be altered if by assigning to `var`, but a value
which doesn't satisfy the assertion will trigger an error.

## Set A Function's Return Type

The syntaxes `assertion ? function(<args>) {<body>}` and
`fun <- assertion ? function(<args>) {<body>}` can be used to create a
function of class `c("typed", "function")`. The returned function will
have its body modified so that return values are wrapped inside a
[`check_output()`](https://moodymudskipper.github.io/typed/reference/check_arg.md)
call. Printing the function will display the return type.

## Set A Function Argument's Type

When using the above syntax, or if we don't want to force a return type,
the simpler `? function(<args>) {<body>}` or
`fun <- ? function(<args>) {<body>}` syntax, we can set argument types
by providing arguments as `arg = default_value ? assertion` or
`arg = ? assertion`. When entering the function, argument types will be
checked.

By default the arguments are only checked at the top, and might be
assigned later in the function's body values that don't satisfy the
assertion, to avoid this we can type `arg = default_value ? +assertion`
or `arg = ? +assertion`.

Note that forgetting the `?` before `function` is an easy mistake to do!

If we'd rather check the quoted argument rather than the argument's
value, we can type `arg = default_value ? ~assertion` or
`arg = ? ~assertion`. A possible use case might be `arg = ? ~ Symbol()`.

Dots can be checked too, `... = ? assertion` will make sure that every
argument passed to dots satisfies the assertion.

The special assertion factory `Dots` can also be used, in that case the
checks will apply to `list(...)` rather than to each element
individually, for instance `function(... = ? Dots(2))` makes sure the
dots were fed 2 values.

The returned function will have its body modified so the arguments are
checked by
[`check_arg()`](https://moodymudskipper.github.io/typed/reference/check_arg.md)
calls at the top. Printing the function will display the argument types.

## Examples

``` r
Integer() ? function (x= ? Integer()) {
  Integer() ? y <- 2L
  res <- x + y
  res
}
#> # typed function
#> function (x) 
#> {
#>     check_arg(x, Integer())
#>     declare("y", Integer(), value = 2L)
#>     res <- x + y
#>     check_output(res, Integer())
#> }
#> <environment: 0x55e534f53008>
#> # Return type: Integer()
#> # Arg types:
#> # x: Integer()
```
