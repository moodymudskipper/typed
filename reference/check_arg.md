# Check Argument Types and Return Type

These functions are not designed to be used directly, we advise to use
the syntaxes described in
[`?declare`](https://moodymudskipper.github.io/typed/reference/declare.md)
instead. `check_arg` checks that arguments satisfy an assertion, and if
relevant make them into active bindings to make sure they always satisy
it. `check_output` checks that the value, presumably a return value,
satisfies an assertion,

## Usage

``` r
check_output(.output, .assertion, ...)

check_arg(.arg, .assertion, ..., .bind = FALSE)
```

## Arguments

- .output:

  function output

- .assertion:

  an assertion

- ...:

  additional arguments passed to assertion

- .arg:

  function argument

- .bind:

  whether to actively bind the argument so it cannot be modified unless
  it satisfies the assertion

## Value

`.output`if it satisfies the assertion, fails otherwise.

returns `NULL` invisibly, called for side effects.
