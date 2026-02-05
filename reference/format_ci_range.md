# Format CI Range

Formats confidence interval bounds as a range string.

## Usage

``` r
format_ci_range(lower, upper, digits = 2, format = "bracket")
```

## Arguments

- lower:

  Lower bound.

- upper:

  Upper bound.

- digits:

  Number of decimal places.

- format:

  CI format: "bracket" for `[low, up]` or "dash" for `low-up`.

## Value

Formatted CI range string.
