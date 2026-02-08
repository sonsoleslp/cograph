# Grid Layout

Arrange nodes in a grid pattern.

## Usage

``` r
layout_grid_fn(network, ncol = NULL, ...)
```

## Arguments

- network:

  A CographNetwork object.

- ncol:

  Number of columns. If NULL, computed as ceiling(sqrt(n)).

- ...:

  Additional arguments (ignored).

## Value

Data frame with x, y coordinates.
