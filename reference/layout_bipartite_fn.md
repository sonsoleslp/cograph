# Bipartite Layout

Arrange nodes in two columns by type.

## Usage

``` r
layout_bipartite_fn(network, types = NULL, ...)
```

## Arguments

- network:

  A CographNetwork object.

- types:

  Vector of type assignments. If NULL, alternates between two types.

- ...:

  Additional arguments (ignored).

## Value

Data frame with x, y coordinates.
