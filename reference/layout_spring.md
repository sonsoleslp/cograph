# Fruchterman-Reingold Spring Layout

Compute node positions using the Fruchterman-Reingold force-directed
algorithm. Nodes connected by edges are attracted to each other while
all nodes repel each other.

## Usage

``` r
layout_spring(
  network,
  iterations = 500,
  cooling = 0.95,
  repulsion = 1,
  attraction = 1,
  seed = NULL,
  initial = NULL
)
```

## Arguments

- network:

  A CographNetwork object.

- iterations:

  Number of iterations (default: 500).

- cooling:

  Rate of temperature decrease (default: 0.95).

- repulsion:

  Repulsion constant (default: 1).

- attraction:

  Attraction constant (default: 1).

- seed:

  Random seed for reproducibility.

- initial:

  Optional initial coordinates (matrix or data frame).

## Value

Data frame with x, y coordinates.

## Examples

``` r
adj <- matrix(c(0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0), nrow = 4)
net <- CographNetwork$new(adj)
coords <- layout_spring(net, seed = 42)
```
