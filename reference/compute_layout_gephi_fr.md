# Wrapper for Gephi FR Layout (for layout registry)

Wrapper for Gephi FR Layout (for layout registry)

## Usage

``` r
compute_layout_gephi_fr(
  network,
  area = 10000,
  gravity = 10,
  speed = 1,
  niter = 100,
  ...
)
```

## Arguments

- network:

  A cograph_network object.

- area:

  Area parameter. Default 10000.

- gravity:

  Gravity force. Default 10.0.

- speed:

  Speed parameter. Default 1.0.

- niter:

  Number of iterations. Default 100.

- ...:

  Additional arguments (ignored).

## Value

Data frame with x, y coordinates.
