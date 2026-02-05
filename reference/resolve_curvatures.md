# Resolve Curvature Parameter

Determines edge curvatures, handling reciprocal edges.

## Usage

``` r
resolve_curvatures(curve, edges, curveScale = TRUE, default_curve = 0.2)
```

## Arguments

- curve:

  User-specified curvature(s).

- edges:

  Edge data frame.

- curveScale:

  Logical: scale curvature for reciprocal edges?

- default_curve:

  Default curvature for reciprocal edges.

## Value

Vector of curvatures.
