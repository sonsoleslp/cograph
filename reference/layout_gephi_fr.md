# Gephi Fruchterman-Reingold Layout

Force-directed layout that replicates Gephi's Fruchterman-Reingold
algorithm. This is a strict port of the Java implementation from Gephi's
source code.

## Usage

``` r
layout_gephi_fr(g, area = 10000, gravity = 10, speed = 1, niter = 100)
```

## Arguments

- g:

  An igraph graph object.

- area:

  Area parameter controlling node spread. Default 10000.

- gravity:

  Gravity force pulling nodes toward center. Default 10.0.

- speed:

  Speed/cooling parameter. Default 1.0.

- niter:

  Number of iterations. Default 100.

## Value

A matrix with x,y coordinates for each node.

## Details

This layout is a direct port of Gephi's ForceAtlas algorithm variant of
Fruchterman-Reingold. Key differences from igraph's layout_with_fr:

- Uses Gephi's specific constants (SPEED_DIVISOR=800,
  AREA_MULTIPLICATOR=10000)

- Includes configurable gravity toward center

- Different cooling/speed mechanism

## Examples

``` r
if (FALSE) { # \dontrun{
library(igraph)
g <- make_ring(10)
coords <- layout_gephi_fr(g)
plot(g, layout = coords)
} # }
```
