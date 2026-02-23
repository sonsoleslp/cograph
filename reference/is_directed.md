# Check if Network is Directed

Checks whether a cograph_network is directed.

## Usage

``` r
is_directed(x)
```

## Arguments

- x:

  A cograph_network object.

## Value

Logical: TRUE if directed, FALSE if undirected.

## See also

[`as_cograph`](http://sonsoles.me/cograph/reference/as_cograph.md)

## Examples

``` r
# Symmetric matrix -> undirected
mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- as_cograph(mat)
is_directed(net)  # FALSE
#> Error in ensure_igraph(graph): Must provide a graph object (provided wrong object type).

# Asymmetric matrix -> directed
mat2 <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), nrow = 3)
net2 <- as_cograph(mat2)
is_directed(net2)  # TRUE
#> Error in ensure_igraph(graph): Must provide a graph object (provided wrong object type).
```
