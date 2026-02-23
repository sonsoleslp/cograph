# Convert to Cograph Network

Creates a lightweight cograph_network object from various network
inputs. The resulting object is a named list with all data accessible
via `$`.

## Usage

``` r
as_cograph(x, directed = NULL, ...)
```

## Arguments

- x:

  Network input. Can be:

  - A square numeric matrix (adjacency/weight matrix)

  - A data frame with edge list (from, to, optional weight columns)

  - An igraph object

  - A statnet network object

  - A qgraph object

  - A tna object

  - An existing cograph_network object (returned as-is)

- directed:

  Logical. Force directed interpretation. NULL for auto-detect.

- ...:

  Additional arguments (currently unused).

## Value

A cograph_network object: a named list with components:

- `from`:

  Integer vector of source node indices

- `to`:

  Integer vector of target node indices

- `weight`:

  Numeric vector of edge weights

- `nodes`:

  Data frame with id, label, (x, y if layout applied)

- `directed`:

  Logical indicating if network is directed

- `n_nodes`:

  Integer count of nodes

- `n_edges`:

  Integer count of edges

- `labels`:

  Character vector of node labels

- `source`:

  Character indicating input type

- `layout`:

  Layout coordinates (NULL until computed)

- `layout_info`:

  Layout algorithm info (NULL until computed)

## Details

The cograph_network format is designed to be:

- Simple: All data accessible via `net$from`, `net$to`, `net$weight`,
  etc.

- Modern: Uses named list elements instead of attributes for clean `$`
  access

- Compatible: Works seamlessly with splot() and other cograph functions

Use getter functions for programmatic access:
[`get_nodes`](http://sonsoles.me/cograph/reference/get_nodes.md),
[`get_edges`](http://sonsoles.me/cograph/reference/get_edges.md),
[`get_labels`](http://sonsoles.me/cograph/reference/get_labels.md)

Use setter functions to modify:
[`set_nodes`](http://sonsoles.me/cograph/reference/set_nodes.md),
[`set_edges`](http://sonsoles.me/cograph/reference/set_edges.md),
[`set_layout`](http://sonsoles.me/cograph/reference/set_layout.md)

## See also

[`get_nodes`](http://sonsoles.me/cograph/reference/get_nodes.md) to
extract the nodes data frame,
[`get_edges`](http://sonsoles.me/cograph/reference/get_edges.md) to
extract edges as a data frame,
[`n_nodes`](http://sonsoles.me/cograph/reference/n_nodes.md) and
[`n_edges`](http://sonsoles.me/cograph/reference/n_edges.md) for counts,
[`is_directed`](http://sonsoles.me/cograph/reference/is_directed.md) to
check directedness,
[`splot`](http://sonsoles.me/cograph/reference/splot.md) for plotting

## Examples

``` r
# From adjacency matrix
mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
net <- as_cograph(mat)

# Direct $ access to all data
net$from       # edge sources
#> [1] 1 1 2
net$to         # edge targets
#> [1] 2 3 3
net$weight     # edge weights
#> [1] 1 1 1
net$nodes      # nodes data frame
#>   id label name  x  y
#> 1  1     1    1 NA NA
#> 2  2     2    2 NA NA
#> 3  3     3    3 NA NA
net$directed   # TRUE/FALSE
#> [1] FALSE
net$n_nodes    # 3
#> [1] 3
net$n_edges    # 3
#> [1] 3

# Getter functions (recommended for programmatic use)
get_nodes(net)   # nodes data frame
#>   id label name  x  y
#> 1  1     1    1 NA NA
#> 2  2     2    2 NA NA
#> 3  3     3    3 NA NA
get_edges(net)   # edges data frame (from, to, weight)
#>   from to weight
#> 1    1  2      1
#> 2    1  3      1
#> 3    2  3      1
get_labels(net)  # character vector of labels
#> [1] "1" "2" "3"
n_nodes(net)     # 3
#> [1] 3
n_edges(net)     # 3
#> [1] 3
is_directed(net) # FALSE (symmetric matrix)
#> [1] FALSE

# Setter functions
net <- set_nodes(net, data.frame(id = 1:3, label = c("A", "B", "C")))
net <- set_edges(net, data.frame(from = c(1,2), to = c(2,3), weight = c(0.5, 0.8)))
net <- set_layout(net, data.frame(x = c(0, 1, 0.5), y = c(0, 0, 1)))

# Plot it
splot(net)


# From igraph (if installed)
if (requireNamespace("igraph", quietly = TRUE)) {
  library(igraph)
  g <- make_ring(10)
  net <- as_cograph(g)
  splot(net)
}
#> 
#> Attaching package: ‘igraph’
#> The following object is masked from ‘package:cograph’:
#> 
#>     is_directed
#> The following objects are masked from ‘package:stats’:
#> 
#>     decompose, spectrum
#> The following object is masked from ‘package:base’:
#> 
#>     union
```
