# Multi-Cluster TNA Network Plot

Visualizes multiple network clusters with summary edges between clusters
and individual edges within clusters. Each cluster is displayed as a
shape (circle, square, diamond, triangle) containing its nodes.

## Usage

``` r
plot_mtna(
  x,
  cluster_list,
  layout = "circle",
  spacing = 3,
  shape_size = 1.2,
  node_spacing = 0.5,
  colors = NULL,
  shapes = NULL,
  edge_colors = NULL,
  bundle_edges = TRUE,
  bundle_strength = 0.8,
  summary_edges = TRUE,
  within_edges = TRUE,
  show_border = TRUE,
  legend = TRUE,
  legend_position = "topright",
  curvature = 0.3,
  node_size = 2,
  scale = 1,
  ...
)

mtna(
  x,
  cluster_list,
  layout = "circle",
  spacing = 3,
  shape_size = 1.2,
  node_spacing = 0.5,
  colors = NULL,
  shapes = NULL,
  edge_colors = NULL,
  bundle_edges = TRUE,
  bundle_strength = 0.8,
  summary_edges = TRUE,
  within_edges = TRUE,
  show_border = TRUE,
  legend = TRUE,
  legend_position = "topright",
  curvature = 0.3,
  node_size = 2,
  scale = 1,
  ...
)
```

## Arguments

- x:

  A tna object or weight matrix.

- cluster_list:

  List of character vectors defining clusters. Each cluster becomes a
  separate shape in the layout.

- layout:

  How to arrange the clusters: "circle" (default), "grid", "horizontal",
  "vertical".

- spacing:

  Distance between cluster centers. Default 3.

- shape_size:

  Size of each cluster shape (shell radius). Default 1.2.

- node_spacing:

  Radius for node placement within shapes (0-1 relative to shape_size).
  Default 0.5.

- colors:

  Vector of colors for each cluster. Default auto-generated.

- shapes:

  Vector of shapes for each cluster: "circle", "square", "diamond",
  "triangle". Default cycles through these.

- edge_colors:

  Vector of edge colors by source cluster. Default auto-generated.

- bundle_edges:

  Logical. Bundle inter-cluster edges through channels. Default TRUE.

- bundle_strength:

  How tightly to bundle edges (0-1). Default 0.8.

- summary_edges:

  Logical. Show aggregated summary edges between clusters instead of
  individual node edges. Default TRUE.

- within_edges:

  Logical. When summary_edges is TRUE, also show individual edges within
  each cluster. Default TRUE.

- show_border:

  Logical. Draw a border around each cluster. Default TRUE.

- legend:

  Logical. Whether to show legend. Default TRUE.

- legend_position:

  Position for legend. Default "topright".

- curvature:

  Edge curvature. Default 0.3.

- node_size:

  Size of nodes inside shapes. Default 2.

- ...:

  Additional parameters passed to plot_tna().

## Value

Invisibly returns NULL for summary mode, or the plot_tna result.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create network with 4 clusters
nodes <- paste0("N", 1:20)
m <- matrix(runif(400, 0, 0.3), 20, 20)
diag(m) <- 0
colnames(m) <- rownames(m) <- nodes

clusters <- list(
  North = paste0("N", 1:5),
  East = paste0("N", 6:10),
  South = paste0("N", 11:15),
  West = paste0("N", 16:20)
)

# Summary edges between clusters + individual edges within
plot_mtna(m, clusters, summary_edges = TRUE)

# Control spacing and sizes
plot_mtna(m, clusters, spacing = 4, shape_size = 1.5, node_spacing = 0.6)
} # }
```
