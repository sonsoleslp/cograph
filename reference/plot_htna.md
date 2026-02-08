# Plot Heterogeneous TNA Network (Multi-Group Layout)

Plots a TNA model with nodes arranged in multiple groups using geometric
layouts:

- 2 groups: Bipartite (two vertical columns or horizontal rows)

- 3+ groups: Polygon (nodes along edges of a regular polygon)

Supports triangle (3), rectangle (4), pentagon (5), hexagon (6), and
beyond.

## Usage

``` r
plot_htna(
  x,
  node_list,
  layout = "auto",
  use_list_order = TRUE,
  jitter = TRUE,
  jitter_amount = 0.8,
  jitter_side = "first",
  orientation = "vertical",
  group1_pos = -1.2,
  group2_pos = 1.2,
  curvature = 0.4,
  group1_color = "#ffd89d",
  group2_color = "#a68ba5",
  group1_shape = "circle",
  group2_shape = "square",
  group_colors = NULL,
  group_shapes = NULL,
  angle_spacing = 0.15,
  edge_colors = NULL,
  legend = TRUE,
  legend_position = "topright",
  extend_lines = FALSE,
  scale = 1,
  ...
)
```

## Arguments

- x:

  A tna object or weight matrix.

- node_list:

  List of 2+ character vectors defining node groups.

- layout:

  Layout type: "auto" (default), "bipartite", "polygon", or "circular".
  When "auto", uses bipartite for 2 groups and polygon for 3+ groups.
  "circular" places groups along arcs of a circle. Legacy values
  "triangle" and "rectangle" are supported as aliases for "polygon".

- use_list_order:

  Logical. Use node_list order (TRUE) or weight-based order (FALSE).
  Only applies to bipartite layout.

- jitter:

  Controls horizontal spread of nodes. Options:

  - TRUE (default): Auto-compute jitter based on edge connectivity

  - FALSE or 0: No jitter (nodes aligned in columns)

  - Numeric (0-1): Amount of jitter (0.3 = spread nodes 30\\

  - Named list: Manual per-node offsets by label (e.g., list(Wrong =
    -0.2))

  - Numeric vector of length n: Direct x-offsets for each node

  Only applies to bipartite layout.

- jitter_amount:

  Base jitter amount when jitter=TRUE. Default 0.5. Higher values spread
  nodes more toward the center. Only applies to bipartite layout.

- jitter_side:

  Which side(s) to apply jitter: "first", "second", "both", or "none".
  Default "first" (only first group nodes are jittered toward center).
  Only applies to bipartite layout.

- orientation:

  Layout orientation for bipartite: "vertical" (two columns, default) or
  "horizontal" (two rows). Ignored for triangle/rectangle layouts.

- group1_pos:

  Position for first group in bipartite layout. Default -1.2.

- group2_pos:

  Position for second group in bipartite layout. Default 1.2.

- curvature:

  Edge curvature amount. Default 0.4 for visible curves.

- group1_color:

  Color for first group nodes. Default "#ffd89d".

- group2_color:

  Color for second group nodes. Default "#a68ba5".

- group1_shape:

  Shape for first group nodes. Default "circle".

- group2_shape:

  Shape for second group nodes. Default "square".

- group_colors:

  Vector of colors for each group. Overrides group1_color/group2_color.
  Required for 3+ groups if not using defaults.

- group_shapes:

  Vector of shapes for each group. Overrides group1_shape/group2_shape.
  Required for 3+ groups if not using defaults.

- angle_spacing:

  Controls empty space at corners (0-1). Default 0.15. Higher values
  create larger empty angles at vertices. Only applies to
  triangle/rectangle layouts.

- edge_colors:

  Vector of colors for edges by source group. If NULL (default), uses
  darker versions of group_colors. Set to FALSE to use default edge
  color.

- legend:

  Logical. Whether to show a legend. Default TRUE for polygon layouts.

- legend_position:

  Position for legend: "topright", "topleft", "bottomright",
  "bottomleft", "right", "left", "top", "bottom". Default "topright".

- extend_lines:

  Logical or numeric. Draw extension lines from nodes. Only applies to
  bipartite layout.

  - FALSE (default): No extension lines

  - TRUE: Draw lines extending toward the other group (default length
    0.1)

  - Numeric: Length of extension lines

- ...:

  Additional parameters passed to tplot().

## Value

Invisibly returns the result from tplot().

## Examples

``` r
if (FALSE) { # \dontrun{
# Define node groups (2 groups - bipartite)
node_types <- list(
  Student = c("Wrong", "Retry", "Right", "Attempt", "Instruction", "Skip"),
  AI = c("Order", "Correct", "Hint", "Quit", "Clarify", "Question", "Praise")
)

# Basic bipartite plot
plot_htna(model, node_types)

# With custom jitter
plot_htna(model, node_types, jitter_amount = 0.5)

# Triangle layout (3 groups)
node_types_3 <- list(
  Teacher = c("Explain", "Question", "Feedback"),
  Student = c("Answer", "Ask", "Attempt"),
  System = c("Hint", "Score", "Progress")
)
plot_htna(model, node_types_3)  # Auto-detects triangle

# Rectangle layout (4 groups)
node_types_4 <- list(
  Input = c("Click", "Type", "Scroll"),
  Process = c("Validate", "Transform"),
  Output = c("Display", "Alert"),
  Storage = c("Save", "Load", "Cache")
)
plot_htna(model, node_types_4)  # Auto-detects rectangle

# Explicit layout selection
plot_htna(model, node_types_3, layout = "triangle")
} # }
```
