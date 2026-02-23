# Convert a tna object to cograph parameters

Extracts the transition matrix, labels, and initial state probabilities
from a `tna` object and plots with cograph. Initial probabilities are
mapped to donut fills.

## Usage

``` r
from_tna(
  tna_object,
  engine = c("splot", "soplot"),
  plot = TRUE,
  weight_digits = 2,
  show_zero_edges = FALSE,
  ...
)
```

## Arguments

- tna_object:

  A `tna` object from
  [`tna::tna()`](http://sonsoles.me/tna/reference/build_model.md)

- engine:

  Which cograph renderer to use: `"splot"` or `"soplot"`. Default:
  `"splot"`.

- plot:

  Logical. If TRUE (default), immediately plot using the chosen engine.

- weight_digits:

  Number of decimal places to round edge weights to. Default 2. Edges
  that round to zero are removed unless `show_zero_edges = TRUE`.

- show_zero_edges:

  Logical. If TRUE, keep edges even if their weight rounds to zero.
  Default: FALSE.

- ...:

  Additional parameters passed to the plotting engine (e.g., `layout`,
  `node_fill`, `donut_color`).

## Value

Invisibly, a named list of cograph parameters that can be passed to
[`splot()`](http://sonsoles.me/cograph/reference/splot.md) or
[`soplot()`](http://sonsoles.me/cograph/reference/soplot.md).

## Details

### Conversion Process

The tna object's transition matrix becomes edge weights, labels become
node labels, and initial state probabilities (`inits`) are mapped to
`donut_fill` values to visualize starting state distributions.

TNA networks are always treated as directed because transition matrices
represent directional state changes.

The default `donut_inner_ratio` of 0.8 creates thin rings that
effectively visualize probability values without obscuring node labels.

### Parameter Mapping

The following tna properties are automatically extracted:

- **weights**: Transition matrix `->` edge weights

- **labels**: State labels `->` node labels

- **inits**: Initial probabilities `->` donut_fill (0-1 scale)

### TNA Visual Defaults

The following visual defaults are applied for TNA plots (all can be
overridden via `...`):

- `layout = "oval"`: Oval/elliptical node arrangement

- `node_fill`: Colors from TNA palette (Accent/Set3 based on state
  count)

- `node_size = 7`: Larger nodes for readability

- `arrow_size = 0.61`: Prominent directional arrows

- `edge_color = "#003355"`: Dark blue edges

- `edge_labels = TRUE`: Show transition weights on edges

- `edge_label_size = 0.6`: Readable edge labels

- `edge_label_position = 0.7`: Labels positioned toward target

- `edge_start_style = "dotted"`: Dotted line at edge source

- `edge_start_length = 0.2`: 20% of edge is dotted

## See also

[`cograph`](http://sonsoles.me/cograph/reference/cograph.md) for
creating networks from scratch,
[`splot`](http://sonsoles.me/cograph/reference/splot.md) and
[`soplot`](http://sonsoles.me/cograph/reference/soplot.md) for plotting
engines,
[`from_qgraph`](http://sonsoles.me/cograph/reference/from_qgraph.md) for
qgraph object conversion

## Examples

``` r
# Convert and plot a tna object
if (requireNamespace("tna", quietly = TRUE)) {
  library(tna)
  trans <- tna(group_regulation)
  from_tna(trans)  # Plots with donut rings showing initial probabilities

  # Use soplot engine instead
  from_tna(trans, engine = "soplot")

  # Customize the visualization
  from_tna(trans, layout = "circle", donut_color = c("steelblue", "gray90"))

  # Extract parameters without plotting
  params <- from_tna(trans, plot = FALSE)
  # Modify and plot manually
  params$node_fill <- "coral"
  do.call(splot, params)
}
#> 'tna' package version 1.2.0
#> ------------------------------------------------------
#>   Tikka, S., López-Pernas, S., and Saqr, M. (2025). 
#>   tna: An R Package for Transition Network Analysis.
#>   Applied Psychological Measurement.
#>   https://doi.org/10.1177/01466216251348840
#> ------------------------------------------------------
#> Please type 'citation("tna")' for more citation information.
#> See the package website at https://sonsoles.me/tna/
#> 
#> Attaching package: ‘tna’
#> The following objects are masked from ‘package:igraph’:
#> 
#>     cliques, communities, compare




```
