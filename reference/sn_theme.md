# Apply Theme to Network

Apply a visual theme to the network.

## Usage

``` r
sn_theme(network, theme, ...)
```

## Arguments

- network:

  A cograph_network object, matrix, data.frame, or igraph object.
  Matrices and other inputs are auto-converted.

- theme:

  Theme name (string) or CographTheme object.

- ...:

  Additional theme parameters to override.

## Value

Modified cograph_network object.

## Details

### Available Themes

- **classic**:

  Default theme with white background, blue nodes, gray edges.

- **dark**:

  Dark background with light nodes. Good for presentations.

- **minimal**:

  Subtle styling with thin edges and muted colors.

- **colorblind**:

  Optimized for color vision deficiency.

- **grayscale**:

  Black and white only.

- **vibrant**:

  Bold, saturated colors.

Use
[`list_themes()`](http://sonsoles.me/cograph/reference/list_themes.md)
to see all available themes.

## See also

[`cograph`](http://sonsoles.me/cograph/reference/cograph.md) for network
creation,
[`sn_palette`](http://sonsoles.me/cograph/reference/sn_palette.md) for
color palettes,
[`sn_nodes`](http://sonsoles.me/cograph/reference/sn_nodes.md) for node
customization,
[`sn_edges`](http://sonsoles.me/cograph/reference/sn_edges.md) for edge
customization,
[`list_themes`](http://sonsoles.me/cograph/reference/list_themes.md) to
see available themes,
[`splot`](http://sonsoles.me/cograph/reference/splot.md) and
[`soplot`](http://sonsoles.me/cograph/reference/soplot.md) for plotting

## Examples

``` r
adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)

# Apply different themes
cograph(adj) |> sn_theme("dark") |> splot()

cograph(adj) |> sn_theme("minimal") |> splot()

# Override specific theme properties
cograph(adj) |> sn_theme("classic", background = "lightgray") |> splot()

# Direct matrix input
adj |> sn_theme("dark")
#> Cograph Network
#> ==============
#> Nodes: 3 
#> Edges: 3 
#> Directed: FALSE 
#> Weighted: FALSE 
#> Layout: computed 
#> Theme: dark 
#> 
#> Use plot() or sn_render() to visualize
#> Use sn_ggplot() to convert to ggplot2
```
