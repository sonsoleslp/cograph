#' @title Base R Graphics Network Plotting
#' @description Network visualization using base R graphics (similar to qgraph).
#' @name splot
NULL

#' Plot Network with Base R Graphics
#'
#' Creates a network visualization using base R graphics functions (polygon,
#' lines, xspline, etc.) instead of grid graphics. This provides better
#' performance for large networks and qgraph-familiar parameter names.
#'
#' @param x Network input. Can be:
#'   - A square numeric matrix (adjacency/weight matrix)
#'   - A data frame with edge list (from, to, optional weight columns)
#'   - An igraph object
#'   - A sonnet_network object
#' @param layout Layout algorithm: "circle", "spring", "groups", or a matrix
#'   of x,y coordinates, or an igraph layout function.
#' @param directed Logical. Force directed interpretation. NULL for auto-detect.
#'
#' @section Node Aesthetics:
#' @param vsize Node size(s). Single value or vector.
#' @param vsize2 Secondary node size for ellipse/rectangle height.
#' @param shape Node shape(s): "circle", "square", "triangle", "diamond",
#'   "pentagon", "hexagon", "star", "heart", "ellipse", "cross".
#' @param color Node fill color(s).
#' @param border.color Node border color(s).
#' @param border.width Node border width(s).
#' @param labels Node labels: TRUE (use node names/indices), FALSE (none),
#'   or character vector.
#' @param label.cex Label character expansion factor.
#' @param label.color Label text color.
#'
#' @section Pie/Donut Nodes:
#' @param pie List of numeric vectors for pie chart nodes. Each element
#'   corresponds to a node and contains values for pie segments.
#' @param pieColor List of color vectors for pie segments.
#' @param donut List of values for donut chart nodes. Single value (0-1)
#'   for progress donut, or vector for segmented donut.
#' @param donutColor List of color vectors for donut segments.
#'
#' @section Edge Aesthetics:
#' @param edge.color Edge color(s). If NULL, uses posCol/negCol based on weight.
#' @param edge.width Edge width(s). If NULL, scales by weight.
#' @param edge.labels Edge labels: TRUE (show weights), FALSE (none),
#'   or character vector.
#' @param edge.label.cex Edge label size.
#' @param edge.label.bg Edge label background color.
#' @param edge.label.position Position along edge (0-1).
#' @param lty Line type(s): 1=solid, 2=dashed, 3=dotted, etc.
#' @param curve Edge curvature. 0 for straight, positive/negative for curves.
#' @param curveScale Logical: auto-curve reciprocal edges?
#' @param curvePivot Position along edge for curve control point (0-1).
#' @param asize Arrow head size.
#' @param arrows Logical or vector: show arrows on directed edges?
#' @param bidirectional Logical or vector: show arrows at both ends?
#' @param loopRotation Angle(s) in radians for self-loop direction.
#'
#' @section Weight Handling:
#' @param minimum Minimum absolute weight to display.
#' @param maximum Maximum weight for scaling. NULL for auto.
#' @param posCol Color for positive weights.
#' @param negCol Color for negative weights.
#'
#' @section Plot Settings:
#' @param title Plot title.
#' @param mar Margins as c(bottom, left, top, right).
#' @param background Background color.
#' @param rescale Logical: rescale layout to [-1, 1]?
#' @param aspect Logical: maintain aspect ratio?
#' @param legend Logical: show legend?
#' @param groups Group assignments for node coloring.
#'
#' @section Output:
#' @param filetype Output format: "default" (screen), "png", "pdf", "svg".
#' @param filename Output filename (without extension).
#' @param width Output width in inches.
#' @param height Output height in inches.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns the sonnet_network object.
#'
#' @export
#'
#' @examples
#' # Basic network from adjacency matrix
#' adj <- matrix(c(0, 1, 1, 0,
#'                 0, 0, 1, 1,
#'                 0, 0, 0, 1,
#'                 0, 0, 0, 0), 4, 4, byrow = TRUE)
#' splot(adj)
#'
#' # With curved edges
#' splot(adj, curve = 0.2)
#'
#' # Weighted network with colors
#' w_adj <- matrix(c(0, 0.5, -0.3, 0,
#'                   0.8, 0, 0.4, -0.2,
#'                   0, 0, 0, 0.6,
#'                   0, 0, 0, 0), 4, 4, byrow = TRUE)
#' splot(w_adj, posCol = "darkgreen", negCol = "red")
#'
#' # Pie chart nodes
#' splot(adj, pie = list(c(1,2,3), c(2,2), c(1,1,1,1), c(3,1)))
#'
#' # Circle layout with labels
#' splot(adj, layout = "circle", labels = c("A", "B", "C", "D"))
#'
splot <- function(
  x,
  layout = "spring",
  directed = NULL,

  # Node aesthetics
  vsize = NULL,
  vsize2 = NULL,
  shape = "circle",
  color = NULL,
  border.color = NULL,
  border.width = 1,
  labels = TRUE,
  label.cex = NULL,
  label.color = "black",

  # Pie/Donut
  pie = NULL,
  pieColor = NULL,
  donut = NULL,
  donutColor = NULL,

  # Edge aesthetics
  edge.color = NULL,
  edge.width = NULL,
  edge.labels = FALSE,
  edge.label.cex = 0.8,
  edge.label.bg = "white",
  edge.label.position = 0.5,
  lty = 1,
  curve = 0,
  curveScale = TRUE,
  curvePivot = 0.5,
  asize = 1,
  arrows = TRUE,
  bidirectional = FALSE,
  loopRotation = NULL,

  # Weight handling
  minimum = 0,
  maximum = NULL,
  posCol = "#2E7D32",
  negCol = "#C62828",

  # Plot settings
  title = NULL,
  mar = c(0.1, 0.1, 0.1, 0.1),
  background = "white",
  rescale = TRUE,
  aspect = TRUE,
  legend = FALSE,
  groups = NULL,

  # Output
  filetype = "default",
  filename = "splot",
  width = 7,
  height = 7,
  ...
) {

  # ============================================
  # 1. INPUT PROCESSING
  # ============================================

  # Convert to sonnet_network if needed
  network <- ensure_sonnet_network(x, layout = layout)

  nodes <- network$network$get_nodes()
  edges <- network$network$get_edges()
  layout_coords <- network$network$get_layout()

  n_nodes <- nrow(nodes)
  n_edges <- if (!is.null(edges)) nrow(edges) else 0

  # Determine if directed
  if (is.null(directed)) {
    directed <- network$network$is_directed
  }

  # ============================================
  # 2. LAYOUT HANDLING
  # ============================================

  if (is.null(layout_coords)) {
    stop("Layout coordinates not available", call. = FALSE)
  }

  layout_mat <- as.matrix(layout_coords[, c("x", "y")])

  # Rescale to [-1, 1]
  if (rescale) {
    layout_mat <- as.matrix(rescale_layout(layout_mat, mar = 0.1))
  }

  # ============================================
  # 3. PARAMETER VECTORIZATION
  # ============================================

  # Node sizes (qgraph-style)
  if (is.null(vsize)) vsize <- 3
  vsize_usr <- resolve_node_sizes(vsize, n_nodes, default_size = 3, scale_factor = 0.04)
  vsize2_usr <- if (!is.null(vsize2)) {
    resolve_node_sizes(vsize2, n_nodes, default_size = 3, scale_factor = 0.04)
  } else {
    vsize_usr
  }

  # Node shapes
  shapes <- resolve_shapes(shape, n_nodes)

  # Node colors
  node_colors <- resolve_node_colors(color, n_nodes, nodes, groups)

  # Border colors
  if (is.null(border.color)) {
    # Default: slightly darker than fill
    border.color <- sapply(node_colors, function(c) {
      tryCatch(adjust_brightness(c, -0.3), error = function(e) "black")
    })
  }
  border_colors <- recycle_to_length(border.color, n_nodes)

  # Border widths
  border_widths <- recycle_to_length(border.width, n_nodes)

  # Labels
  node_labels <- resolve_labels(labels, nodes, n_nodes)

  # Label sizes
  if (is.null(label.cex)) {
    # Auto-size based on node size
    label.cex <- pmin(1, vsize_usr * 8)
  }
  label_cex <- recycle_to_length(label.cex, n_nodes)
  label_colors <- recycle_to_length(label.color, n_nodes)

  # Edge parameters (only if edges exist)
  if (n_edges > 0) {
    # Filter by minimum weight
    edges <- filter_edges_by_weight(edges, minimum)
    n_edges <- nrow(edges)
  }

  if (n_edges > 0) {
    # Edge colors
    edge_colors <- resolve_edge_colors(edges, edge.color, posCol, negCol)

    # Edge widths
    edge_widths <- resolve_edge_widths(edges, edge.width, maximum, minimum)

    # Line types
    ltys <- recycle_to_length(lty, n_edges)

    # Curvatures
    curves <- resolve_curvatures(curve, edges, curveScale)
    curve_pivots <- recycle_to_length(curvePivot, n_edges)

    # Arrows
    if (is.logical(arrows) && length(arrows) == 1) {
      show_arrows <- rep(directed && arrows, n_edges)
    } else {
      show_arrows <- recycle_to_length(arrows, n_edges)
    }

    # Arrow size (convert from qgraph scale)
    arrow_size <- asize * 0.03
    arrow_sizes <- recycle_to_length(arrow_size, n_edges)

    # Bidirectional
    bidirectionals <- recycle_to_length(bidirectional, n_edges)

    # Loop rotation
    loop_rotations <- resolve_loop_rotation(loopRotation, edges, layout_mat)

    # Edge labels
    edge_labels <- resolve_edge_labels(edge.labels, edges, n_edges)
  }

  # ============================================
  # 4. DEVICE SETUP
  # ============================================

  # Handle file output
  if (filetype != "default") {
    full_filename <- paste0(filename, ".", filetype)

    if (filetype == "png") {
      grDevices::png(full_filename, width = width, height = height,
                     units = "in", res = 300)
    } else if (filetype == "pdf") {
      grDevices::pdf(full_filename, width = width, height = height)
    } else if (filetype == "svg") {
      grDevices::svg(full_filename, width = width, height = height)
    } else {
      stop("Unknown filetype: ", filetype, call. = FALSE)
    }

    on.exit(grDevices::dev.off(), add = TRUE)
  }

  # Set up plot area
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  # Margins
  graphics::par(mar = c(mar[1], mar[2], mar[3] + if (!is.null(title)) 1 else 0, mar[4]))

  # Calculate plot limits
  x_range <- range(layout_mat[, 1], na.rm = TRUE)
  y_range <- range(layout_mat[, 2], na.rm = TRUE)

  # Add margin to limits
  x_margin <- diff(x_range) * 0.15
  y_margin <- diff(y_range) * 0.15

  xlim <- c(x_range[1] - x_margin, x_range[2] + x_margin)
  ylim <- c(y_range[1] - y_margin, y_range[2] + y_margin)

  # Create plot
  graphics::plot(
    1, type = "n",
    xlim = xlim,
    ylim = ylim,
    axes = FALSE,
    ann = FALSE,
    asp = if (aspect) 1 else NA,
    xaxs = "i", yaxs = "i"
  )

  # Background
  if (!is.null(background) && background != "transparent") {
    graphics::rect(
      xleft = xlim[1], ybottom = ylim[1],
      xright = xlim[2], ytop = ylim[2],
      col = background, border = NA
    )
  }

  # Title
  if (!is.null(title)) {
    graphics::title(main = title)
  }

  # ============================================
  # 5. RENDER EDGES
  # ============================================

  if (n_edges > 0) {
    render_edges_base(
      edges = edges,
      layout = layout_mat,
      node_sizes = vsize_usr,
      shapes = shapes,
      edge.color = edge_colors,
      edge.width = edge_widths,
      lty = ltys,
      curve = curves,
      curvePivot = curve_pivots,
      arrows = show_arrows,
      asize = arrow_sizes,
      bidirectional = bidirectionals,
      loopRotation = loop_rotations,
      edge.labels = edge_labels,
      edge.label.cex = edge.label.cex,
      edge.label.bg = edge.label.bg,
      edge.label.position = edge.label.position
    )
  }

  # ============================================
  # 6. RENDER NODES
  # ============================================

  render_nodes_base(
    layout = layout_mat,
    vsize = vsize_usr,
    vsize2 = vsize2_usr,
    shape = shapes,
    color = node_colors,
    border.color = border_colors,
    border.width = border_widths,
    pie = pie,
    pieColor = pieColor,
    donut = donut,
    donutColor = donutColor,
    labels = node_labels,
    label.cex = label_cex,
    label.color = label_colors
  )

  # ============================================
  # 7. LEGEND (if requested)
  # ============================================

  if (legend && !is.null(groups)) {
    unique_groups <- unique(groups)
    n_groups <- length(unique_groups)
    palette <- grDevices::rainbow(n_groups, s = 0.7, v = 0.9)

    graphics::legend(
      "topright",
      legend = unique_groups,
      fill = palette,
      border = "black",
      bty = "n",
      cex = 0.8
    )
  }

  # ============================================
  # 8. RETURN
  # ============================================

  invisible(network)
}
