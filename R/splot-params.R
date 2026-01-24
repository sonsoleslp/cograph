#' @title splot Parameter Vectorization Helpers
#' @description Functions for resolving and vectorizing splot() parameters.
#' @name splot-params
#' @keywords internal
NULL

#' Resolve Edge Colors
#'
#' Determines edge colors based on weights, explicit colors, or defaults.
#'
#' @param edges Edge data frame with from, to, weight columns.
#' @param edge.color User-specified edge color(s) or NULL.
#' @param posCol Color for positive weights.
#' @param negCol Color for negative weights.
#' @param default_col Default color when no weight.
#' @return Vector of colors for each edge.
#' @keywords internal
resolve_edge_colors <- function(edges, edge.color = NULL, posCol = "#2E7D32",
                                negCol = "#C62828", default_col = "gray50") {
  m <- nrow(edges)
  if (m == 0) return(character(0))

  if (!is.null(edge.color)) {
    # User-specified colors
    return(recycle_to_length(edge.color, m))
  }

  # Color by weight sign
  if ("weight" %in% names(edges)) {
    weights <- edges$weight
    colors <- ifelse(
      weights > 0, posCol,
      ifelse(weights < 0, negCol, default_col)
    )
    return(colors)
  }

  # Default
  rep(default_col, m)
}

#' Resolve Edge Widths
#'
#' Determines edge widths based on weights or explicit values.
#'
#' @param edges Edge data frame.
#' @param edge.width User-specified width(s) or NULL.
#' @param maximum Maximum weight for scaling (NULL for auto).
#' @param minimum Minimum weight threshold.
#' @param base_width Base width value.
#' @param scale_factor Width scaling factor.
#' @return Vector of widths for each edge.
#' @keywords internal
resolve_edge_widths <- function(edges, edge.width = NULL, maximum = NULL,
                                minimum = 0, base_width = 1, scale_factor = 3) {
  m <- nrow(edges)
  if (m == 0) return(numeric(0))

  if (!is.null(edge.width)) {
    return(recycle_to_length(edge.width, m))
  }

  # Scale by weight
  if ("weight" %in% names(edges)) {
    weights <- abs(edges$weight)

    # Apply minimum threshold
    weights[weights < minimum] <- 0

    # Get maximum for scaling
    if (is.null(maximum)) {
      maximum <- max(weights, na.rm = TRUE)
    }
    if (maximum == 0) maximum <- 1

    # Scale weights to width range
    widths <- (weights / maximum) * scale_factor + base_width * 0.5
    return(widths)
  }

  rep(base_width, m)
}

#' Resolve Node Sizes
#'
#' Converts vsize parameter to user coordinate sizes.
#'
#' @param vsize User-specified node size(s).
#' @param n Number of nodes.
#' @param default_size Default size if NULL.
#' @param scale_factor Scale factor to apply.
#' @return Vector of node sizes.
#' @keywords internal
resolve_node_sizes <- function(vsize, n, default_size = 3, scale_factor = 0.03) {
  if (is.null(vsize)) {
    vsize <- default_size
  }

  sizes <- recycle_to_length(vsize, n)

  # Convert to user coordinates (qgraph-style sizing)
  sizes * scale_factor
}

#' Resolve Node Colors
#'
#' Determines node colors from various inputs.
#'
#' @param color User-specified color(s) or NULL.
#' @param n Number of nodes.
#' @param nodes Node data frame (for group coloring).
#' @param groups Group assignments for color mapping.
#' @param default_col Default node color.
#' @return Vector of colors for each node.
#' @keywords internal
resolve_node_colors <- function(color, n, nodes = NULL, groups = NULL,
                                default_col = "#4A90D9") {
  if (!is.null(color)) {
    return(recycle_to_length(color, n))
  }

  # Color by groups if provided
  if (!is.null(groups)) {
    unique_groups <- unique(groups)
    n_groups <- length(unique_groups)
    palette <- grDevices::rainbow(n_groups, s = 0.7, v = 0.9)
    colors <- palette[match(groups, unique_groups)]
    return(colors)
  }

  # Color from node data if available
  if (!is.null(nodes) && "color" %in% names(nodes)) {
    return(nodes$color)
  }

  rep(default_col, n)
}

#' Resolve Labels
#'
#' Determines node labels from various inputs.
#'
#' @param labels User-specified labels: TRUE, FALSE, character vector, or NULL.
#' @param nodes Node data frame.
#' @param n Number of nodes.
#' @return Character vector of labels (or NULL for no labels).
#' @keywords internal
resolve_labels <- function(labels, nodes, n) {
  if (is.null(labels) || identical(labels, FALSE)) {
    return(NULL)
  }

  if (identical(labels, TRUE)) {
    # Use node labels from data or indices
    if (!is.null(nodes) && "label" %in% names(nodes)) {
      return(as.character(nodes$label))
    }
    return(as.character(seq_len(n)))
  }

  # User-provided labels
  recycle_to_length(as.character(labels), n)
}

#' Resolve Edge Labels
#'
#' Determines edge labels from various inputs.
#'
#' @param edge.labels User-specified labels: TRUE, FALSE, character vector, or NULL.
#' @param edges Edge data frame.
#' @param m Number of edges.
#' @return Character vector of labels (or NULL for no labels).
#' @keywords internal
resolve_edge_labels <- function(edge.labels, edges, m) {
  if (is.null(edge.labels) || identical(edge.labels, FALSE)) {
    return(NULL)
  }

  if (identical(edge.labels, TRUE)) {
    # Use weights as labels if available
    if (!is.null(edges) && "weight" %in% names(edges)) {
      return(as.character(round(edges$weight, 2)))
    }
    return(rep("", m))
  }

  # User-provided labels
  recycle_to_length(as.character(edge.labels), m)
}

#' Resolve Shape Parameter
#'
#' Converts shape specification to vector of shape names.
#'
#' @param shape Shape specification.
#' @param n Number of nodes.
#' @return Character vector of shape names.
#' @keywords internal
resolve_shapes <- function(shape, n) {
  if (is.null(shape)) {
    shape <- "circle"
  }
  recycle_to_length(shape, n)
}

#' Resolve Curvature Parameter
#'
#' Determines edge curvatures, handling reciprocal edges.
#'
#' @param curve User-specified curvature(s).
#' @param edges Edge data frame.
#' @param curveScale Logical: scale curvature for reciprocal edges?
#' @param default_curve Default curvature for reciprocal edges.
#' @return Vector of curvatures.
#' @keywords internal
resolve_curvatures <- function(curve, edges, curveScale = TRUE,
                               default_curve = 0.2) {
  m <- nrow(edges)
  if (m == 0) return(numeric(0))

  curves <- recycle_to_length(curve, m)

  if (!curveScale) {
    return(curves)
  }

  # Identify reciprocal edges and apply default curvature
  for (i in seq_len(m)) {
    from_i <- edges$from[i]
    to_i <- edges$to[i]

    if (from_i == to_i) next  # Skip self-loops

    # Check for reciprocal
    for (j in seq_len(m)) {
      if (j != i && edges$from[j] == to_i && edges$to[j] == from_i) {
        # Found reciprocal - apply curvature if not already set
        if (curves[i] == 0) {
          curves[i] <- default_curve
        }
        break
      }
    }
  }

  curves
}

#' Resolve Loop Rotation
#'
#' Determines rotation angle for self-loops.
#'
#' @param loopRotation User-specified rotation(s) or NULL.
#' @param edges Edge data frame.
#' @param layout Layout coordinates (to auto-calculate optimal rotation).
#' @return Vector of rotation angles in radians.
#' @keywords internal
resolve_loop_rotation <- function(loopRotation, edges, layout = NULL) {
  m <- nrow(edges)
  if (m == 0) return(numeric(0))

  # Find self-loops
  is_loop <- edges$from == edges$to

  if (is.null(loopRotation)) {
    # Default: loop at top (pi/2)
    rotations <- rep(pi/2, m)

    # If layout provided, point away from center
    if (!is.null(layout)) {
      center_x <- mean(layout[, 1], na.rm = TRUE)
      center_y <- mean(layout[, 2], na.rm = TRUE)

      for (i in which(is_loop)) {
        node_idx <- edges$from[i]
        node_x <- layout[node_idx, 1]
        node_y <- layout[node_idx, 2]

        # Angle away from center
        rotations[i] <- atan2(node_y - center_y, node_x - center_x)
      }
    }

    return(rotations)
  }

  recycle_to_length(loopRotation, m)
}

#' Filter Edges by Weight Threshold
#'
#' Removes edges below the minimum weight threshold.
#'
#' @param edges Edge data frame.
#' @param minimum Minimum absolute weight to include.
#' @return Filtered edge data frame.
#' @keywords internal
filter_edges_by_weight <- function(edges, minimum = 0) {
  if (minimum == 0 || !"weight" %in% names(edges)) {
    return(edges)
  }

  edges[abs(edges$weight) >= minimum, , drop = FALSE]
}

#' Get Edge Rendering Order
#'
#' Returns indices for rendering edges from weakest to strongest.
#'
#' @param edges Edge data frame.
#' @return Integer vector of indices.
#' @keywords internal
get_edge_order <- function(edges) {
  if (!"weight" %in% names(edges) || nrow(edges) == 0) {
    return(seq_len(nrow(edges)))
  }

  order(abs(edges$weight))
}

#' Get Node Rendering Order
#'
#' Returns indices for rendering nodes from largest to smallest.
#'
#' @param sizes Vector of node sizes.
#' @return Integer vector of indices.
#' @keywords internal
get_node_order <- function(sizes) {
  order(sizes, decreasing = TRUE)
}
