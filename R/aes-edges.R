#' @title Edge Aesthetics
#' @description Functions for setting edge aesthetic properties.
#' @name aes-edges
NULL

#' Set Edge Aesthetics
#'
#' Customize the visual appearance of edges in a network plot.
#'
#' @param network A sonnet_network object, matrix, data.frame, or igraph object.
#'   Matrices and other inputs are auto-converted.
#' @param width Edge width. Can be a single value, vector (per-edge), or "weight".
#' @param color Edge color. Can be a single color, vector, or "weight" for
#'   automatic coloring based on edge weights.
#' @param positive_color Color for positive edge weights.
#' @param negative_color Color for negative edge weights.
#' @param alpha Edge transparency (0-1).
#' @param style Line style: "solid", "dashed", "dotted", "longdash", "twodash".
#' @param curvature Edge curvature amount (0 = straight).
#' @param arrow_size Size of arrow heads for directed networks.
#' @param show_arrows Logical. Show arrows? Default TRUE for directed networks.
#' @param maximum Maximum edge weight for scaling width. Weights above this are
#'   capped. Similar to qgraph's maximum parameter.
#' @param width_scale Scale factor for edge widths. Values > 1 make edges thicker,
#'   values < 1 make them thinner. Applied after all other width calculations.
#' @param labels Edge labels. Can be TRUE (show weights), a vector, or column name.
#' @param label_size Edge label text size.
#' @param label_color Edge label text color.
#' @param label_position Position along edge (0 = source, 0.5 = middle, 1 = target).
#' @param label_offset Perpendicular offset from edge line.
#' @param bidirectional Logical. Show arrows at both ends of edges?
#' @param loop_rotation Angle in radians for self-loop direction (default: pi/2 = top).
#' @param curve_shape Spline tension for curved edges (-1 to 1, default: 0).
#' @param curve_pivot Pivot position along edge for curve control point (0-1, default: 0.5).
#' @return Modified sonnet_network object.
#' @export
#'
#' @examples
#' adj <- matrix(c(0, 1, -0.5, 1, 0, 1, -0.5, 1, 0), nrow = 3)
#' # With sonnet()
#' sonnet(adj) |>
#'   sn_edges(width = "weight", color = "weight")
#'
#' # Direct matrix input
#' adj |> sn_edges(width = 2, color = "gray50")
sn_edges <- function(network,
                     width = NULL,
                     color = NULL,
                     positive_color = NULL,
                     negative_color = NULL,
                     alpha = NULL,
                     style = NULL,
                     curvature = NULL,
                     arrow_size = NULL,
                     show_arrows = NULL,
                     maximum = NULL,
                     width_scale = NULL,
                     labels = NULL,
                     label_size = NULL,
                     label_color = NULL,
                     label_position = NULL,
                     label_offset = NULL,
                     bidirectional = NULL,
                     loop_rotation = NULL,
                     curve_shape = NULL,
                     curve_pivot = NULL) {

  # Auto-convert matrix/data.frame/igraph to sonnet_network
  network <- ensure_sonnet_network(network)

  # Clone the network to maintain immutability
  new_net <- network$network$clone_network()

  # Get edge count for validation
  edges_df <- new_net$get_edges()
  m <- if (is.null(edges_df)) 0 else nrow(edges_df)

  # Build aesthetics list
  aes <- list()

  if (!is.null(width)) {
    if (identical(width, "weight") && !is.null(edges_df$weight)) {
      # Scale width by weight, respecting maximum if set
      weights_for_scale <- abs(edges_df$weight)
      if (!is.null(maximum)) {
        weights_for_scale <- pmin(weights_for_scale, maximum)
      }
      aes$width <- scale_edge_widths(weights_for_scale, maximum = maximum)
    } else {
      aes$width <- resolve_aesthetic(width, edges_df, m)
    }
  }

  if (!is.null(maximum)) {
    aes$maximum <- maximum
  }

  if (!is.null(width_scale)) {
    aes$width_scale <- width_scale
  }

  if (!is.null(color)) {
    if (identical(color, "weight") && !is.null(edges_df$weight)) {
      # Color by weight sign: positive = green, negative = red
      current_aes <- new_net$get_edge_aes()
      pos_col <- if (!is.null(positive_color)) positive_color else current_aes$positive_color
      neg_col <- if (!is.null(negative_color)) negative_color else current_aes$negative_color
      aes$color <- ifelse(edges_df$weight >= 0, pos_col, neg_col)
    } else {
      aes$color <- resolve_aesthetic(color, edges_df, m)
    }
  }

  if (!is.null(positive_color)) {
    aes$positive_color <- positive_color
  }

  if (!is.null(negative_color)) {
    aes$negative_color <- negative_color
  }

  if (!is.null(alpha)) {
    validate_range(alpha, 0, 1, "alpha")
    aes$alpha <- resolve_aesthetic(alpha, edges_df, m)
  }

  if (!is.null(style)) {
    valid_styles <- c("solid", "dashed", "dotted", "longdash", "twodash")
    style_vals <- resolve_aesthetic(style, edges_df, m)
    if (!all(style_vals %in% valid_styles)) {
      stop("style must be one of: ", paste(valid_styles, collapse = ", "),
           call. = FALSE)
    }
    aes$style <- style_vals
  }

  if (!is.null(curvature)) {
    aes$curvature <- resolve_aesthetic(curvature, edges_df, m)
  }

  if (!is.null(arrow_size)) {
    aes$arrow_size <- arrow_size
  }

  if (!is.null(show_arrows)) {
    aes$show_arrows <- show_arrows
  }

  if (!is.null(labels)) {
    if (isTRUE(labels) && !is.null(edges_df$weight)) {
      # Auto-show weights as labels
      aes$labels <- round(edges_df$weight, 2)
    } else if (!isTRUE(labels) && !isFALSE(labels)) {
      aes$labels <- resolve_aesthetic(labels, edges_df, m)
    }
  }

  if (!is.null(label_size)) {
    aes$label_size <- label_size
  }

  if (!is.null(label_color)) {
    aes$label_color <- label_color
  }

  if (!is.null(label_position)) {
    aes$label_position <- label_position
  }

  if (!is.null(label_offset)) {
    aes$label_offset <- label_offset
  }

  if (!is.null(bidirectional)) {
    aes$bidirectional <- resolve_aesthetic(bidirectional, edges_df, m)
  }

  if (!is.null(loop_rotation)) {
    aes$loop_rotation <- resolve_aesthetic(loop_rotation, edges_df, m)
  }

  if (!is.null(curve_shape)) {
    aes$curve_shape <- resolve_aesthetic(curve_shape, edges_df, m)
  }

  if (!is.null(curve_pivot)) {
    aes$curve_pivot <- resolve_aesthetic(curve_pivot, edges_df, m)
  }

  # Apply aesthetics
  new_net$set_edge_aes(aes)

  # Return wrapped object
  as_sonnet_network(new_net)
}

#' Scale Edge Widths
#'
#' Scale edge widths based on a numeric variable.
#'
#' @param values Numeric values to scale.
#' @param range Target width range (min, max).
#' @param maximum Optional maximum value for scaling. If provided, this value
#'   maps to the max of range, and values above it are capped.
#' @return Scaled width values.
#' @keywords internal
scale_edge_widths <- function(values, range = c(0.5, 3), maximum = NULL) {
  if (all(is.na(values))) return(rep(mean(range), length(values)))

  # Use maximum as the upper bound if provided
  if (!is.null(maximum)) {
    val_min <- 0
    val_max <- maximum
    # Cap values at maximum
    values <- pmin(values, maximum)
  } else {
    val_min <- min(values, na.rm = TRUE)
    val_max <- max(values, na.rm = TRUE)
  }

  if (val_max == val_min) {
    return(rep(mean(range), length(values)))
  }

  # Linear scaling
  scaled <- (values - val_min) / (val_max - val_min)
  range[1] + scaled * diff(range)
}

#' Map Edge Colors by Weight
#'
#' Map edge colors based on weight values.
#'
#' @param weights Numeric weight values.
#' @param positive_color Color for positive weights.
#' @param negative_color Color for negative weights.
#' @param zero_color Color for zero weights.
#' @return Character vector of colors.
#' @keywords internal
map_edge_colors <- function(weights, positive_color = "#2E7D32",
                            negative_color = "#C62828",
                            zero_color = "gray50") {
  colors <- character(length(weights))
  colors[weights > 0] <- positive_color
  colors[weights < 0] <- negative_color
  colors[weights == 0] <- zero_color
  colors[is.na(weights)] <- zero_color
  colors
}
