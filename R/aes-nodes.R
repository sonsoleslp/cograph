#' @title Node Aesthetics
#' @description Functions for setting node aesthetic properties.
#' @name aes-nodes
NULL

#' Set Node Aesthetics
#'
#' Customize the visual appearance of nodes in a network plot.
#'
#' @param network A sonnet_network object, matrix, data.frame, or igraph object.
#'   Matrices and other inputs are auto-converted.
#' @param size Node size. Can be a single value, vector (per-node), or column name.
#' @param shape Node shape. Options: "circle", "square", "triangle", "diamond",
#'   "pentagon", "hexagon", "ellipse", "heart", "star", "pie", "donut", "cross", "rectangle".
#' @param fill Node fill color. Can be a single color, vector, or column name.
#' @param border_color Node border color.
#' @param border_width Node border width.
#' @param alpha Node transparency (0-1).
#' @param label_size Label text size.
#' @param label_color Label text color.
#' @param label_position Label position: "center", "above", "below", "left", "right".
#' @param show_labels Logical. Show node labels? Default TRUE.
#' @param pie_values For pie/donut/donut_pie shape: list or matrix of values for segments.
#'   For donut with single value (0-1), shows that proportion filled.
#' @param pie_colors For pie/donut/donut_pie shape: colors for pie segments.
#' @param pie_border_width Border width for pie chart nodes.
#' @param donut_values For donut_pie shape: vector of values (0-1) for the outer ring proportion.
#' @param donut_border_width Border width for donut chart nodes.
#' @param donut_inner_ratio For donut shape: inner radius ratio (0-1). Default 0.5.
#' @param donut_bg_color For donut shape: background color for unfilled portion.
#' @param donut_show_value For donut shape: show value in center? Default TRUE.
#' @param donut_value_size For donut shape: font size for center value.
#' @param donut_value_color For donut shape: color for center value text.
#' @param node_names Alternative names for legend (separate from display labels).
#' @return Modified sonnet_network object.
#' @export
#'
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' # With sonnet()
#' sonnet(adj) |>
#'   sn_nodes(size = 0.08, fill = "steelblue", shape = "circle")
#'
#' # Direct matrix input
#' adj |> sn_nodes(fill = "coral", size = 0.1)
sn_nodes <- function(network,
                     size = NULL,
                     shape = NULL,
                     fill = NULL,
                     border_color = NULL,
                     border_width = NULL,
                     alpha = NULL,
                     label_size = NULL,
                     label_color = NULL,
                     label_position = NULL,
                     show_labels = NULL,
                     pie_values = NULL,
                     pie_colors = NULL,
                     pie_border_width = NULL,
                     donut_values = NULL,
                     donut_border_width = NULL,
                     donut_inner_ratio = NULL,
                     donut_bg_color = NULL,
                     donut_show_value = NULL,
                     donut_value_size = NULL,
                     donut_value_color = NULL,
                     node_names = NULL) {

  # Auto-convert matrix/data.frame/igraph to sonnet_network
  network <- ensure_sonnet_network(network)

  # Clone the network to maintain immutability
  new_net <- network$network$clone_network()

  # Get node count for validation
  n <- new_net$n_nodes
  nodes_df <- new_net$get_nodes()

  # Build aesthetics list
  aes <- list()

  if (!is.null(size)) {
    aes$size <- resolve_aesthetic(size, nodes_df, n)
  }

  if (!is.null(shape)) {
    aes$shape <- resolve_aesthetic(shape, nodes_df, n)
  }

  if (!is.null(fill)) {
    aes$fill <- resolve_aesthetic(fill, nodes_df, n)
  }

  if (!is.null(border_color)) {
    aes$border_color <- resolve_aesthetic(border_color, nodes_df, n)
  }

  if (!is.null(border_width)) {
    aes$border_width <- resolve_aesthetic(border_width, nodes_df, n)
  }

  if (!is.null(alpha)) {
    validate_range(alpha, 0, 1, "alpha")
    aes$alpha <- resolve_aesthetic(alpha, nodes_df, n)
  }

  if (!is.null(label_size)) {
    aes$label_size <- resolve_aesthetic(label_size, nodes_df, n)
  }

  if (!is.null(label_color)) {
    aes$label_color <- resolve_aesthetic(label_color, nodes_df, n)
  }

  if (!is.null(label_position)) {
    valid_pos <- c("center", "above", "below", "left", "right")
    if (!all(label_position %in% valid_pos)) {
      stop("label_position must be one of: ", paste(valid_pos, collapse = ", "),
           call. = FALSE)
    }
    aes$label_position <- resolve_aesthetic(label_position, nodes_df, n)
  }

  if (!is.null(show_labels)) {
    aes$show_labels <- show_labels
  }

  if (!is.null(pie_values)) {
    aes$pie_values <- pie_values
  }

  if (!is.null(pie_colors)) {
    aes$pie_colors <- pie_colors
  }

  if (!is.null(pie_border_width)) {
    aes$pie_border_width <- pie_border_width
  }

  if (!is.null(donut_values)) {
    aes$donut_values <- donut_values
  }

  if (!is.null(donut_border_width)) {
    aes$donut_border_width <- donut_border_width
  }

  if (!is.null(donut_inner_ratio)) {
    aes$donut_inner_ratio <- donut_inner_ratio
  }

  if (!is.null(donut_bg_color)) {
    aes$donut_bg_color <- donut_bg_color
  }

  if (!is.null(donut_show_value)) {
    aes$donut_show_value <- donut_show_value
  }

  if (!is.null(donut_value_size)) {
    aes$donut_value_size <- donut_value_size
  }

  if (!is.null(donut_value_color)) {
    aes$donut_value_color <- donut_value_color
  }

  if (!is.null(node_names)) {
    aes$node_names <- resolve_aesthetic(node_names, nodes_df, n)
  }

  # Apply aesthetics
  new_net$set_node_aes(aes)

  # Return wrapped object
  as_sonnet_network(new_net)
}

#' Map Node Colors by Group
#'
#' Helper function to map node colors based on group membership.
#'
#' @param groups Vector of group assignments.
#' @param palette Color palette (function or character vector).
#' @return Character vector of colors.
#' @keywords internal
map_node_colors <- function(groups, palette = NULL) {
  groups <- as.factor(groups)
  n_groups <- length(levels(groups))

  if (is.null(palette)) {
    colors <- palette_colorblind(n_groups)
  } else if (is.function(palette)) {
    colors <- palette(n_groups)
  } else {
    colors <- rep(palette, length.out = n_groups)
  }

  colors[as.integer(groups)]
}

#' Scale Node Sizes
#'
#' Scale node sizes based on a numeric variable.
#'
#' @param values Numeric values to scale.
#' @param range Target size range (min, max).
#' @return Scaled size values.
#' @keywords internal
scale_node_sizes <- function(values, range = c(0.03, 0.1)) {
  if (all(is.na(values))) return(rep(mean(range), length(values)))

  val_range <- range(values, na.rm = TRUE)

  if (diff(val_range) == 0) {
    return(rep(mean(range), length(values)))
  }

  # Linear scaling
  scaled <- (values - val_range[1]) / diff(val_range)
  range[1] + scaled * diff(range)
}
