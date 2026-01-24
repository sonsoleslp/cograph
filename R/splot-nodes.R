#' @title Base R Node Rendering
#' @description Node drawing functions for splot() using base R graphics.
#' @name splot-nodes
#' @keywords internal
NULL

#' Draw a Single Node
#'
#' Renders a node at the specified position with given aesthetics.
#'
#' @param x,y Node center coordinates.
#' @param size Node radius in user coordinates.
#' @param size2 Secondary size (for ellipse height).
#' @param shape Node shape name.
#' @param col Fill color.
#' @param border.col Border color.
#' @param border.width Border line width.
#' @param ... Additional parameters.
#' @keywords internal
draw_node_base <- function(x, y, size, size2 = NULL, shape = "circle",
                           col = "#4A90D9", border.col = "#2C5AA0",
                           border.width = 1, ...) {
  if (is.null(size2)) size2 <- size

  if (shape == "circle") {
    # Use symbols() for perfect circles
    graphics::symbols(
      x = x, y = y,
      circles = size,
      inches = FALSE,
      add = TRUE,
      fg = border.col,
      bg = col,
      lwd = border.width
    )

  } else if (shape == "square") {
    # Square using rect()
    graphics::rect(
      xleft = x - size,
      ybottom = y - size,
      xright = x + size,
      ytop = y + size,
      col = col,
      border = border.col,
      lwd = border.width
    )

  } else if (shape == "rectangle" || shape == "ellipse") {
    # Use polygon for ellipse/rectangle
    verts <- get_shape_vertices(shape, x, y, size, size2)
    graphics::polygon(
      x = verts$x,
      y = verts$y,
      col = col,
      border = border.col,
      lwd = border.width
    )

  } else {
    # All other shapes via polygon
    verts <- get_shape_vertices(shape, x, y, size, size2, ...)
    graphics::polygon(
      x = verts$x,
      y = verts$y,
      col = col,
      border = border.col,
      lwd = border.width
    )
  }
}

#' Draw Pie Chart Node
#'
#' Renders a node as a pie chart with multiple colored segments.
#'
#' @param x,y Node center coordinates.
#' @param size Node radius.
#' @param values Numeric vector of values (will be normalized to proportions).
#' @param colors Vector of colors for each segment.
#' @param border.col Border color.
#' @param border.width Border line width.
#' @keywords internal
draw_pie_node_base <- function(x, y, size, values, colors = NULL,
                               border.col = "black", border.width = 1) {
  if (is.null(values) || length(values) == 0) {
    return(invisible())
  }

  # Normalize to proportions
  props <- values / sum(values)
  n <- length(props)

  # Default colors
  if (is.null(colors)) {
    colors <- grDevices::rainbow(n, s = 0.7, v = 0.9)
  }
  colors <- recycle_to_length(colors, n)

  # Draw slices
  start_angle <- pi / 2  # Start at top
  n_points <- 50

  for (i in seq_len(n)) {
    if (props[i] <= 0) next

    end_angle <- start_angle - 2 * pi * props[i]

    # Create slice polygon
    angles <- seq(start_angle, end_angle, length.out = max(3, ceiling(n_points * props[i])))

    xs <- c(x, x + size * cos(angles), x)
    ys <- c(y, y + size * sin(angles), y)

    graphics::polygon(
      x = xs, y = ys,
      col = colors[i],
      border = NA
    )

    start_angle <- end_angle
  }

  # Draw border circle
  angles <- seq(0, 2 * pi, length.out = 100)
  graphics::lines(
    x = x + size * cos(angles),
    y = y + size * sin(angles),
    col = border.col,
    lwd = border.width
  )
}

#' Draw Donut Chart Node
#'
#' Renders a node as a donut chart with an inner hole.
#'
#' @param x,y Node center coordinates.
#' @param size Outer radius.
#' @param values Numeric vector of values (or single value 0-1 for progress).
#' @param colors Vector of colors for segments.
#' @param inner_ratio Ratio of inner to outer radius (0-1).
#' @param bg_color Background color for unfilled portion.
#' @param border.col Border color.
#' @param border.width Border line width.
#' @param show_value Logical: show value in center?
#' @param value_cex Text size for center value.
#' @param value_col Text color for center value.
#' @keywords internal
draw_donut_node_base <- function(x, y, size, values, colors = NULL,
                                 inner_ratio = 0.5, bg_color = "gray90",
                                 border.col = "black", border.width = 1,
                                 show_value = TRUE, value_cex = 0.8,
                                 value_col = "black") {
  outer_r <- size
  inner_r <- size * inner_ratio

  n_points <- 100

  # Helper to draw ring segment
  draw_ring_segment <- function(start_ang, end_ang, outer_r, inner_r, col) {
    n_pts <- max(10, ceiling(abs(end_ang - start_ang) / (2 * pi) * n_points))
    angles <- seq(start_ang, end_ang, length.out = n_pts)

    # Outer arc
    outer_x <- x + outer_r * cos(angles)
    outer_y <- y + outer_r * sin(angles)

    # Inner arc (reversed)
    inner_x <- x + inner_r * cos(rev(angles))
    inner_y <- y + inner_r * sin(rev(angles))

    graphics::polygon(
      x = c(outer_x, inner_x),
      y = c(outer_y, inner_y),
      col = col,
      border = NA
    )
  }

  center_value <- NULL

  if (length(values) == 1) {
    # Single value: progress donut
    prop <- max(0, min(1, values))
    center_value <- prop

    # Draw background ring
    draw_ring_segment(0, 2 * pi, outer_r, inner_r, bg_color)

    # Draw filled portion
    if (prop > 0) {
      start_ang <- pi / 2
      end_ang <- pi / 2 - 2 * pi * prop
      fill_col <- if (!is.null(colors)) colors[1] else "#4A90D9"
      draw_ring_segment(start_ang, end_ang, outer_r, inner_r, fill_col)
    }

  } else {
    # Multiple values: segmented donut
    props <- values / sum(values)
    n <- length(props)

    if (is.null(colors)) {
      colors <- grDevices::rainbow(n, s = 0.7, v = 0.9)
    }
    colors <- recycle_to_length(colors, n)

    start_ang <- pi / 2
    for (i in seq_len(n)) {
      if (props[i] <= 0) next

      end_ang <- start_ang - 2 * pi * props[i]
      draw_ring_segment(start_ang, end_ang, outer_r, inner_r, colors[i])
      start_ang <- end_ang
    }
  }

  # Fill inner hole with white
  angles <- seq(0, 2 * pi, length.out = n_points)
  graphics::polygon(
    x = x + inner_r * cos(angles),
    y = y + inner_r * sin(angles),
    col = "white",
    border = NA
  )

  # Draw borders
  graphics::lines(
    x = x + outer_r * cos(seq(0, 2*pi, length.out = n_points)),
    y = y + outer_r * sin(seq(0, 2*pi, length.out = n_points)),
    col = border.col,
    lwd = border.width
  )
  graphics::lines(
    x = x + inner_r * cos(seq(0, 2*pi, length.out = n_points)),
    y = y + inner_r * sin(seq(0, 2*pi, length.out = n_points)),
    col = border.col,
    lwd = border.width
  )

  # Show value in center
  if (show_value && !is.null(center_value)) {
    graphics::text(
      x = x, y = y,
      labels = round(center_value, 2),
      cex = value_cex,
      col = value_col,
      font = 2
    )
  }
}

#' Draw Donut with Inner Pie
#'
#' Renders a node with outer donut ring and inner pie chart.
#'
#' @param x,y Node center coordinates.
#' @param size Outer radius.
#' @param donut_value Single value (0-1) for donut progress.
#' @param donut_color Fill color for donut ring.
#' @param pie_values Numeric vector for pie segments.
#' @param pie_colors Vector of colors for pie segments.
#' @param inner_ratio Ratio of inner to outer radius.
#' @param bg_color Background color.
#' @param border.col Border color.
#' @param border.width Border line width.
#' @keywords internal
draw_donut_pie_node_base <- function(x, y, size, donut_value = 1,
                                     donut_color = "#4A90D9",
                                     pie_values = NULL, pie_colors = NULL,
                                     inner_ratio = 0.5, bg_color = "gray90",
                                     border.col = "black", border.width = 1) {
  outer_r <- size
  inner_r <- size * inner_ratio
  n_points <- 100

  # Helper to draw ring segment
  draw_ring_segment <- function(start_ang, end_ang, outer_r, inner_r, col) {
    n_pts <- max(10, ceiling(abs(end_ang - start_ang) / (2 * pi) * n_points))
    angles <- seq(start_ang, end_ang, length.out = n_pts)

    outer_x <- x + outer_r * cos(angles)
    outer_y <- y + outer_r * sin(angles)
    inner_x <- x + inner_r * cos(rev(angles))
    inner_y <- y + inner_r * sin(rev(angles))

    graphics::polygon(
      x = c(outer_x, inner_x),
      y = c(outer_y, inner_y),
      col = col,
      border = NA
    )
  }

  # Draw donut ring background
  draw_ring_segment(0, 2 * pi, outer_r, inner_r, bg_color)

  # Draw donut filled portion
  donut_prop <- max(0, min(1, donut_value))
  if (donut_prop > 0) {
    start_ang <- pi / 2
    end_ang <- pi / 2 - 2 * pi * donut_prop
    draw_ring_segment(start_ang, end_ang, outer_r, inner_r, donut_color)
  }

  # Draw inner pie
  pie_r <- inner_r * 0.95

  if (!is.null(pie_values) && length(pie_values) > 0) {
    props <- pie_values / sum(pie_values)
    n <- length(props)

    if (is.null(pie_colors)) {
      pie_colors <- grDevices::rainbow(n, s = 0.7, v = 0.9)
    }
    pie_colors <- recycle_to_length(pie_colors, n)

    start_ang <- pi / 2
    for (i in seq_len(n)) {
      if (props[i] <= 0) next

      end_ang <- start_ang - 2 * pi * props[i]
      n_pts <- max(3, ceiling(50 * props[i]))
      angles <- seq(start_ang, end_ang, length.out = n_pts)

      xs <- c(x, x + pie_r * cos(angles), x)
      ys <- c(y, y + pie_r * sin(angles), y)

      graphics::polygon(x = xs, y = ys, col = pie_colors[i], border = NA)
      start_ang <- end_ang
    }
  } else {
    # Fill inner with white
    angles <- seq(0, 2 * pi, length.out = n_points)
    graphics::polygon(
      x = x + pie_r * cos(angles),
      y = y + pie_r * sin(angles),
      col = "white",
      border = NA
    )
  }

  # Draw borders
  angles <- seq(0, 2 * pi, length.out = n_points)
  graphics::lines(x = x + outer_r * cos(angles), y = y + outer_r * sin(angles),
                  col = border.col, lwd = border.width)
  graphics::lines(x = x + inner_r * cos(angles), y = y + inner_r * sin(angles),
                  col = border.col, lwd = border.width)
}

#' Draw Node Label
#'
#' Renders a text label at or near a node.
#'
#' @param x,y Label position coordinates.
#' @param label Text to display.
#' @param cex Character expansion factor.
#' @param col Text color.
#' @param font Font face (1=plain, 2=bold, 3=italic, 4=bold italic).
#' @param pos Position relative to point (NULL=centered, 1=below, 2=left, 3=above, 4=right).
#' @param offset Offset distance when using pos.
#' @keywords internal
draw_node_label_base <- function(x, y, label, cex = 1, col = "black",
                                 font = 1, pos = NULL, offset = 0.5) {
  if (is.null(label) || is.na(label) || label == "") {
    return(invisible())
  }

  graphics::text(
    x = x, y = y,
    labels = label,
    cex = cex,
    col = col,
    font = font,
    pos = pos,
    offset = offset
  )
}

#' Render All Nodes
#'
#' Renders all nodes in the network.
#'
#' @param layout Matrix with x, y columns.
#' @param vsize Vector of node sizes.
#' @param vsize2 Vector of secondary sizes (for ellipse).
#' @param shape Vector of shape names.
#' @param color Vector of fill colors.
#' @param border.color Vector of border colors.
#' @param border.width Vector of border widths.
#' @param pie List of pie value vectors (one per node) or NULL.
#' @param pieColor List of pie color vectors or NULL.
#' @param donut List of donut values or NULL.
#' @param donutColor List of donut color vectors or NULL.
#' @param labels Vector of labels or NULL.
#' @param label.cex Vector of label sizes.
#' @param label.color Vector of label colors.
#' @keywords internal
render_nodes_base <- function(layout, vsize, vsize2 = NULL, shape = "circle",
                              color = "#4A90D9", border.color = "#2C5AA0",
                              border.width = 1, pie = NULL, pieColor = NULL,
                              donut = NULL, donutColor = NULL,
                              labels = NULL, label.cex = 1, label.color = "black") {
  n <- nrow(layout)
  if (n == 0) return(invisible())

  # Vectorize all parameters
  vsize <- recycle_to_length(vsize, n)
  vsize2 <- if (!is.null(vsize2)) recycle_to_length(vsize2, n) else vsize
  shape <- recycle_to_length(shape, n)
  color <- recycle_to_length(color, n)
  border.color <- recycle_to_length(border.color, n)
  border.width <- recycle_to_length(border.width, n)
  label.cex <- recycle_to_length(label.cex, n)
  label.color <- recycle_to_length(label.color, n)

  # Render order: largest to smallest
  order_idx <- get_node_order(vsize)

  for (i in order_idx) {
    x <- layout[i, 1]
    y <- layout[i, 2]

    # Check for pie/donut
    has_pie <- !is.null(pie) && length(pie) >= i && !is.null(pie[[i]]) && length(pie[[i]]) > 0
    has_donut <- !is.null(donut) && length(donut) >= i && !is.null(donut[[i]])

    if (has_donut && has_pie) {
      # Donut with inner pie
      donut_val <- if (length(donut[[i]]) == 1) donut[[i]] else 1
      donut_col <- if (!is.null(donutColor) && length(donutColor) >= i) donutColor[[i]][1] else color[i]
      pie_vals <- pie[[i]]
      pie_cols <- if (!is.null(pieColor) && length(pieColor) >= i) pieColor[[i]] else NULL

      draw_donut_pie_node_base(
        x, y, vsize[i],
        donut_value = donut_val,
        donut_color = donut_col,
        pie_values = pie_vals,
        pie_colors = pie_cols,
        border.col = border.color[i],
        border.width = border.width[i]
      )

    } else if (has_donut) {
      # Donut only
      donut_vals <- donut[[i]]
      donut_cols <- if (!is.null(donutColor) && length(donutColor) >= i) donutColor[[i]] else color[i]

      draw_donut_node_base(
        x, y, vsize[i],
        values = donut_vals,
        colors = donut_cols,
        border.col = border.color[i],
        border.width = border.width[i]
      )

    } else if (has_pie) {
      # Pie only
      pie_vals <- pie[[i]]
      pie_cols <- if (!is.null(pieColor) && length(pieColor) >= i) pieColor[[i]] else NULL

      draw_pie_node_base(
        x, y, vsize[i],
        values = pie_vals,
        colors = pie_cols,
        border.col = border.color[i],
        border.width = border.width[i]
      )

    } else {
      # Standard node
      draw_node_base(
        x, y, vsize[i], vsize2[i],
        shape = shape[i],
        col = color[i],
        border.col = border.color[i],
        border.width = border.width[i]
      )
    }
  }

  # Render labels (all at once, on top of nodes)
  if (!is.null(labels)) {
    for (i in seq_len(n)) {
      if (!is.null(labels[i]) && !is.na(labels[i]) && labels[i] != "") {
        draw_node_label_base(
          layout[i, 1], layout[i, 2],
          label = labels[i],
          cex = label.cex[i],
          col = label.color[i]
        )
      }
    }
  }
}
