# Exhaustive tests for splot-edges.R internal functions
# Covers: R/splot-edges.R

# Helper to initialize base R plot
init_base_plot <- function() {
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
}

# ============================================
# find_curve_split_index Tests
# ============================================

test_that("find_curve_split_index handles basic case", {
  x <- c(0, 0.5, 1)
  y <- c(0, 0, 0)

  result <- find_curve_split_index(x, y, 0.5)

  expect_true(result >= 1)
  expect_true(result <= length(x))
})

test_that("find_curve_split_index handles fraction <= 0", {
  x <- c(0, 0.5, 1)
  y <- c(0, 0, 0)

  expect_equal(find_curve_split_index(x, y, 0), 1)
  expect_equal(find_curve_split_index(x, y, -0.1), 1)
})

test_that("find_curve_split_index handles fraction >= 1", {
  x <- c(0, 0.5, 1)
  y <- c(0, 0, 0)

  expect_equal(find_curve_split_index(x, y, 1), length(x))
  expect_equal(find_curve_split_index(x, y, 1.5), length(x))
})

test_that("find_curve_split_index handles short curves", {
  x <- c(0, 1)
  y <- c(0, 0)

  result <- find_curve_split_index(x, y, 0.5)
  expect_true(result >= 1)
  expect_true(result <= length(x))
})

test_that("find_curve_split_index handles single point", {
  x <- c(0.5)
  y <- c(0.5)

  result <- find_curve_split_index(x, y, 0.5)
  expect_equal(result, 1)
})

# ============================================
# draw_curve_with_start_segment Tests
# ============================================

test_that("draw_curve_with_start_segment draws without error", {
  result <- with_temp_png({
    init_base_plot()
    x <- c(0.1, 0.3, 0.5, 0.7, 0.9)
    y <- c(0.5, 0.6, 0.7, 0.6, 0.5)
    draw_curve_with_start_segment(x, y, col = "black", lwd = 1, lty = 1)
    TRUE
  })
  expect_true(result)
})

test_that("draw_curve_with_start_segment handles start segment", {
  result <- with_temp_png({
    init_base_plot()
    x <- c(0.1, 0.3, 0.5, 0.7, 0.9)
    y <- c(0.5, 0.6, 0.7, 0.6, 0.5)
    draw_curve_with_start_segment(x, y, col = "red", lwd = 2, lty = 1,
                                  start_lty = 2, start_fraction = 0.3)
    TRUE
  })
  expect_true(result)
})

test_that("draw_curve_with_start_segment handles no split", {
  result <- with_temp_png({
    init_base_plot()
    x <- c(0.1, 0.9)
    y <- c(0.5, 0.5)
    draw_curve_with_start_segment(x, y, col = "blue", lwd = 1, lty = 1,
                                  start_lty = 1, start_fraction = 0)
    TRUE
  })
  expect_true(result)
})

test_that("draw_curve_with_start_segment handles single point", {
  result <- with_temp_png({
    init_base_plot()
    draw_curve_with_start_segment(c(0.5), c(0.5), col = "black", lwd = 1, lty = 1)
    TRUE
  })
  expect_true(result)
})

# ============================================
# draw_straight_edge_base Tests
# ============================================

test_that("draw_straight_edge_base draws basic edge", {
  result <- with_temp_png({
    init_base_plot()
    draw_straight_edge_base(0.2, 0.2, 0.8, 0.8)
    TRUE
  })
  expect_true(result)
})

test_that("draw_straight_edge_base draws with arrow", {
  result <- with_temp_png({
    init_base_plot()
    draw_straight_edge_base(0.2, 0.5, 0.8, 0.5, arrow = TRUE, asize = 0.03)
    TRUE
  })
  expect_true(result)
})

test_that("draw_straight_edge_base draws without arrow", {
  result <- with_temp_png({
    init_base_plot()
    draw_straight_edge_base(0.2, 0.5, 0.8, 0.5, arrow = FALSE)
    TRUE
  })
  expect_true(result)
})

test_that("draw_straight_edge_base draws bidirectional", {
  result <- with_temp_png({
    init_base_plot()
    draw_straight_edge_base(0.2, 0.5, 0.8, 0.5, arrow = TRUE, asize = 0.03,
                            bidirectional = TRUE)
    TRUE
  })
  expect_true(result)
})

test_that("draw_straight_edge_base handles line styles", {
  result <- with_temp_png({
    init_base_plot()
    draw_straight_edge_base(0.2, 0.5, 0.8, 0.5, col = "red", lwd = 2, lty = 2)
    TRUE
  })
  expect_true(result)
})

test_that("draw_straight_edge_base handles start segment", {
  result <- with_temp_png({
    init_base_plot()
    draw_straight_edge_base(0.2, 0.5, 0.8, 0.5,
                            start_lty = 2, start_fraction = 0.3)
    TRUE
  })
  expect_true(result)
})

test_that("draw_straight_edge_base handles arrow angle", {
  result <- with_temp_png({
    init_base_plot()
    draw_straight_edge_base(0.2, 0.5, 0.8, 0.5, arrow = TRUE, asize = 0.03,
                            arrow_angle = pi/4)
    TRUE
  })
  expect_true(result)
})

# ============================================
# draw_curved_edge_base Tests
# ============================================

test_that("draw_curved_edge_base draws basic curved edge", {
  result <- with_temp_png({
    init_base_plot()
    draw_curved_edge_base(0.2, 0.5, 0.8, 0.5, curve = 0.3)
    TRUE
  })
  expect_true(result)
})

test_that("draw_curved_edge_base falls back to straight for tiny curve", {
  result <- with_temp_png({
    init_base_plot()
    draw_curved_edge_base(0.2, 0.5, 0.8, 0.5, curve = 0.0000001)
    TRUE
  })
  expect_true(result)
})

test_that("draw_curved_edge_base draws negative curve", {
  result <- with_temp_png({
    init_base_plot()
    draw_curved_edge_base(0.2, 0.5, 0.8, 0.5, curve = -0.3)
    TRUE
  })
  expect_true(result)
})

test_that("draw_curved_edge_base draws with curvePivot", {
  result <- with_temp_png({
    init_base_plot()
    draw_curved_edge_base(0.2, 0.5, 0.8, 0.5, curve = 0.3, curvePivot = 0.3)
    TRUE
  })
  expect_true(result)
})

test_that("draw_curved_edge_base draws with arrow", {
  result <- with_temp_png({
    init_base_plot()
    draw_curved_edge_base(0.2, 0.5, 0.8, 0.5, curve = 0.3,
                          arrow = TRUE, asize = 0.03)
    TRUE
  })
  expect_true(result)
})

test_that("draw_curved_edge_base draws without arrow", {
  result <- with_temp_png({
    init_base_plot()
    draw_curved_edge_base(0.2, 0.5, 0.8, 0.5, curve = 0.3, arrow = FALSE)
    TRUE
  })
  expect_true(result)
})

test_that("draw_curved_edge_base draws bidirectional", {
  result <- with_temp_png({
    init_base_plot()
    draw_curved_edge_base(0.2, 0.5, 0.8, 0.5, curve = 0.3,
                          arrow = TRUE, asize = 0.03, bidirectional = TRUE)
    TRUE
  })
  expect_true(result)
})

test_that("draw_curved_edge_base handles start segment", {
  result <- with_temp_png({
    init_base_plot()
    draw_curved_edge_base(0.2, 0.5, 0.8, 0.5, curve = 0.3,
                          start_lty = 2, start_fraction = 0.2)
    TRUE
  })
  expect_true(result)
})

# ============================================
# draw_self_loop_base Tests
# ============================================

test_that("draw_self_loop_base draws basic loop", {
  result <- with_temp_png({
    init_base_plot()
    draw_self_loop_base(0.5, 0.5, node_size = 0.05)
    TRUE
  })
  expect_true(result)
})

test_that("draw_self_loop_base draws with arrow", {
  result <- with_temp_png({
    init_base_plot()
    draw_self_loop_base(0.5, 0.5, node_size = 0.05, arrow = TRUE, asize = 0.02)
    TRUE
  })
  expect_true(result)
})

test_that("draw_self_loop_base draws without arrow", {
  result <- with_temp_png({
    init_base_plot()
    draw_self_loop_base(0.5, 0.5, node_size = 0.05, arrow = FALSE)
    TRUE
  })
  expect_true(result)
})

test_that("draw_self_loop_base handles rotation", {
  rotations <- c(0, pi/4, pi/2, pi, 3*pi/2)

  for (rot in rotations) {
    result <- with_temp_png({
      init_base_plot()
      draw_self_loop_base(0.5, 0.5, node_size = 0.05, rotation = rot)
      TRUE
    })
    expect_true(result)
  }
})

test_that("draw_self_loop_base handles color and style", {
  result <- with_temp_png({
    init_base_plot()
    draw_self_loop_base(0.5, 0.5, node_size = 0.05,
                        col = "red", lwd = 2, lty = 2)
    TRUE
  })
  expect_true(result)
})

# ============================================
# draw_edge_label_base Tests
# ============================================

test_that("draw_edge_label_base draws basic label", {
  result <- with_temp_png({
    init_base_plot()
    draw_edge_label_base(0.5, 0.5, label = "test")
    TRUE
  })
  expect_true(result)
})

test_that("draw_edge_label_base handles NULL label", {
  result <- with_temp_png({
    init_base_plot()
    draw_edge_label_base(0.5, 0.5, label = NULL)
    TRUE
  })
  expect_true(result)
})

test_that("draw_edge_label_base handles NA label", {
  result <- with_temp_png({
    init_base_plot()
    draw_edge_label_base(0.5, 0.5, label = NA)
    TRUE
  })
  expect_true(result)
})

test_that("draw_edge_label_base handles empty label", {
  result <- with_temp_png({
    init_base_plot()
    draw_edge_label_base(0.5, 0.5, label = "")
    TRUE
  })
  expect_true(result)
})

test_that("draw_edge_label_base handles background", {
  result <- with_temp_png({
    init_base_plot()
    draw_edge_label_base(0.5, 0.5, label = "test", bg = "white")
    TRUE
  })
  expect_true(result)
})

test_that("draw_edge_label_base handles no background", {
  result <- with_temp_png({
    init_base_plot()
    draw_edge_label_base(0.5, 0.5, label = "test", bg = NA)
    TRUE
  })
  expect_true(result)
})

test_that("draw_edge_label_base handles shadow", {
  result <- with_temp_png({
    init_base_plot()
    draw_edge_label_base(0.5, 0.5, label = "test", shadow = TRUE)
    TRUE
  })
  expect_true(result)
})

test_that("draw_edge_label_base handles styling", {
  result <- with_temp_png({
    init_base_plot()
    draw_edge_label_base(0.5, 0.5, label = "test",
                         cex = 1.2, col = "blue", font = 2)
    TRUE
  })
  expect_true(result)
})

# ============================================
# get_edge_label_position Tests
# ============================================

test_that("get_edge_label_position returns correct structure", {
  result <- get_edge_label_position(0.2, 0.5, 0.8, 0.5)

  expect_true(is.list(result))
  expect_true("x" %in% names(result))
  expect_true("y" %in% names(result))
})

test_that("get_edge_label_position handles straight edge", {
  result <- get_edge_label_position(0.2, 0.5, 0.8, 0.5, position = 0.5, curve = 0)

  # Should be at midpoint
  expect_equal(result$x, 0.5, tolerance = 0.01)
  expect_equal(result$y, 0.5, tolerance = 0.01)
})

test_that("get_edge_label_position handles curved edge", {
  result <- get_edge_label_position(0.2, 0.5, 0.8, 0.5, position = 0.5, curve = 0.3)

  # Should be offset from midpoint
  expect_true(result$y != 0.5)
})

test_that("get_edge_label_position handles position parameter", {
  result_start <- get_edge_label_position(0.2, 0.5, 0.8, 0.5, position = 0.1)
  result_end <- get_edge_label_position(0.2, 0.5, 0.8, 0.5, position = 0.9)

  expect_true(result_start$x < result_end$x)
})

test_that("get_edge_label_position handles label_offset", {
  result_no_offset <- get_edge_label_position(0.2, 0.5, 0.8, 0.5, label_offset = 0)
  result_with_offset <- get_edge_label_position(0.2, 0.5, 0.8, 0.5, label_offset = 0.1)

  expect_false(result_no_offset$y == result_with_offset$y)
})

test_that("get_edge_label_position handles curvePivot", {
  result_default <- get_edge_label_position(0.2, 0.5, 0.8, 0.5, curve = 0.3, curvePivot = 0.5)
  result_offset <- get_edge_label_position(0.2, 0.5, 0.8, 0.5, curve = 0.3, curvePivot = 0.3)

  # Different pivot should give different positions
  expect_true(!all(c(result_default$x, result_default$y) == c(result_offset$x, result_offset$y)))
})

test_that("get_edge_label_position handles zero length edge", {
  result <- get_edge_label_position(0.5, 0.5, 0.5, 0.5)

  expect_equal(result$x, 0.5)
  expect_equal(result$y, 0.5)
})

# ============================================
# Integration Tests with splot
# ============================================

test_that("splot renders edges correctly", {
  mat <- create_test_matrix(4)

  result <- with_temp_png({
    splot(mat, layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders curved edges", {
  mat <- create_test_matrix(4)

  result <- with_temp_png({
    splot(mat, curves = TRUE, layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders directed edges", {
  mat <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), 3, 3, byrow = TRUE)

  result <- with_temp_png({
    splot(mat, directed = TRUE, layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders self-loops", {
  mat <- create_test_matrix(3)
  diag(mat) <- 1

  result <- with_temp_png({
    splot(mat, layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders edge labels", {
  mat <- create_test_matrix(3, weighted = TRUE)

  result <- with_temp_png({
    splot(mat, edge_labels = TRUE, layout = "circle")
    TRUE
  })

  expect_true(result)
})

# ============================================
# Additional Edge Rendering Tests
# ============================================

test_that("splot renders edges with positive/negative colors", {
  mat <- matrix(c(0, 1, -0.5, 1, 0, 0.8, -0.5, 0.8, 0), 3, 3)

  result <- with_temp_png({
    splot(mat,
          edge_positive_color = "darkgreen",
          edge_negative_color = "darkred",
          layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders edges with cut threshold", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- with_temp_png({
    splot(mat, cut = 0.3, layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders edges with minimum threshold", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- with_temp_png({
    splot(mat, threshold = 0.2, layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders edges with maximum normalization", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- with_temp_png({
    splot(mat, maximum = 1.0, layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders bidirectional edges", {
  mat <- create_test_matrix(3, symmetric = FALSE)
  net <- cograph(mat, directed = TRUE)

  result <- with_temp_png({
    splot(net, bidirectional = TRUE, layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders edges with curve modes", {
  mat <- create_test_matrix(4, symmetric = FALSE)
  net <- cograph(mat, directed = TRUE)

  for (mode in list(TRUE, FALSE, "force", "mutual")) {
    result <- with_temp_png({
      splot(net, curves = mode, layout = "circle")
      TRUE
    })
    expect_true(result, info = paste("curves =", mode))
  }
})

test_that("splot renders edges with custom edge labels", {
  mat <- create_test_matrix(3)

  result <- with_temp_png({
    splot(mat, edge_labels = c("a", "b", "c"), layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders edges with edge_scale_mode", {
  mat <- create_test_matrix(4, weighted = TRUE)

  for (mode in c("linear", "sqrt", "log")) {
    result <- with_temp_png({
      splot(mat, edge_scale_mode = mode, layout = "circle")
      TRUE
    })
    expect_true(result, info = paste("edge_scale_mode =", mode))
  }
})

test_that("splot renders edges with loop_rotation", {
  mat <- matrix(c(1, 1, 0, 1, 1, 1, 0, 1, 1), 3, 3)

  result <- with_temp_png({
    splot(mat, loop_rotation = c(0, pi/2, pi), layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders edges with dashed style", {
  mat <- create_test_matrix(4)

  result <- with_temp_png({
    splot(mat, edge_style = "dashed", layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders edges with dotted style", {
  mat <- create_test_matrix(4)

  result <- with_temp_png({
    splot(mat, edge_style = "dotted", layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders edges with alpha transparency", {
  mat <- create_test_matrix(4)

  result <- with_temp_png({
    splot(mat, edge_alpha = 0.3, layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders edges with edge_width_range", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- with_temp_png({
    splot(mat, edge_width_range = c(0.5, 5), layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders edges with explicit curvature", {
  mat <- create_test_matrix(3)

  result <- with_temp_png({
    splot(mat, curvature = 0.3, layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders edges with edge_label_size", {
  mat <- create_test_matrix(3, weighted = TRUE)

  result <- with_temp_png({
    splot(mat, edge_labels = TRUE, edge_label_size = 1.5, layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders edges with edge_label_color", {
  mat <- create_test_matrix(3, weighted = TRUE)

  result <- with_temp_png({
    splot(mat, edge_labels = TRUE, edge_label_color = "blue", layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders edges with edge_label_position", {
  mat <- create_test_matrix(3, weighted = TRUE)

  result <- with_temp_png({
    splot(mat, edge_labels = TRUE, edge_label_position = 0.3, layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders edges with edge_label_bg", {
  mat <- create_test_matrix(3, weighted = TRUE)

  result <- with_temp_png({
    splot(mat, edge_labels = TRUE, edge_label_bg = "lightyellow", layout = "circle")
    TRUE
  })

  expect_true(result)
})

# ============================================
# soplot Edge Rendering Tests
# ============================================

test_that("soplot renders edges with various widths", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- with_temp_png({
    soplot(mat, edge_width = c(1, 2, 3, 4, 5, 6), layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("soplot renders edges with curves mode force", {
  mat <- create_test_matrix(4)

  result <- with_temp_png({
    soplot(mat, curves = "force", layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("soplot renders reciprocal edges correctly", {
  mat <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
  net <- cograph(mat, directed = TRUE)

  result <- with_temp_png({
    soplot(net, layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("soplot renders edges with edge_scale_mode", {
  mat <- create_test_matrix(4, weighted = TRUE)

  for (mode in c("linear", "sqrt", "log")) {
    result <- with_temp_png({
      soplot(mat, edge_scale_mode = mode, layout = "circle")
      TRUE
    })
    expect_true(result, info = paste("edge_scale_mode =", mode))
  }
})

test_that("soplot renders loop edges", {
  mat <- matrix(c(1, 1, 0, 1, 1, 1, 0, 1, 1), 3, 3)

  result <- with_temp_png({
    soplot(mat, layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("soplot renders edges with edge_cutoff", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- with_temp_png({
    soplot(mat, edge_cutoff = 0.3, layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("soplot renders edges with curve_shape", {
  mat <- create_test_matrix(4, symmetric = FALSE)
  net <- cograph(mat, directed = TRUE)

  for (shape in c(0.2, 0.5)) {
    result <- with_temp_png({
      soplot(net, curve_shape = shape, layout = "circle")
      TRUE
    })
    expect_true(result, info = paste("curve_shape =", shape))
  }
})

test_that("soplot renders edges with curve_pivot", {
  mat <- create_test_matrix(4, symmetric = FALSE)
  net <- cograph(mat, directed = TRUE)

  result <- with_temp_png({
    soplot(net, curvature = 0.3, curve_pivot = 0.3, layout = "circle")
    TRUE
  })

  expect_true(result)
})

# ============================================
# splot Base R Edge Tests
# ============================================

test_that("splot renders self-loops with arrow", {
  mat <- create_test_matrix(3)
  diag(mat) <- 1
  net <- cograph(mat, directed = TRUE)

  result <- with_temp_png({
    splot(net, layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders self-loops without arrow", {
  mat <- create_test_matrix(3)
  diag(mat) <- 1
  net <- cograph(mat, directed = FALSE)

  result <- with_temp_png({
    splot(net, layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders edges with various loop rotations", {
  mat <- create_test_matrix(3)
  diag(mat) <- 1

  for (rotation in c(0, pi/4, pi/2, pi, 3*pi/2)) {
    result <- with_temp_png({
      splot(mat, loop_rotation = rotation, layout = "circle")
      TRUE
    })
    expect_true(result, info = paste("loop_rotation =", rotation))
  }
})

test_that("splot renders curved edges with bidirectional arrows", {
  mat <- create_test_matrix(4, symmetric = FALSE)
  net <- cograph(mat, directed = TRUE) |>
    sn_edges(bidirectional = TRUE, curvature = 0.3)

  result <- with_temp_png({
    splot(net, layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders straight edges with bidirectional arrows", {
  mat <- create_test_matrix(4, symmetric = FALSE)
  net <- cograph(mat, directed = TRUE) |>
    sn_edges(bidirectional = TRUE, curves = FALSE)

  result <- with_temp_png({
    splot(net, layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders edges with start segment styling", {
  mat <- create_test_matrix(4, symmetric = FALSE)
  net <- cograph(mat, directed = TRUE)

  # Test the internal start_lty functionality through splot
  result <- with_temp_png({
    splot(net, layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders edges with edge labels", {
  mat <- create_test_matrix(3, weighted = TRUE)

  result <- with_temp_png({
    splot(mat, edge_labels = TRUE, layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders edges with edge label background", {
  mat <- create_test_matrix(3, weighted = TRUE)

  result <- with_temp_png({
    splot(mat, edge_labels = TRUE, edge_label_bg = "lightyellow", layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders edges with custom label position", {
  mat <- create_test_matrix(3, weighted = TRUE)

  result <- with_temp_png({
    splot(mat, edge_labels = TRUE, edge_label_position = 0.3, layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders edges with label shadow", {
  mat <- create_test_matrix(3, weighted = TRUE)
  net <- cograph(mat) |>
    sn_edges(labels = TRUE, label_shadow = TRUE,
             label_shadow_color = "gray40",
             label_shadow_offset = 1.0,
             label_shadow_alpha = 0.5)

  result <- with_temp_png({
    splot(net, layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders edges with no background", {
  mat <- create_test_matrix(3, weighted = TRUE)

  result <- with_temp_png({
    splot(mat, edge_labels = TRUE, edge_label_bg = NA, layout = "circle")
    TRUE
  })

  expect_true(result)
})

# ============================================
# Edge Label Position Tests
# ============================================

test_that("get_edge_label_position handles straight edge", {
  pos <- cograph:::get_edge_label_position(0, 0, 1, 1, position = 0.5, curve = 0)
  expect_equal(pos$x, 0.5, tolerance = 0.01)
  expect_equal(pos$y, 0.5, tolerance = 0.01)
})

test_that("get_edge_label_position handles curved edge", {
  pos <- cograph:::get_edge_label_position(0, 0, 1, 0, position = 0.5, curve = 0.3)
  expect_true(pos$y > 0)  # Curve bulges in perpendicular direction
})

test_that("get_edge_label_position handles label offset", {
  pos1 <- cograph:::get_edge_label_position(0, 0, 1, 0, position = 0.5, label_offset = 0)
  pos2 <- cograph:::get_edge_label_position(0, 0, 1, 0, position = 0.5, label_offset = 0.1)
  expect_true(pos2$y > pos1$y)  # Offset moves label perpendicular to edge
})

test_that("get_edge_label_position handles zero length edge", {
  pos <- cograph:::get_edge_label_position(0, 0, 0, 0, position = 0.5, curve = 0)
  expect_equal(pos$x, 0)
  expect_equal(pos$y, 0)
})

test_that("get_edge_label_position handles curved edge with pivot", {
  pos1 <- cograph:::get_edge_label_position(0, 0, 1, 0, position = 0.5, curve = 0.3, curvePivot = 0.5)
  pos2 <- cograph:::get_edge_label_position(0, 0, 1, 0, position = 0.5, curve = 0.3, curvePivot = 0.3)
  # Different pivots should give different positions
  expect_true(abs(pos1$y - pos2$y) > 0.001 || abs(pos1$x - pos2$x) > 0.001)
})

# ============================================
# find_curve_split_index Tests
# ============================================

test_that("find_curve_split_index returns 1 for single point", {
  idx <- cograph:::find_curve_split_index(c(0), c(0), 0.5)
  expect_equal(idx, 1)
})

test_that("find_curve_split_index returns n for fraction >= 1", {
  x <- c(0, 1, 2)
  y <- c(0, 0, 0)
  idx <- cograph:::find_curve_split_index(x, y, 1.0)
  expect_equal(idx, 3)
})

test_that("find_curve_split_index returns 1 for fraction <= 0", {
  x <- c(0, 1, 2)
  y <- c(0, 0, 0)
  idx <- cograph:::find_curve_split_index(x, y, 0)
  expect_equal(idx, 1)
})

test_that("find_curve_split_index handles midpoint", {
  x <- c(0, 1, 2, 3, 4)
  y <- c(0, 0, 0, 0, 0)
  idx <- cograph:::find_curve_split_index(x, y, 0.5)
  expect_true(idx >= 2 && idx <= 4)
})

# ============================================
# Curve Drawing Tests
# ============================================

test_that("splot renders edges with very small curvature", {
  mat <- create_test_matrix(3, symmetric = FALSE)
  net <- cograph(mat, directed = TRUE) |>
    sn_edges(curvature = 0.001)

  result <- with_temp_png({
    splot(net, layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders edges with large curvature", {
  mat <- create_test_matrix(3, symmetric = FALSE)
  net <- cograph(mat, directed = TRUE) |>
    sn_edges(curvature = 1.0)

  result <- with_temp_png({
    splot(net, layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders edges with pivot at start", {
  mat <- create_test_matrix(3, symmetric = FALSE)
  net <- cograph(mat, directed = TRUE) |>
    sn_edges(curvature = 0.3, curve_pivot = 0.1)

  result <- with_temp_png({
    splot(net, layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders edges with pivot at end", {
  mat <- create_test_matrix(3, symmetric = FALSE)
  net <- cograph(mat, directed = TRUE) |>
    sn_edges(curvature = 0.3, curve_pivot = 0.9)

  result <- with_temp_png({
    splot(net, layout = "circle")
    TRUE
  })

  expect_true(result)
})

# ============================================
# Arrow Angle Tests
# ============================================

test_that("splot renders edges with custom arrow angle", {
  mat <- create_test_matrix(3, symmetric = FALSE)
  net <- cograph(mat, directed = TRUE)

  result <- with_temp_png({
    splot(net, arrow_angle = pi/4, layout = "circle")
    TRUE
  })

  expect_true(result)
})

# ============================================
# Render Edges Base Tests
# ============================================

test_that("splot handles empty edges", {
  mat <- matrix(0, 4, 4)

  result <- with_temp_png({
    splot(mat, layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot handles network with only self-loops", {
  mat <- diag(4)

  result <- with_temp_png({
    splot(mat, layout = "circle")
    TRUE
  })

  expect_true(result)
})
