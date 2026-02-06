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
