# Exhaustive tests for splot-arrows.R internal functions
# Covers: R/splot-arrows.R

# Helper to initialize base R plot
init_base_plot <- function() {
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
}

# ============================================
# arrow_base_midpoint Tests
# ============================================

test_that("arrow_base_midpoint calculates correct midpoint", {
  # Test with 0 angle (pointing right)
  result <- arrow_base_midpoint(x = 0.5, y = 0.5, angle = 0, size = 0.1)

  expect_true(is.list(result))
  expect_true("x" %in% names(result))
  expect_true("y" %in% names(result))
  expect_true(result$x < 0.5)  # Midpoint should be behind tip
})

test_that("arrow_base_midpoint handles different angles", {
  # Test with pi/2 angle (pointing up)
  result_up <- arrow_base_midpoint(x = 0.5, y = 0.5, angle = pi/2, size = 0.1)

  # Test with pi angle (pointing left)
  result_left <- arrow_base_midpoint(x = 0.5, y = 0.5, angle = pi, size = 0.1)

  # Midpoints should be different for different angles
  expect_false(result_up$x == result_left$x && result_up$y == result_left$y)
})

test_that("arrow_base_midpoint handles custom arrow_angle", {
  result_narrow <- arrow_base_midpoint(x = 0.5, y = 0.5, angle = 0, size = 0.1, arrow_angle = pi/12)
  result_wide <- arrow_base_midpoint(x = 0.5, y = 0.5, angle = 0, size = 0.1, arrow_angle = pi/4)

  expect_true(is.numeric(result_narrow$x))
  expect_true(is.numeric(result_wide$x))
})

# ============================================
# arrow_radius Tests
# ============================================

test_that("arrow_radius returns size value", {
  result <- arrow_radius(size = 0.1)
  expect_equal(result, 0.1)
})

test_that("arrow_radius handles different sizes", {
  expect_equal(arrow_radius(0.05), 0.05)
  expect_equal(arrow_radius(0.2), 0.2)
  expect_equal(arrow_radius(1), 1)
})

test_that("arrow_radius works with arrow_angle parameter", {
  result <- arrow_radius(size = 0.1, arrow_angle = pi/4)
  expect_equal(result, 0.1)
})

# ============================================
# draw_arrow_base Tests (requires graphics device)
# ============================================

test_that("draw_arrow_base draws without error", {
  result <- with_temp_png({
    init_base_plot()
    draw_arrow_base(x = 0.5, y = 0.5, angle = 0, size = 0.1)
    TRUE
  })
  expect_true(result)
})

test_that("draw_arrow_base handles color parameters", {
  result <- with_temp_png({
    init_base_plot()
    draw_arrow_base(x = 0.5, y = 0.5, angle = pi/4, size = 0.1,
                    col = "red", border = "black", lwd = 2)
    TRUE
  })
  expect_true(result)
})

test_that("draw_arrow_base handles NULL border", {
  result <- with_temp_png({
    init_base_plot()
    draw_arrow_base(x = 0.5, y = 0.5, angle = 0, size = 0.1,
                    col = "blue", border = NULL)
    TRUE
  })
  expect_true(result)
})

test_that("draw_arrow_base handles various angles", {
  angles <- c(0, pi/4, pi/2, pi, 3*pi/2)

  for (angle in angles) {
    result <- with_temp_png({
      init_base_plot()
      draw_arrow_base(x = 0.5, y = 0.5, angle = angle, size = 0.1)
      TRUE
    })
    expect_true(result)
  }
})

# ============================================
# arrow_head_points Tests
# ============================================

test_that("arrow_head_points returns correct structure", {
  result <- arrow_head_points(x = 0.5, y = 0.5, angle = 0, size = 0.1)

  expect_true(is.list(result))
  expect_true("x" %in% names(result))
  expect_true("y" %in% names(result))
  expect_true("mid_x" %in% names(result))
  expect_true("mid_y" %in% names(result))
  expect_true("back_len" %in% names(result))

  # x and y should have 3 points (triangle)
  expect_equal(length(result$x), 3)
  expect_equal(length(result$y), 3)
})

test_that("arrow_head_points calculates correct vertices", {
  result <- arrow_head_points(x = 1, y = 0.5, angle = 0, size = 0.1)

  # First point should be the tip
  expect_equal(result$x[1], 1)
  expect_equal(result$y[1], 0.5)
})

test_that("arrow_head_points handles different arrow angles", {
  result_narrow <- arrow_head_points(x = 0.5, y = 0.5, angle = 0, size = 0.1, arrow_angle = pi/12)
  result_wide <- arrow_head_points(x = 0.5, y = 0.5, angle = 0, size = 0.1, arrow_angle = pi/4)

  # Wide angle should have more spread in y
  expect_true(abs(result_wide$y[2] - result_wide$y[3]) > abs(result_narrow$y[2] - result_narrow$y[3]))
})

# ============================================
# draw_curved_arrow_base Tests
# ============================================

test_that("draw_curved_arrow_base draws without error", {
  result <- with_temp_png({
    init_base_plot()
    spline_x <- c(0.1, 0.3, 0.5, 0.7, 0.9)
    spline_y <- c(0.5, 0.6, 0.7, 0.6, 0.5)
    draw_curved_arrow_base(spline_x, spline_y, size = 0.05)
    TRUE
  })
  expect_true(result)
})

test_that("draw_curved_arrow_base handles short splines", {
  result <- with_temp_png({
    init_base_plot()
    spline_x <- c(0.1, 0.9)
    spline_y <- c(0.5, 0.5)
    draw_curved_arrow_base(spline_x, spline_y, size = 0.05, col = "blue")
    TRUE
  })
  expect_true(result)
})

test_that("draw_curved_arrow_base handles single point gracefully", {
  result <- with_temp_png({
    init_base_plot()
    draw_curved_arrow_base(c(0.5), c(0.5), size = 0.05)
    TRUE
  })
  expect_true(result)  # Should return invisibly without error
})

test_that("draw_curved_arrow_base handles color parameters", {
  result <- with_temp_png({
    init_base_plot()
    spline_x <- c(0.1, 0.5, 0.9)
    spline_y <- c(0.3, 0.7, 0.3)
    draw_curved_arrow_base(spline_x, spline_y, size = 0.05,
                           col = "red", border = "darkred", arrow_angle = pi/4)
    TRUE
  })
  expect_true(result)
})

# ============================================
# draw_open_arrow_base Tests
# ============================================

test_that("draw_open_arrow_base draws without error", {
  result <- with_temp_png({
    init_base_plot()
    draw_open_arrow_base(x = 0.5, y = 0.5, angle = 0, size = 0.1)
    TRUE
  })
  expect_true(result)
})

test_that("draw_open_arrow_base handles color and width", {
  result <- with_temp_png({
    init_base_plot()
    draw_open_arrow_base(x = 0.5, y = 0.5, angle = pi/4, size = 0.1,
                         col = "green", lwd = 3)
    TRUE
  })
  expect_true(result)
})

test_that("draw_open_arrow_base handles different arrow angles", {
  result <- with_temp_png({
    init_base_plot()
    draw_open_arrow_base(x = 0.5, y = 0.5, angle = 0, size = 0.1, arrow_angle = pi/3)
    TRUE
  })
  expect_true(result)
})

# ============================================
# draw_circle_arrow_base Tests
# ============================================

test_that("draw_circle_arrow_base draws without error", {
  result <- with_temp_png({
    init_base_plot()
    draw_circle_arrow_base(x = 0.5, y = 0.5, size = 0.05)
    TRUE
  })
  expect_true(result)
})

test_that("draw_circle_arrow_base handles color parameters", {
  result <- with_temp_png({
    init_base_plot()
    draw_circle_arrow_base(x = 0.5, y = 0.5, size = 0.05, col = "red", border = "black")
    TRUE
  })
  expect_true(result)
})

test_that("draw_circle_arrow_base handles NULL border", {
  result <- with_temp_png({
    init_base_plot()
    draw_circle_arrow_base(x = 0.5, y = 0.5, size = 0.05, col = "blue", border = NULL)
    TRUE
  })
  expect_true(result)
})

# ============================================
# shorten_edge_for_arrow Tests
# ============================================

test_that("shorten_edge_for_arrow calculates correct shortened endpoint", {
  # Horizontal edge pointing right
  result <- shorten_edge_for_arrow(x1 = 0, y1 = 0.5, x2 = 1, y2 = 0.5, arrow_size = 0.1)

  expect_true(is.list(result))
  expect_true("x" %in% names(result))
  expect_true("y" %in% names(result))

  # New endpoint should be 0.1 back from original
  expect_equal(result$x, 0.9)
  expect_equal(result$y, 0.5)
})

test_that("shorten_edge_for_arrow handles vertical edges", {
  # Vertical edge pointing up
  result <- shorten_edge_for_arrow(x1 = 0.5, y1 = 0, x2 = 0.5, y2 = 1, arrow_size = 0.1)

  expect_equal(result$x, 0.5)
  expect_equal(result$y, 0.9, tolerance = 1e-10)
})

test_that("shorten_edge_for_arrow handles diagonal edges", {
  result <- shorten_edge_for_arrow(x1 = 0, y1 = 0, x2 = 1, y2 = 1, arrow_size = 0.1)

  # Diagonal: shorten by 0.1 along the diagonal direction
  expect_true(result$x < 1)
  expect_true(result$y < 1)
  expect_true(result$x > 0.9)  # Should be close to endpoint
})

test_that("shorten_edge_for_arrow handles different arrow sizes", {
  result_small <- shorten_edge_for_arrow(x1 = 0, y1 = 0.5, x2 = 1, y2 = 0.5, arrow_size = 0.05)
  result_large <- shorten_edge_for_arrow(x1 = 0, y1 = 0.5, x2 = 1, y2 = 0.5, arrow_size = 0.2)

  expect_true(result_large$x < result_small$x)
})

# ============================================
# Integration Tests with splot
# ============================================

test_that("splot uses arrow functions for directed networks", {
  mat <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), 3, 3, byrow = TRUE)

  result <- with_temp_png({
    splot(mat, directed = TRUE, layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders custom arrow sizes", {
  mat <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), 3, 3, byrow = TRUE)

  result <- with_temp_png({
    splot(mat, directed = TRUE, arrow_size = 0.02, layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders custom arrow angles", {
  mat <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), 3, 3, byrow = TRUE)

  result <- with_temp_png({
    splot(mat, directed = TRUE, arrow_angle = pi/4, layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders arrows on curved edges", {
  mat <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3, byrow = TRUE)

  result <- with_temp_png({
    splot(mat, directed = TRUE, curves = TRUE, layout = "circle")
    TRUE
  })

  expect_true(result)
})
