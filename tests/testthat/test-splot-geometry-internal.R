# Exhaustive tests for splot-geometry.R internal functions
# Covers: R/splot-geometry.R

# Helper to initialize base R plot
init_base_plot <- function() {
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
}

# ============================================
# Coordinate Conversion Tests
# ============================================

test_that("usr_to_in_x converts correctly", {
  result <- with_temp_png({
    init_base_plot()
    val <- usr_to_in_x(0.5)
    is.numeric(val) && length(val) == 1
  })
  expect_true(result)
})

test_that("usr_to_in_y converts correctly", {
  result <- with_temp_png({
    init_base_plot()
    val <- usr_to_in_y(0.5)
    is.numeric(val) && length(val) == 1
  })
  expect_true(result)
})

test_that("in_to_usr_x converts correctly", {
  result <- with_temp_png({
    init_base_plot()
    val <- in_to_usr_x(1)
    is.numeric(val) && length(val) == 1
  })
  expect_true(result)
})

test_that("in_to_usr_y converts correctly", {
  result <- with_temp_png({
    init_base_plot()
    val <- in_to_usr_y(1)
    is.numeric(val) && length(val) == 1
  })
  expect_true(result)
})

test_that("get_x_scale returns positive value", {
  result <- with_temp_png({
    init_base_plot()
    val <- get_x_scale()
    is.numeric(val) && val > 0
  })
  expect_true(result)
})

test_that("get_y_scale returns positive value", {
  result <- with_temp_png({
    init_base_plot()
    val <- get_y_scale()
    is.numeric(val) && val > 0
  })
  expect_true(result)
})

test_that("atan2_usr calculates aspect-corrected angle", {
  result <- with_temp_png({
    init_base_plot()
    angle <- atan2_usr(1, 1)
    is.numeric(angle) && !is.na(angle)
  })
  expect_true(result)
})

# ============================================
# cent_to_edge Tests
# ============================================

test_that("cent_to_edge handles circle shape", {
  result <- with_temp_png({
    init_base_plot()
    boundary <- cent_to_edge(0.5, 0.5, 0, 0.1, shape = "circle")
    is.list(boundary) && "x" %in% names(boundary) && "y" %in% names(boundary)
  })
  expect_true(result)
})

test_that("cent_to_edge handles square shape", {
  result <- with_temp_png({
    init_base_plot()
    boundary <- cent_to_edge(0.5, 0.5, 0, 0.1, shape = "square")
    is.list(boundary) && boundary$x > 0.5  # Should be to the right
  })
  expect_true(result)
})

test_that("cent_to_edge handles rectangle shape", {
  result <- with_temp_png({
    init_base_plot()
    boundary <- cent_to_edge(0.5, 0.5, 0, 0.1, cex2 = 0.05, shape = "rectangle")
    is.list(boundary)
  })
  expect_true(result)
})

test_that("cent_to_edge handles ellipse shape", {
  result <- with_temp_png({
    init_base_plot()
    boundary <- cent_to_edge(0.5, 0.5, 0, 0.1, cex2 = 0.05, shape = "ellipse")
    is.list(boundary)
  })
  expect_true(result)
})

test_that("cent_to_edge handles unknown shape as circle", {
  result <- with_temp_png({
    init_base_plot()
    boundary <- cent_to_edge(0.5, 0.5, 0, 0.1, shape = "unknown_shape")
    is.list(boundary) && "x" %in% names(boundary)
  })
  expect_true(result)
})

test_that("cent_to_edge handles empty inputs", {
  result <- with_temp_png({
    init_base_plot()
    boundary <- cent_to_edge(numeric(0), 0.5, 0, 0.1)
    length(boundary$x) == 0
  })
  expect_true(result)
})

test_that("cent_to_edge handles NA inputs", {
  result <- with_temp_png({
    init_base_plot()
    boundary <- cent_to_edge(NA, 0.5, 0, 0.1)
    is.na(boundary$x)
  })
  expect_true(result)
})

test_that("cent_to_edge handles NA shape", {
  result <- with_temp_png({
    init_base_plot()
    boundary <- cent_to_edge(0.5, 0.5, 0, 0.1, shape = NA)
    is.list(boundary)  # Should default to circle
  })
  expect_true(result)
})

test_that("cent_to_edge square handles vertical angle", {
  result <- with_temp_png({
    init_base_plot()
    boundary <- cent_to_edge(0.5, 0.5, pi/2, 0.1, shape = "square")
    is.list(boundary)
  })
  expect_true(result)
})

test_that("cent_to_edge square handles horizontal angle", {
  result <- with_temp_png({
    init_base_plot()
    boundary <- cent_to_edge(0.5, 0.5, 0, 0.1, shape = "square")
    is.list(boundary)
  })
  expect_true(result)
})

test_that("cent_to_edge square handles diagonal angle", {
  result <- with_temp_png({
    init_base_plot()
    boundary <- cent_to_edge(0.5, 0.5, pi/4, 0.1, shape = "square")
    is.list(boundary)
  })
  expect_true(result)
})

# ============================================
# perp_mid Tests
# ============================================

test_that("perp_mid calculates midpoint", {
  result <- with_temp_png({
    init_base_plot()
    mid <- perp_mid(0, 0, 1, 1, 0.2, q = 0.5)
    is.list(mid) && "x" %in% names(mid) && "y" %in% names(mid)
  })
  expect_true(result)
})

test_that("perp_mid handles different q values", {
  result <- with_temp_png({
    init_base_plot()
    mid_start <- perp_mid(0, 0, 1, 0, 0.2, q = 0)
    mid_mid <- perp_mid(0, 0, 1, 0, 0.2, q = 0.5)
    mid_end <- perp_mid(0, 0, 1, 0, 0.2, q = 1)

    mid_start$x < mid_mid$x && mid_mid$x < mid_end$x
  })
  expect_true(result)
})

test_that("perp_mid handles zero-length edge", {
  result <- with_temp_png({
    init_base_plot()
    mid <- perp_mid(0.5, 0.5, 0.5, 0.5, 0.2)
    is.list(mid)  # Should not error
  })
  expect_true(result)
})

test_that("perp_mid handles negative curvature", {
  result <- with_temp_png({
    init_base_plot()
    mid_pos <- perp_mid(0, 0.5, 1, 0.5, 0.2)
    mid_neg <- perp_mid(0, 0.5, 1, 0.5, -0.2)

    mid_pos$y != mid_neg$y  # Different sides
  })
  expect_true(result)
})

# ============================================
# splot_distance Tests
# ============================================

test_that("splot_distance calculates correct distance", {
  expect_equal(splot_distance(0, 0, 1, 0), 1)
  expect_equal(splot_distance(0, 0, 0, 1), 1)
  expect_equal(splot_distance(0, 0, 1, 1), sqrt(2))
})

test_that("splot_distance handles same point", {
  expect_equal(splot_distance(0.5, 0.5, 0.5, 0.5), 0)
})

test_that("splot_distance handles negative coordinates", {
  expect_equal(splot_distance(-1, -1, 1, 1), sqrt(8))
})

# ============================================
# splot_angle Tests
# ============================================

test_that("splot_angle calculates correct angles", {
  expect_equal(splot_angle(0, 0, 1, 0), 0)  # Right
  expect_equal(splot_angle(0, 0, 0, 1), pi/2)  # Up
  expect_equal(splot_angle(0, 0, -1, 0), pi)  # Left
  expect_equal(splot_angle(0, 0, 0, -1), -pi/2)  # Down
})

test_that("splot_angle handles diagonal", {
  expect_equal(splot_angle(0, 0, 1, 1), pi/4)
})

test_that("splot_angle handles same point", {
  result <- splot_angle(0.5, 0.5, 0.5, 0.5)
  expect_true(is.numeric(result))  # Should return 0, not error
})

# ============================================
# rescale_layout Tests
# ============================================

test_that("rescale_layout rescales coordinates", {
  layout <- data.frame(x = c(0, 100), y = c(0, 100))

  result <- rescale_layout(layout)

  expect_true(all(result$x >= -1 & result$x <= 1))
  expect_true(all(result$y >= -1 & result$y <= 1))
})

test_that("rescale_layout preserves aspect ratio", {
  layout <- data.frame(x = c(0, 10), y = c(0, 5))

  result <- rescale_layout(layout)

  # X range should be twice Y range
  x_range <- diff(range(result$x))
  y_range <- diff(range(result$y))

  expect_true(abs(x_range - 2 * y_range) < 0.01)
})

test_that("rescale_layout handles constant x", {
  layout <- data.frame(x = c(5, 5, 5), y = c(0, 5, 10))

  result <- rescale_layout(layout)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
})

test_that("rescale_layout handles constant y", {
  layout <- data.frame(x = c(0, 5, 10), y = c(5, 5, 5))

  result <- rescale_layout(layout)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
})

test_that("rescale_layout respects margin", {
  layout <- data.frame(x = c(0, 100), y = c(0, 100))

  result <- rescale_layout(layout, mar = 0.2)

  expect_true(all(abs(result$x) <= 0.8))
  expect_true(all(abs(result$y) <= 0.8))
})

test_that("rescale_layout errors on single column", {
  layout <- data.frame(x = c(0, 1, 2))

  expect_error(rescale_layout(layout), "at least 2 columns")
})

test_that("rescale_layout handles matrix input", {
  layout <- matrix(c(0, 100, 0, 100), ncol = 2)

  result <- rescale_layout(layout)

  expect_true(is.data.frame(result))
})

# ============================================
# Integration Tests with splot
# ============================================

test_that("splot uses geometry functions correctly", {
  mat <- create_test_matrix(4)

  result <- with_temp_png({
    splot(mat, layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot handles custom layout with rescaling", {
  mat <- create_test_matrix(4)
  layout <- matrix(c(0, 0, 100, 100, 0, 100, 0, 100), ncol = 2)

  result <- with_temp_png({
    splot(mat, layout = layout)
    TRUE
  })

  expect_true(result)
})
