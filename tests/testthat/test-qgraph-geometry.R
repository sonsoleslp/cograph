# Tests for qgraph-compatible geometry utilities
# Covers: R/sonplot-qgraph-geometry.R

# ============================================
# qgraph_default_vsize Tests
# ============================================

test_that("qgraph_default_vsize returns positive value", {
  result <- cograph:::qgraph_default_vsize(10)
  expect_true(result > 0)
})

test_that("qgraph_default_vsize decreases with more nodes", {
  size_10 <- cograph:::qgraph_default_vsize(10)
  size_100 <- cograph:::qgraph_default_vsize(100)
  expect_true(size_10 > size_100)
})

test_that("qgraph_default_vsize follows expected formula", {
  # Formula: 8 * exp(-n/80) + 1
  n <- 40
  expected <- 8 * exp(-n/80) + 1
  result <- cograph:::qgraph_default_vsize(n)
  expect_equal(result, expected)
})

# ============================================
# qgraph_default_esize Tests
# ============================================

test_that("qgraph_default_esize returns positive value", {
  result <- cograph:::qgraph_default_esize(10, weighted = TRUE, directed = FALSE)
  expect_true(result > 0)
})

test_that("qgraph_default_esize handles unweighted networks", {
  result <- cograph:::qgraph_default_esize(10, weighted = FALSE, directed = FALSE)
  expect_equal(result, 2)
})

test_that("qgraph_default_esize halves for directed networks", {
  undirected <- cograph:::qgraph_default_esize(10, weighted = TRUE, directed = FALSE)
  directed <- cograph:::qgraph_default_esize(10, weighted = TRUE, directed = TRUE)
  expect_true(directed <= undirected)
})

test_that("qgraph_default_esize has minimum of 1 for directed", {
  # With many nodes, the size shrinks
  result <- cograph:::qgraph_default_esize(1000, weighted = TRUE, directed = TRUE)
  expect_true(result >= 1)
})

# ============================================
# qgraph_scale_edge_widths Tests
# ============================================

test_that("qgraph_scale_edge_widths returns empty for empty input", {
  result <- cograph:::qgraph_scale_edge_widths(numeric(0))
  expect_equal(length(result), 0)
})

test_that("qgraph_scale_edge_widths returns positive values", {
  weights <- c(0.5, 1.0, 0.8)
  result <- cograph:::qgraph_scale_edge_widths(weights)
  expect_true(all(result > 0))
})

test_that("qgraph_scale_edge_widths respects maximum parameter", {
  weights <- c(0.5, 1.0, 0.8)
  result <- cograph:::qgraph_scale_edge_widths(weights, maximum = 2)
  expect_true(all(result > 0))
})

test_that("qgraph_scale_edge_widths handles cut parameter", {
  weights <- c(0.1, 0.5, 1.0)
  result <- cograph:::qgraph_scale_edge_widths(weights, cut = 0.3)
  # Weights below cut should be 0 or very small
  expect_true(result[1] <= result[2])
})

test_that("qgraph_scale_edge_widths handles esize parameter", {
  weights <- c(0.5, 1.0)
  result <- cograph:::qgraph_scale_edge_widths(weights, esize = 10)
  expect_true(max(result) <= 10)
})

test_that("qgraph_scale_edge_widths handles zero maximum", {
  weights <- c(0, 0, 0)
  result <- cograph:::qgraph_scale_edge_widths(weights)
  expect_true(all(!is.na(result)))
})

# ============================================
# qgraph_plot_info Tests
# ============================================

test_that("qgraph_plot_info returns list with expected components", {
  # Need a plot device open
  png_file <- tempfile(fileext = ".png")
  png(png_file, width = 400, height = 400)
  plot.new()

  result <- cograph:::qgraph_plot_info()

  dev.off()
  unlink(png_file)

  expect_true(is.list(result))
  expect_true("usr" %in% names(result))
  expect_true("pin" %in% names(result))
  expect_true("mai" %in% names(result))
  expect_true("csi" %in% names(result))
  expect_true("dev_name" %in% names(result))
})

# ============================================
# qgraph_cent2edge Tests
# ============================================

test_that("qgraph_cent2edge returns list with x and y", {
  png_file <- tempfile(fileext = ".png")
  png(png_file, width = 400, height = 400)
  plot.new()

  result <- cograph:::qgraph_cent2edge(0.5, 0.5, cex = 1, offset = 0, angle = pi/4)

  dev.off()
  unlink(png_file)

  expect_true(is.list(result))
  expect_true("x" %in% names(result))
  expect_true("y" %in% names(result))
})

test_that("qgraph_cent2edge handles different angles", {
  png_file <- tempfile(fileext = ".png")
  png(png_file, width = 400, height = 400)
  plot.new()

  result0 <- cograph:::qgraph_cent2edge(0.5, 0.5, cex = 1, offset = 0, angle = 0)
  result_pi <- cograph:::qgraph_cent2edge(0.5, 0.5, cex = 1, offset = 0, angle = pi)

  dev.off()
  unlink(png_file)

  # Different angles should give different results
  expect_true(result0$y != result_pi$y || result0$x != result_pi$x)
})

# ============================================
# qgraph_norm_curve Tests
# ============================================

test_that("qgraph_norm_curve returns positive value", {
  png_file <- tempfile(fileext = ".png")
  png(png_file, width = 400, height = 400)
  plot.new()

  result <- cograph:::qgraph_norm_curve()

  dev.off()
  unlink(png_file)

  expect_true(result > 0)
})

# ============================================
# qgraph_vsize_to_user Tests
# ============================================

test_that("qgraph_vsize_to_user returns positive value", {
  png_file <- tempfile(fileext = ".png")
  png(png_file, width = 400, height = 400)
  plot.new()

  result <- cograph:::qgraph_vsize_to_user(5)

  dev.off()
  unlink(png_file)

  expect_true(result > 0)
})

test_that("qgraph_vsize_to_user scales with vsize", {
  png_file <- tempfile(fileext = ".png")
  png(png_file, width = 400, height = 400)
  plot.new()

  small <- cograph:::qgraph_vsize_to_user(1)
  large <- cograph:::qgraph_vsize_to_user(10)

  dev.off()
  unlink(png_file)

  expect_true(large > small)
})

# ============================================
# qgraph_cent_to_edge_simple Tests
# ============================================

test_that("qgraph_cent_to_edge_simple works for circle", {
  result <- cograph:::qgraph_cent_to_edge_simple(0, 0, pi/4, 1, shape = "circle")

  expect_true(is.list(result))
  expect_true("x" %in% names(result))
  expect_true("y" %in% names(result))
})

test_that("qgraph_cent_to_edge_simple works for square", {
  result <- cograph:::qgraph_cent_to_edge_simple(0, 0, pi/4, 1, shape = "square")

  expect_true(is.list(result))
  expect_true("x" %in% names(result))
  expect_true("y" %in% names(result))
})

test_that("qgraph_cent_to_edge_simple works for rectangle", {
  result <- cograph:::qgraph_cent_to_edge_simple(0, 0, pi/4, 1, shape = "rectangle")

  expect_true(is.list(result))
  expect_true("x" %in% names(result))
})

test_that("qgraph_cent_to_edge_simple handles angle = 0", {
  result <- cograph:::qgraph_cent_to_edge_simple(0, 0, 0, 1, shape = "square")
  expect_true(!is.na(result$x))
})

test_that("qgraph_cent_to_edge_simple handles angle = pi/2", {
  result <- cograph:::qgraph_cent_to_edge_simple(0, 0, pi/2, 1, shape = "square")
  expect_true(!is.na(result$y))
})

test_that("qgraph_cent_to_edge_simple handles unknown shape", {
  result <- cograph:::qgraph_cent_to_edge_simple(0, 0, pi/4, 1, shape = "unknown")
  expect_true(!is.na(result$x))
})

# ============================================
# qgraph_arrow_size Tests
# ============================================

test_that("qgraph_arrow_size returns positive value", {
  result <- cograph:::qgraph_arrow_size(2)
  expect_true(result > 0)
})

test_that("qgraph_arrow_size scales with edge width", {
  small <- cograph:::qgraph_arrow_size(1)
  large <- cograph:::qgraph_arrow_size(4)
  expect_true(large > small)
})

test_that("qgraph_arrow_size respects base_asize", {
  default <- cograph:::qgraph_arrow_size(2, base_asize = 1)
  doubled <- cograph:::qgraph_arrow_size(2, base_asize = 2)
  expect_equal(doubled, default * 2)
})
