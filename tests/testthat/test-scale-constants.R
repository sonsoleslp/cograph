# test-scale-constants.R - Tests for scaling constants and functions
# Covers: R/scale-constants.R

# Make internal functions available
get_scale_constants <- cograph:::get_scale_constants
compute_adaptive_esize <- cograph:::compute_adaptive_esize
scale_edge_widths <- cograph:::scale_edge_widths
QGRAPH_SCALE <- cograph:::QGRAPH_SCALE
COGRAPH_SCALE <- cograph:::COGRAPH_SCALE
COGRAPH_SCALE_LEGACY <- cograph:::COGRAPH_SCALE_LEGACY

# ============================================
# QGRAPH_SCALE Constants Tests
# ============================================

test_that("QGRAPH_SCALE is a list", {
  expect_true(is.list(QGRAPH_SCALE))
})

test_that("QGRAPH_SCALE contains expected keys", {
  expected_keys <- c("vsize_base", "vsize_decay", "vsize_min", "vsize_factor",
                     "esize_base", "esize_decay", "esize_min", "esize_unweighted",
                     "esize_scale", "arrow_factor")

  for (key in expected_keys) {
    expect_true(key %in% names(QGRAPH_SCALE),
                info = paste("Missing key:", key))
  }
})

test_that("QGRAPH_SCALE values are positive", {
  for (key in names(QGRAPH_SCALE)) {
    value <- QGRAPH_SCALE[[key]]
    if (is.numeric(value)) {
      expect_true(value > 0,
                  info = paste("Expected positive value for", key, "got", value))
    }
  }
})

# ============================================
# COGRAPH_SCALE Constants Tests
# ============================================

test_that("COGRAPH_SCALE is a list", {
  expect_true(is.list(COGRAPH_SCALE))
})

test_that("COGRAPH_SCALE contains expected keys", {
  expected_keys <- c("node_factor", "node_default", "label_default",
                     "edge_base", "edge_scale", "edge_default",
                     "edge_width_range", "arrow_factor", "arrow_default")

  for (key in expected_keys) {
    expect_true(key %in% names(COGRAPH_SCALE),
                info = paste("Missing key:", key))
  }
})

test_that("COGRAPH_SCALE edge_width_range is valid", {
  range <- COGRAPH_SCALE$edge_width_range

  expect_true(is.numeric(range))
  expect_equal(length(range), 2)
  expect_true(range[1] < range[2])
  expect_true(range[1] >= 0)
})

# ============================================
# COGRAPH_SCALE_LEGACY Constants Tests
# ============================================

test_that("COGRAPH_SCALE_LEGACY is a list", {
  expect_true(is.list(COGRAPH_SCALE_LEGACY))
})

test_that("COGRAPH_SCALE_LEGACY differs from COGRAPH_SCALE", {
  # Legacy should have different node_factor
  expect_false(COGRAPH_SCALE_LEGACY$node_factor == COGRAPH_SCALE$node_factor)
})

test_that("COGRAPH_SCALE_LEGACY has coupled labels", {
  expect_true(COGRAPH_SCALE_LEGACY$label_coupled)
  expect_false(COGRAPH_SCALE$label_coupled)
})

# ============================================
# get_scale_constants Tests
# ============================================

test_that("get_scale_constants returns default constants", {
  result <- get_scale_constants("default")

  expect_true(is.list(result))
  expect_equal(result$node_factor, COGRAPH_SCALE$node_factor)
})

test_that("get_scale_constants returns legacy constants", {
  result <- get_scale_constants("legacy")

  expect_true(is.list(result))
  expect_equal(result$node_factor, COGRAPH_SCALE_LEGACY$node_factor)
})

test_that("get_scale_constants with no argument returns default", {
  result <- get_scale_constants()

  expect_true(is.list(result))
  expect_equal(result$node_factor, COGRAPH_SCALE$node_factor)
})

# ============================================
# compute_adaptive_esize Tests
# ============================================

test_that("compute_adaptive_esize returns numeric", {
  result <- compute_adaptive_esize(10)

  expect_true(is.numeric(result))
  expect_equal(length(result), 1)
})

test_that("compute_adaptive_esize decreases with more nodes", {
  small <- compute_adaptive_esize(5)
  medium <- compute_adaptive_esize(50)
  large <- compute_adaptive_esize(200)

  expect_true(small > medium)
  expect_true(medium > large)
})

test_that("compute_adaptive_esize is smaller for directed networks", {
  undirected <- compute_adaptive_esize(50, directed = FALSE)
  directed <- compute_adaptive_esize(50, directed = TRUE)

  expect_true(directed < undirected)
})

test_that("compute_adaptive_esize handles edge cases", {
  # Very small network
  result1 <- compute_adaptive_esize(1)
  expect_true(result1 > 0)

  # Very large network
  result2 <- compute_adaptive_esize(1000)
  expect_true(result2 > 0)
  expect_true(result2 >= 1)  # Should never go below ~1
})

test_that("compute_adaptive_esize with n=0", {
  result <- compute_adaptive_esize(0)
  expect_true(is.numeric(result))
  expect_true(result > 0)
})

# ============================================
# scale_edge_widths Basic Tests
# ============================================

test_that("scale_edge_widths returns numeric vector", {
  weights <- c(0.1, 0.5, 1.0)
  result <- scale_edge_widths(weights)

  expect_true(is.numeric(result))
  expect_equal(length(result), length(weights))
})

test_that("scale_edge_widths handles empty input", {
  result <- scale_edge_widths(numeric(0))

  expect_true(is.numeric(result))
  expect_equal(length(result), 0)
})

test_that("scale_edge_widths output is within range", {
  weights <- c(0.1, 0.3, 0.5, 0.7, 1.0)
  range <- c(0.5, 4)
  result <- scale_edge_widths(weights, range = range)

  expect_true(all(result >= range[1]))
  expect_true(all(result <= range[2]))
})

# ============================================
# scale_edge_widths Mode Tests
# ============================================

test_that("scale_edge_widths linear mode works", {
  weights <- c(0.1, 0.5, 1.0)
  result <- scale_edge_widths(weights, mode = "linear")

  expect_true(is.numeric(result))
})

test_that("scale_edge_widths log mode works", {
  weights <- c(0.1, 1, 10, 100)
  result <- scale_edge_widths(weights, mode = "log")

  expect_true(is.numeric(result))
  # Log mode should compress large values
  linear <- scale_edge_widths(weights, mode = "linear")
  # The range should be less for log
  expect_true(diff(range(result)) <= diff(range(linear)))
})

test_that("scale_edge_widths sqrt mode works", {
  weights <- c(0.1, 0.5, 1.0)
  result <- scale_edge_widths(weights, mode = "sqrt")

  expect_true(is.numeric(result))
})

test_that("scale_edge_widths rank mode works", {
  weights <- c(0.1, 0.5, 1.0, 2.0, 10.0)
  result <- scale_edge_widths(weights, mode = "rank")

  expect_true(is.numeric(result))
})

test_that("scale_edge_widths errors on invalid mode", {
  weights <- c(0.1, 0.5, 1.0)

  expect_error(scale_edge_widths(weights, mode = "invalid"),
               "edge_scale_mode must be one of")
})

# ============================================
# scale_edge_widths Parameter Tests
# ============================================

test_that("scale_edge_widths respects maximum parameter", {
  weights <- c(0.1, 0.5, 1.0)
  result <- scale_edge_widths(weights, maximum = 2)

  # With max=2, weight=1.0 should be at midpoint of range
  expect_true(is.numeric(result))
})

test_that("scale_edge_widths respects minimum threshold", {
  weights <- c(0.1, 0.5, 1.0)
  range <- c(0.5, 4)
  result <- scale_edge_widths(weights, minimum = 0.3, range = range)

  # Weights below 0.3 should get minimum width
  expect_equal(result[1], range[1])
})

test_that("scale_edge_widths respects esize parameter", {
  weights <- c(0.1, 0.5, 1.0)
  result <- scale_edge_widths(weights, esize = 3)

  # Max width should be 3
  expect_equal(max(result), 3)
})

test_that("scale_edge_widths respects custom range", {
  weights <- c(0.1, 0.5, 1.0)
  custom_range <- c(1, 5)
  result <- scale_edge_widths(weights, range = custom_range)

  expect_true(all(result >= custom_range[1]))
  expect_true(all(result <= custom_range[2]))
})

# ============================================
# scale_edge_widths Edge Cases
# ============================================

test_that("scale_edge_widths handles all zeros", {
  weights <- c(0, 0, 0)
  result <- scale_edge_widths(weights)

  expect_true(is.numeric(result))
  expect_false(any(is.na(result)))
})

test_that("scale_edge_widths handles single weight", {
  weights <- c(0.5)
  result <- scale_edge_widths(weights)

  expect_true(is.numeric(result))
  expect_equal(length(result), 1)
})

test_that("scale_edge_widths handles identical weights", {
  weights <- c(0.5, 0.5, 0.5)
  result <- scale_edge_widths(weights)

  expect_true(is.numeric(result))
  # All should be equal
  expect_true(all(result == result[1]))
})

test_that("scale_edge_widths handles negative weights", {
  weights <- c(-1.0, -0.5, 0.5, 1.0)
  result <- scale_edge_widths(weights)

  expect_true(is.numeric(result))
  expect_false(any(is.na(result)))
  # Should use absolute values
  expect_equal(result[1], result[4])  # |-1| == |1|
})

test_that("scale_edge_widths handles NA values", {
  weights <- c(0.1, NA, 1.0)
  range <- c(0.5, 4)
  result <- scale_edge_widths(weights, range = range)

  expect_true(is.numeric(result))
  # NA weights should get minimum width
  expect_equal(result[2], range[1])
})

test_that("scale_edge_widths handles very large weights", {
  weights <- c(1, 100, 10000)
  range <- c(0.5, 4)
  result <- scale_edge_widths(weights, range = range)

  expect_true(all(result >= range[1]))
  expect_true(all(result <= range[2]))
})

test_that("scale_edge_widths handles very small weights", {
  weights <- c(0.0001, 0.001, 0.01)
  result <- scale_edge_widths(weights)

  expect_true(is.numeric(result))
  expect_false(any(is.na(result)))
})

# ============================================
# Integration Tests
# ============================================

test_that("scaling constants are used in splot", {
  mat <- create_test_matrix(5, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(splot(mat, node_size = 8, edge_width_range = c(0.5, 3)))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("adaptive esize works in practice", {
  # Small network should have thicker edges
  mat_small <- create_test_matrix(3)
  # Large network should have thinner edges
  mat_large <- create_test_matrix(30)

  result1 <- tryCatch({
    with_temp_png(splot(mat_small))
    TRUE
  }, error = function(e) FALSE)

  result2 <- tryCatch({
    with_temp_png(splot(mat_large))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result1)
  expect_true(result2)
})
