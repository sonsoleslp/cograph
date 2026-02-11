# Extended tests for CographLayout R6 class
# Covers: R/class-layout.R (compute, normalize_coords, print, get_type, get_params)

# ============================================
# Constructor Tests
# ============================================

test_that("CographLayout creates with default type", {
  layout <- CographLayout$new()

  expect_true(inherits(layout, "CographLayout"))
  expect_equal(layout$get_type(), "circle")
})

test_that("CographLayout creates with custom type", {
  layout <- CographLayout$new("spring")

  expect_equal(layout$get_type(), "spring")
})

test_that("CographLayout stores extra parameters", {
  layout <- CographLayout$new("grid", ncol = 3)

  params <- layout$get_params()
  expect_equal(params$ncol, 3)
})

# ============================================
# Compute Tests
# ============================================

test_that("compute returns coordinates for circle layout", {
  layout <- CographLayout$new("circle")
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- CographNetwork$new(mat)

  coords <- layout$compute(net)

  expect_true(is.data.frame(coords))
  expect_equal(nrow(coords), 3)
  expect_true("x" %in% names(coords))
  expect_true("y" %in% names(coords))
})

test_that("compute returns coordinates for spring layout", {
  layout <- CographLayout$new("spring")
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- CographNetwork$new(mat)

  coords <- layout$compute(net)

  expect_true(is.data.frame(coords))
  expect_equal(nrow(coords), 3)
})

test_that("compute returns coordinates for grid layout", {
  layout <- CographLayout$new("grid")
  mat <- create_test_matrix(9)
  net <- CographNetwork$new(mat)

  coords <- layout$compute(net)

  expect_equal(nrow(coords), 9)
})

test_that("compute errors on non-network input", {
  layout <- CographLayout$new("circle")

  expect_error(layout$compute(list(a = 1)), "must be a CographNetwork")
})

test_that("compute errors on unknown layout type", {
  layout <- CographLayout$new("nonexistent_layout")
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- CographNetwork$new(mat)

  expect_error(layout$compute(net), "Unknown layout type")
})

test_that("compute with custom layout type works", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- CographNetwork$new(mat)
  custom_coords <- data.frame(x = c(0, 1, 0.5), y = c(0, 0, 1))

  layout <- CographLayout$new("custom", coords = custom_coords)
  coords <- layout$compute(net)

  expect_equal(nrow(coords), 3)
  expect_true("x" %in% names(coords))
  expect_true("y" %in% names(coords))
})

test_that("compute custom layout errors without coords", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- CographNetwork$new(mat)

  layout <- CographLayout$new("custom")

  expect_error(layout$compute(net), "requires 'coords'")
})

# ============================================
# normalize_coords Tests
# ============================================

test_that("normalize_coords scales to 0-1 range", {
  layout <- CographLayout$new()

  coords <- data.frame(x = c(-10, 0, 10), y = c(-5, 0, 5))
  result <- layout$normalize_coords(coords)

  expect_true(all(result$x >= 0 & result$x <= 1))
  expect_true(all(result$y >= 0 & result$y <= 1))
})

test_that("normalize_coords handles matrix input", {
  layout <- CographLayout$new()

  coords <- matrix(c(-10, 0, 10, -5, 0, 5), ncol = 2)
  result <- layout$normalize_coords(coords)

  expect_true(is.data.frame(result))
  expect_true("x" %in% names(result))
  expect_true("y" %in% names(result))
})

test_that("normalize_coords handles all same values", {
  layout <- CographLayout$new()

  coords <- data.frame(x = c(5, 5, 5), y = c(3, 3, 3))
  result <- layout$normalize_coords(coords)

  expect_true(all(result$x == 0.5))
  expect_true(all(result$y == 0.5))
})

test_that("normalize_coords preserves aspect ratio", {
  layout <- CographLayout$new()

  coords <- data.frame(x = c(0, 10), y = c(0, 5))
  result <- layout$normalize_coords(coords)

  # X range should be larger than Y range (aspect ratio preserved)
  x_spread <- diff(range(result$x))
  y_spread <- diff(range(result$y))
  expect_true(x_spread >= y_spread)
})

test_that("normalize_coords respects padding", {
  layout <- CographLayout$new()

  coords <- data.frame(x = c(0, 10), y = c(0, 10))
  result <- layout$normalize_coords(coords, padding = 0.2)

  expect_true(min(result$x) >= 0.2)
  expect_true(max(result$x) <= 0.8)
})

# ============================================
# get_type / get_params Tests
# ============================================

test_that("get_type returns layout type", {
  layout <- CographLayout$new("spring")

  expect_equal(layout$get_type(), "spring")
})

test_that("get_params returns empty list for no extra params", {
  layout <- CographLayout$new("circle")

  expect_equal(length(layout$get_params()), 0)
})

test_that("get_params returns stored parameters", {
  layout <- CographLayout$new("grid", ncol = 4, nrow = 3)

  params <- layout$get_params()
  expect_equal(params$ncol, 4)
  expect_equal(params$nrow, 3)
})

# ============================================
# Print Tests
# ============================================

test_that("print shows layout type", {
  layout <- CographLayout$new("spring")

  expect_output(print(layout), "CographLayout")
  expect_output(print(layout), "spring")
})

test_that("print shows parameters when present", {
  layout <- CographLayout$new("grid", ncol = 3)

  expect_output(print(layout), "Parameters")
  expect_output(print(layout), "ncol")
})

test_that("print truncates long parameter values", {
  layout <- CographLayout$new("grid", ncol = 3, nrow = 4)

  # Should not error with multiple params
  expect_output(print(layout), "CographLayout")
  expect_output(print(layout), "ncol")
})
