# test-class-layout.R - Tests for CographLayout R6 class
# Covers: R/class-layout.R

# ============================================
# Constructor Tests
# ============================================

test_that("CographLayout can be created with default type", {
  layout <- CographLayout$new()
  expect_true(inherits(layout, "CographLayout"))
  expect_equal(layout$get_type(), "circle")
})

test_that("CographLayout can be created with custom type", {
  layout <- CographLayout$new("spring")
  expect_equal(layout$get_type(), "spring")
})

test_that("CographLayout can be created with parameters", {
  layout <- CographLayout$new("grid", ncol = 3)
  params <- layout$get_params()
  expect_equal(params$ncol, 3)
})

test_that("CographLayout accepts multiple parameters", {
  layout <- CographLayout$new("gephi_fr", niter = 100, gravity = 5.0)
  params <- layout$get_params()
  expect_equal(params$niter, 100)
  expect_equal(params$gravity, 5.0)
})

# ============================================
# compute() Tests
# ============================================

test_that("CographLayout compute() works with CographNetwork", {
  mat <- create_test_matrix(4)
  net <- CographNetwork$new(mat)

  layout <- CographLayout$new("circle")
  coords <- layout$compute(net)

  expect_true(is.data.frame(coords))
  expect_true("x" %in% names(coords))
  expect_true("y" %in% names(coords))
  expect_equal(nrow(coords), 4)
})

test_that("CographLayout compute() errors on non-network", {
  layout <- CographLayout$new("circle")

  expect_error(layout$compute(list(a = 1)), "CographNetwork")
})

test_that("CographLayout compute() works with different layout types", {
  mat <- create_test_matrix(5)
  net <- CographNetwork$new(mat)

  types <- c("circle", "grid", "spring", "star", "random")

  for (type in types) {
    layout <- CographLayout$new(type)
    coords <- layout$compute(net)
    expect_equal(nrow(coords), 5, info = paste("Layout:", type))
  }
})

test_that("CographLayout compute() passes parameters to layout function", {
  mat <- create_test_matrix(6)
  net <- CographNetwork$new(mat)

  layout <- CographLayout$new("grid", ncol = 2)
  coords <- layout$compute(net)

  expect_equal(nrow(coords), 6)
})

test_that("CographLayout compute() accepts additional ... parameters", {
  mat <- create_test_matrix(5)
  net <- CographNetwork$new(mat)

  layout <- CographLayout$new("star")
  coords <- layout$compute(net, center = 3)

  # Center node should be at (0.5, 0.5)
  expect_equal(coords$x[3], 0.5)
  expect_equal(coords$y[3], 0.5)
})

test_that("CographLayout compute() errors on unknown layout type", {
  mat <- create_test_matrix(3)
  net <- CographNetwork$new(mat)

  layout <- CographLayout$new("nonexistent_layout_xyz")

  expect_error(layout$compute(net), "Unknown layout type")
})

# ============================================
# Custom Layout Tests
# ============================================

test_that("CographLayout handles custom layout with matrix coords", {
  mat <- create_test_matrix(3)
  net <- CographNetwork$new(mat)

  custom_coords <- matrix(c(0, 0.5, 1, 0, 1, 0.5), ncol = 2)
  layout <- CographLayout$new("custom", coords = custom_coords)
  coords <- layout$compute(net)

  expect_equal(nrow(coords), 3)
})

test_that("CographLayout handles custom layout with data frame coords", {
  mat <- create_test_matrix(3)
  net <- CographNetwork$new(mat)

  custom_coords <- data.frame(x = c(0, 0.5, 1), y = c(0, 1, 0.5))
  layout <- CographLayout$new("custom", coords = custom_coords)
  coords <- layout$compute(net)

  expect_equal(nrow(coords), 3)
})

test_that("CographLayout errors on custom layout without coords", {
  mat <- create_test_matrix(3)
  net <- CographNetwork$new(mat)

  layout <- CographLayout$new("custom")

  expect_error(layout$compute(net), "coords")
})

# ============================================
# normalize_coords() Tests
# ============================================

test_that("normalize_coords converts matrix to data frame", {
  layout <- CographLayout$new()
  coords <- matrix(c(0, 1, 0, 1), ncol = 2)

  result <- layout$normalize_coords(coords)

  expect_true(is.data.frame(result))
  expect_true("x" %in% names(result))
  expect_true("y" %in% names(result))
})

test_that("normalize_coords adds column names if missing", {
  layout <- CographLayout$new()
  coords <- data.frame(a = c(0, 1), b = c(0, 1))

  result <- layout$normalize_coords(coords)

  expect_true("x" %in% names(result))
  expect_true("y" %in% names(result))
})

test_that("normalize_coords respects padding", {
  layout <- CographLayout$new()
  coords <- data.frame(x = c(0, 1), y = c(0, 1))

  result <- layout$normalize_coords(coords, padding = 0.1)

  # Coords should be within 0-1 range
  expect_true(all(result$x >= 0))
  expect_true(all(result$x <= 1))
  expect_true(all(result$y >= 0))
  expect_true(all(result$y <= 1))
})

test_that("normalize_coords handles single point", {
  layout <- CographLayout$new()
  coords <- data.frame(x = 5, y = 10)

  result <- layout$normalize_coords(coords)

  expect_equal(result$x, 0.5)
  expect_equal(result$y, 0.5)
})

test_that("normalize_coords preserves aspect ratio", {
  layout <- CographLayout$new()
  coords <- data.frame(x = c(0, 2, 4), y = c(0, 1, 0))

  result <- layout$normalize_coords(coords, padding = 0)

  # X range is 4, Y range is 1
  # After uniform scaling, x range should be wider proportionally
  x_range <- diff(range(result$x))
  y_range <- diff(range(result$y))

  # Aspect ratio should be preserved (4:1)
  expect_equal(x_range / y_range, 4, tolerance = 0.01)
})

test_that("normalize_coords handles zero spread", {
  layout <- CographLayout$new()
  coords <- data.frame(x = c(5, 5, 5), y = c(3, 3, 3))

  result <- layout$normalize_coords(coords)

  expect_true(all(result$x == 0.5))
  expect_true(all(result$y == 0.5))
})

# ============================================
# Getter Methods Tests
# ============================================

test_that("get_type returns correct type", {
  layout <- CographLayout$new("spring")
  expect_equal(layout$get_type(), "spring")
})

test_that("get_params returns empty list when no params", {
  layout <- CographLayout$new("circle")
  params <- layout$get_params()
  expect_equal(length(params), 0)
})

test_that("get_params returns all parameters", {
  layout <- CographLayout$new("gephi_fr", niter = 50, gravity = 3.0, area = 10000)
  params <- layout$get_params()

  expect_equal(params$niter, 50)
  expect_equal(params$gravity, 3.0)
  expect_equal(params$area, 10000)
})

# ============================================
# print() Method Tests
# ============================================

test_that("print() shows layout type", {
  layout <- CographLayout$new("spring")
  output <- capture.output(layout$print())

  expect_true(any(grepl("CographLayout", output)))
  expect_true(any(grepl("Type: spring", output)))
})

test_that("print() shows parameters", {
  layout <- CographLayout$new("grid", ncol = 3)
  output <- capture.output(layout$print())

  expect_true(any(grepl("Parameters:", output)))
  expect_true(any(grepl("ncol", output)))
})

test_that("print() truncates long parameter values", {
  layout <- CographLayout$new("custom", coords = matrix(1:20, ncol = 2))
  output <- capture.output(layout$print())

  expect_true(any(grepl("\\.\\.\\.", output)))
})

test_that("print() returns invisible self", {
  layout <- CographLayout$new("circle")
  result <- capture.output(print_result <- layout$print())

  expect_identical(print_result, layout)
})

# ============================================
# Integration with cograph Pipeline
# ============================================

test_that("CographLayout works with cograph objects", {
  mat <- create_test_matrix(4)
  net <- cograph(mat)

  layout <- CographLayout$new("circle")
  coords <- layout$compute(net$network)

  expect_equal(nrow(coords), 4)
})

test_that("CographLayout integrates with sn_layout", {
  mat <- create_test_matrix(4)

  net <- cograph(mat) |>
    sn_layout("circle")

  layout <- net$network$get_layout()

  expect_true(!is.null(layout))
  expect_true("x" %in% names(layout))
  expect_true("y" %in% names(layout))
})

test_that("CographLayout coordinates are within valid range", {
  mat <- create_test_matrix(10)
  net <- CographNetwork$new(mat)

  types <- c("circle", "grid", "spring", "random")

  for (type in types) {
    layout <- CographLayout$new(type)
    coords <- layout$compute(net)

    expect_true(all(coords$x >= 0), info = paste("Layout:", type))
    expect_true(all(coords$x <= 1), info = paste("Layout:", type))
    expect_true(all(coords$y >= 0), info = paste("Layout:", type))
    expect_true(all(coords$y <= 1), info = paste("Layout:", type))
  }
})

# ============================================
# Edge Case Tests
# ============================================

test_that("CographLayout handles single node network", {
  mat <- matrix(0, 1, 1)
  net <- CographNetwork$new(mat)

  layout <- CographLayout$new("circle")
  coords <- layout$compute(net)

  expect_equal(nrow(coords), 1)
  expect_equal(coords$x, 0.5)
  expect_equal(coords$y, 0.5)
})

test_that("CographLayout handles two node network", {
  mat <- matrix(c(0, 1, 1, 0), 2, 2)
  net <- CographNetwork$new(mat)

  layout <- CographLayout$new("circle")
  coords <- layout$compute(net)

  expect_equal(nrow(coords), 2)
})

test_that("CographLayout handles large network", {
  mat <- create_test_matrix(50)
  net <- CographNetwork$new(mat)

  layout <- CographLayout$new("circle")
  coords <- layout$compute(net)

  expect_equal(nrow(coords), 50)
})

test_that("CographLayout handles disconnected nodes", {
  mat <- matrix(0, 5, 5)
  mat[1, 2] <- mat[2, 1] <- 1
  mat[3, 4] <- mat[4, 3] <- 1
  net <- CographNetwork$new(mat)

  layout <- CographLayout$new("spring")
  coords <- layout$compute(net)

  expect_equal(nrow(coords), 5)
})
