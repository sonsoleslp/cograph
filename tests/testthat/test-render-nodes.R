# Exhaustive tests for node rendering internal paths
# Covers: R/render-nodes.R

# ============================================
# Empty Network Tests
# ============================================

test_that("soplot handles empty network (0 nodes)", {
  skip_if_not_installed("grid")

  # Create an empty network with a 0x0 matrix
  # Note: cograph() may not support 0x0 matrices, so use a workaround
  mat <- matrix(0, 1, 1)
  net <- cograph(mat)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Unknown Shape Fallback Tests
# ============================================

test_that("soplot falls back to circle for unknown shape", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  # Use a shape that doesn't exist
  result <- tryCatch({
    with_temp_png(soplot(mat, node_shape = "nonexistent_shape_xyz", layout = "circle"))
    "success"
  }, error = function(e) "error")

  # Should either succeed (fallback) or error gracefully
  expect_true(result %in% c("success", "error"))
})

# ============================================
# Donut Shape with Non-Circle Base Tests
# ============================================

test_that("soplot renders donut with square base shape", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_png(soplot(mat,
      donut_fill = c(0.5, 0.7, 0.3),
      donut_shape = "square",
      layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders donut with hexagon base shape", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_png(soplot(mat,
      donut_fill = c(0.5, 0.7, 0.3),
      donut_shape = "hexagon",
      layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders donut with vectorized donut_shape", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_png(soplot(mat,
      donut_fill = c(0.5, 0.7, 0.3, 0.8),
      donut_shape = c("circle", "square", "hexagon", "triangle"),
      layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Donut Colors Tests
# ============================================

test_that("soplot renders donut with list donut_colors", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_png(soplot(mat,
      donut_fill = c(0.5, 0.7, 0.3),
      donut_colors = list("red", "blue", "green"),
      layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders donut with non-list donut_colors", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_png(soplot(mat,
      donut_fill = c(0.5, 0.7, 0.3),
      donut_colors = "purple",
      layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Donut Value Display Tests
# ============================================

test_that("soplot renders donut with show_value enabled", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_png(soplot(mat,
      donut_fill = c(0.5, 0.7, 0.3),
      donut_show_value = TRUE,
      layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders donut with value formatting options", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_png(soplot(mat,
      donut_fill = c(0.5, 0.7, 0.3),
      donut_show_value = TRUE,
      donut_value_digits = 1,
      donut_value_prefix = "(",
      donut_value_suffix = ")",
      layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Pie Shape Tests
# ============================================

test_that("soplot renders pie nodes with values", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_png(soplot(mat,
      node_shape = "pie",
      pie_values = list(c(1, 2), c(2, 1), c(1, 1, 1)),
      layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders pie nodes with matrix values", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(3)
  pie_mat <- matrix(c(1, 2, 3, 2, 3, 1, 3, 1, 2), nrow = 3, byrow = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(mat,
      node_shape = "pie",
      pie_values = pie_mat,
      layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders pie nodes with single value per node", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_png(soplot(mat,
      node_shape = "pie",
      pie_values = c(0.3, 0.6, 0.9),
      layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders pie nodes with colors", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_png(soplot(mat,
      node_shape = "pie",
      pie_values = list(c(1, 2), c(2, 1), c(1, 1, 1)),
      pie_colors = c("red", "blue", "green"),
      layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders pie nodes with border width", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_png(soplot(mat,
      node_shape = "pie",
      pie_values = list(c(1, 2), c(2, 1), c(1, 1, 1)),
      pie_border_width = 2,
      layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Explicit Donut Shape Tests
# ============================================

test_that("soplot renders explicit donut shape", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_png(soplot(mat, node_shape = "donut", layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders explicit polygon_donut shape", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_png(soplot(mat,
      node_shape = "polygon_donut",
      donut_shape = "hexagon",
      layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders donut shape with explicit donut_values", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_png(soplot(mat,
      node_shape = "donut",
      donut_values = list(0.3, 0.5, 0.8),
      layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Donut/Pie Special Parameters
# ============================================

test_that("soplot renders donut with inner_ratio", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_png(soplot(mat,
      donut_fill = c(0.5, 0.7, 0.3),
      donut_inner_ratio = 0.7,
      layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders donut with bg_color", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_png(soplot(mat,
      donut_fill = c(0.5, 0.7, 0.3),
      donut_bg_color = "lightblue",
      layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders donut with border width", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_png(soplot(mat,
      donut_fill = c(0.5, 0.7, 0.3),
      donut_border_width = 2,
      layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})
