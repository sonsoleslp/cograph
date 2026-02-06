# Tests for shapes registry functions
# Covers: R/shapes-registry.R

# ============================================
# Built-in Shapes Registration Tests
# ============================================

test_that("basic shapes are registered", {
  shapes <- list_shapes()
  expect_true("circle" %in% shapes)
  expect_true("square" %in% shapes)
  expect_true("triangle" %in% shapes)
  expect_true("diamond" %in% shapes)
  expect_true("pentagon" %in% shapes)
  expect_true("hexagon" %in% shapes)
})

test_that("special shapes are registered", {
  shapes <- list_shapes()
  expect_true("ellipse" %in% shapes)
  expect_true("heart" %in% shapes)
  expect_true("star" %in% shapes)
  expect_true("pie" %in% shapes)
  expect_true("donut" %in% shapes)
  expect_true("polygon_donut" %in% shapes)
  expect_true("donut_pie" %in% shapes)
  expect_true("double_donut_pie" %in% shapes)
  expect_true("cross" %in% shapes)
})

test_that("cross has plus alias", {
  shapes <- list_shapes()
  expect_true("plus" %in% shapes)
})

test_that("AI-themed shapes are registered", {
  shapes <- list_shapes()
  expect_true("neural" %in% shapes)
  expect_true("chip" %in% shapes)
  expect_true("robot" %in% shapes)
  expect_true("brain" %in% shapes)
  expect_true("network" %in% shapes)
  expect_true("database" %in% shapes)
  expect_true("cloud" %in% shapes)
  expect_true("gear" %in% shapes)
})

test_that("rectangle shape is registered", {
  expect_true("rectangle" %in% list_shapes())
})

test_that("none shape is registered", {
  expect_true("none" %in% list_shapes())
})

# ============================================
# Shape Function Tests
# ============================================

test_that("rectangle shape works in plot", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_png(soplot(mat, node_shape = "rectangle", layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("none shape works in plot (labels only)", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(3)
  rownames(mat) <- c("A", "B", "C")

  result <- tryCatch({
    with_temp_png(soplot(mat, node_shape = "none", layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("basic shapes render without error", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(3)
  basic_shapes <- c("circle", "square", "triangle", "diamond", "pentagon", "hexagon")

  for (shape in basic_shapes) {
    result <- tryCatch({
      with_temp_png(soplot(mat, node_shape = shape, layout = "circle"))
      TRUE
    }, error = function(e) FALSE)

    expect_true(result, info = paste("Shape", shape, "failed"))
  }
})

test_that("AI-themed shapes render without error", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(3)
  ai_shapes <- c("neural", "chip", "robot", "brain", "network", "database", "cloud", "gear")

  for (shape in ai_shapes) {
    result <- tryCatch({
      with_temp_png(soplot(mat, node_shape = shape, layout = "circle"))
      TRUE
    }, error = function(e) FALSE)

    expect_true(result, info = paste("Shape", shape, "failed"))
  }
})

# ============================================
# Shape Registry Functions Tests
# ============================================

test_that("get_shape returns function for valid shape", {
  shape_func <- get_shape("circle")
  expect_true(is.function(shape_func))
})

test_that("get_shape returns NULL for invalid shape", {
  result <- get_shape("nonexistent_shape_xyz")
  expect_null(result)
})

test_that("register_shape works", {
  # Create a dummy shape function
  dummy_shape <- function(x, y, size, fill, border_color, border_width, alpha = 1, ...) {
    grid::circleGrob(x = x, y = y, r = size)
  }

  # Register
  register_shape("test_dummy_shape", dummy_shape)
  expect_true("test_dummy_shape" %in% list_shapes())

  # Get it back
  retrieved <- get_shape("test_dummy_shape")
  expect_true(is.function(retrieved))
})
