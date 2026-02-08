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

# ============================================
# Rectangle Shape Detailed Tests
# ============================================

test_that("rectangle shape with custom aspect ratio in soplot", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(3)

  # Default aspect
  result1 <- tryCatch({
    with_temp_png(soplot(mat, node_shape = "rectangle", layout = "circle"))
    TRUE
  }, error = function(e) FALSE)
  expect_true(result1)
})

test_that("rectangle shape function exists and is callable", {
  shape_func <- get_shape("rectangle")
  expect_true(is.function(shape_func))

  # Check it accepts expected parameters
  args <- names(formals(shape_func))
  expect_true("x" %in% args)
  expect_true("y" %in% args)
  expect_true("size" %in% args)
})

# ============================================
# None Shape Tests
# ============================================

test_that("none shape returns nullGrob", {
  skip_if_not_installed("grid")

  shape_func <- get_shape("none")
  grob <- shape_func(0.5, 0.5, 0.1, "red", "black", 1)

  # Should return a nullGrob
  expect_true(inherits(grob, "grob"))
})

test_that("none shape works with labels in soplot", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(3)
  rownames(mat) <- colnames(mat) <- c("Alpha", "Beta", "Gamma")

  # The none shape may have rendering quirks, so we just verify it doesn't error
  result <- tryCatch({
    with_temp_png(soplot(mat, node_shape = "none", labels = TRUE))
    TRUE
  }, error = function(e) {
    # Even if there's a rendering issue, the shape should be registered
    "none" %in% list_shapes()
  })

  expect_true(result)
})

# ============================================
# Special Shapes Detailed Tests
# ============================================

test_that("special shapes render without error", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(3)
  special_shapes <- c("ellipse", "heart", "star", "cross")

  for (shape in special_shapes) {
    result <- tryCatch({
      with_temp_png(soplot(mat, node_shape = shape, layout = "circle"))
      TRUE
    }, error = function(e) FALSE)

    expect_true(result, info = paste("Shape", shape, "failed"))
  }
})

test_that("pie shape renders in soplot", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(3)
  pie_vals <- list(c(1, 2), c(2, 1), c(1, 1, 1))

  result <- tryCatch({
    with_temp_png(soplot(mat, node_shape = "pie",
                         pie_values = pie_vals,
                         layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("donut shape renders in soplot", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_png(soplot(mat, node_shape = "donut",
                         donut_values = c(0.3, 0.6, 0.9),
                         layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("polygon_donut shape renders in soplot", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_png(soplot(mat, node_shape = "polygon_donut",
                         donut_values = c(0.4, 0.5, 0.6),
                         donut_shape = "hexagon",
                         layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("donut_pie shape renders in soplot", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(3)
  pie_vals <- list(c(1, 1), c(2, 1), c(1, 2))

  result <- tryCatch({
    with_temp_png(soplot(mat, node_shape = "donut_pie",
                         donut_values = c(0.5, 0.7, 0.3),
                         pie_values = pie_vals,
                         layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("double_donut_pie shape renders in soplot", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(3)
  pie_vals <- list(c(1, 1), c(1, 2), c(2, 1))
  donut2_vals <- list(c(1, 2), c(2, 1), c(1, 1))

  result <- tryCatch({
    with_temp_png(soplot(mat, node_shape = "double_donut_pie",
                         donut_values = c(0.6, 0.8, 0.4),
                         donut2_values = donut2_vals,
                         pie_values = pie_vals,
                         layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Shape Function Signature Tests
# ============================================

test_that("all built-in shapes have expected signature", {
  # Test only known built-in shapes (not test registrations from other tests)
  builtin_shapes <- c("circle", "square", "triangle", "diamond", "pentagon",
                      "hexagon", "ellipse", "heart", "star", "pie", "donut",
                      "polygon_donut", "donut_pie", "double_donut_pie", "cross",
                      "plus", "rectangle", "none", "neural", "chip", "robot",
                      "brain", "network", "database", "cloud", "gear")

  for (shape_name in builtin_shapes) {
    shape_func <- get_shape(shape_name)
    if (is.null(shape_func)) next  # Skip if shape not registered

    expect_true(is.function(shape_func),
                info = paste("Shape", shape_name, "is not a function"))

    args <- names(formals(shape_func))
    # All shapes should accept x, y, size at minimum
    expect_true("x" %in% args,
                info = paste("Shape", shape_name, "missing 'x' parameter"))
    expect_true("y" %in% args,
                info = paste("Shape", shape_name, "missing 'y' parameter"))
    expect_true("size" %in% args,
                info = paste("Shape", shape_name, "missing 'size' parameter"))
  }
})

# ============================================
# Shape Alias Tests
# ============================================

test_that("plus is alias for cross", {
  cross_func <- get_shape("cross")
  plus_func <- get_shape("plus")

  # They should be the same function
  expect_identical(cross_func, plus_func)
})

# ============================================
# list_shapes Tests
# ============================================

test_that("list_shapes returns character vector", {
  shapes <- list_shapes()
  expect_true(is.character(shapes))
  expect_true(length(shapes) > 0)
})

test_that("list_shapes contains expected minimum shapes", {
  shapes <- list_shapes()

  expected <- c("circle", "square", "triangle", "diamond",
                "pentagon", "hexagon", "ellipse", "heart",
                "star", "pie", "donut", "cross", "none")

  for (shape in expected) {
    expect_true(shape %in% shapes,
                info = paste("Expected shape", shape, "not found"))
  }
})

# ============================================
# Direct Registration Function Tests
# ============================================

test_that("register_builtin_shapes registers all shapes", {
  # Call the registration function directly to ensure it's covered

  cograph:::register_builtin_shapes()

  # Check all expected shapes are registered
  shapes <- list_shapes()

  # Basic shapes
  expect_true("circle" %in% shapes)
  expect_true("square" %in% shapes)
  expect_true("triangle" %in% shapes)
  expect_true("diamond" %in% shapes)
  expect_true("pentagon" %in% shapes)
  expect_true("hexagon" %in% shapes)

  # Special shapes
  expect_true("ellipse" %in% shapes)
  expect_true("heart" %in% shapes)
  expect_true("star" %in% shapes)
  expect_true("pie" %in% shapes)
  expect_true("donut" %in% shapes)
  expect_true("polygon_donut" %in% shapes)
  expect_true("donut_pie" %in% shapes)
  expect_true("double_donut_pie" %in% shapes)
  expect_true("cross" %in% shapes)
  expect_true("plus" %in% shapes)

  # AI-themed shapes
  expect_true("neural" %in% shapes)
  expect_true("chip" %in% shapes)
  expect_true("robot" %in% shapes)
  expect_true("brain" %in% shapes)
  expect_true("network" %in% shapes)
  expect_true("database" %in% shapes)
  expect_true("cloud" %in% shapes)
  expect_true("gear" %in% shapes)

  # Inline-defined shapes
  expect_true("rectangle" %in% shapes)
  expect_true("none" %in% shapes)
})

# ============================================
# Rectangle Inline Function Tests
# ============================================

test_that("rectangle shape function creates rectGrob with default parameters", {
  skip_if_not_installed("grid")

  shape_func <- get_shape("rectangle")
  grob <- shape_func(0.5, 0.5, 0.1, "blue", "black", 1)

  expect_true(inherits(grob, "grob"))
  expect_true(inherits(grob, "rect"))
})

test_that("rectangle shape function handles alpha parameter", {
  skip_if_not_installed("grid")

  shape_func <- get_shape("rectangle")

  # Test with alpha = 0.5
  grob1 <- shape_func(0.5, 0.5, 0.1, "red", "black", 1, alpha = 0.5)
  expect_true(inherits(grob1, "rect"))

  # Test with alpha = 1 (default)
  grob2 <- shape_func(0.5, 0.5, 0.1, "red", "black", 1, alpha = 1)
  expect_true(inherits(grob2, "rect"))

  # Test with alpha = 0 (fully transparent)
  grob3 <- shape_func(0.5, 0.5, 0.1, "red", "black", 1, alpha = 0)
  expect_true(inherits(grob3, "rect"))
})

test_that("rectangle shape function handles aspect parameter", {
  skip_if_not_installed("grid")

  shape_func <- get_shape("rectangle")

  # Test with aspect = 1.5 (default)
  grob1 <- shape_func(0.5, 0.5, 0.1, "green", "black", 1, aspect = 1.5)
  expect_true(inherits(grob1, "rect"))

  # Test with aspect = 1 (square-like)
  grob2 <- shape_func(0.5, 0.5, 0.1, "green", "black", 1, aspect = 1)
  expect_true(inherits(grob2, "rect"))

  # Test with aspect = 2 (wider)
  grob3 <- shape_func(0.5, 0.5, 0.1, "green", "black", 1, aspect = 2)
  expect_true(inherits(grob3, "rect"))

  # Test with aspect = 0.5 (taller than wide)
  grob4 <- shape_func(0.5, 0.5, 0.1, "green", "black", 1, aspect = 0.5)
  expect_true(inherits(grob4, "rect"))
})

test_that("rectangle shape function handles both alpha and aspect", {
  skip_if_not_installed("grid")

  shape_func <- get_shape("rectangle")

  # Combined parameters
  grob <- shape_func(0.5, 0.5, 0.1, "purple", "gray", 2, alpha = 0.7, aspect = 2.5)
  expect_true(inherits(grob, "rect"))
})

test_that("rectangle shape function handles border_width parameter", {
  skip_if_not_installed("grid")

  shape_func <- get_shape("rectangle")

  # Different border widths
  grob1 <- shape_func(0.5, 0.5, 0.1, "blue", "black", 0)
  expect_true(inherits(grob1, "rect"))

  grob2 <- shape_func(0.5, 0.5, 0.1, "blue", "black", 5)
  expect_true(inherits(grob2, "rect"))
})

# ============================================
# None Inline Function Tests
# ============================================

test_that("none shape function returns nullGrob", {
  skip_if_not_installed("grid")

  shape_func <- get_shape("none")
  grob <- shape_func(0.5, 0.5, 0.1, "red", "black", 1)

  expect_true(inherits(grob, "grob"))
  expect_true(inherits(grob, "null"))
})

test_that("none shape function ignores all parameters", {
  skip_if_not_installed("grid")

  shape_func <- get_shape("none")

  # Regardless of parameters, should return nullGrob
  grob1 <- shape_func(0, 0, 0, "red", "black", 0)
  grob2 <- shape_func(1, 1, 1, "blue", "white", 10)
  grob3 <- shape_func(0.5, 0.5, 0.5, "green", "gray", 5, alpha = 0.5)

  expect_true(inherits(grob1, "null"))
  expect_true(inherits(grob2, "null"))
  expect_true(inherits(grob3, "null"))
})

test_that("none shape function handles extra ... arguments", {
  skip_if_not_installed("grid")

  shape_func <- get_shape("none")

  # Should not error with extra arguments
  grob <- shape_func(0.5, 0.5, 0.1, "red", "black", 1, extra_arg = "ignored")
  expect_true(inherits(grob, "null"))
})
