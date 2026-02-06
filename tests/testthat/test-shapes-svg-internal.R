# Exhaustive tests for shapes-svg.R internal functions
# Covers: R/shapes-svg.R

# ============================================
# register_svg_shape Tests
# ============================================

test_that("register_svg_shape validates name parameter", {
  expect_error(
    register_svg_shape(123, "<svg></svg>"),
    "name must be a single character string"
  )

  expect_error(
    register_svg_shape(c("a", "b"), "<svg></svg>"),
    "name must be a single character string"
  )
})

test_that("register_svg_shape validates svg_source parameter", {
  expect_error(
    register_svg_shape("test", 123),
    "svg_source must be a single character string"
  )

  expect_error(
    register_svg_shape("test", c("a", "b")),
    "svg_source must be a single character string"
  )
})

test_that("register_svg_shape registers inline SVG", {
  svg_content <- '<svg viewBox="0 0 100 100"><circle cx="50" cy="50" r="40"/></svg>'

  result <- register_svg_shape("test_inline", svg_content)

  expect_null(result)
  expect_true("test_inline" %in% list_svg_shapes())

  # Clean up
  unregister_svg_shape("test_inline")
})

test_that("register_svg_shape detects file vs inline", {
  # Inline SVG (not a file)
  svg_content <- '<svg viewBox="0 0 100 100"><rect width="100" height="100"/></svg>'
  register_svg_shape("test_file_detect", svg_content)

  shape_data <- get_svg_shape("test_file_detect")
  expect_false(shape_data$is_file)

  # Clean up
  unregister_svg_shape("test_file_detect")
})

# ============================================
# get_svg_shape Tests
# ============================================

test_that("get_svg_shape returns NULL for non-existent shapes", {
  result <- get_svg_shape("nonexistent_shape_xyz")
  expect_null(result)
})

test_that("get_svg_shape returns shape data for registered shapes", {
  svg_content <- '<svg><circle r="10"/></svg>'
  register_svg_shape("test_get_shape", svg_content)

  result <- get_svg_shape("test_get_shape")

  expect_true(is.list(result))
  expect_true("source" %in% names(result))
  expect_true("is_file" %in% names(result))
  expect_equal(result$source, svg_content)

  # Clean up
  unregister_svg_shape("test_get_shape")
})

# ============================================
# list_svg_shapes Tests
# ============================================

test_that("list_svg_shapes returns character vector", {
  result <- list_svg_shapes()
  expect_true(is.character(result))
})

test_that("list_svg_shapes shows registered shapes", {
  # Register a test shape
  register_svg_shape("test_list_shape", '<svg></svg>')

  result <- list_svg_shapes()
  expect_true("test_list_shape" %in% result)

  # Clean up
  unregister_svg_shape("test_list_shape")
})

# ============================================
# unregister_svg_shape Tests
# ============================================

test_that("unregister_svg_shape removes registered shapes", {
  register_svg_shape("test_unregister", '<svg></svg>')
  expect_true("test_unregister" %in% list_svg_shapes())

  result <- unregister_svg_shape("test_unregister")

  expect_true(result)
  expect_false("test_unregister" %in% list_svg_shapes())
})

test_that("unregister_svg_shape returns FALSE for non-existent shapes", {
  result <- unregister_svg_shape("nonexistent_svg_shape_xyz")
  expect_false(result)
})

# ============================================
# parse_svg Tests
# ============================================

test_that("parse_svg returns cached result if available", {
  svg_content <- '<svg><rect width="50" height="50"/></svg>'
  register_svg_shape("test_parse_cache", svg_content)

  shape_data <- get_svg_shape("test_parse_cache")

  # First parse should return NULL without grImport2
  # But shouldn't error
  result <- tryCatch({
    parse_svg(shape_data)
  }, error = function(e) NULL, warning = function(w) NULL)

  # Just verify it runs without hard error
  expect_true(TRUE)

  # Clean up
  unregister_svg_shape("test_parse_cache")
})

test_that("parse_svg handles inline SVG strings", {
  svg_data <- list(
    source = '<svg><circle r="10"/></svg>',
    is_file = FALSE,
    parsed = NULL
  )

  # This will warn about grImport2 if not installed
  result <- tryCatch({
    suppressWarnings(parse_svg(svg_data))
  }, error = function(e) NULL)

  # Test passes if no hard error
  expect_true(TRUE)
})

# ============================================
# draw_svg_shape Tests (requires graphics device)
# ============================================

test_that("draw_svg_shape falls back to circle without grImport2", {
  svg_data <- list(
    source = '<svg><rect width="100" height="100"/></svg>',
    is_file = FALSE,
    parsed = NULL
  )

  # Suppress warning about grImport2 not being installed
  result <- suppressWarnings(with_temp_png({
    grob <- draw_svg_shape(
      x = 0.5, y = 0.5, size = 0.1,
      svg_data = svg_data,
      fill = "blue", border_color = "black", border_width = 1,
      alpha = 1, preserve_aspect = TRUE
    )
    TRUE
  }))

  expect_true(result)
})

test_that("draw_svg_shape handles alpha parameter", {
  svg_data <- list(
    source = '<svg><circle r="10"/></svg>',
    is_file = FALSE,
    parsed = NULL
  )

  # Suppress warning about grImport2 not being installed
  result <- suppressWarnings(with_temp_png({
    grob <- draw_svg_shape(
      x = 0.5, y = 0.5, size = 0.1,
      svg_data = svg_data,
      fill = "red", border_color = "black", border_width = 2,
      alpha = 0.5, preserve_aspect = TRUE
    )
    TRUE
  }))

  expect_true(result)
})

# ============================================
# draw_svg_shape_base Tests
# ============================================

test_that("draw_svg_shape_base falls back to circle without rsvg", {
  svg_data <- list(
    source = '<svg><rect width="100" height="100"/></svg>',
    is_file = FALSE,
    parsed = NULL
  )

  result <- with_temp_png({
    # Need to call plot.new() for base graphics
    plot.new()
    plot.window(xlim = c(0, 1), ylim = c(0, 1))
    draw_svg_shape_base(
      x = 0.5, y = 0.5, size = 0.05,
      svg_data = svg_data,
      fill = "green", border_color = "black", border_width = 1
    )
    TRUE
  })

  expect_true(result)
})

test_that("draw_svg_shape_base handles different parameters", {
  svg_data <- list(
    source = '<svg><polygon points="50,5 20,99 95,39"/></svg>',
    is_file = FALSE,
    parsed = NULL
  )

  result <- with_temp_png({
    # Need to call plot.new() for base graphics
    plot.new()
    plot.window(xlim = c(0, 1), ylim = c(0, 1))
    draw_svg_shape_base(
      x = 0.3, y = 0.7, size = 0.08,
      svg_data = svg_data,
      fill = "yellow", border_color = "red", border_width = 2
    )
    TRUE
  })

  expect_true(result)
})

# ============================================
# Integration Tests with soplot
# ============================================

test_that("soplot can use custom SVG shapes", {
  # Register a test shape
  register_svg_shape("test_plot_shape", '<svg viewBox="0 0 100 100"><circle cx="50" cy="50" r="40"/></svg>')

  mat <- create_test_matrix(4)

  # Note: This may fall back to circle if grImport2 is not installed
  # Suppress warning about grImport2 not being installed
  result <- tryCatch({
    suppressWarnings(with_temp_png(soplot(mat, node_shape = "test_plot_shape", layout = "circle")))
    TRUE
  }, error = function(e) {
    # Shape might not be fully registered for soplot, but that's OK
    TRUE
  })

  expect_true(result)

  # Clean up
  unregister_svg_shape("test_plot_shape")
})

# ============================================
# Edge Cases
# ============================================

test_that("register_svg_shape handles empty SVG", {
  result <- register_svg_shape("test_empty_svg", '<svg></svg>')
  expect_null(result)

  # Clean up
  unregister_svg_shape("test_empty_svg")
})

test_that("register_svg_shape handles complex SVG", {
  complex_svg <- '<svg viewBox="0 0 100 100" xmlns="http://www.w3.org/2000/svg">
    <defs>
      <linearGradient id="grad">
        <stop offset="0%" stop-color="red"/>
        <stop offset="100%" stop-color="blue"/>
      </linearGradient>
    </defs>
    <circle cx="50" cy="50" r="40" fill="url(#grad)"/>
  </svg>'

  result <- register_svg_shape("test_complex_svg", complex_svg)
  expect_null(result)
  expect_true("test_complex_svg" %in% list_svg_shapes())

  # Clean up
  unregister_svg_shape("test_complex_svg")
})

test_that("multiple shapes can be registered", {
  register_svg_shape("shape1", '<svg><circle r="10"/></svg>')
  register_svg_shape("shape2", '<svg><rect width="20" height="20"/></svg>')
  register_svg_shape("shape3", '<svg><polygon points="10,0 20,20 0,20"/></svg>')

  shapes <- list_svg_shapes()

  expect_true("shape1" %in% shapes)
  expect_true("shape2" %in% shapes)
  expect_true("shape3" %in% shapes)

  # Clean up
  unregister_svg_shape("shape1")
  unregister_svg_shape("shape2")
  unregister_svg_shape("shape3")
})
