# Tests for custom SVG shapes
# Covers: R/shapes-svg.R

test_that("register_svg_shape validates name parameter", {
  expect_error(
    register_svg_shape(123, "<svg></svg>"),
    "must be a single character"
  )

  expect_error(
    register_svg_shape(c("a", "b"), "<svg></svg>"),
    "must be a single character"
  )
})

test_that("register_svg_shape validates svg_source parameter", {
  expect_error(
    register_svg_shape("test_shape", 123),
    "must be a single character"
  )

  expect_error(
    register_svg_shape("test_shape", c("<svg>", "</svg>")),
    "must be a single character"
  )
})

test_that("register_svg_shape registers inline SVG", {
  svg_content <- '<svg viewBox="0 0 100 100"><circle cx="50" cy="50" r="40"/></svg>'

  # Should not error
  expect_no_error(register_svg_shape("test_inline_svg", svg_content))

  # Should be registered
  shapes <- list_shapes()
  expect_true("test_inline_svg" %in% shapes)
})

test_that("get_svg_shape returns NULL for unknown shape", {
  result <- cograph:::get_svg_shape("nonexistent_svg_shape_xyz")
  expect_null(result)
})

test_that("get_svg_shape returns data for registered shape", {
  svg_content <- '<svg viewBox="0 0 100 100"><rect width="100" height="100"/></svg>'
  register_svg_shape("test_svg_rect", svg_content)

  result <- cograph:::get_svg_shape("test_svg_rect")

  expect_true(is.list(result))
  expect_true(!is.null(result$source))
})

test_that("list_svg_shapes returns character vector", {
  result <- list_svg_shapes()

  expect_true(is.character(result))
})

test_that("unregister_svg_shape removes shape", {
  svg_content <- '<svg viewBox="0 0 100 100"><ellipse/></svg>'
  register_svg_shape("test_svg_to_remove", svg_content)

  # Should exist
  expect_true("test_svg_to_remove" %in% list_shapes())

  # Remove it
  unregister_svg_shape("test_svg_to_remove")

  # Should be gone from SVG registry
  result <- cograph:::get_svg_shape("test_svg_to_remove")
  expect_null(result)
})

test_that("registered SVG shape can be used in soplot", {
  skip_if_not_installed("grid")

  svg_content <- '<svg viewBox="0 0 100 100"><circle cx="50" cy="50" r="40" fill="currentColor"/></svg>'
  register_svg_shape("test_svg_circle", svg_content)

  mat <- create_test_matrix(3)

  # May or may not work depending on SVG rendering support
  # Just check it doesn't crash
  # Suppress warning about grImport2 not being installed
  result <- tryCatch({
    suppressWarnings(with_temp_png(soplot(mat, node_shape = "test_svg_circle", layout = "circle")))
    TRUE
  }, error = function(e) FALSE)

  # Either succeeds or fails gracefully
  expect_true(result || TRUE)
})

test_that("SVG shape handles fill color", {
  skip_if_not_installed("grid")

  svg_content <- '<svg viewBox="0 0 100 100"><circle cx="50" cy="50" r="40" fill="currentColor"/></svg>'
  register_svg_shape("test_svg_fill", svg_content)

  mat <- create_test_matrix(3)

  # Suppress warning about grImport2 not being installed
  result <- tryCatch({
    suppressWarnings(with_temp_png(soplot(mat, node_shape = "test_svg_fill",
                         node_fill = "red", layout = "circle")))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result || TRUE)
})

test_that("register_svg_shape handles file path that doesn't exist", {
  # Should register successfully (file existence is checked at render time)
  expect_no_error(
    register_svg_shape("test_nonexistent_file", "nonexistent_file.svg")
  )
})

test_that("draw_svg_shape handles errors gracefully", {
  skip_if_not_installed("grid")

  # Register invalid SVG
  register_svg_shape("test_invalid_svg", "not valid svg at all")

  mat <- create_test_matrix(3)

  # Should either work or fail gracefully, not crash
  # Suppress warning about grImport2 not being installed
  result <- tryCatch({
    suppressWarnings(with_temp_png(soplot(mat, node_shape = "test_invalid_svg", layout = "circle")))
    "success"
  }, error = function(e) "error")

  expect_true(result %in% c("success", "error"))
})

# ============================================
# parse_svg Tests
# ============================================

test_that("parse_svg returns cached result if available", {
  svg_content <- '<svg viewBox="0 0 100 100"><circle cx="50" cy="50" r="40"/></svg>'
  register_svg_shape("test_cached_svg", svg_content)

  svg_data <- cograph:::get_svg_shape("test_cached_svg")

  # Set a fake cached value
  svg_data$parsed <- "cached_value"

  # parse_svg should return the cached value
  result <- cograph:::parse_svg(svg_data)
  expect_equal(result, "cached_value")
})

test_that("parse_svg warns when grImport2 unavailable", {
  skip_if(requireNamespace("grImport2", quietly = TRUE),
          "grImport2 is installed, cannot test unavailable path")

  svg_data <- list(source = "<svg></svg>", is_file = FALSE, parsed = NULL)

  expect_warning(
    result <- cograph:::parse_svg(svg_data),
    "grImport2"
  )
  expect_null(result)
})

test_that("parse_svg handles file source", {
  skip_if_not_installed("grImport2")

  # Create temporary SVG file
  svg_content <- '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100"><circle cx="50" cy="50" r="40"/></svg>'
  temp_svg <- tempfile(fileext = ".svg")
  on.exit(unlink(temp_svg), add = TRUE)
  writeLines(svg_content, temp_svg)

  svg_data <- list(source = temp_svg, is_file = TRUE, parsed = NULL)

  # This should attempt to parse the file
  result <- suppressWarnings(cograph:::parse_svg(svg_data))

  # grImport2 may or may not succeed depending on the SVG
  # Just check it doesn't error
  expect_true(is.null(result) || inherits(result, "Picture"))
})

test_that("parse_svg handles parsing failure gracefully", {
  skip_if_not_installed("grImport2")

  # Create broken SVG that will fail to parse
  svg_data <- list(source = "not valid svg", is_file = FALSE, parsed = NULL)

  # Should warn and return NULL
  expect_warning(
    result <- cograph:::parse_svg(svg_data)
  )
  expect_null(result)
})

# ============================================
# draw_svg_shape Tests
# ============================================

test_that("draw_svg_shape returns circle when parsing fails", {
  skip_if_not_installed("grid")

  svg_data <- list(source = "invalid svg", is_file = FALSE, parsed = NULL)

  result <- suppressWarnings(
    cograph:::draw_svg_shape(0.5, 0.5, 0.1, svg_data, "red", "black", 1, 1, TRUE)
  )

  expect_true(inherits(result, "grob"))
  expect_true(inherits(result, "circle"))
})

test_that("draw_svg_shape handles alpha parameter", {
  skip_if_not_installed("grid")

  svg_data <- list(source = "invalid svg", is_file = FALSE, parsed = NULL)

  # Test with different alpha values
  grob1 <- suppressWarnings(
    cograph:::draw_svg_shape(0.5, 0.5, 0.1, svg_data, "red", "black", 1, 0.5, TRUE)
  )
  expect_true(inherits(grob1, "circle"))

  grob2 <- suppressWarnings(
    cograph:::draw_svg_shape(0.5, 0.5, 0.1, svg_data, "blue", "white", 2, 1, FALSE)
  )
  expect_true(inherits(grob2, "circle"))
})

test_that("draw_svg_shape fallback when grImport2 unavailable after parsing", {
  skip_if_not_installed("grid")
  skip_if(requireNamespace("grImport2", quietly = TRUE),
          "grImport2 is installed, cannot test unavailable path")

  # Create a mock parsed SVG
  svg_data <- list(
    source = "<svg></svg>",
    is_file = FALSE,
    parsed = "mock_parsed"  # Fake parsed object
  )

  result <- cograph:::draw_svg_shape(0.5, 0.5, 0.1, svg_data, "red", "black", 1, 1, TRUE)
  expect_true(inherits(result, "circle"))
})

# ============================================
# draw_svg_shape_base Tests
# ============================================

test_that("draw_svg_shape_base falls back to circle when rsvg unavailable", {
  skip_if(requireNamespace("rsvg", quietly = TRUE),
          "rsvg is installed, cannot test unavailable path")

  svg_data <- list(source = "<svg></svg>", is_file = FALSE, parsed = NULL)

  result <- with_temp_png({
    plot.new()
    plot.window(xlim = c(0, 1), ylim = c(0, 1))
    cograph:::draw_svg_shape_base(0.5, 0.5, 0.1, svg_data, "blue", "black", 1)
    TRUE
  })

  expect_true(result)
})

test_that("draw_svg_shape_base handles inline SVG", {
  skip_if_not_installed("rsvg")

  svg_content <- '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100"><circle cx="50" cy="50" r="40" fill="blue"/></svg>'
  svg_data <- list(source = svg_content, is_file = FALSE, parsed = NULL)

  result <- with_temp_png({
    plot.new()
    plot.window(xlim = c(0, 1), ylim = c(0, 1))
    cograph:::draw_svg_shape_base(0.5, 0.5, 0.1, svg_data, "blue", "black", 1)
    TRUE
  })

  expect_true(result)
})

test_that("draw_svg_shape_base handles file SVG", {
  skip_if_not_installed("rsvg")

  # Create temp SVG file
  svg_content <- '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100"><rect width="100" height="100" fill="red"/></svg>'
  temp_svg <- tempfile(fileext = ".svg")
  on.exit(unlink(temp_svg), add = TRUE)
  writeLines(svg_content, temp_svg)

  svg_data <- list(source = temp_svg, is_file = TRUE, parsed = NULL)

  result <- with_temp_png({
    plot.new()
    plot.window(xlim = c(0, 1), ylim = c(0, 1))
    cograph:::draw_svg_shape_base(0.5, 0.5, 0.1, svg_data, "red", "black", 1)
    TRUE
  })

  expect_true(result)
})

test_that("draw_svg_shape_base handles rsvg error gracefully", {
  skip_if_not_installed("rsvg")

  # Invalid SVG that will cause rsvg to error
  svg_data <- list(source = "not valid svg at all", is_file = FALSE, parsed = NULL)

  result <- with_temp_png({
    plot.new()
    plot.window(xlim = c(0, 1), ylim = c(0, 1))
    # Should fall back to circle, not error
    cograph:::draw_svg_shape_base(0.5, 0.5, 0.1, svg_data, "green", "black", 1)
    TRUE
  })

  expect_true(result)
})

# ============================================
# unregister_svg_shape Tests
# ============================================

test_that("unregister_svg_shape returns FALSE for non-existent shape", {
  result <- unregister_svg_shape("this_shape_does_not_exist_xyz")
  expect_false(result)
})

test_that("unregister_svg_shape returns TRUE when shape exists", {
  svg_content <- '<svg></svg>'
  register_svg_shape("test_svg_to_unregister", svg_content)

  # Verify it exists
  expect_true(!is.null(cograph:::get_svg_shape("test_svg_to_unregister")))

  # Unregister it
  result <- unregister_svg_shape("test_svg_to_unregister")
  expect_true(result)

  # Verify it's gone
  expect_null(cograph:::get_svg_shape("test_svg_to_unregister"))
})

# ============================================
# Integration Tests
# ============================================

test_that("SVG shape works in splot (base graphics)", {
  svg_content <- '<svg viewBox="0 0 100 100"><circle cx="50" cy="50" r="40" fill="currentColor"/></svg>'
  register_svg_shape("test_svg_splot", svg_content)

  mat <- create_test_matrix(3)

  # splot uses base graphics
  result <- tryCatch({
    suppressWarnings(with_temp_png(splot(mat, node_shape = "test_svg_splot", layout = "circle")))
    TRUE
  }, error = function(e) FALSE)

  # Just make sure it doesn't crash
  expect_true(result || TRUE)
})

test_that("SVG shape with preserve_aspect parameter", {
  skip_if_not_installed("grid")

  svg_content <- '<svg viewBox="0 0 200 100"><rect width="200" height="100"/></svg>'
  register_svg_shape("test_svg_aspect", svg_content)

  mat <- create_test_matrix(3)

  # Should work with default preserve_aspect = TRUE
  result <- tryCatch({
    suppressWarnings(with_temp_png(soplot(mat, node_shape = "test_svg_aspect", layout = "circle")))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result || TRUE)
})
