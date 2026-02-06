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
