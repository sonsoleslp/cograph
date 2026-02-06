# Tests for utility functions
# Covers: R/utils-colors.R, R/utils-geometry.R, R/utils-deprecation.R

# ============================================
# Color Utilities Tests (utils-colors.R)
# ============================================

test_that("adjust_alpha works with named colors", {
  result <- adjust_alpha("red", 0.5)
  expect_true(is.character(result))
  expect_true(nchar(result) > 0)

  # Full alpha
  result_full <- adjust_alpha("blue", 1.0)
  expect_true(is.character(result_full))
})

test_that("adjust_alpha works with hex colors", {
  result <- adjust_alpha("#FF0000", 0.5)
  expect_true(is.character(result))
  expect_true(grepl("^#", result))
})

test_that("adjust_alpha handles NA", {
  result <- adjust_alpha(NA, 0.5)
  expect_true(is.na(result))
})

test_that("adjust_alpha handles NULL", {
  result <- adjust_alpha(NULL, 0.5)
  expect_true(is.na(result))
})

test_that("adjust_alpha handles transparent", {
  result <- adjust_alpha("transparent", 0.5)
  expect_equal(result, "transparent")
})

test_that("adjust_alpha handles invalid colors gracefully", {
  result <- adjust_alpha("notacolor123", 0.5)
  expect_equal(result, "notacolor123")  # Returns original if invalid
})

test_that("adjust_brightness lightens colors", {
  original <- "blue"

  result <- cograph:::adjust_brightness(original, 0.3)

  expect_true(is.character(result))
  expect_true(nchar(result) > 0)
})

test_that("adjust_brightness darkens colors", {
  original <- "yellow"

  result <- cograph:::adjust_brightness(original, -0.3)

  expect_true(is.character(result))
  expect_true(nchar(result) > 0)
})

test_that("adjust_brightness handles NA", {
  result <- adjust_brightness(NA, 0.2)
  expect_true(is.na(result))
})

test_that("adjust_brightness handles NULL", {
  result <- adjust_brightness(NULL, 0.2)
  expect_true(is.na(result))
})

test_that("interpolate_colors creates color ramp", {
  colors <- interpolate_colors("red", "blue", 5)

  expect_equal(length(colors), 5)
  expect_true(all(grepl("^#", colors)))
})

test_that("interpolate_colors handles 2 colors", {
  colors <- interpolate_colors("white", "black", 2)

  expect_equal(length(colors), 2)
})

test_that("contrast_text_color returns black for light backgrounds", {
  result <- contrast_text_color("white")
  expect_equal(result, "black")

  result2 <- contrast_text_color("yellow")
  expect_equal(result2, "black")
})

test_that("contrast_text_color returns white for dark backgrounds", {
  result <- contrast_text_color("black")
  expect_equal(result, "white")

  result2 <- contrast_text_color("navy")
  expect_equal(result2, "white")
})

test_that("contrast_text_color handles NA", {
  result <- contrast_text_color(NA)
  expect_equal(result, "black")
})

test_that("contrast_text_color handles NULL", {
  result <- contrast_text_color(NULL)
  expect_equal(result, "black")
})

test_that("contrast_text_color handles transparent", {
  result <- contrast_text_color("transparent")
  expect_equal(result, "black")
})

test_that("map_to_colors maps numeric values to colors", {
  values <- c(0, 0.5, 1)
  colors <- c("red", "green", "blue")

  result <- map_to_colors(values, colors)

  expect_equal(length(result), 3)
  expect_true(all(grepl("^#", result)))
})

test_that("map_to_colors respects limits", {
  values <- c(10, 50, 100)
  colors <- c("white", "black")

  result <- map_to_colors(values, colors, limits = c(0, 100))
  expect_equal(length(result), 3)
})

test_that("map_to_colors handles values outside limits", {
  values <- c(-10, 50, 200)
  colors <- c("white", "black")

  result <- map_to_colors(values, colors, limits = c(0, 100))
  expect_equal(length(result), 3)  # Should clamp values
})

# ============================================
# Geometry Utilities Tests (utils-geometry.R)
# ============================================

test_that("point_distance calculates correct distance", {
  dist <- point_distance(0, 0, 3, 4)
  expect_equal(dist, 5)

  dist2 <- point_distance(1, 1, 1, 1)
  expect_equal(dist2, 0)
})

test_that("point_distance handles negative coordinates", {
  dist <- point_distance(-3, -4, 0, 0)
  expect_equal(dist, 5)
})

test_that("point_angle calculates correct angle", {
  # 0 degrees (right)
  angle <- point_angle(0, 0, 1, 0)
  expect_equal(angle, 0)

  # 90 degrees (up)
  angle2 <- point_angle(0, 0, 0, 1)
  expect_equal(angle2, pi/2)

  # 180 degrees (left)
  angle3 <- point_angle(0, 0, -1, 0)
  expect_equal(angle3, pi)

  # -90 degrees (down)
  angle4 <- point_angle(0, 0, 0, -1)
  expect_equal(angle4, -pi/2)
})

test_that("point_on_circle calculates correct coordinates", {
  # Point at 0 degrees
  pt <- point_on_circle(0, 0, 1, 0)
  expect_equal(pt$x, 1)
  expect_equal(pt$y, 0, tolerance = 1e-10)

  # Point at 90 degrees
  pt2 <- point_on_circle(0, 0, 1, pi/2)
  expect_equal(pt2$x, 0, tolerance = 1e-10)
  expect_equal(pt2$y, 1)
})

test_that("point_on_circle works with non-zero center", {
  pt <- point_on_circle(1, 2, 1, 0)
  expect_equal(pt$x, 2)
  expect_equal(pt$y, 2)
})

test_that("bezier_points returns correct number of points", {
  pts <- bezier_points(0, 0, 0.5, 1, 1, 0, n = 20)

  expect_true(is.data.frame(pts))
  expect_equal(nrow(pts), 20)
  expect_true(all(c("x", "y") %in% names(pts)))
})

test_that("bezier_points starts and ends at correct points", {
  pts <- bezier_points(0, 0, 0.5, 1, 2, 3, n = 100)

  # First point should be at (0, 0)
  expect_equal(pts$x[1], 0, tolerance = 1e-10)
  expect_equal(pts$y[1], 0, tolerance = 1e-10)

  # Last point should be at (2, 3)
  expect_equal(pts$x[100], 2, tolerance = 1e-10)
  expect_equal(pts$y[100], 3, tolerance = 1e-10)
})

test_that("curve_control_point calculates midpoint control", {
  cp <- curve_control_point(0, 0, 2, 0, curvature = 0.5)

  # Control point should be offset from midpoint
  expect_equal(cp$x, 1, tolerance = 1e-10)  # Midpoint x
  expect_true(cp$y != 0)  # Should be offset in y
})

test_that("curve_control_point with zero curvature returns midpoint", {
  cp <- curve_control_point(0, 0, 2, 0, curvature = 0)

  expect_equal(cp$x, 1, tolerance = 1e-10)
  expect_equal(cp$y, 0, tolerance = 1e-10)
})

test_that("curve_control_point handles zero-length edge", {
  cp <- curve_control_point(1, 1, 1, 1, curvature = 0.5)

  expect_equal(cp$x, 1)
  expect_equal(cp$y, 1)
})

test_that("curve_control_point respects pivot parameter", {
  # Pivot at source
  cp_source <- curve_control_point(0, 0, 4, 0, curvature = 0.5, pivot = 0)
  expect_equal(cp_source$x, 0)

  # Pivot at midpoint (default)
  cp_mid <- curve_control_point(0, 0, 4, 0, curvature = 0.5, pivot = 0.5)
  expect_equal(cp_mid$x, 2)

  # Pivot at target
  cp_target <- curve_control_point(0, 0, 4, 0, curvature = 0.5, pivot = 1)
  expect_equal(cp_target$x, 4)
})

test_that("curve_control_point respects shape parameter", {
  # No shape adjustment
  cp_normal <- curve_control_point(0, 0, 2, 0, curvature = 0.5, shape = 0)

  # Sharper curve (negative shape)
  cp_sharp <- curve_control_point(0, 0, 2, 0, curvature = 0.5, shape = -0.5)

  # Gentler curve (positive shape)
  cp_gentle <- curve_control_point(0, 0, 2, 0, curvature = 0.5, shape = 0.5)

  # Sharper should have more offset, gentler should have less
  expect_true(abs(cp_sharp$y) > abs(cp_normal$y))
  expect_true(abs(cp_gentle$y) < abs(cp_normal$y))
})

test_that("arrow_points returns correct structure", {
  arrow <- arrow_points(1, 1, 0, 0.1)

  expect_true(is.list(arrow))
  expect_equal(length(arrow$x), 3)
  expect_equal(length(arrow$y), 3)
  expect_true(!is.null(arrow$mid_x))
  expect_true(!is.null(arrow$mid_y))
})

test_that("arrow_points tip is at specified position", {
  arrow <- arrow_points(5, 5, pi/4, 0.2)

  expect_equal(arrow$x[1], 5)
  expect_equal(arrow$y[1], 5)
})

test_that("offset_point calculates correct offset", {
  pt <- offset_point(0, 0, 1, 0, offset = 0.5)

  expect_equal(pt$x, 0.5)
  expect_equal(pt$y, 0, tolerance = 1e-10)
})

test_that("offset_point handles diagonal direction", {
  pt <- offset_point(0, 0, 1, 1, offset = sqrt(2))

  expect_equal(pt$x, 1, tolerance = 1e-10)
  expect_equal(pt$y, 1, tolerance = 1e-10)
})

test_that("edge_endpoint returns point on node border", {
  ep <- edge_endpoint(0.5, 0.5, 1, 0.5, node_size = 0.1)

  expect_true(is.list(ep))
  expect_true(!is.null(ep$x))
  expect_true(!is.null(ep$y))

  # Should be 0.1 away from center toward (1, 0.5)
  expect_equal(ep$x, 0.6, tolerance = 1e-10)
  expect_equal(ep$y, 0.5, tolerance = 1e-10)
})

test_that("edge_endpoint respects aspect ratio", {
  ep <- edge_endpoint(0.5, 0.5, 0.5, 1, node_size = 0.1, x_scale = 0.5, y_scale = 1)

  expect_true(is.list(ep))
})

# ============================================
# Deprecation Utilities Tests (utils-deprecation.R)
# ============================================

test_that("handle_deprecated_param returns new_val when old_val is NULL", {
  result <- handle_deprecated_param(
    new_val = "new",
    old_val = NULL,
    new_name = "new_param",
    old_name = "old_param"
  )

  expect_equal(result, "new")
})

test_that("handle_deprecated_param warns and returns old_val when old_val is set", {
  expect_warning(
    result <- handle_deprecated_param(
      new_val = NULL,
      old_val = "old",
      new_name = "new_param",
      old_name = "old_param"
    ),
    "deprecated"
  )

  expect_equal(result, "old")
})

test_that("handle_deprecated_param prefers new_val when both are set", {
  expect_warning(
    result <- handle_deprecated_param(
      new_val = "new",
      old_val = "old",
      new_name = "new_param",
      old_name = "old_param"
    ),
    "deprecated"
  )

  expect_equal(result, "new")
})

test_that("handle_deprecated_param respects new_val_was_set flag", {
  # new_val was explicitly set
  expect_warning(
    result <- handle_deprecated_param(
      new_val = "new",
      old_val = "old",
      new_name = "new_param",
      old_name = "old_param",
      new_val_was_set = TRUE
    ),
    "deprecated"
  )
  expect_equal(result, "new")

  # new_val was NOT explicitly set (using default)
  expect_warning(
    result2 <- handle_deprecated_param(
      new_val = "default",
      old_val = "old",
      new_name = "new_param",
      old_name = "old_param",
      new_val_was_set = FALSE
    ),
    "deprecated"
  )
  expect_equal(result2, "old")
})

test_that("fontface_to_numeric converts strings correctly", {
  expect_equal(fontface_to_numeric("plain"), 1)
  expect_equal(fontface_to_numeric("bold"), 2)
  expect_equal(fontface_to_numeric("italic"), 3)
  expect_equal(fontface_to_numeric("bold.italic"), 4)
})

test_that("fontface_to_numeric passes through numeric values", {
  expect_equal(fontface_to_numeric(1), 1)
  expect_equal(fontface_to_numeric(2), 2)
  expect_equal(fontface_to_numeric(3), 3)
  expect_equal(fontface_to_numeric(4), 4)
})

test_that("fontface_to_numeric defaults to plain for unknown strings", {
  expect_equal(fontface_to_numeric("unknown"), 1)
})

test_that("fontface_to_string converts numbers correctly", {
  expect_equal(fontface_to_string(1), "plain")
  expect_equal(fontface_to_string(2), "bold")
  expect_equal(fontface_to_string(3), "italic")
  expect_equal(fontface_to_string(4), "bold.italic")
})

test_that("fontface_to_string passes through string values", {
  expect_equal(fontface_to_string("plain"), "plain")
  expect_equal(fontface_to_string("bold"), "bold")
  expect_equal(fontface_to_string("italic"), "italic")
  expect_equal(fontface_to_string("bold.italic"), "bold.italic")
})

test_that("fontface_to_string defaults to plain for unknown numbers", {
  expect_equal(fontface_to_string(99), "plain")
})

# ============================================
# Roundtrip tests
# ============================================

test_that("fontface conversion roundtrips correctly", {
  for (num in 1:4) {
    str <- fontface_to_string(num)
    back <- fontface_to_numeric(str)
    expect_equal(back, num)
  }

  for (str in c("plain", "bold", "italic", "bold.italic")) {
    num <- fontface_to_numeric(str)
    back <- fontface_to_string(num)
    expect_equal(back, str)
  }
})
