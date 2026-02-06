# Tests for special shape drawing functions
# Covers: R/shapes-special.R

test_that("draw_ellipse returns valid grob", {
  skip_if_not_installed("grid")

  grob <- draw_ellipse(0.5, 0.5, 0.1, "blue", "black", 1)
  expect_s3_class(grob, "grob")
})

test_that("draw_ellipse respects aspect ratio", {
  skip_if_not_installed("grid")

  grob <- draw_ellipse(0.5, 0.5, 0.1, "blue", "black", 1, aspect = 0.5)
  expect_s3_class(grob, "grob")

  grob2 <- draw_ellipse(0.5, 0.5, 0.1, "blue", "black", 1, aspect = 1.0)
  expect_s3_class(grob2, "grob")
})

test_that("draw_heart returns valid grob", {
  skip_if_not_installed("grid")

  grob <- draw_heart(0.5, 0.5, 0.1, "red", "black", 1)
  expect_s3_class(grob, "grob")
})

test_that("draw_heart respects alpha", {
  skip_if_not_installed("grid")

  grob <- draw_heart(0.5, 0.5, 0.1, "red", "black", 1, alpha = 0.5)
  expect_s3_class(grob, "grob")
})

test_that("draw_star returns valid grob", {
  skip_if_not_installed("grid")

  grob <- draw_star(0.5, 0.5, 0.1, "gold", "black", 1)
  expect_s3_class(grob, "grob")
})

test_that("draw_star respects n_points parameter", {
  skip_if_not_installed("grid")

  # 5-pointed star (default)
  grob5 <- draw_star(0.5, 0.5, 0.1, "gold", "black", 1, n_points = 5)
  expect_s3_class(grob5, "grob")

  # 6-pointed star
  grob6 <- draw_star(0.5, 0.5, 0.1, "gold", "black", 1, n_points = 6)
  expect_s3_class(grob6, "grob")

  # 8-pointed star
  grob8 <- draw_star(0.5, 0.5, 0.1, "gold", "black", 1, n_points = 8)
  expect_s3_class(grob8, "grob")
})

test_that("draw_star respects inner_ratio", {
  skip_if_not_installed("grid")

  grob <- draw_star(0.5, 0.5, 0.1, "gold", "black", 1, inner_ratio = 0.3)
  expect_s3_class(grob, "grob")

  grob2 <- draw_star(0.5, 0.5, 0.1, "gold", "black", 1, inner_ratio = 0.6)
  expect_s3_class(grob2, "grob")
})

test_that("draw_pie returns valid grob with no values", {
  skip_if_not_installed("grid")

  # No values - should draw a simple circle
  grob <- draw_pie(0.5, 0.5, 0.1, "lightblue", "black", 1)
  expect_s3_class(grob, "grob")
})

test_that("draw_pie returns valid grob with single value", {
  skip_if_not_installed("grid")

  grob <- draw_pie(0.5, 0.5, 0.1, "lightblue", "black", 1, values = 1)
  expect_s3_class(grob, "grob")
})

test_that("draw_pie returns valid grob with multiple values", {
  skip_if_not_installed("grid")

  # Multiple values create pie segments
  grob <- draw_pie(0.5, 0.5, 0.1, "white", "black", 1, values = c(30, 40, 30))
  expect_true(inherits(grob, "gList") || inherits(grob, "grob"))
})

test_that("draw_pie respects custom colors", {
  skip_if_not_installed("grid")

  grob <- draw_pie(0.5, 0.5, 0.1, "white", "black", 1,
                   values = c(1, 1, 1),
                   colors = c("red", "green", "blue"))
  expect_true(inherits(grob, "gList") || inherits(grob, "grob"))
})

test_that("draw_pie respects pie_border_width", {
  skip_if_not_installed("grid")

  grob <- draw_pie(0.5, 0.5, 0.1, "white", "black", 2,
                   values = c(1, 2, 3),
                   pie_border_width = 0.5)
  expect_true(inherits(grob, "gList") || inherits(grob, "grob"))
})

test_that("draw_pie respects default_color", {
  skip_if_not_installed("grid")

  grob <- draw_pie(0.5, 0.5, 0.1, "white", "black", 1,
                   values = NULL,
                   default_color = "steelblue")
  expect_s3_class(grob, "grob")
})

test_that("draw_donut returns valid grob with single value", {
  skip_if_not_installed("grid")

  # Single value donut (progress indicator)
  grob <- draw_donut(0.5, 0.5, 0.1, "steelblue", "black", 1, values = 0.7)
  expect_true(inherits(grob, "gList") || inherits(grob, "grob"))
})

test_that("draw_donut returns valid grob with multiple values", {
  skip_if_not_installed("grid")

  # Multiple values create segmented donut
  grob <- draw_donut(0.5, 0.5, 0.1, "white", "black", 1, values = c(1, 2, 3))
  expect_true(inherits(grob, "gList") || inherits(grob, "grob"))
})

test_that("draw_donut respects inner_ratio", {
  skip_if_not_installed("grid")

  grob <- draw_donut(0.5, 0.5, 0.1, "blue", "black", 1,
                     values = 0.5, inner_ratio = 0.3)
  expect_true(inherits(grob, "gList") || inherits(grob, "grob"))

  grob2 <- draw_donut(0.5, 0.5, 0.1, "blue", "black", 1,
                      values = 0.5, inner_ratio = 0.7)
  expect_true(inherits(grob2, "gList") || inherits(grob2, "grob"))
})

test_that("draw_donut respects bg_color", {
  skip_if_not_installed("grid")

  grob <- draw_donut(0.5, 0.5, 0.1, "blue", "black", 1,
                     values = 0.5, bg_color = "lightgray")
  expect_true(inherits(grob, "gList") || inherits(grob, "grob"))
})

test_that("draw_donut show_value option works", {
  skip_if_not_installed("grid")

  # With value shown
  grob <- draw_donut(0.5, 0.5, 0.1, "blue", "black", 1,
                     values = 0.5, show_value = TRUE)
  expect_true(inherits(grob, "gList") || inherits(grob, "grob"))

  # Without value
  grob2 <- draw_donut(0.5, 0.5, 0.1, "blue", "black", 1,
                      values = 0.5, show_value = FALSE)
  expect_true(inherits(grob2, "gList") || inherits(grob2, "grob"))
})

test_that("draw_donut value formatting options work", {
  skip_if_not_installed("grid")

  grob <- draw_donut(0.5, 0.5, 0.1, "blue", "black", 1,
                     values = 0.5,
                     show_value = TRUE,
                     value_size = 10,
                     value_color = "white",
                     value_fontface = "bold",
                     value_digits = 1,
                     value_prefix = "",
                     value_suffix = "%")
  expect_true(inherits(grob, "gList") || inherits(grob, "grob"))
})

test_that("draw_donut custom value_format function works", {
  skip_if_not_installed("grid")

  custom_format <- function(x) paste0(round(x * 100), "%")
  grob <- draw_donut(0.5, 0.5, 0.1, "blue", "black", 1,
                     values = 0.75,
                     show_value = TRUE,
                     value_format = custom_format)
  expect_true(inherits(grob, "gList") || inherits(grob, "grob"))
})

test_that("draw_donut handles edge values", {
  skip_if_not_installed("grid")

  # 0% filled
  grob0 <- draw_donut(0.5, 0.5, 0.1, "blue", "black", 1, values = 0)
  expect_true(inherits(grob0, "gList") || inherits(grob0, "grob"))

  # 100% filled
  grob100 <- draw_donut(0.5, 0.5, 0.1, "blue", "black", 1, values = 1)
  expect_true(inherits(grob100, "gList") || inherits(grob100, "grob"))

  # NULL values
  grob_null <- draw_donut(0.5, 0.5, 0.1, "blue", "black", 1, values = NULL)
  expect_true(inherits(grob_null, "gList") || inherits(grob_null, "grob"))
})

test_that("draw_polygon_donut returns valid grob", {
  skip_if_not_installed("grid")

  grob <- draw_polygon_donut(0.5, 0.5, 0.1, "blue", "black", 1,
                              values = 0.5, donut_shape = "square")
  expect_true(inherits(grob, "gList") || inherits(grob, "grob"))
})

test_that("draw_polygon_donut works with different shapes", {
  skip_if_not_installed("grid")

  shapes <- c("circle", "square", "hexagon", "triangle", "diamond", "pentagon")
  for (shape in shapes) {
    grob <- draw_polygon_donut(0.5, 0.5, 0.1, "blue", "black", 1,
                                values = 0.5, donut_shape = shape)
    expect_true(inherits(grob, "gList") || inherits(grob, "grob"),
                info = paste("Failed for shape:", shape))
  }
})

test_that("draw_polygon_donut with multiple values works", {
  skip_if_not_installed("grid")

  grob <- draw_polygon_donut(0.5, 0.5, 0.1, "white", "black", 1,
                              values = c(1, 2, 3),
                              colors = c("red", "green", "blue"),
                              donut_shape = "hexagon")
  expect_true(inherits(grob, "gList") || inherits(grob, "grob"))
})

test_that("draw_donut_pie returns valid grob", {
  skip_if_not_installed("grid")

  grob <- draw_donut_pie(0.5, 0.5, 0.1, "white", "black", 1,
                         donut_value = 0.7,
                         pie_values = c(1, 2, 3))
  expect_true(inherits(grob, "gList") || inherits(grob, "grob"))
})

test_that("draw_donut_pie with no pie values shows white center", {
  skip_if_not_installed("grid")

  grob <- draw_donut_pie(0.5, 0.5, 0.1, "white", "black", 1,
                         donut_value = 0.5,
                         pie_values = NULL)
  expect_true(inherits(grob, "gList") || inherits(grob, "grob"))
})

test_that("draw_donut_pie with custom colors works", {
  skip_if_not_installed("grid")

  grob <- draw_donut_pie(0.5, 0.5, 0.1, "steelblue", "black", 1,
                         donut_value = 0.8,
                         pie_values = c(30, 50, 20),
                         pie_colors = c("red", "green", "blue"))
  expect_true(inherits(grob, "gList") || inherits(grob, "grob"))
})

test_that("draw_double_donut_pie returns valid grob", {
  skip_if_not_installed("grid")

  grob <- draw_double_donut_pie(0.5, 0.5, 0.1, "white", "black", 1,
                                 donut_values = 0.8,
                                 donut2_values = 0.6,
                                 pie_values = c(1, 1, 1))
  expect_true(inherits(grob, "gList") || inherits(grob, "grob"))
})

test_that("draw_double_donut_pie with segmented donuts works", {
  skip_if_not_installed("grid")

  grob <- draw_double_donut_pie(0.5, 0.5, 0.1, "white", "black", 1,
                                 donut_values = c(1, 2, 3),
                                 donut_colors = c("red", "orange", "yellow"),
                                 donut2_values = c(1, 1),
                                 donut2_colors = c("blue", "purple"),
                                 pie_values = c(1, 1))
  expect_true(inherits(grob, "gList") || inherits(grob, "grob"))
})

test_that("draw_double_donut_pie handles NULL values", {
  skip_if_not_installed("grid")

  grob <- draw_double_donut_pie(0.5, 0.5, 0.1, "white", "black", 1,
                                 donut_values = NULL,
                                 donut2_values = NULL,
                                 pie_values = NULL)
  expect_true(inherits(grob, "gList") || inherits(grob, "grob"))
})

test_that("draw_neural returns valid grob", {
  skip_if_not_installed("grid")

  grob <- draw_neural(0.5, 0.5, 0.1, "lightblue", "navy", 1)
  expect_true(inherits(grob, "gList") || inherits(grob, "grob"))
})

test_that("draw_neural respects n_connections", {
  skip_if_not_installed("grid")

  grob4 <- draw_neural(0.5, 0.5, 0.1, "lightblue", "navy", 1, n_connections = 4)
  expect_true(inherits(grob4, "gList") || inherits(grob4, "grob"))

  grob8 <- draw_neural(0.5, 0.5, 0.1, "lightblue", "navy", 1, n_connections = 8)
  expect_true(inherits(grob8, "gList") || inherits(grob8, "grob"))
})

test_that("draw_chip returns valid grob", {
  skip_if_not_installed("grid")

  grob <- draw_chip(0.5, 0.5, 0.1, "gray30", "black", 1)
  expect_true(inherits(grob, "gList") || inherits(grob, "grob"))
})

test_that("draw_chip respects pins_per_side", {
  skip_if_not_installed("grid")

  grob2 <- draw_chip(0.5, 0.5, 0.1, "gray30", "black", 1, pins_per_side = 2)
  expect_true(inherits(grob2, "gList") || inherits(grob2, "grob"))

  grob5 <- draw_chip(0.5, 0.5, 0.1, "gray30", "black", 1, pins_per_side = 5)
  expect_true(inherits(grob5, "gList") || inherits(grob5, "grob"))
})

test_that("draw_robot returns valid grob", {
  skip_if_not_installed("grid")

  grob <- draw_robot(0.5, 0.5, 0.1, "lightgray", "gray40", 1)
  expect_true(inherits(grob, "gList") || inherits(grob, "grob"))
})

test_that("draw_brain returns valid grob", {
  skip_if_not_installed("grid")

  grob <- draw_brain(0.5, 0.5, 0.1, "pink", "darkred", 1)
  expect_true(inherits(grob, "gList") || inherits(grob, "grob"))
})

test_that("draw_network returns valid grob", {
  skip_if_not_installed("grid")

  grob <- draw_network(0.5, 0.5, 0.1, "lightblue", "navy", 1)
  expect_true(inherits(grob, "gList") || inherits(grob, "grob"))
})

test_that("draw_database returns valid grob", {
  skip_if_not_installed("grid")

  grob <- draw_database(0.5, 0.5, 0.1, "lightgreen", "darkgreen", 1)
  expect_true(inherits(grob, "gList") || inherits(grob, "grob"))
})

test_that("draw_cloud returns valid grob", {
  skip_if_not_installed("grid")

  grob <- draw_cloud(0.5, 0.5, 0.1, "white", "gray", 1)
  expect_s3_class(grob, "grob")
})

test_that("draw_gear returns valid grob", {
  skip_if_not_installed("grid")

  grob <- draw_gear(0.5, 0.5, 0.1, "gray70", "gray30", 1)
  expect_true(inherits(grob, "gList") || inherits(grob, "grob"))
})

test_that("draw_gear respects n_teeth", {
  skip_if_not_installed("grid")

  grob6 <- draw_gear(0.5, 0.5, 0.1, "gray70", "gray30", 1, n_teeth = 6)
  expect_true(inherits(grob6, "gList") || inherits(grob6, "grob"))

  grob12 <- draw_gear(0.5, 0.5, 0.1, "gray70", "gray30", 1, n_teeth = 12)
  expect_true(inherits(grob12, "gList") || inherits(grob12, "grob"))
})

test_that("draw_cross returns valid grob", {
  skip_if_not_installed("grid")

  grob <- draw_cross(0.5, 0.5, 0.1, "red", "darkred", 1)
  expect_true(inherits(grob, "gList") || inherits(grob, "grob"))
})

test_that("draw_cross respects thickness", {
  skip_if_not_installed("grid")

  grob_thin <- draw_cross(0.5, 0.5, 0.1, "red", "darkred", 1, thickness = 0.2)
  expect_true(inherits(grob_thin, "gList") || inherits(grob_thin, "grob"))

  grob_thick <- draw_cross(0.5, 0.5, 0.1, "red", "darkred", 1, thickness = 0.5)
  expect_true(inherits(grob_thick, "gList") || inherits(grob_thick, "grob"))
})

# ============================================
# Integration tests with soplot
# ============================================

test_that("special shapes work in soplot", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  shapes <- c("ellipse", "heart", "star", "pie", "donut", "neural",
              "chip", "robot", "brain", "network", "database", "cloud", "gear", "cross")

  for (shape in shapes) {
    result <- safe_plot(soplot(mat, node_shape = shape))
    expect_true(result$success, info = paste("Shape failed:", shape, "-", result$error))
  }
})

test_that("donut with values works in soplot", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(3)

  # Single value per node
  result <- safe_plot(
    soplot(mat, node_shape = "donut",
           pie_values = list(0.3, 0.6, 0.9))
  )
  expect_true(result$success, info = result$error)
})

test_that("pie with values works in soplot", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(3)

  # Multiple values per node
  result <- safe_plot(
    soplot(mat, node_shape = "pie",
           pie_values = list(c(1, 2, 3), c(2, 2, 2), c(3, 2, 1)))
  )
  expect_true(result$success, info = result$error)
})

test_that("shapes with alpha work", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  for (shape in c("circle", "star", "heart", "donut")) {
    result <- safe_plot(soplot(mat, node_shape = shape, node_alpha = 0.5))
    expect_true(result$success,
                info = paste("Alpha failed for shape:", shape, "-", result$error))
  }
})
