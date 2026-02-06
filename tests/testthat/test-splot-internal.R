# Exhaustive tests for splot internal paths
# Covers: R/splot-nodes.R, R/splot-edges.R, R/splot-arrows.R, R/splot-labels.R, R/splot-polygons.R

# ============================================
# Helper for splot testing
# ============================================

with_temp_plot <- function(expr) {
  png_file <- tempfile(fileext = ".png")
  png(png_file, width = 400, height = 400)
  on.exit({
    dev.off()
    unlink(png_file)
  })
  force(expr)
}

# ============================================
# Donut Node Tests
# ============================================

test_that("splot renders donut nodes with single value", {
  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_plot(splot(mat, node_shape = "donut", donut_values = c(0.3, 0.6, 0.9)))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders donut with custom colors", {
  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_plot(splot(mat, node_shape = "donut",
                         donut_values = c(0.5, 0.7, 0.4),
                         donut_color = c("red", "blue", "green")))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders donut with background color", {
  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_plot(splot(mat, node_shape = "donut",
                         donut_values = c(0.5, 0.7, 0.4),
                         donut_bg_color = "lightgray"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders donut with inner ratio", {
  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_plot(splot(mat, node_shape = "donut",
                         donut_values = c(0.5, 0.7, 0.4),
                         donut_inner_ratio = 0.3))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders donut with center value shown", {
  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_plot(splot(mat, node_shape = "donut",
                         donut_values = c(0.5, 0.7, 0.4),
                         donut_show_value = TRUE))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders donut with value formatting", {
  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_plot(splot(mat, node_shape = "donut",
                         donut_values = c(0.5, 0.7, 0.4),
                         donut_show_value = TRUE,
                         donut_value_size = 1.5,
                         donut_value_color = "navy"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders donut with border options", {
  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_plot(splot(mat, node_shape = "donut",
                         donut_values = c(0.5, 0.7, 0.4),
                         donut_border_color = "black",
                         donut_border_width = 2))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders donut with line type", {
  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_plot(splot(mat, node_shape = "donut",
                         donut_values = c(0.5, 0.7, 0.4),
                         donut_line_type = "dashed"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Pie Node Tests
# ============================================

test_that("splot renders pie nodes", {
  mat <- create_test_matrix(3)

  pie_vals <- list(c(0.3, 0.7), c(0.5, 0.3, 0.2), c(0.25, 0.25, 0.25, 0.25))

  result <- tryCatch({
    with_temp_plot(splot(mat, node_shape = "pie", pie_values = pie_vals))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders pie with custom colors", {
  mat <- create_test_matrix(3)

  pie_vals <- list(c(0.5, 0.5), c(0.5, 0.5), c(0.5, 0.5))
  pie_cols <- list(c("red", "blue"), c("green", "yellow"), c("purple", "orange"))

  result <- tryCatch({
    with_temp_plot(splot(mat, node_shape = "pie",
                         pie_values = pie_vals,
                         pie_colors = pie_cols))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders pie with border width", {
  mat <- create_test_matrix(3)

  pie_vals <- list(c(0.3, 0.7), c(0.5, 0.5), c(0.4, 0.6))

  result <- tryCatch({
    with_temp_plot(splot(mat, node_shape = "pie",
                         pie_values = pie_vals,
                         pie_border_width = 2))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Polygon Donut Tests
# ============================================

test_that("splot renders polygon_donut nodes", {
  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_plot(splot(mat, node_shape = "polygon_donut",
                         donut_values = c(0.5, 0.7, 0.3),
                         donut_shape = "hexagon"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders polygon_donut with multi-segment values", {
  mat <- create_test_matrix(3)

  donut_vals <- list(c(0.3, 0.4, 0.3), c(0.5, 0.5), c(0.25, 0.25, 0.25, 0.25))

  result <- tryCatch({
    with_temp_plot(splot(mat, node_shape = "polygon_donut",
                         donut_values = donut_vals,
                         donut_shape = "hexagon"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Donut Pie Tests
# ============================================

test_that("splot renders donut_pie nodes", {
  mat <- create_test_matrix(3)

  pie_vals <- list(c(0.5, 0.5), c(0.3, 0.4, 0.3), c(0.25, 0.25, 0.25, 0.25))

  result <- tryCatch({
    with_temp_plot(splot(mat, node_shape = "donut_pie",
                         donut_values = c(0.6, 0.8, 0.4),
                         pie_values = pie_vals))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders donut_pie with custom colors", {
  mat <- create_test_matrix(3)

  pie_vals <- list(c(0.5, 0.5), c(0.5, 0.5), c(0.5, 0.5))

  result <- tryCatch({
    with_temp_plot(splot(mat, node_shape = "donut_pie",
                         donut_values = c(0.6, 0.8, 0.4),
                         donut_color = c("navy", "darkgreen", "darkred"),
                         pie_values = pie_vals,
                         donut_bg_color = "white"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Double Donut Pie Tests
# ============================================

test_that("splot renders double_donut_pie nodes", {
  mat <- create_test_matrix(3)

  pie_vals <- list(c(0.5, 0.5), c(0.3, 0.4, 0.3), c(1.0))
  outer_vals <- list(c(0.3, 0.7), c(0.5, 0.5), c(0.4, 0.6))
  inner_vals <- c(0.6, 0.8, 0.4)

  result <- tryCatch({
    with_temp_plot(splot(mat, node_shape = "double_donut_pie",
                         donut_values = inner_vals,
                         donut2_values = outer_vals,
                         pie_values = pie_vals))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Edge Rendering Tests
# ============================================

test_that("splot renders edges with various line styles", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(splot(mat, edge_style = 2))  # dashed
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders edges with positive/negative colors", {
  mat <- create_test_matrix(4)
  mat[1, 2] <- -0.5
  mat[2, 1] <- -0.5

  result <- tryCatch({
    with_temp_plot(splot(mat, edge_positive_color = "darkgreen",
                         edge_negative_color = "darkred"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders curved edges", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(splot(mat, curves = TRUE, curvature = 0.3))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders bidirectional edges", {
  mat <- matrix(c(0, 1, 0, 2, 0, 1, 0, 0, 0), 3, 3, byrow = TRUE)

  result <- tryCatch({
    with_temp_plot(splot(mat, directed = TRUE, bidirectional = TRUE))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders edges with alpha", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(splot(mat, edge_alpha = 0.5))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Arrow Rendering Tests
# ============================================

test_that("splot renders arrows with custom size", {
  mat <- create_test_matrix(4, symmetric = FALSE)

  result <- tryCatch({
    with_temp_plot(splot(mat, directed = TRUE, arrow_size = 1.5))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders arrows with custom angle", {
  mat <- create_test_matrix(4, symmetric = FALSE)

  result <- tryCatch({
    with_temp_plot(splot(mat, directed = TRUE, arrow_angle = pi/4))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot can hide arrows on directed network", {
  mat <- create_test_matrix(4, symmetric = FALSE)

  result <- tryCatch({
    with_temp_plot(splot(mat, directed = TRUE, show_arrows = FALSE))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Label Rendering Tests
# ============================================

test_that("splot renders labels with custom positioning", {
  mat <- create_test_matrix(4)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  result <- tryCatch({
    with_temp_plot(splot(mat, labels = TRUE, label_size = 1.2, label_color = "navy"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders labels outside nodes", {
  mat <- create_test_matrix(4)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  result <- tryCatch({
    with_temp_plot(splot(mat, labels = TRUE, label_position = "above"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders labels with custom font", {
  mat <- create_test_matrix(4)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  result <- tryCatch({
    with_temp_plot(splot(mat, labels = TRUE, label_fontface = "bold"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders labels with angle", {
  mat <- create_test_matrix(4)
  rownames(mat) <- colnames(mat) <- c("Alpha", "Beta", "Gamma", "Delta")

  result <- tryCatch({
    with_temp_plot(splot(mat, labels = TRUE, label_angle = 45))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Node Shape Variations
# ============================================

test_that("splot renders nodes with varying shapes per node", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(splot(mat, node_shape = c("circle", "square", "triangle", "diamond")))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders nodes with varying colors per node", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(splot(mat, node_fill = c("red", "green", "blue", "yellow")))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders nodes with varying sizes per node", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(splot(mat, node_size = c(0.05, 0.08, 0.1, 0.12)))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders nodes with border variations", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(splot(mat,
                         node_border_color = c("black", "red", "blue", "green"),
                         node_border_width = c(1, 2, 3, 4)))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders nodes with alpha", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(splot(mat, node_alpha = 0.7))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Edge Weight Scaling Tests
# ============================================

test_that("splot renders edges with esize scaling", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    # esize is deprecated, expect the warning
    expect_warning(
      with_temp_plot(splot(mat, esize = 10)),
      "deprecated"
    )
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders edges with minimum threshold", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_plot(splot(mat, minimum = 0.3))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders edges with maximum threshold", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_plot(splot(mat, maximum = 0.8))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders edges with cut threshold", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    # cut is deprecated, expect the warning
    expect_warning(
      with_temp_plot(splot(mat, cut = 0.2)),
      "deprecated"
    )
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders edges with threshold", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_plot(splot(mat, threshold = 0.3))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Layout Parameter Tests
# ============================================

test_that("splot with custom layout coordinates", {
  mat <- create_test_matrix(4)
  layout <- matrix(c(0, 1, 0, 1, 0, 0, 1, 1), ncol = 2)

  result <- tryCatch({
    with_temp_plot(splot(mat, layout = layout))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot with rescale parameter", {
  mat <- create_test_matrix(4)
  layout <- matrix(c(0, 100, 0, 100, 0, 0, 100, 100), ncol = 2)

  result <- tryCatch({
    with_temp_plot(splot(mat, layout = layout, rescale = TRUE))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot with aspect ratio", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(splot(mat, aspect = TRUE))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot with layout scale", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(splot(mat, layout_scale = 0.8))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot with layout margin", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(splot(mat, layout_margin = 0.2))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Special Cases
# ============================================

test_that("splot handles self-loops", {
  mat <- create_test_matrix(4)
  diag(mat) <- c(0.5, 0, 0.3, 0)

  result <- tryCatch({
    with_temp_plot(splot(mat, loop_rotation = pi/4))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot handles groups parameter", {
  mat <- create_test_matrix(6)

  result <- tryCatch({
    with_temp_plot(splot(mat, groups = c(1, 1, 1, 2, 2, 2)))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot handles legend parameter", {
  mat <- create_test_matrix(6)

  result <- tryCatch({
    with_temp_plot(splot(mat, groups = c(1, 1, 1, 2, 2, 2), legend = TRUE))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot handles title parameter", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(splot(mat, title = "Test Network", title_size = 1.5))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot handles margins parameter", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(splot(mat, margins = c(0.2, 0.2, 0.2, 0.2)))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Edge Labels Tests
# ============================================

test_that("splot renders edge labels", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_plot(splot(mat, edge_labels = TRUE))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders edge labels with custom size", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_plot(splot(mat, edge_labels = TRUE, edge_label_size = 1.2))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders edge labels with custom color", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_plot(splot(mat, edge_labels = TRUE, edge_label_color = "darkblue"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders edge labels with background", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_plot(splot(mat, edge_labels = TRUE, edge_label_bg = "lightyellow"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Background and Borders Tests
# ============================================

test_that("splot renders with background color", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(splot(mat, background = "lightgray"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders with transparent background", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(splot(mat, background = "transparent"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Edge Width Range Tests
# ============================================

test_that("splot renders edges with width range", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_plot(splot(mat, edge_width_range = c(0.5, 5)))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Curve Options Tests
# ============================================

test_that("splot with curve scale", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(splot(mat, curves = TRUE, curve_scale = TRUE))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot with curve shape", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(splot(mat, curves = TRUE, curve_shape = 0.5))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot with curve pivot", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(splot(mat, curves = TRUE, curve_pivot = 0.3))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# usePCH Tests
# ============================================

test_that("splot with use_pch option", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(splot(mat, use_pch = TRUE))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Node Names Tests
# ============================================

test_that("splot with custom node names", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(splot(mat, node_names = c("Alpha", "Beta", "Gamma", "Delta")))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})
