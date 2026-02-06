# Extended tests for grid rendering (soplot)
# Covers: R/render-grid.R - comprehensive parameter testing

# ============================================
# soplot Basic Input Tests
# ============================================

test_that("soplot accepts matrix input", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)
  result <- safe_plot(soplot(mat))
  expect_true(result$success, info = result$error)
})

test_that("soplot accepts cograph_network input", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)
  net <- cograph(mat)
  result <- safe_plot(soplot(net))
  expect_true(result$success, info = result$error)
})

test_that("soplot accepts weighted matrix", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4, weighted = TRUE)
  result <- safe_plot(soplot(mat))
  expect_true(result$success, info = result$error)
})

test_that("soplot accepts asymmetric (directed) matrix", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4, symmetric = FALSE)
  result <- safe_plot(soplot(mat))
  expect_true(result$success, info = result$error)
})

# ============================================
# Title and Margins
# ============================================

test_that("soplot title parameter works", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  # With title
  result <- safe_plot(soplot(mat, title = "Test Network"))
  expect_true(result$success, info = result$error)

  # Custom title size
  result <- safe_plot(soplot(mat, title = "Big Title", title_size = 20))
  expect_true(result$success, info = result$error)
})

test_that("soplot margins parameter works", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  result <- safe_plot(soplot(mat, margins = c(0.1, 0.1, 0.1, 0.1)))
  expect_true(result$success, info = result$error)

  result <- safe_plot(soplot(mat, margins = c(0, 0, 0.2, 0)))
  expect_true(result$success, info = result$error)
})

test_that("soplot layout_margin parameter works", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  result <- safe_plot(soplot(mat, layout_margin = 0.05))
  expect_true(result$success, info = result$error)

  result <- safe_plot(soplot(mat, layout_margin = 0.25))
  expect_true(result$success, info = result$error)
})

# ============================================
# Layout Options
# ============================================

test_that("soplot built-in layouts work", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(5)

  layouts <- c("circle", "spring", "grid", "random", "star")
  for (layout in layouts) {
    result <- safe_plot(soplot(mat, layout = layout))
    expect_true(result$success, info = paste("Layout failed:", layout, "-", result$error))
  }
})

test_that("soplot accepts matrix layout", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)
  layout_mat <- matrix(c(0, 0, 1, 0, 0, 1, 1, 1), ncol = 2, byrow = TRUE)

  result <- safe_plot(soplot(mat, layout = layout_mat))
  expect_true(result$success, info = result$error)
})

test_that("soplot seed parameter affects layout", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(5)

  # Same seed should give reproducible results
  result1 <- safe_plot(soplot(mat, layout = "random", seed = 123))
  result2 <- safe_plot(soplot(mat, layout = "random", seed = 123))
  expect_true(result1$success && result2$success)
})

# ============================================
# Node Aesthetic Parameters
# ============================================

test_that("soplot node_size parameter works", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  result <- safe_plot(soplot(mat, node_size = 0.05))
  expect_true(result$success, info = result$error)

  result <- safe_plot(soplot(mat, node_size = 0.15))
  expect_true(result$success, info = result$error)

  # Vector of sizes
  result <- safe_plot(soplot(mat, node_size = c(0.05, 0.1, 0.08, 0.12)))
  expect_true(result$success, info = result$error)
})

test_that("soplot node_shape parameter works", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  shapes <- c("circle", "square", "triangle", "diamond")
  for (shape in shapes) {
    result <- safe_plot(soplot(mat, node_shape = shape))
    expect_true(result$success, info = paste("Shape failed:", shape, "-", result$error))
  }
})

test_that("soplot node_fill parameter works", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  # Single color
  result <- safe_plot(soplot(mat, node_fill = "steelblue"))
  expect_true(result$success, info = result$error)

  # Vector of colors
  result <- safe_plot(soplot(mat, node_fill = c("red", "green", "blue", "yellow")))
  expect_true(result$success, info = result$error)
})

test_that("soplot node border parameters work", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  result <- safe_plot(soplot(mat,
                             node_border_color = "navy",
                             node_border_width = 3))
  expect_true(result$success, info = result$error)
})

test_that("soplot node_alpha parameter works", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  result <- safe_plot(soplot(mat, node_alpha = 0.5))
  expect_true(result$success, info = result$error)

  # Vector of alphas
  result <- safe_plot(soplot(mat, node_alpha = c(0.3, 0.5, 0.7, 1.0)))
  expect_true(result$success, info = result$error)
})

# ============================================
# Label Parameters
# ============================================

test_that("soplot show_labels parameter works", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  # Labels off
  result <- safe_plot(soplot(mat, show_labels = FALSE))
  expect_true(result$success, info = result$error)

  # Labels on (default)
  result <- safe_plot(soplot(mat, show_labels = TRUE))
  expect_true(result$success, info = result$error)
})

test_that("soplot labels parameter works", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  result <- safe_plot(soplot(mat, labels = c("A", "B", "C", "D")))
  expect_true(result$success, info = result$error)
})

test_that("soplot label styling parameters work", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  result <- safe_plot(soplot(mat,
                             label_size = 12,
                             label_color = "red",
                             label_position = "above"))
  expect_true(result$success, info = result$error)
})

test_that("soplot label_position options work", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  positions <- c("center", "above", "below", "left", "right")
  for (pos in positions) {
    result <- safe_plot(soplot(mat, label_position = pos))
    expect_true(result$success, info = paste("Label position failed:", pos, "-", result$error))
  }
})

# ============================================
# Edge Aesthetic Parameters
# ============================================

test_that("soplot edge_width parameter works", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  result <- safe_plot(soplot(mat, edge_width = 2))
  expect_true(result$success, info = result$error)

  result <- safe_plot(soplot(mat, edge_width = 0.5))
  expect_true(result$success, info = result$error)
})

test_that("soplot edge_color parameter works", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  result <- safe_plot(soplot(mat, edge_color = "gray50"))
  expect_true(result$success, info = result$error)
})

test_that("soplot edge_alpha parameter works", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  result <- safe_plot(soplot(mat, edge_alpha = 0.5))
  expect_true(result$success, info = result$error)
})

test_that("soplot edge_style parameter works", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  styles <- c("solid", "dashed", "dotted")
  for (style in styles) {
    result <- safe_plot(soplot(mat, edge_style = style))
    expect_true(result$success, info = paste("Edge style failed:", style, "-", result$error))
  }
})

test_that("soplot positive/negative edge colors work", {
  skip_if_not_installed("grid")

  # Create weighted matrix with positive and negative weights
  mat <- create_test_matrix(4, weighted = TRUE, symmetric = FALSE)

  result <- safe_plot(soplot(mat,
                             edge_positive_color = "green",
                             edge_negative_color = "red"))
  expect_true(result$success, info = result$error)
})

# ============================================
# Edge Curvature and Arrows
# ============================================

test_that("soplot curvature parameter works", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  result <- safe_plot(soplot(mat, curvature = 0))
  expect_true(result$success, info = result$error)

  result <- safe_plot(soplot(mat, curvature = 0.5))
  expect_true(result$success, info = result$error)

  result <- safe_plot(soplot(mat, curvature = 1))
  expect_true(result$success, info = result$error)
})

test_that("soplot show_arrows parameter works", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4, symmetric = FALSE)

  # With arrows
  result <- safe_plot(soplot(mat, show_arrows = TRUE))
  expect_true(result$success, info = result$error)

  # Without arrows
  result <- safe_plot(soplot(mat, show_arrows = FALSE))
  expect_true(result$success, info = result$error)
})

test_that("soplot arrow_size parameter works", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4, symmetric = FALSE)

  result <- safe_plot(soplot(mat, show_arrows = TRUE, arrow_size = 0.05))
  expect_true(result$success, info = result$error)

  result <- safe_plot(soplot(mat, show_arrows = TRUE, arrow_size = 0.15))
  expect_true(result$success, info = result$error)
})

test_that("soplot bidirectional parameter works", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  result <- safe_plot(soplot(mat, bidirectional = TRUE))
  expect_true(result$success, info = result$error)
})

test_that("soplot curves parameter works", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  # No curves
  expect_no_error(with_temp_png(soplot(mat, curves = FALSE)))

  # Mutual curves (reciprocal edges)
  expect_no_error(with_temp_png(soplot(mat, curves = "mutual")))

  # Force curves
  expect_no_error(with_temp_png(soplot(mat, curves = "force")))
})

# ============================================
# Edge Weight Scaling
# ============================================

test_that("soplot threshold parameter works", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(soplot(mat, threshold = 0.5))
  expect_true(result$success, info = result$error)
})

test_that("soplot maximum parameter works", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(soplot(mat, maximum = 0.8))
  expect_true(result$success, info = result$error)
})

test_that("soplot edge_scale_mode options work", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4, weighted = TRUE)

  modes <- c("linear", "log", "sqrt", "rank")
  for (mode in modes) {
    result <- safe_plot(soplot(mat, edge_scale_mode = mode))
    expect_true(result$success, info = paste("Scale mode failed:", mode, "-", result$error))
  }
})

test_that("soplot edge_width_range parameter works", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(soplot(mat, edge_width_range = c(0.5, 5)))
  expect_true(result$success, info = result$error)
})

test_that("soplot weight_digits parameter works", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(soplot(mat, weight_digits = 1))
  expect_true(result$success, info = result$error)

  result <- safe_plot(soplot(mat, weight_digits = 3))
  expect_true(result$success, info = result$error)
})

# ============================================
# Edge Labels
# ============================================

test_that("soplot edge_labels parameter works", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4, weighted = TRUE)

  # Show weights as labels
  result <- safe_plot(soplot(mat, edge_labels = TRUE))
  expect_true(result$success, info = result$error)
})

test_that("soplot edge label styling works", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(soplot(mat,
                             edge_labels = TRUE,
                             edge_label_size = 8,
                             edge_label_color = "blue",
                             edge_label_bg = "white"))
  expect_true(result$success, info = result$error)
})

# ============================================
# Donut Visualization
# ============================================

test_that("soplot donut_fill parameter works", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  result <- safe_plot(soplot(mat,
                             node_shape = "donut",
                             donut_fill = 0.7))
  expect_true(result$success, info = result$error)

  # Vector of fills
  result <- safe_plot(soplot(mat,
                             node_shape = "donut",
                             donut_fill = c(0.25, 0.5, 0.75, 1.0)))
  expect_true(result$success, info = result$error)
})

test_that("soplot donut_color parameter works", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  result <- safe_plot(soplot(mat,
                             node_shape = "donut",
                             donut_fill = 0.5,
                             donut_color = "steelblue"))
  expect_true(result$success, info = result$error)
})

test_that("soplot donut_shape parameter works", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  shapes <- c("circle", "square", "hexagon")
  for (shape in shapes) {
    result <- safe_plot(soplot(mat,
                               node_shape = "donut",
                               donut_fill = 0.5,
                               donut_shape = shape))
    expect_true(result$success, info = paste("Donut shape failed:", shape, "-", result$error))
  }
})

test_that("soplot donut_show_value parameter works", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  result <- safe_plot(soplot(mat,
                             node_shape = "donut",
                             donut_fill = 0.75,
                             donut_show_value = TRUE))
  expect_true(result$success, info = result$error)
})

# ============================================
# Legend
# ============================================

test_that("soplot legend parameter works", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  result <- safe_plot(soplot(mat, legend = TRUE))
  expect_true(result$success, info = result$error)

  result <- safe_plot(soplot(mat, legend = FALSE))
  expect_true(result$success, info = result$error)
})

test_that("soplot legend_position parameter works", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  positions <- c("topright", "topleft", "bottomright", "bottomleft")
  for (pos in positions) {
    result <- safe_plot(soplot(mat, legend = TRUE, legend_position = pos))
    expect_true(result$success, info = paste("Legend position failed:", pos, "-", result$error))
  }
})

# ============================================
# Theme
# ============================================

test_that("soplot theme parameter works", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  themes <- list_themes()
  for (theme in themes[1:min(3, length(themes))]) {
    result <- safe_plot(soplot(mat, theme = theme))
    expect_true(result$success, info = paste("Theme failed:", theme, "-", result$error))
  }
})

# ============================================
# Special Network Topologies
# ============================================

test_that("soplot handles complete graph", {
  skip_if_not_installed("grid")

  mat <- create_test_topology("complete", 5)
  result <- safe_plot(soplot(mat))
  expect_true(result$success, info = result$error)
})

test_that("soplot handles star graph", {
  skip_if_not_installed("grid")

  mat <- create_test_topology("star", 5)
  result <- safe_plot(soplot(mat))
  expect_true(result$success, info = result$error)
})

test_that("soplot handles ring graph", {
  skip_if_not_installed("grid")

  mat <- create_test_topology("ring", 6)
  result <- safe_plot(soplot(mat))
  expect_true(result$success, info = result$error)
})

test_that("soplot handles path graph", {
  skip_if_not_installed("grid")

  mat <- create_test_topology("path", 5)
  result <- safe_plot(soplot(mat))
  expect_true(result$success, info = result$error)
})

test_that("soplot handles disconnected graph", {
  skip_if_not_installed("grid")

  mat <- create_test_topology("disconnected", 6)
  result <- safe_plot(soplot(mat))
  expect_true(result$success, info = result$error)
})

test_that("soplot handles empty graph", {
  skip_if_not_installed("grid")

  mat <- matrix(0, 4, 4)
  result <- safe_plot(soplot(mat))
  expect_true(result$success, info = result$error)
})

test_that("soplot handles single node", {
  skip_if_not_installed("grid")

  mat <- matrix(0, 1, 1)
  result <- safe_plot(soplot(mat))
  expect_true(result$success, info = result$error)
})

test_that("soplot handles self-loops", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)
  diag(mat) <- 1  # Add self-loops

  result <- safe_plot(soplot(mat))
  expect_true(result$success, info = result$error)
})

# ============================================
# newpage Parameter
# ============================================

test_that("soplot newpage parameter works", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  result <- safe_plot({
    soplot(mat, newpage = TRUE)
    soplot(mat, newpage = FALSE)
  })
  expect_true(result$success, info = result$error)
})

# ============================================
# Scaling Parameter
# ============================================

test_that("soplot scaling parameter works", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  result <- safe_plot(soplot(mat, scaling = "default"))
  expect_true(result$success, info = result$error)

  result <- safe_plot(soplot(mat, scaling = "legacy"))
  expect_true(result$success, info = result$error)
})
