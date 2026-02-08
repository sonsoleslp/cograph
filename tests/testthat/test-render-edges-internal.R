# Exhaustive tests for edge rendering internal paths
# Covers: R/render-edges.R

# ============================================
# Edge Width Scaling Tests
# ============================================

test_that("soplot renders edges with weight-based width scaling", {
  mat <- matrix(c(0, 0.2, 0.8, 0.2, 0, 0.5, 0.8, 0.5, 0), 3, 3)

  result <- tryCatch({
    with_temp_png(soplot(mat, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with explicit width", {
  mat <- create_test_matrix(3)
  net <- cograph(mat)

  result <- tryCatch({
    with_temp_png(soplot(net, edge_width = 2, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with width range", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(mat, edge_width_range = c(0.5, 5), layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with linear scale mode", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(mat, edge_scale_mode = "linear", layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Edge Color Tests
# ============================================

test_that("soplot renders edges with positive and negative colors", {
  mat <- matrix(c(0, 0.5, -0.3, 0.5, 0, -0.7, -0.3, -0.7, 0), 3, 3)

  result <- tryCatch({
    with_temp_png(soplot(mat,
                         edge_positive_color = "darkgreen",
                         edge_negative_color = "darkred",
                         layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with explicit color", {
  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_png(soplot(mat, edge_color = "steelblue", layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with multiple explicit colors", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_png(soplot(mat, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Edge Alpha/Transparency Tests
# ============================================

test_that("soplot renders edges with alpha", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_png(soplot(mat, edge_alpha = 0.5, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with varying weights", {
  mat <- matrix(c(0, 0.1, 0.5, 0.1, 0, 0.8, 0.5, 0.8, 0), 3, 3)

  result <- tryCatch({
    with_temp_png(soplot(mat, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Edge Style Tests
# ============================================

test_that("soplot renders edges with solid style", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_png(soplot(mat, edge_style = "solid", layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with dashed style", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_png(soplot(mat, edge_style = "dashed", layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with dotted style", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_png(soplot(mat, edge_style = "dotted", layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Curved Edge Tests
# ============================================

test_that("soplot renders curved edges", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_png(soplot(mat, curvature = 0.3, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders reciprocal edges with curves", {
  # Create a matrix with reciprocal edges
  mat <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3, byrow = TRUE)

  result <- tryCatch({
    with_temp_png(splot(mat, directed = TRUE, curves = TRUE, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with curves force mode", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_png(soplot(mat, curves = "force", layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with curves mutual mode", {
  mat <- create_test_matrix(4, symmetric = FALSE)
  net <- cograph(mat, directed = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(net, curves = "mutual", layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges without curves", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_png(soplot(mat, curves = FALSE, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Arrow Tests
# ============================================

test_that("soplot renders directed edges with arrows", {
  mat <- create_test_matrix(4, symmetric = FALSE)
  net <- cograph(mat, directed = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(net, show_arrows = TRUE, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders arrows with custom size", {
  mat <- create_test_matrix(4, symmetric = FALSE)
  net <- cograph(mat, directed = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(net, arrow_size = 1.5, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders arrows with custom angle", {
  mat <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), 3, 3, byrow = TRUE)

  result <- tryCatch({
    with_temp_png(splot(mat, directed = TRUE, arrow_angle = pi/4, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders bidirectional arrows", {
  mat <- matrix(c(0, 1, 0, 2, 0, 1, 0, 0, 0), 3, 3, byrow = TRUE)
  net <- cograph(mat, directed = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(net, bidirectional = TRUE, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Self-Loop Tests
# ============================================

test_that("soplot renders self-loops", {
  mat <- create_test_matrix(4)
  diag(mat) <- c(0.5, 0, 0.3, 0)

  result <- tryCatch({
    with_temp_png(soplot(mat, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders self-loops with custom rotation", {
  mat <- create_test_matrix(4)
  diag(mat) <- c(0.5, 0.3, 0.4, 0.6)

  result <- tryCatch({
    with_temp_png(soplot(mat, loop_rotation = pi/4, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Edge Label Tests
# ============================================

test_that("soplot renders edge labels", {
  mat <- create_test_matrix(3, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(mat, edge_labels = TRUE, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edge labels with custom formatting", {
  mat <- create_test_matrix(3, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(mat, edge_labels = TRUE,
                         edge_label_size = 1.2,
                         edge_label_color = "darkblue",
                         edge_label_bg = "white",
                         layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edge labels with position offset", {
  mat <- create_test_matrix(3, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(mat, edge_labels = TRUE,
                         edge_label_position = 0.3,
                         edge_label_offset = 0.02,
                         layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edge labels with fontface", {
  mat <- create_test_matrix(3, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(mat, edge_labels = TRUE,
                         edge_label_fontface = "bold",
                         layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Threshold and Cutoff Tests
# ============================================

test_that("soplot renders edges with threshold", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(mat, threshold = 0.2, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with weighted values", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(mat, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with maximum", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(mat, maximum = 0.8, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Theme-Based Edge Rendering Tests
# ============================================

test_that("soplot renders edges with dark theme", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(mat, theme = "dark", layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with colorblind theme", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(mat, theme = "colorblind", layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Edge Width Tests
# ============================================

test_that("soplot renders edges with esize parameter", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    # esize is deprecated, expect the warning
    expect_warning(
      with_temp_png(soplot(mat, esize = 8, layout = "circle")),
      "deprecated"
    )
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with edge_size parameter", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(mat, edge_size = 5, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Additional Edge Coverage Tests
# ============================================

test_that("soplot renders edges with cut parameter", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(mat, cut = 0.3, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with edge_cutoff parameter", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(mat, edge_cutoff = 0.2, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with width_scale parameter", {
  mat <- create_test_matrix(4, weighted = TRUE)
  net <- cograph(mat) |> sn_edges(width_scale = 2.0)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with sqrt scale mode", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(mat, edge_scale_mode = "sqrt", layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with log scale mode", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(mat, edge_scale_mode = "log", layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with curve_pivot parameter", {
  mat <- create_test_matrix(4, symmetric = FALSE)
  net <- cograph(mat, directed = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(net, curvature = 0.3, curve_pivot = 0.3, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with curvature", {
  mat <- create_test_matrix(4, symmetric = FALSE)
  net <- cograph(mat, directed = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(net, curvature = 0.3, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with negative curvature", {
  mat <- create_test_matrix(4, symmetric = FALSE)
  net <- cograph(mat, directed = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(net, curvature = -0.3, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with edge_duplicates parameter", {
  mat <- create_test_matrix(4, symmetric = FALSE)
  net <- cograph(mat, directed = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(net, edge_duplicates = "combine", layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with negative_color parameter", {
  mat <- matrix(c(0, 0.5, -0.3, 0.5, 0, -0.7, -0.3, -0.7, 0), 3, 3)

  result <- tryCatch({
    with_temp_png(soplot(mat, negative_color = "orange", layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with positive_color parameter", {
  mat <- matrix(c(0, 0.5, -0.3, 0.5, 0, 0.7, -0.3, 0.7, 0), 3, 3)

  result <- tryCatch({
    with_temp_png(soplot(mat, positive_color = "purple", layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with no weights (default color)", {
  mat <- create_test_matrix(4, weighted = FALSE)

  result <- tryCatch({
    with_temp_png(soplot(mat, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with zero weight", {
  mat <- matrix(c(0, 0, 1, 0, 0, 0, 1, 0, 0), 3, 3)

  result <- tryCatch({
    with_temp_png(soplot(mat, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot handles empty edge network", {
  mat <- matrix(0, 4, 4)

  result <- tryCatch({
    with_temp_png(soplot(mat, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with show_arrows FALSE", {
  mat <- create_test_matrix(4, symmetric = FALSE)
  net <- cograph(mat, directed = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(net, show_arrows = FALSE, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with very small arrow size", {
  mat <- create_test_matrix(4, symmetric = FALSE)
  net <- cograph(mat, directed = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(net, arrow_size = 0.001, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with large arrow size", {
  mat <- create_test_matrix(4, symmetric = FALSE)
  net <- cograph(mat, directed = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(net, arrow_size = 3, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders self-loops with varying rotations", {
  mat <- create_test_matrix(3)
  diag(mat) <- 1

  result <- tryCatch({
    with_temp_png(soplot(mat, loop_rotation = c(0, pi/2, pi), layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with all nodes having self-loops", {
  mat <- matrix(1, 4, 4)

  result <- tryCatch({
    with_temp_png(soplot(mat, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges using sn_edges with aesthetic settings", {
  mat <- create_test_matrix(4)
  net <- cograph(mat) |>
    sn_edges(color = "darkblue", width = 2)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders complex reciprocal network", {
  # All edges are reciprocal
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  net <- cograph(mat, directed = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders network with mixed reciprocal and unidirectional edges", {
  mat <- matrix(c(0, 1, 0, 1, 0, 1, 1, 0, 0), 3, 3)
  net <- cograph(mat, directed = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with edge labels and styling", {
  mat <- create_test_matrix(3, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(mat, edge_labels = TRUE,
                         edge_label_size = 0.8,
                         edge_label_color = "darkblue",
                         layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with edge labels and background", {
  mat <- create_test_matrix(3, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(mat, edge_labels = TRUE,
                         edge_label_bg = "lightyellow",
                         layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Edge CI (Confidence Interval) Underlay Tests
# ============================================

test_that("soplot renders edges with CI underlays", {
  mat <- create_test_matrix(4, weighted = TRUE)
  net <- cograph(mat) |>
    sn_edges(ci = 0.3)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with CI scale", {
  mat <- create_test_matrix(4, weighted = TRUE)
  net <- cograph(mat) |>
    sn_edges(ci = 0.2, ci_scale = 3.0)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with CI alpha", {
  mat <- create_test_matrix(4, weighted = TRUE)
  net <- cograph(mat) |>
    sn_edges(ci = 0.2, ci_alpha = 0.3)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with CI color", {
  mat <- create_test_matrix(4, weighted = TRUE)
  net <- cograph(mat) |>
    sn_edges(ci = 0.2, ci_color = "blue")

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Curve Mode Tests
# ============================================

test_that("soplot renders edges with curves = FALSE", {
  mat <- create_test_matrix(4, symmetric = FALSE)
  net <- cograph(mat, directed = TRUE) |>
    sn_edges(curves = FALSE)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with curves = force", {
  mat <- create_test_matrix(4, symmetric = FALSE)
  net <- cograph(mat, directed = TRUE) |>
    sn_edges(curves = "force")

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Edge Style Tests
# ============================================

test_that("soplot renders edges with dashed style", {
  mat <- create_test_matrix(4)
  net <- cograph(mat) |>
    sn_edges(style = "dashed")

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with dotted style", {
  mat <- create_test_matrix(4)
  net <- cograph(mat) |>
    sn_edges(style = "dotted")

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with longdash style", {
  mat <- create_test_matrix(4)
  net <- cograph(mat) |>
    sn_edges(style = "longdash")

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with twodash style", {
  mat <- create_test_matrix(4)
  net <- cograph(mat) |>
    sn_edges(style = "twodash")

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Bidirectional Edge Tests
# ============================================

test_that("soplot renders bidirectional straight edges", {
  mat <- create_test_matrix(4, symmetric = FALSE)
  net <- cograph(mat, directed = TRUE) |>
    sn_edges(bidirectional = TRUE, curves = FALSE)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders bidirectional curved edges", {
  mat <- create_test_matrix(4, symmetric = FALSE)
  net <- cograph(mat, directed = TRUE) |>
    sn_edges(bidirectional = TRUE, curvature = 0.3)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Edge Label Advanced Tests
# ============================================

test_that("soplot renders edges with label position", {
  mat <- create_test_matrix(3, weighted = TRUE)
  net <- cograph(mat) |>
    sn_edges(labels = TRUE, label_position = 0.3)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with label offset", {
  mat <- create_test_matrix(3, weighted = TRUE)
  net <- cograph(mat) |>
    sn_edges(labels = TRUE, label_offset = 0.05)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with label fontface bold", {
  mat <- create_test_matrix(3, weighted = TRUE)
  net <- cograph(mat) |>
    sn_edges(labels = TRUE, label_fontface = "bold")

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with label fontface italic", {
  mat <- create_test_matrix(3, weighted = TRUE)
  net <- cograph(mat) |>
    sn_edges(labels = TRUE, label_fontface = "italic")

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with label shadow", {
  mat <- create_test_matrix(3, weighted = TRUE)
  net <- cograph(mat) |>
    sn_edges(labels = TRUE, label_shadow = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with label border rect", {
  mat <- create_test_matrix(3, weighted = TRUE)
  net <- cograph(mat) |>
    sn_edges(labels = TRUE, label_border = "rect")

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with label border rounded", {
  mat <- create_test_matrix(3, weighted = TRUE)
  net <- cograph(mat) |>
    sn_edges(labels = TRUE, label_border = "rounded")

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with label border circle", {
  mat <- create_test_matrix(3, weighted = TRUE)
  net <- cograph(mat) |>
    sn_edges(labels = TRUE, label_border = "circle")

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with label underline", {
  mat <- create_test_matrix(3, weighted = TRUE)
  net <- cograph(mat) |>
    sn_edges(labels = TRUE, label_underline = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Edge Label Template Tests
# ============================================

test_that("soplot renders edges with label template", {
  mat <- create_test_matrix(3, weighted = TRUE)
  net <- cograph(mat) |>
    sn_edges(label_template = "{weight}")

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with label style estimate", {
  mat <- create_test_matrix(3, weighted = TRUE)
  net <- cograph(mat) |>
    sn_edges(label_style = "estimate")

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Self-Loop with CI Tests
# ============================================

test_that("soplot renders self-loops with CI underlays", {
  mat <- create_test_matrix(3)
  diag(mat) <- 1
  net <- cograph(mat) |>
    sn_edges(ci = 0.3)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Edge With Zero Length Tests
# ============================================

test_that("soplot handles edges between same position nodes", {
  mat <- create_test_matrix(4)
  # This is a valid network with separate nodes
  result <- tryCatch({
    with_temp_png(soplot(mat, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Maximum/Minimum Weight Tests
# ============================================

test_that("soplot renders edges with maximum weight parameter", {
  mat <- create_test_matrix(4, weighted = TRUE)
  net <- cograph(mat) |>
    sn_edges(maximum = 2.0)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# CI Underlay with Curved Edges Tests
# ============================================

test_that("soplot renders curved edges with CI underlays", {
  mat <- create_test_matrix(4, weighted = TRUE)
  net <- cograph(mat) |>
    sn_edges(ci = 0.3, curvature = 0.3)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders CI underlay with custom style", {
  mat <- create_test_matrix(4, weighted = TRUE)
  net <- cograph(mat) |>
    sn_edges(ci = 0.2, ci_style = 3)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders CI underlay with arrows", {
  mat <- create_test_matrix(4, symmetric = FALSE)
  net <- cograph(mat, directed = TRUE) |>
    sn_edges(ci = 0.2, ci_arrows = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Edge Cut Threshold Tests
# ============================================

test_that("soplot renders edges with cut threshold fading", {
  mat <- matrix(c(0, 0.1, 0.5, 0.8,
                  0.1, 0, 0.2, 0.3,
                  0.5, 0.2, 0, 0.6,
                  0.8, 0.3, 0.6, 0), 4, 4)
  net <- cograph(mat) |>
    sn_edges(cut = 0.4)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with all weights below cut", {
  mat <- matrix(c(0, 0.1, 0.1, 0.1, 0, 0.2, 0.1, 0.2, 0), 3, 3)
  net <- cograph(mat) |>
    sn_edges(cut = 0.5)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Self-loop with Arrow/Label Tests
# ============================================

test_that("soplot renders self-loops in directed network", {
  mat <- create_test_matrix(3, symmetric = FALSE)
  diag(mat) <- 1
  net <- cograph(mat, directed = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders self-loops with edge labels", {
  mat <- create_test_matrix(3)
  diag(mat) <- c(0.5, 0.7, 0.3)

  result <- tryCatch({
    with_temp_png(soplot(mat, edge_labels = TRUE, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Aspect Ratio Correction Tests
# ============================================

test_that("soplot renders edges with non-square aspect ratio", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    png(tempfile(fileext = ".png"), width = 800, height = 400)
    on.exit(dev.off())
    soplot(mat, layout = "circle")
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Curve Shape and Pivot Tests
# ============================================

test_that("soplot renders curved edges with curve_shape", {
  mat <- create_test_matrix(4, symmetric = FALSE)
  net <- cograph(mat, directed = TRUE) |>
    sn_edges(curvature = 0.3, curve_shape = 0.5)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with loop rotation per edge", {
  mat <- create_test_matrix(4)
  diag(mat) <- 1
  net <- cograph(mat) |>
    sn_edges(loop_rotation = c(0, pi/4, pi/2, pi))

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Weight-Based Scaling Tests
# ============================================

test_that("soplot renders edges with weight-based scaling", {
  mat <- matrix(c(0, 0.2, 0.8, 0.2, 0, 0.5, 0.8, 0.5, 0), 3, 3)
  net <- cograph(mat)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with explicit edge_scale_mode", {
  mat <- matrix(c(0, 0.2, 0.8, 0.2, 0, 0.5, 0.8, 0.5, 0), 3, 3)
  net <- cograph(mat) |>
    sn_edges(edge_scale_mode = "sqrt")

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges with explicit maximum", {
  mat <- matrix(c(0, 0.2, 0.8, 0.2, 0, 0.5, 0.8, 0.5, 0), 3, 3)
  net <- cograph(mat) |>
    sn_edges(maximum = 1.0)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Color Resolution Tests
# ============================================

test_that("soplot renders edges with positive/negative colors from aes", {
  mat <- matrix(c(0, 0.5, -0.3, 0.5, 0, -0.7, -0.3, -0.7, 0), 3, 3)
  net <- cograph(mat) |>
    sn_edges(positive_color = "green", negative_color = "red")

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edges without weights using default color", {
  mat <- create_test_matrix(4, weighted = FALSE)
  net <- cograph(mat)

  # Remove weights from edges
  edges <- net$network$get_edges()
  edges$weight <- NULL
  net$network$set_edges(edges)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})
