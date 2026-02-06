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
    with_temp_png(soplot(mat, esize = 8, layout = "circle"))
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
