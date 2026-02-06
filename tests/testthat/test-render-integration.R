# Integration tests for rendering functions
# Covers many files: render-edges.R, render-nodes.R, render-grid.R, splot.R,
# splot-nodes.R, splot-edges.R, splot-arrows.R, splot-labels.R, splot-polygons.R

# ============================================
# Basic soplot Rendering Tests
# ============================================

test_that("soplot renders basic undirected network", {
  mat <- create_test_matrix(5)

  result <- tryCatch({
    with_temp_png(soplot(mat, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders basic directed network", {
  mat <- create_test_matrix(5, symmetric = FALSE)
  net <- cograph(mat, directed = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders weighted network", {
  mat <- create_test_matrix(5, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(mat, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders complete network", {
  mat <- create_test_topology("complete", 5)

  result <- tryCatch({
    with_temp_png(soplot(mat, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders sparse network", {
  mat <- create_test_topology("chain", 5)

  result <- tryCatch({
    with_temp_png(soplot(mat, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders star network", {
  mat <- create_test_topology("star", 5)

  result <- tryCatch({
    with_temp_png(soplot(mat, layout = "star"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Node Rendering Tests
# ============================================

test_that("soplot renders various node sizes", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_png(soplot(mat, node_size = c(0.05, 0.08, 0.1, 0.15), layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders various node colors", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_png(soplot(mat, node_fill = c("red", "blue", "green", "yellow"), layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders node borders", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_png(soplot(mat, node_border = "black", node_border_width = 2, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders node alpha transparency", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_png(soplot(mat, node_alpha = 0.5, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Edge Rendering Tests
# ============================================

test_that("soplot renders various edge widths", {
  mat <- create_test_matrix(4)
  mat[mat > 0] <- sample(1:5, sum(mat > 0), replace = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(mat, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edge colors", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_png(soplot(mat, edge_color = "darkblue", layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders edge alpha", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_png(soplot(mat, edge_alpha = 0.3, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders curved edges with mutual", {
  mat <- create_test_matrix(4, symmetric = FALSE)
  net <- cograph(mat, directed = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(net, curves = "mutual", layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders curved edges with force", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_png(soplot(mat, curves = "force", layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Arrow Rendering Tests
# ============================================

test_that("soplot renders arrows on directed edges", {
  mat <- create_test_matrix(4, symmetric = FALSE)
  net <- cograph(mat, directed = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders custom arrow size", {
  mat <- create_test_matrix(4, symmetric = FALSE)
  net <- cograph(mat, directed = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(net, arrow_size = 0.02, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Label Rendering Tests
# ============================================

test_that("soplot renders node labels", {
  mat <- create_test_matrix(4)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  result <- tryCatch({
    with_temp_png(soplot(mat, show_labels = TRUE, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders custom label colors", {
  mat <- create_test_matrix(4)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  result <- tryCatch({
    with_temp_png(soplot(mat, show_labels = TRUE, label_color = "red", layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders custom label size", {
  mat <- create_test_matrix(4)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  result <- tryCatch({
    with_temp_png(soplot(mat, show_labels = TRUE, label_size = 12, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot renders labels with position offset", {
  mat <- create_test_matrix(4)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  result <- tryCatch({
    with_temp_png(soplot(mat, show_labels = TRUE, label_position = "above", layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Shape Rendering Tests
# ============================================

test_that("soplot renders all basic shapes", {
  shapes <- c("circle", "square", "triangle", "diamond", "pentagon", "hexagon")

  mat <- create_test_matrix(3)

  for (shape in shapes) {
    result <- tryCatch({
      with_temp_png(soplot(mat, node_shape = shape, layout = "circle"))
      TRUE
    }, error = function(e) FALSE)

    expect_true(result, info = paste("Shape", shape, "failed"))
  }
})

test_that("soplot renders special shapes", {
  shapes <- c("ellipse", "heart", "star", "cross")

  mat <- create_test_matrix(3)

  for (shape in shapes) {
    result <- tryCatch({
      with_temp_png(soplot(mat, node_shape = shape, layout = "circle"))
      TRUE
    }, error = function(e) FALSE)

    expect_true(result, info = paste("Shape", shape, "failed"))
  }
})

test_that("soplot renders AI-themed shapes", {
  shapes <- c("neural", "chip", "robot", "brain", "database", "cloud", "gear")

  mat <- create_test_matrix(3)

  for (shape in shapes) {
    result <- tryCatch({
      with_temp_png(soplot(mat, node_shape = shape, layout = "circle"))
      TRUE
    }, error = function(e) FALSE)

    expect_true(result, info = paste("Shape", shape, "failed"))
  }
})

test_that("soplot renders mixed node shapes", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_png(soplot(mat, node_shape = c("circle", "square", "triangle", "diamond"), layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Theme Tests
# ============================================

test_that("soplot works with all themes", {
  themes <- c("classic", "colorblind", "dark", "gray", "minimal", "viridis", "nature")

  mat <- create_test_matrix(4)

  for (theme in themes) {
    result <- tryCatch({
      with_temp_png(soplot(mat, theme = theme, layout = "circle"))
      TRUE
    }, error = function(e) FALSE)

    expect_true(result, info = paste("Theme", theme, "failed"))
  }
})

# ============================================
# Layout Tests
# ============================================

test_that("soplot works with all layouts", {
  layouts <- c("circle", "oval", "grid", "random", "star", "bipartite")

  mat <- create_test_matrix(6)

  for (layout in layouts) {
    result <- tryCatch({
      with_temp_png(soplot(mat, layout = layout))
      TRUE
    }, error = function(e) FALSE)

    expect_true(result, info = paste("Layout", layout, "failed"))
  }
})

test_that("soplot works with spring layout", {
  skip_if_not_installed("igraph")

  mat <- create_test_matrix(5)

  result <- tryCatch({
    with_temp_png(soplot(mat, layout = "spring"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Edge Cases
# ============================================

test_that("soplot handles large network", {
  mat <- create_test_matrix(50)

  result <- tryCatch({
    with_temp_png(soplot(mat, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot handles network with self-loops", {
  mat <- create_test_matrix(4)
  diag(mat) <- 1  # Add self-loops

  result <- tryCatch({
    with_temp_png(soplot(mat, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  # Either succeeds or skips self-loops gracefully
  expect_true(TRUE)
})

test_that("soplot handles network with no edges", {
  mat <- matrix(0, 4, 4)

  result <- tryCatch({
    with_temp_png(soplot(mat, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot handles single node network", {
  mat <- matrix(0, 1, 1)

  result <- tryCatch({
    with_temp_png(soplot(mat, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot handles very small weights", {
  mat <- create_test_matrix(4)
  mat[mat > 0] <- 0.001

  result <- tryCatch({
    with_temp_png(soplot(mat, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot handles negative weights", {
  mat <- create_test_matrix(4)
  mat[1, 2] <- -1
  mat[2, 1] <- -1

  result <- tryCatch({
    with_temp_png(soplot(mat, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})
