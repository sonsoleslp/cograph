# Extended tests for splot rendering (base R)
# Covers gaps in: R/splot-nodes.R, R/splot-edges.R, R/render-nodes.R, R/render-edges.R

# ============================================
# splot with Various Node Shapes (splot-nodes.R)
# ============================================

test_that("splot renders circle nodes", {
  mat <- create_test_matrix(4, weighted = TRUE)
  expect_splot_works(mat, node_shape = "circle", layout = "circle")
})

test_that("splot renders square nodes", {
  mat <- create_test_matrix(4, weighted = TRUE)
  expect_splot_works(mat, node_shape = "square", layout = "circle")
})

test_that("splot renders triangle nodes", {
  mat <- create_test_matrix(4, weighted = TRUE)
  expect_splot_works(mat, node_shape = "triangle", layout = "circle")
})

test_that("splot renders diamond nodes", {
  mat <- create_test_matrix(4, weighted = TRUE)
  expect_splot_works(mat, node_shape = "diamond", layout = "circle")
})

test_that("splot renders pentagon nodes", {
  mat <- create_test_matrix(4, weighted = TRUE)
  expect_splot_works(mat, node_shape = "pentagon", layout = "circle")
})

test_that("splot renders hexagon nodes", {
  mat <- create_test_matrix(4, weighted = TRUE)
  expect_splot_works(mat, node_shape = "hexagon", layout = "circle")
})

test_that("splot renders ellipse nodes", {
  mat <- create_test_matrix(4, weighted = TRUE)
  expect_splot_works(mat, node_shape = "ellipse", layout = "circle")
})

test_that("splot renders heart nodes", {
  mat <- create_test_matrix(4, weighted = TRUE)
  expect_splot_works(mat, node_shape = "heart", layout = "circle")
})

test_that("splot renders star nodes", {
  mat <- create_test_matrix(4, weighted = TRUE)
  expect_splot_works(mat, node_shape = "star", layout = "circle")
})

test_that("splot renders cross nodes", {
  mat <- create_test_matrix(4, weighted = TRUE)
  expect_splot_works(mat, node_shape = "cross", layout = "circle")
})

test_that("splot renders rectangle nodes", {
  mat <- create_test_matrix(4, weighted = TRUE)
  expect_splot_works(mat, node_shape = "rectangle", layout = "circle")
})

test_that("splot renders none shape (label only)", {
  mat <- create_test_matrix(4, weighted = TRUE)
  rownames(mat) <- c("A", "B", "C", "D")
  expect_splot_works(mat, node_shape = "none", layout = "circle")
})

# ============================================
# splot with AI-Themed Shapes (splot-nodes.R)
# ============================================

test_that("splot renders neural shape", {
  mat <- create_test_matrix(3, weighted = TRUE)
  expect_splot_works(mat, node_shape = "neural", layout = "circle")
})

test_that("splot renders chip shape", {
  mat <- create_test_matrix(3, weighted = TRUE)
  expect_splot_works(mat, node_shape = "chip", layout = "circle")
})

test_that("splot renders robot shape", {
  mat <- create_test_matrix(3, weighted = TRUE)
  expect_splot_works(mat, node_shape = "robot", layout = "circle")
})

test_that("splot renders network shape", {
  mat <- create_test_matrix(3, weighted = TRUE)
  expect_splot_works(mat, node_shape = "network", layout = "circle")
})

test_that("splot renders database shape", {
  mat <- create_test_matrix(3, weighted = TRUE)
  expect_splot_works(mat, node_shape = "database", layout = "circle")
})

test_that("splot renders cloud shape", {
  mat <- create_test_matrix(3, weighted = TRUE)
  expect_splot_works(mat, node_shape = "cloud", layout = "circle")
})

test_that("splot renders gear shape", {
  mat <- create_test_matrix(3, weighted = TRUE)
  expect_splot_works(mat, node_shape = "gear", layout = "circle")
})

test_that("splot renders brain shape", {
  mat <- create_test_matrix(3, weighted = TRUE)
  expect_splot_works(mat, node_shape = "brain", layout = "circle")
})

# ============================================
# splot with Donut/Pie Shapes (splot-nodes.R)
# ============================================

test_that("splot renders pie nodes", {
  mat <- create_test_matrix(3, weighted = TRUE)
  expect_splot_works(mat, node_shape = "pie",
                     pie_values = list(c(0.3, 0.7), c(0.5, 0.5), c(0.2, 0.8)),
                     layout = "circle")
})

test_that("splot renders donut nodes with fill values", {
  mat <- create_test_matrix(3, weighted = TRUE)
  expect_splot_works(mat, donut_fill = c(0.3, 0.6, 0.9), layout = "circle")
})

# ============================================
# splot with Mixed Shapes (splot-nodes.R)
# ============================================

test_that("splot renders per-node shapes", {
  mat <- create_test_matrix(4, weighted = TRUE)
  shapes <- c("circle", "square", "diamond", "triangle")
  expect_splot_works(mat, node_shape = shapes, layout = "circle")
})

# ============================================
# splot Edge Rendering (splot-edges.R)
# ============================================

test_that("splot renders directed edges with arrows", {
  mat <- create_test_matrix(4, weighted = TRUE, symmetric = FALSE)
  expect_splot_works(mat, directed = TRUE, layout = "circle")
})

test_that("splot renders undirected edges", {
  mat <- create_test_matrix(4, weighted = TRUE)
  expect_splot_works(mat, directed = FALSE, layout = "circle")
})

test_that("splot renders with edge labels", {
  mat <- create_test_matrix(4, weighted = TRUE)
  expect_splot_works(mat, edge_labels = TRUE, layout = "circle")
})

test_that("splot renders with custom edge colors", {
  mat <- create_test_matrix(4, weighted = TRUE)
  expect_splot_works(mat, edge_color = "darkblue", layout = "circle")
})

test_that("splot renders with edge threshold", {
  mat <- create_test_matrix(6, weighted = TRUE)
  expect_splot_works(mat, threshold = 0.3, layout = "circle")
})

test_that("splot renders with curved edges", {
  mat <- create_test_matrix(4, weighted = TRUE, symmetric = FALSE)
  expect_splot_works(mat, directed = TRUE, curvature = 0.3, layout = "circle")
})

test_that("splot renders self-loops", {
  mat <- create_test_matrix(3, weighted = TRUE)
  mat[1, 1] <- 0.5  # Add self-loop
  expect_splot_works(mat, layout = "circle")
})

test_that("splot renders with dotted start style", {
  mat <- create_test_matrix(4, weighted = TRUE)
  expect_splot_works(mat, edge_start_style = "dotted", layout = "circle")
})

test_that("splot renders with custom arrow size", {
  mat <- create_test_matrix(4, weighted = TRUE, symmetric = FALSE)
  expect_splot_works(mat, directed = TRUE, arrow_size = 1.0, layout = "circle")
})

# ============================================
# splot with Various Layouts (triggers different code paths)
# ============================================

test_that("splot with spring layout", {
  mat <- create_test_matrix(6, weighted = TRUE)
  expect_splot_works(mat, layout = "spring")
})

test_that("splot with grid layout", {
  mat <- create_test_matrix(9, weighted = TRUE)
  expect_splot_works(mat, layout = "grid")
})

test_that("splot with random layout", {
  mat <- create_test_matrix(5, weighted = TRUE)
  expect_splot_works(mat, layout = "random")
})

test_that("splot with star layout", {
  mat <- create_test_matrix(5, weighted = TRUE)
  expect_splot_works(mat, layout = "star")
})

# ============================================
# splot with Titles and Node Labels (splot-nodes.R)
# ============================================

test_that("splot renders with title", {
  mat <- create_test_matrix(4, weighted = TRUE)
  expect_splot_works(mat, title = "Test Plot", layout = "circle")
})

test_that("splot renders with node labels", {
  mat <- create_test_matrix(4, weighted = TRUE)
  expect_splot_works(mat, labels = c("A", "B", "C", "D"), layout = "circle")
})

test_that("splot renders with labels = TRUE", {
  mat <- create_test_matrix(4, weighted = TRUE)
  rownames(mat) <- c("X", "Y", "Z", "W")
  expect_splot_works(mat, labels = TRUE, layout = "circle")
})

test_that("splot renders with custom node sizes", {
  mat <- create_test_matrix(4, weighted = TRUE)
  expect_splot_works(mat, node_size = c(5, 10, 15, 20), layout = "circle")
})

test_that("splot renders with custom node colors", {
  mat <- create_test_matrix(4, weighted = TRUE)
  expect_splot_works(mat, node_fill = c("red", "blue", "green", "orange"),
                     layout = "circle")
})

# ============================================
# splot with Themes (triggers theme code paths)
# ============================================

test_that("splot renders with classic theme", {
  mat <- create_test_matrix(4, weighted = TRUE)
  expect_splot_works(mat, theme = "classic", layout = "circle")
})

test_that("splot renders with dark theme", {
  mat <- create_test_matrix(4, weighted = TRUE)
  expect_splot_works(mat, theme = "dark", layout = "circle")
})

test_that("splot renders with colorblind theme", {
  mat <- create_test_matrix(4, weighted = TRUE)
  expect_splot_works(mat, theme = "colorblind", layout = "circle")
})

test_that("splot renders with minimal theme", {
  mat <- create_test_matrix(4, weighted = TRUE)
  expect_splot_works(mat, theme = "minimal", layout = "circle")
})

# ============================================
# soplot Grid Rendering (render-nodes.R, render-edges.R)
# ============================================

test_that("soplot renders basic network", {
  skip_if_not_installed("grid")
  mat <- create_test_matrix(4, weighted = TRUE)
  expect_soplot_works(mat, layout = "circle")
})

test_that("soplot renders with node labels", {
  skip_if_not_installed("grid")
  mat <- create_test_matrix(4, weighted = TRUE)
  rownames(mat) <- c("A", "B", "C", "D")
  expect_soplot_works(mat, show_labels = TRUE, layout = "circle")
})

test_that("soplot renders with edge labels", {
  skip_if_not_installed("grid")
  mat <- create_test_matrix(4, weighted = TRUE)
  expect_soplot_works(mat, edge_labels = TRUE, layout = "circle")
})

test_that("soplot renders directed with arrows", {
  skip_if_not_installed("grid")
  mat <- create_test_matrix(4, weighted = TRUE, symmetric = FALSE)
  expect_soplot_works(mat, show_arrows = TRUE, layout = "circle")
})

test_that("soplot renders with various shapes", {
  skip_if_not_installed("grid")
  mat <- create_test_matrix(4, weighted = TRUE)
  shapes <- c("circle", "square", "diamond", "triangle")

  for (shape in shapes) {
    result <- tryCatch({
      with_temp_png(soplot(mat, node_shape = shape, layout = "circle"))
      TRUE
    }, error = function(e) FALSE)
    expect_true(result, info = paste("soplot failed with shape:", shape))
  }
})

test_that("soplot renders self-loops", {
  skip_if_not_installed("grid")
  mat <- create_test_matrix(3, weighted = TRUE)
  mat[1, 1] <- 0.5
  expect_soplot_works(mat, layout = "circle")
})

test_that("soplot renders with curved edges", {
  skip_if_not_installed("grid")
  mat <- create_test_matrix(4, weighted = TRUE, symmetric = FALSE)
  expect_soplot_works(mat, show_arrows = TRUE, curvature = 0.3, layout = "circle")
})

test_that("soplot renders with custom node colors", {
  skip_if_not_installed("grid")
  mat <- create_test_matrix(4, weighted = TRUE)
  expect_soplot_works(mat, node_fill = c("red", "blue", "green", "orange"),
                      layout = "circle")
})

test_that("soplot renders with edge threshold", {
  skip_if_not_installed("grid")
  mat <- create_test_matrix(6, weighted = TRUE)
  expect_soplot_works(mat, threshold = 0.3, layout = "circle")
})
