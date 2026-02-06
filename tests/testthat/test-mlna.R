# Tests for multilevel network visualization
# Covers: R/mlna.R

test_that("plot_mlna validates layer_list is a list", {
  mat <- create_test_matrix(6)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]

  expect_error(
    plot_mlna(mat, c("A", "B", "C")),
    "must be a list"
  )
})

test_that("plot_mlna validates layer_list has 2+ layers", {
  mat <- create_test_matrix(6)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]

  expect_error(
    plot_mlna(mat, list(LETTERS[1:6])),
    "2"
  )
})

test_that("plot_mlna validates no overlap between layers", {
  mat <- create_test_matrix(6)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]

  expect_error(
    plot_mlna(mat, list(L1 = c("A", "B", "C"), L2 = c("C", "D", "E"))),
    "overlap"
  )
})

test_that("plot_mlna validates all nodes exist in matrix", {
  mat <- create_test_matrix(6)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]

  expect_error(
    plot_mlna(mat, list(L1 = c("A", "B", "X"), L2 = c("D", "E", "F"))),
    "not found"
  )
})

test_that("plot_mlna works with 2 layers", {
  mat <- create_test_matrix(6)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]

  layers <- list(
    Top = c("A", "B", "C"),
    Bottom = c("D", "E", "F")
  )

  result <- safe_plot(plot_mlna(mat, layers))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna works with 3 layers", {
  mat <- create_test_matrix(9)
  colnames(mat) <- rownames(mat) <- LETTERS[1:9]

  layers <- list(
    Top = c("A", "B", "C"),
    Middle = c("D", "E", "F"),
    Bottom = c("G", "H", "I")
  )

  result <- safe_plot(plot_mlna(mat, layers))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna horizontal layout works", {
  mat <- create_test_matrix(6)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]

  layers <- list(L1 = c("A", "B", "C"), L2 = c("D", "E", "F"))

  result <- safe_plot(plot_mlna(mat, layers, layout = "horizontal"))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna circle layout works", {
  mat <- create_test_matrix(6)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]

  layers <- list(L1 = c("A", "B", "C"), L2 = c("D", "E", "F"))

  result <- safe_plot(plot_mlna(mat, layers, layout = "circle"))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna spring layout works", {
  skip_if_no_igraph()

  mat <- create_test_matrix(6)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]

  layers <- list(L1 = c("A", "B", "C"), L2 = c("D", "E", "F"))

  result <- safe_plot(plot_mlna(mat, layers, layout = "spring"))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna custom colors work", {
  mat <- create_test_matrix(6)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]

  layers <- list(L1 = c("A", "B", "C"), L2 = c("D", "E", "F"))

  result <- safe_plot(plot_mlna(mat, layers, colors = c("red", "blue")))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna custom shapes work", {
  mat <- create_test_matrix(6)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]

  layers <- list(L1 = c("A", "B", "C"), L2 = c("D", "E", "F"))

  result <- safe_plot(plot_mlna(mat, layers, shapes = c("square", "triangle")))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna within_edges parameter works", {
  mat <- create_test_matrix(6)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]

  layers <- list(L1 = c("A", "B", "C"), L2 = c("D", "E", "F"))

  # With within edges
  result <- safe_plot(plot_mlna(mat, layers, within_edges = TRUE))
  expect_true(result$success, info = result$error)

  # Without within edges
  result <- safe_plot(plot_mlna(mat, layers, within_edges = FALSE))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna between_edges parameter works", {
  mat <- create_test_matrix(6)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]

  layers <- list(L1 = c("A", "B", "C"), L2 = c("D", "E", "F"))

  # With between edges
  result <- safe_plot(plot_mlna(mat, layers, between_edges = TRUE))
  expect_true(result$success, info = result$error)

  # Without between edges
  result <- safe_plot(plot_mlna(mat, layers, between_edges = FALSE))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna show_border parameter works", {
  mat <- create_test_matrix(6)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]

  layers <- list(L1 = c("A", "B", "C"), L2 = c("D", "E", "F"))

  # With border
  result <- safe_plot(plot_mlna(mat, layers, show_border = TRUE))
  expect_true(result$success, info = result$error)

  # Without border
  result <- safe_plot(plot_mlna(mat, layers, show_border = FALSE))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna legend parameter works", {
  mat <- create_test_matrix(6)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]

  layers <- list(L1 = c("A", "B", "C"), L2 = c("D", "E", "F"))

  # With legend
  result <- safe_plot(plot_mlna(mat, layers, legend = TRUE))
  expect_true(result$success, info = result$error)

  # Without legend
  result <- safe_plot(plot_mlna(mat, layers, legend = FALSE))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna legend_position parameter works", {
  mat <- create_test_matrix(6)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]

  layers <- list(L1 = c("A", "B", "C"), L2 = c("D", "E", "F"))

  for (pos in c("topright", "topleft", "bottomright", "bottomleft")) {
    result <- safe_plot(plot_mlna(mat, layers, legend_position = pos))
    expect_true(result$success, info = paste("Position", pos, "failed:", result$error))
  }
})

test_that("plot_mlna spacing parameters work", {
  mat <- create_test_matrix(6)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]

  layers <- list(L1 = c("A", "B", "C"), L2 = c("D", "E", "F"))

  result <- safe_plot(plot_mlna(mat, layers,
                                layer_spacing = 3,
                                layer_width = 5,
                                layer_depth = 2.5))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna skew_angle parameter works", {
  mat <- create_test_matrix(6)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]

  layers <- list(L1 = c("A", "B", "C"), L2 = c("D", "E", "F"))

  result <- safe_plot(plot_mlna(mat, layers, skew_angle = 35))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna node_spacing parameter works", {
  mat <- create_test_matrix(6)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]

  layers <- list(L1 = c("A", "B", "C"), L2 = c("D", "E", "F"))

  result <- safe_plot(plot_mlna(mat, layers, node_spacing = 0.9))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna curvature parameter works", {
  mat <- create_test_matrix(6)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]

  layers <- list(L1 = c("A", "B", "C"), L2 = c("D", "E", "F"))

  result <- safe_plot(plot_mlna(mat, layers, curvature = 0.3))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna node_size parameter works", {
  mat <- create_test_matrix(6)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]

  layers <- list(L1 = c("A", "B", "C"), L2 = c("D", "E", "F"))

  result <- safe_plot(plot_mlna(mat, layers, node_size = 4))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna minimum parameter filters edges", {
  mat <- create_test_matrix(6, weighted = TRUE)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]

  layers <- list(L1 = c("A", "B", "C"), L2 = c("D", "E", "F"))

  result <- safe_plot(plot_mlna(mat, layers, minimum = 0.5))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna between_style parameter works", {
  mat <- create_test_matrix(6)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]

  layers <- list(L1 = c("A", "B", "C"), L2 = c("D", "E", "F"))

  # Solid
  result <- safe_plot(plot_mlna(mat, layers, between_style = 1))
  expect_true(result$success, info = result$error)

  # Dashed
  result <- safe_plot(plot_mlna(mat, layers, between_style = 2))
  expect_true(result$success, info = result$error)

  # Dotted
  result <- safe_plot(plot_mlna(mat, layers, between_style = 3))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna edge_colors parameter works", {
  mat <- create_test_matrix(6)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]

  layers <- list(L1 = c("A", "B", "C"), L2 = c("D", "E", "F"))

  result <- safe_plot(plot_mlna(mat, layers, edge_colors = c("red", "blue")))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna handles tna input", {
  skip_if_no_tna()

  # Create mock tna object
  tna_obj <- structure(
    list(
      weights = matrix(runif(36), 6, 6),
      labels = LETTERS[1:6],
      inits = rep(1/6, 6)
    ),
    class = "tna"
  )

  layers <- list(L1 = c("A", "B", "C"), L2 = c("D", "E", "F"))

  result <- safe_plot(plot_mlna(tna_obj, layers))
  expect_true(result$success, info = result$error)
})

test_that("plot_mlna handles single node layers", {
  mat <- create_test_matrix(4)
  colnames(mat) <- rownames(mat) <- LETTERS[1:4]

  layers <- list(L1 = "A", L2 = c("B", "C", "D"))

  result <- safe_plot(plot_mlna(mat, layers))
  expect_true(result$success, info = result$error)
})
