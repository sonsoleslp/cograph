# Tests for multi-cluster TNA network visualization
# Covers: R/plot-htna-multi.R (plot_mtna)

test_that("plot_mtna validates cluster_list is a list", {
  mat <- create_test_matrix(8)
  colnames(mat) <- rownames(mat) <- LETTERS[1:8]

  expect_error(
    plot_mtna(mat, c("A", "B", "C")),
    "must be a list"
  )
})

test_that("plot_mtna validates cluster_list has 2+ clusters", {
  mat <- create_test_matrix(8)
  colnames(mat) <- rownames(mat) <- LETTERS[1:8]

  expect_error(
    plot_mtna(mat, list(All = LETTERS[1:8])),
    "2"
  )
})

test_that("plot_mtna validates no overlap between clusters", {
  mat <- create_test_matrix(8)
  colnames(mat) <- rownames(mat) <- LETTERS[1:8]

  expect_error(
    plot_mtna(mat, list(C1 = c("A", "B", "C"), C2 = c("C", "D", "E"))),
    "overlap"
  )
})

test_that("plot_mtna validates x type", {
  expect_error(
    plot_mtna("not a matrix", list(C1 = c("A", "B"), C2 = c("C", "D"))),
    "must be a tna object or matrix"
  )
})

test_that("plot_mtna works with 2 clusters", {
  mat <- create_test_matrix(8)
  colnames(mat) <- rownames(mat) <- LETTERS[1:8]

  clusters <- list(
    North = c("A", "B", "C", "D"),
    South = c("E", "F", "G", "H")
  )

  result <- safe_plot(plot_mtna(mat, clusters))
  expect_true(result$success, info = result$error)
})

test_that("plot_mtna works with 4 clusters", {
  mat <- create_test_matrix(12)
  colnames(mat) <- rownames(mat) <- LETTERS[1:12]

  clusters <- list(
    North = c("A", "B", "C"),
    East = c("D", "E", "F"),
    South = c("G", "H", "I"),
    West = c("J", "K", "L")
  )

  result <- safe_plot(plot_mtna(mat, clusters))
  expect_true(result$success, info = result$error)
})

test_that("plot_mtna circle layout works", {
  mat <- create_test_matrix(8)
  colnames(mat) <- rownames(mat) <- LETTERS[1:8]

  clusters <- list(C1 = c("A", "B", "C", "D"), C2 = c("E", "F", "G", "H"))

  result <- safe_plot(plot_mtna(mat, clusters, layout = "circle"))
  expect_true(result$success, info = result$error)
})

test_that("plot_mtna grid layout works", {
  mat <- create_test_matrix(8)
  colnames(mat) <- rownames(mat) <- LETTERS[1:8]

  clusters <- list(C1 = c("A", "B", "C", "D"), C2 = c("E", "F", "G", "H"))

  result <- safe_plot(plot_mtna(mat, clusters, layout = "grid"))
  expect_true(result$success, info = result$error)
})

test_that("plot_mtna horizontal layout works", {
  mat <- create_test_matrix(8)
  colnames(mat) <- rownames(mat) <- LETTERS[1:8]

  clusters <- list(C1 = c("A", "B", "C", "D"), C2 = c("E", "F", "G", "H"))

  result <- safe_plot(plot_mtna(mat, clusters, layout = "horizontal"))
  expect_true(result$success, info = result$error)
})

test_that("plot_mtna vertical layout works", {
  mat <- create_test_matrix(8)
  colnames(mat) <- rownames(mat) <- LETTERS[1:8]

  clusters <- list(C1 = c("A", "B", "C", "D"), C2 = c("E", "F", "G", "H"))

  result <- safe_plot(plot_mtna(mat, clusters, layout = "vertical"))
  expect_true(result$success, info = result$error)
})

test_that("plot_mtna custom colors work", {
  mat <- create_test_matrix(8)
  colnames(mat) <- rownames(mat) <- LETTERS[1:8]

  clusters <- list(C1 = c("A", "B", "C", "D"), C2 = c("E", "F", "G", "H"))

  result <- safe_plot(plot_mtna(mat, clusters, colors = c("red", "blue")))
  expect_true(result$success, info = result$error)
})

test_that("plot_mtna custom shapes work", {
  mat <- create_test_matrix(8)
  colnames(mat) <- rownames(mat) <- LETTERS[1:8]

  clusters <- list(C1 = c("A", "B", "C", "D"), C2 = c("E", "F", "G", "H"))

  result <- safe_plot(plot_mtna(mat, clusters, shapes = c("square", "diamond")))
  expect_true(result$success, info = result$error)
})

test_that("plot_mtna summary_edges parameter works", {
  mat <- create_test_matrix(8)
  colnames(mat) <- rownames(mat) <- LETTERS[1:8]

  clusters <- list(C1 = c("A", "B", "C", "D"), C2 = c("E", "F", "G", "H"))

  # With summary edges
  result <- safe_plot(plot_mtna(mat, clusters, summary_edges = TRUE))
  expect_true(result$success, info = result$error)

  # Without summary edges
  result <- safe_plot(plot_mtna(mat, clusters, summary_edges = FALSE))
  expect_true(result$success, info = result$error)
})

test_that("plot_mtna within_edges parameter works", {
  mat <- create_test_matrix(8)
  colnames(mat) <- rownames(mat) <- LETTERS[1:8]

  clusters <- list(C1 = c("A", "B", "C", "D"), C2 = c("E", "F", "G", "H"))

  # With within edges
  result <- safe_plot(plot_mtna(mat, clusters, within_edges = TRUE))
  expect_true(result$success, info = result$error)

  # Without within edges
  result <- safe_plot(plot_mtna(mat, clusters, within_edges = FALSE))
  expect_true(result$success, info = result$error)
})

test_that("plot_mtna bundle_edges parameter works", {
  mat <- create_test_matrix(8)
  colnames(mat) <- rownames(mat) <- LETTERS[1:8]

  clusters <- list(C1 = c("A", "B", "C", "D"), C2 = c("E", "F", "G", "H"))

  # With bundling
  result <- safe_plot(plot_mtna(mat, clusters, bundle_edges = TRUE))
  expect_true(result$success, info = result$error)

  # Without bundling
  result <- safe_plot(plot_mtna(mat, clusters, bundle_edges = FALSE))
  expect_true(result$success, info = result$error)
})

test_that("plot_mtna bundle_strength parameter works", {
  mat <- create_test_matrix(8)
  colnames(mat) <- rownames(mat) <- LETTERS[1:8]

  clusters <- list(C1 = c("A", "B", "C", "D"), C2 = c("E", "F", "G", "H"))

  result <- safe_plot(plot_mtna(mat, clusters, bundle_strength = 0.5))
  expect_true(result$success, info = result$error)
})

test_that("plot_mtna show_border parameter works", {
  mat <- create_test_matrix(8)
  colnames(mat) <- rownames(mat) <- LETTERS[1:8]

  clusters <- list(C1 = c("A", "B", "C", "D"), C2 = c("E", "F", "G", "H"))

  # With border
  result <- safe_plot(plot_mtna(mat, clusters, show_border = TRUE))
  expect_true(result$success, info = result$error)

  # Without border
  result <- safe_plot(plot_mtna(mat, clusters, show_border = FALSE))
  expect_true(result$success, info = result$error)
})

test_that("plot_mtna legend parameter works", {
  mat <- create_test_matrix(8)
  colnames(mat) <- rownames(mat) <- LETTERS[1:8]

  clusters <- list(C1 = c("A", "B", "C", "D"), C2 = c("E", "F", "G", "H"))

  # With legend
  result <- safe_plot(plot_mtna(mat, clusters, legend = TRUE))
  expect_true(result$success, info = result$error)

  # Without legend
  result <- safe_plot(plot_mtna(mat, clusters, legend = FALSE))
  expect_true(result$success, info = result$error)
})

test_that("plot_mtna legend_position parameter works", {
  mat <- create_test_matrix(8)
  colnames(mat) <- rownames(mat) <- LETTERS[1:8]

  clusters <- list(C1 = c("A", "B", "C", "D"), C2 = c("E", "F", "G", "H"))

  for (pos in c("topright", "topleft", "bottomright", "bottomleft")) {
    result <- safe_plot(plot_mtna(mat, clusters, legend_position = pos))
    expect_true(result$success, info = paste("Position", pos, "failed:", result$error))
  }
})

test_that("plot_mtna spacing parameter works", {
  mat <- create_test_matrix(8)
  colnames(mat) <- rownames(mat) <- LETTERS[1:8]

  clusters <- list(C1 = c("A", "B", "C", "D"), C2 = c("E", "F", "G", "H"))

  result <- safe_plot(plot_mtna(mat, clusters, spacing = 4))
  expect_true(result$success, info = result$error)
})

test_that("plot_mtna shape_size parameter works", {
  mat <- create_test_matrix(8)
  colnames(mat) <- rownames(mat) <- LETTERS[1:8]

  clusters <- list(C1 = c("A", "B", "C", "D"), C2 = c("E", "F", "G", "H"))

  result <- safe_plot(plot_mtna(mat, clusters, shape_size = 1.5))
  expect_true(result$success, info = result$error)
})

test_that("plot_mtna node_spacing parameter works", {
  mat <- create_test_matrix(8)
  colnames(mat) <- rownames(mat) <- LETTERS[1:8]

  clusters <- list(C1 = c("A", "B", "C", "D"), C2 = c("E", "F", "G", "H"))

  result <- safe_plot(plot_mtna(mat, clusters, node_spacing = 0.7))
  expect_true(result$success, info = result$error)
})

test_that("plot_mtna curvature parameter works", {
  mat <- create_test_matrix(8)
  colnames(mat) <- rownames(mat) <- LETTERS[1:8]

  clusters <- list(C1 = c("A", "B", "C", "D"), C2 = c("E", "F", "G", "H"))

  result <- safe_plot(plot_mtna(mat, clusters, curvature = 0.5))
  expect_true(result$success, info = result$error)
})

test_that("plot_mtna node_size parameter works", {
  mat <- create_test_matrix(8)
  colnames(mat) <- rownames(mat) <- LETTERS[1:8]

  clusters <- list(C1 = c("A", "B", "C", "D"), C2 = c("E", "F", "G", "H"))

  result <- safe_plot(plot_mtna(mat, clusters, node_size = 3))
  expect_true(result$success, info = result$error)
})

test_that("plot_mtna edge_colors parameter works", {
  mat <- create_test_matrix(8)
  colnames(mat) <- rownames(mat) <- LETTERS[1:8]

  clusters <- list(C1 = c("A", "B", "C", "D"), C2 = c("E", "F", "G", "H"))

  result <- safe_plot(plot_mtna(mat, clusters, edge_colors = c("red", "blue")))
  expect_true(result$success, info = result$error)
})

test_that("plot_mtna handles tna input", {
  skip_if_no_tna()

  # Create mock tna object
  tna_obj <- structure(
    list(
      weights = matrix(runif(64), 8, 8),
      labels = LETTERS[1:8],
      inits = rep(1/8, 8)
    ),
    class = "tna"
  )

  clusters <- list(C1 = c("A", "B", "C", "D"), C2 = c("E", "F", "G", "H"))

  result <- safe_plot(plot_mtna(tna_obj, clusters))
  expect_true(result$success, info = result$error)
})

test_that("plot_mtna handles single node clusters", {
  mat <- create_test_matrix(4)
  colnames(mat) <- rownames(mat) <- LETTERS[1:4]

  clusters <- list(C1 = "A", C2 = c("B", "C", "D"))

  result <- safe_plot(plot_mtna(mat, clusters))
  expect_true(result$success, info = result$error)
})

test_that("plot_mtna handles weighted matrices", {
  mat <- create_test_matrix(8, weighted = TRUE)
  colnames(mat) <- rownames(mat) <- LETTERS[1:8]

  clusters <- list(C1 = c("A", "B", "C", "D"), C2 = c("E", "F", "G", "H"))

  result <- safe_plot(plot_mtna(mat, clusters))
  expect_true(result$success, info = result$error)
})
