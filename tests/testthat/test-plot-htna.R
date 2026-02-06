# Tests for plot_htna() and related functions
# Covers: R/plot-htna.R

test_that("plot_htna requires valid node_list", {
  mat <- create_test_matrix(6)
  colnames(mat) <- rownames(mat) <- c("A", "B", "C", "D", "E", "F")

  # Must be a list

  expect_error(
    plot_htna(mat, c("A", "B")),
    "node_list must be a list"
  )

  # Must have 2+ groups

  expect_error(
    plot_htna(mat, list(c("A", "B", "C", "D", "E", "F"))),
    "node_list must be a list of 2"
  )

  # Elements must be character vectors
  expect_error(
    plot_htna(mat, list(1:3, 4:6)),
    "character vectors"
  )
})

test_that("plot_htna validates node overlap", {
  mat <- create_test_matrix(6)
  colnames(mat) <- rownames(mat) <- c("A", "B", "C", "D", "E", "F")

  # Duplicate nodes in groups should error
  expect_error(
    plot_htna(mat, list(c("A", "B", "C"), c("C", "D", "E"))),
    "must not overlap"
  )
})

test_that("plot_htna validates missing nodes", {
  mat <- create_test_matrix(6)
  colnames(mat) <- rownames(mat) <- c("A", "B", "C", "D", "E", "F")

  # Unknown node should error
  expect_error(
    plot_htna(mat, list(c("A", "B", "Z"), c("D", "E", "F"))),
    "not found"
  )
})

test_that("plot_htna works with 2 groups (bipartite)", {
  mat <- create_test_matrix(6)
  colnames(mat) <- rownames(mat) <- c("A", "B", "C", "D", "E", "F")

  node_list <- list(
    Group1 = c("A", "B", "C"),
    Group2 = c("D", "E", "F")
  )

  result <- safe_plot(plot_htna(mat, node_list))
  expect_true(result$success, info = result$error)
})

test_that("plot_htna works with 3 groups (polygon/triangle)", {
  mat <- create_test_matrix(9)
  colnames(mat) <- rownames(mat) <- LETTERS[1:9]

  node_list <- list(
    Group1 = c("A", "B", "C"),
    Group2 = c("D", "E", "F"),
    Group3 = c("G", "H", "I")
  )

  result <- safe_plot(plot_htna(mat, node_list))
  expect_true(result$success, info = result$error)
})

test_that("plot_htna works with 4 groups (rectangle)", {
  mat <- create_test_matrix(8)
  colnames(mat) <- rownames(mat) <- LETTERS[1:8]

  node_list <- list(
    G1 = c("A", "B"),
    G2 = c("C", "D"),
    G3 = c("E", "F"),
    G4 = c("G", "H")
  )

  result <- safe_plot(plot_htna(mat, node_list))
  expect_true(result$success, info = result$error)
})

test_that("plot_htna bipartite orientation options work", {
  mat <- create_test_matrix(6)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]
  node_list <- list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F"))

  # Vertical orientation
  result <- safe_plot(plot_htna(mat, node_list, orientation = "vertical"))
  expect_true(result$success, info = result$error)

  # Horizontal orientation
  result <- safe_plot(plot_htna(mat, node_list, orientation = "horizontal"))
  expect_true(result$success, info = result$error)
})

test_that("plot_htna jitter options work", {
  mat <- create_test_matrix(6)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]
  node_list <- list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F"))

  # No jitter
  result <- safe_plot(plot_htna(mat, node_list, jitter = FALSE))
  expect_true(result$success, info = result$error)

  # Auto jitter
  result <- safe_plot(plot_htna(mat, node_list, jitter = TRUE))
  expect_true(result$success, info = result$error)

  # Numeric jitter
  result <- safe_plot(plot_htna(mat, node_list, jitter = 0.5))
  expect_true(result$success, info = result$error)

  # Named list jitter
  result <- safe_plot(plot_htna(mat, node_list, jitter = list(A = 0.1, B = -0.1)))
  expect_true(result$success, info = result$error)

  # Jitter sides
  result <- safe_plot(plot_htna(mat, node_list, jitter = TRUE, jitter_side = "both"))
  expect_true(result$success, info = result$error)

  result <- safe_plot(plot_htna(mat, node_list, jitter = TRUE, jitter_side = "second"))
  expect_true(result$success, info = result$error)

  result <- safe_plot(plot_htna(mat, node_list, jitter = TRUE, jitter_side = "none"))
  expect_true(result$success, info = result$error)
})

test_that("plot_htna validates layout vs group count", {
  mat <- create_test_matrix(6)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]
  node_list_2 <- list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F"))

  mat9 <- create_test_matrix(9)
  colnames(mat9) <- rownames(mat9) <- LETTERS[1:9]
  node_list_3 <- list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F"), G3 = c("G", "H", "I"))

  # Bipartite requires 2 groups
  expect_error(
    plot_htna(mat9, node_list_3, layout = "bipartite"),
    "Bipartite layout requires exactly 2 groups"
  )

  # Polygon requires 3+ groups
  expect_error(
    plot_htna(mat, node_list_2, layout = "polygon"),
    "Polygon layout requires at least 3 groups"
  )
})

test_that("plot_htna accepts custom colors and shapes", {
  mat <- create_test_matrix(6)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]
  node_list <- list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F"))

  result <- safe_plot(
    plot_htna(mat, node_list,
              group1_color = "red",
              group2_color = "blue",
              group1_shape = "square",
              group2_shape = "triangle")
  )
  expect_true(result$success, info = result$error)
})

test_that("plot_htna accepts group_colors and group_shapes vectors", {
  mat <- create_test_matrix(9)
  colnames(mat) <- rownames(mat) <- LETTERS[1:9]
  node_list <- list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F"), G3 = c("G", "H", "I"))

  result <- safe_plot(
    plot_htna(mat, node_list,
              group_colors = c("red", "green", "blue"),
              group_shapes = c("circle", "square", "diamond"))
  )
  expect_true(result$success, info = result$error)
})

test_that("plot_htna validates color/shape vector lengths", {
  mat <- create_test_matrix(9)
  colnames(mat) <- rownames(mat) <- LETTERS[1:9]
  node_list <- list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F"), G3 = c("G", "H", "I"))

  expect_error(
    plot_htna(mat, node_list, group_colors = c("red", "blue")),
    "group_colors must have 3 elements"
  )

  expect_error(
    plot_htna(mat, node_list, group_shapes = c("circle", "square")),
    "group_shapes must have 3 elements"
  )
})

test_that("plot_htna legend options work", {
  mat <- create_test_matrix(9)
  colnames(mat) <- rownames(mat) <- LETTERS[1:9]
  node_list <- list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F"), G3 = c("G", "H", "I"))

  # Legend enabled (default)
  result <- safe_plot(plot_htna(mat, node_list, legend = TRUE))
  expect_true(result$success, info = result$error)

  # Legend disabled
  result <- safe_plot(plot_htna(mat, node_list, legend = FALSE))
  expect_true(result$success, info = result$error)

  # Different positions
  for (pos in c("topright", "topleft", "bottomright", "bottomleft")) {
    result <- safe_plot(plot_htna(mat, node_list, legend_position = pos))
    expect_true(result$success, info = paste("Position", pos, "failed:", result$error))
  }
})

test_that("plot_htna extend_lines option works", {
  mat <- create_test_matrix(6)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]
  node_list <- list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F"))

  # Extend lines enabled
  result <- safe_plot(plot_htna(mat, node_list, extend_lines = TRUE))
  expect_true(result$success, info = result$error)

  # Extend lines with custom length
  result <- safe_plot(plot_htna(mat, node_list, extend_lines = 0.2))
  expect_true(result$success, info = result$error)

  # Horizontal with extend lines
  result <- safe_plot(plot_htna(mat, node_list, orientation = "horizontal", extend_lines = TRUE))
  expect_true(result$success, info = result$error)
})

test_that("plot_htna circular layout works", {
  mat <- create_test_matrix(9)
  colnames(mat) <- rownames(mat) <- LETTERS[1:9]
  node_list <- list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F"), G3 = c("G", "H", "I"))

  result <- safe_plot(plot_htna(mat, node_list, layout = "circular"))
  expect_true(result$success, info = result$error)
})

test_that("plot_htna weight-based ordering works", {
  mat <- create_test_matrix(6, weighted = TRUE)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]
  node_list <- list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F"))

  result <- safe_plot(plot_htna(mat, node_list, use_list_order = FALSE))
  expect_true(result$success, info = result$error)
})

test_that("plot_htna edge_colors option works", {
  mat <- create_test_matrix(6)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]
  node_list <- list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F"))

  # Custom edge colors
  result <- safe_plot(plot_htna(mat, node_list, edge_colors = c("red", "blue")))
  expect_true(result$success, info = result$error)

  # Disable edge colors (use default)
  result <- safe_plot(plot_htna(mat, node_list, edge_colors = FALSE))
  expect_true(result$success, info = result$error)
})

test_that("plot_htna with curvature works", {
  mat <- create_test_matrix(6)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]
  node_list <- list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F"))

  result <- safe_plot(plot_htna(mat, node_list, curvature = 0.8))
  expect_true(result$success, info = result$error)
})

test_that("plot_htna works with single node groups", {
  mat <- create_test_matrix(4)
  colnames(mat) <- rownames(mat) <- LETTERS[1:4]
  node_list <- list(G1 = "A", G2 = c("B", "C", "D"))

  result <- safe_plot(plot_htna(mat, node_list))
  expect_true(result$success, info = result$error)
})

test_that("plot_htna horizontal jitter works", {
  mat <- create_test_matrix(6)
  colnames(mat) <- rownames(mat) <- LETTERS[1:6]
  node_list <- list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F"))

  result <- safe_plot(
    plot_htna(mat, node_list, orientation = "horizontal",
              jitter = TRUE, jitter_side = "both")
  )
  expect_true(result$success, info = result$error)
})

# ============================================
# Tests for helper functions
# ============================================

test_that("compute_connectivity_jitter_horizontal returns valid jitter", {
  mat <- create_test_matrix(6)
  g1 <- 1:3
  g2 <- 4:6

  jitter <- compute_connectivity_jitter_horizontal(mat, g1, g2, amount = 0.5, side = "both")

  expect_equal(length(jitter), 6)
  expect_true(all(is.numeric(jitter)))
})

test_that("compute_connectivity_jitter_horizontal handles zero connectivity", {
  mat <- matrix(0, 6, 6)
  g1 <- 1:3
  g2 <- 4:6

  jitter <- compute_connectivity_jitter_horizontal(mat, g1, g2, amount = 0.5, side = "group1")

  expect_equal(length(jitter), 6)
  expect_true(all(jitter == 0))
})

test_that("compute_connectivity_jitter_vertical returns valid jitter", {
  mat <- create_test_matrix(6)
  g1 <- 1:3
  g2 <- 4:6

  jitter <- compute_connectivity_jitter_vertical(mat, g1, g2, amount = 0.5, side = "both")

  expect_equal(length(jitter), 6)
  expect_true(all(is.numeric(jitter)))
})

test_that("compute_polygon_layout returns valid positions", {
  node_list <- list(G1 = c("A", "B"), G2 = c("C", "D"), G3 = c("E", "F"))
  lab <- c("A", "B", "C", "D", "E", "F")
  group_indices <- list(1:2, 3:4, 5:6)

  pos <- compute_polygon_layout(node_list, lab, group_indices, n_sides = 3)

  expect_true(is.list(pos))
  expect_equal(length(pos$x), 6)
  expect_equal(length(pos$y), 6)
  expect_true(all(is.finite(pos$x)))
  expect_true(all(is.finite(pos$y)))
})

test_that("compute_circular_layout returns valid positions", {
  node_list <- list(G1 = c("A", "B"), G2 = c("C", "D"), G3 = c("E", "F"))
  lab <- c("A", "B", "C", "D", "E", "F")
  group_indices <- list(1:2, 3:4, 5:6)

  pos <- compute_circular_layout(node_list, lab, group_indices, n_groups = 3)

  expect_true(is.list(pos))
  expect_equal(length(pos$x), 6)
  expect_equal(length(pos$y), 6)
  expect_true(all(is.finite(pos$x)))
  expect_true(all(is.finite(pos$y)))
})

test_that("compute_polygon_layout handles single node groups", {
  node_list <- list(G1 = "A", G2 = "B", G3 = "C")
  lab <- c("A", "B", "C")
  group_indices <- list(1, 2, 3)

  pos <- compute_polygon_layout(node_list, lab, group_indices, n_sides = 3)

  expect_equal(length(pos$x), 3)
  expect_equal(length(pos$y), 3)
})

test_that("plot_htna validates x input type", {
  node_list <- list(G1 = c("A", "B"), G2 = c("C", "D"))

  expect_error(
    plot_htna(list(a = 1), node_list),
    "must be a tna object or matrix"
  )
})
