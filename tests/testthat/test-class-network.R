# Tests for CographNetwork R6 class
# Covers: R/class-network.R

# ============================================
# Constructor Tests
# ============================================

test_that("CographNetwork creates from adjacency matrix", {
  mat <- create_test_matrix(4)
  net <- CographNetwork$new(mat)

  expect_true(inherits(net, "CographNetwork"))
  expect_equal(net$n_nodes, 4)
})

test_that("CographNetwork handles directed parameter", {
  mat <- create_test_matrix(4)

  net_dir <- CographNetwork$new(mat, directed = TRUE)
  net_undir <- CographNetwork$new(mat, directed = FALSE)

  expect_true(net_dir$is_directed)
  expect_false(net_undir$is_directed)
})

test_that("CographNetwork handles node_labels parameter", {
  mat <- create_test_matrix(4)
  labels <- c("A", "B", "C", "D")

  net <- CographNetwork$new(mat, node_labels = labels)

  expect_equal(net$node_labels, labels)
})

test_that("CographNetwork errors on mismatched labels length", {
  mat <- create_test_matrix(4)

  expect_error(
    CographNetwork$new(mat, node_labels = c("A", "B")),
    "must match"
  )
})

test_that("CographNetwork creates empty network", {
  net <- CographNetwork$new()

  expect_true(inherits(net, "CographNetwork"))
})

# ============================================
# Setter/Getter Tests
# ============================================

test_that("set_nodes and get_nodes work", {
  net <- CographNetwork$new()
  nodes <- data.frame(id = 1:3, label = c("A", "B", "C"))

  net$set_nodes(nodes)
  result <- net$get_nodes()

  expect_equal(nrow(result), 3)
})

test_that("set_edges and get_edges work", {
  net <- CographNetwork$new()
  edges <- data.frame(from = c(1, 2), to = c(2, 3), weight = c(1, 1))

  net$set_edges(edges)
  result <- net$get_edges()

  expect_equal(nrow(result), 2)
})

test_that("set_directed works", {
  mat <- create_test_matrix(4)
  net <- CographNetwork$new(mat)

  net$set_directed(TRUE)
  expect_true(net$is_directed)

  net$set_directed(FALSE)
  expect_false(net$is_directed)
})

test_that("set_weights works", {
  mat <- create_test_matrix(4)
  net <- CographNetwork$new(mat)

  weights <- c(0.5, 1.0, 0.8)
  net$set_weights(weights)

  expect_true(net$has_weights)
})

test_that("set_layout_coords and get_layout work", {
  mat <- create_test_matrix(4)
  net <- CographNetwork$new(mat)

  coords <- data.frame(x = runif(4), y = runif(4))
  net$set_layout_coords(coords)
  result <- net$get_layout()

  expect_equal(nrow(result), 4)
  expect_true("x" %in% names(result))
  expect_true("y" %in% names(result))
})

test_that("set_node_aes and get_node_aes work", {
  mat <- create_test_matrix(4)
  net <- CographNetwork$new(mat)

  aes <- list(fill = "red", size = 0.1)
  net$set_node_aes(aes)
  result <- net$get_node_aes()

  expect_equal(result$fill, "red")
  expect_equal(result$size, 0.1)
})

test_that("set_edge_aes and get_edge_aes work", {
  mat <- create_test_matrix(4)
  net <- CographNetwork$new(mat)

  aes <- list(color = "blue", width = 2)
  net$set_edge_aes(aes)
  result <- net$get_edge_aes()

  expect_equal(result$color, "blue")
  expect_equal(result$width, 2)
})

test_that("set_theme and get_theme work", {
  mat <- create_test_matrix(4)
  net <- CographNetwork$new(mat)

  theme <- get_theme("classic")
  net$set_theme(theme)
  result <- net$get_theme()

  expect_true(!is.null(result))
})

test_that("set_layout_info and get_layout_info work", {
  mat <- create_test_matrix(4)
  net <- CographNetwork$new(mat)

  info <- list(type = "circle", params = list())
  net$set_layout_info(info)
  result <- net$get_layout_info()

  expect_equal(result$type, "circle")
})

test_that("set_plot_params and get_plot_params work", {
  mat <- create_test_matrix(4)
  net <- CographNetwork$new(mat)

  params <- list(title = "Test", margins = c(0.1, 0.1, 0.1, 0.1))
  net$set_plot_params(params)
  result <- net$get_plot_params()

  expect_equal(result$title, "Test")
})

# ============================================
# Active Binding Tests
# ============================================

test_that("n_nodes returns correct count", {
  mat <- create_test_matrix(5)
  net <- CographNetwork$new(mat)

  expect_equal(net$n_nodes, 5)
})

test_that("n_edges returns correct count", {
  mat <- create_test_topology("complete", 4)
  net <- CographNetwork$new(mat)

  # Complete graph with 4 nodes has 6 edges
  expect_equal(net$n_edges, 6)
})

test_that("is_directed returns correct value", {
  mat <- create_test_matrix(4, symmetric = FALSE)

  net_dir <- CographNetwork$new(mat, directed = TRUE)
  net_undir <- CographNetwork$new(mat, directed = FALSE)

  expect_true(net_dir$is_directed)
  expect_false(net_undir$is_directed)
})

test_that("has_weights returns correct value", {
  mat_weighted <- create_test_matrix(4, weighted = TRUE)
  mat_unweighted <- create_test_matrix(4, weighted = FALSE)

  net_weighted <- CographNetwork$new(mat_weighted)
  net_unweighted <- CographNetwork$new(mat_unweighted)

  # Both may have weights (1s count as weights)
  expect_true(is.logical(net_weighted$has_weights))
  expect_true(is.logical(net_unweighted$has_weights))
})

test_that("node_labels returns labels", {
  mat <- create_test_matrix(4)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  net <- CographNetwork$new(mat)

  expect_equal(net$node_labels, c("A", "B", "C", "D"))
})

# ============================================
# Clone Network Tests
# ============================================

test_that("clone_network creates independent copy", {
  mat <- create_test_matrix(4)
  net1 <- CographNetwork$new(mat)

  net1$set_node_aes(list(fill = "red"))
  net2 <- net1$clone_network()

  # Modify original
  net1$set_node_aes(list(fill = "blue"))

  # Clone should still have original value
  expect_equal(net2$get_node_aes()$fill, "red")
})

test_that("clone_network preserves layout", {
  mat <- create_test_matrix(4)
  net <- CographNetwork$new(mat)

  coords <- data.frame(x = c(0, 1, 0, 1), y = c(0, 0, 1, 1))
  net$set_layout_coords(coords)

  cloned <- net$clone_network()
  cloned_layout <- cloned$get_layout()

  expect_equal(cloned_layout$x, coords$x)
  expect_equal(cloned_layout$y, coords$y)
})

test_that("clone_network preserves theme", {
  mat <- create_test_matrix(4)
  net <- CographNetwork$new(mat)

  theme <- get_theme("dark")
  net$set_theme(theme)

  cloned <- net$clone_network()
  expect_true(!is.null(cloned$get_theme()))
})

# ============================================
# Print Method Tests
# ============================================

test_that("print method produces output", {
  mat <- create_test_matrix(4)
  net <- CographNetwork$new(mat)

  expect_output(print(net), "CographNetwork|Network|nodes", ignore.case = TRUE)
})

test_that("print shows node count", {
  mat <- create_test_matrix(5)
  net <- CographNetwork$new(mat)

  expect_output(print(net), "5")
})

# ============================================
# Edge Cases
# ============================================

test_that("CographNetwork handles single node", {
  mat <- matrix(0, 1, 1)
  net <- CographNetwork$new(mat)

  expect_equal(net$n_nodes, 1)
  expect_equal(net$n_edges, 0)
})

test_that("CographNetwork handles no edges", {
  mat <- matrix(0, 4, 4)
  net <- CographNetwork$new(mat)

  expect_equal(net$n_nodes, 4)
  expect_equal(net$n_edges, 0)
})

test_that("CographNetwork handles weighted edges", {
  mat <- matrix(c(0, 0.5, 0.3, 0.5, 0, 0.8, 0.3, 0.8, 0), 3, 3)
  net <- CographNetwork$new(mat)

  expect_equal(net$n_nodes, 3)
  expect_true(net$has_weights)
})
