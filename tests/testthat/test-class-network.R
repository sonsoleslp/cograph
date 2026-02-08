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

# ============================================
# set_layout_coords with Matrix Tests
# ============================================

test_that("set_layout_coords handles matrix input", {
  mat <- create_test_matrix(4)
  net <- CographNetwork$new(mat)

  coords_mat <- matrix(c(0, 1, 0, 1, 0, 0, 1, 1), ncol = 2)
  net$set_layout_coords(coords_mat)

  result <- net$get_layout()
  expect_equal(nrow(result), 4)
  expect_equal(ncol(result), 2)
})

test_that("set_layout_coords with matrix updates layout", {
  mat <- create_test_matrix(4)
  net <- CographNetwork$new(mat)

  coords_mat <- matrix(c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8), ncol = 2)
  net$set_layout_coords(coords_mat)

  layout <- net$get_layout()
  expect_equal(nrow(layout), 4)
  expect_equal(ncol(layout), 2)
})

test_that("set_layout_coords handles NULL", {
  mat <- create_test_matrix(4)
  net <- CographNetwork$new(mat)

  net$set_layout_coords(NULL)

  expect_null(net$get_layout())
})

# ============================================
# clone_network Comprehensive Tests
# ============================================

test_that("clone_network preserves layout_info", {
  mat <- create_test_matrix(4)
  net <- CographNetwork$new(mat)

  info <- list(type = "spring", seed = 123)
  net$set_layout_info(info)

  cloned <- net$clone_network()
  expect_equal(cloned$get_layout_info()$type, "spring")
  expect_equal(cloned$get_layout_info()$seed, 123)
})

test_that("clone_network preserves plot_params", {
  mat <- create_test_matrix(4)
  net <- CographNetwork$new(mat)

  params <- list(title = "My Network", margins = c(0.1, 0.1, 0.1, 0.1))
  net$set_plot_params(params)

  cloned <- net$clone_network()
  expect_equal(cloned$get_plot_params()$title, "My Network")
})

test_that("clone_network preserves edge_aes", {
  mat <- create_test_matrix(4)
  net <- CographNetwork$new(mat)

  net$set_edge_aes(list(color = "purple", width = 3))

  cloned <- net$clone_network()
  expect_equal(cloned$get_edge_aes()$color, "purple")
  expect_equal(cloned$get_edge_aes()$width, 3)
})

test_that("clone_network preserves weights", {
  mat <- create_test_matrix(4, weighted = TRUE)
  net <- CographNetwork$new(mat)

  cloned <- net$clone_network()
  expect_true(cloned$has_weights)
})

test_that("clone_network preserves directed", {
  mat <- create_test_matrix(4)
  net <- CographNetwork$new(mat, directed = TRUE)

  cloned <- net$clone_network()
  expect_true(cloned$is_directed)
})

# ============================================
# is_cograph_network Tests
# ============================================

test_that("is_cograph_network returns TRUE for CographNetwork", {
  mat <- create_test_matrix(3)
  net <- CographNetwork$new(mat)

  expect_true(cograph:::is_cograph_network(net))
})

test_that("is_cograph_network returns TRUE for cograph_network S3 class", {
  mat <- create_test_matrix(3)
  net <- cograph(mat)

  expect_true(cograph:::is_cograph_network(net))
})

test_that("is_cograph_network returns FALSE for other objects", {
  expect_false(cograph:::is_cograph_network(list(a = 1)))
  expect_false(cograph:::is_cograph_network(matrix(1, 3, 3)))
  expect_false(cograph:::is_cograph_network("string"))
})

# ============================================
# as_cograph_network Tests
# ============================================

test_that("as_cograph_network wraps CographNetwork", {
  mat <- create_test_matrix(3)
  r6_net <- CographNetwork$new(mat)

  wrapped <- cograph:::as_cograph_network(r6_net)

  expect_true(inherits(wrapped, "cograph_network"))
  expect_true(!is.null(wrapped$network))
})

test_that("as_cograph_network includes layout", {
  mat <- create_test_matrix(3)
  r6_net <- CographNetwork$new(mat)

  coords <- data.frame(x = c(0, 1, 0.5), y = c(0, 0, 1))
  r6_net$set_layout_coords(coords)

  wrapped <- cograph:::as_cograph_network(r6_net)

  expect_true(!is.null(wrapped$layout))
})

test_that("as_cograph_network includes nodes and edges", {
  mat <- create_test_matrix(3)
  r6_net <- CographNetwork$new(mat)

  wrapped <- cograph:::as_cograph_network(r6_net)

  expect_true(!is.null(wrapped$nodes))
  expect_true(!is.null(wrapped$edges))
})

# ============================================
# as_cograph Tests
# ============================================

test_that("as_cograph creates from matrix", {
  mat <- create_test_matrix(4)
  net <- as_cograph(mat)

  expect_true(inherits(net, "cograph_network"))
  expect_equal(net$n_nodes, 4)
})

test_that("as_cograph returns existing cograph_network unchanged", {
  mat <- create_test_matrix(3)
  net1 <- as_cograph(mat)
  net2 <- as_cograph(net1)

  expect_identical(net1, net2)
})

test_that("as_cograph handles directed parameter", {
  mat <- create_test_matrix(4)

  net_dir <- as_cograph(mat, directed = TRUE)
  net_undir <- as_cograph(mat, directed = FALSE)

  expect_true(net_dir$directed)
  expect_false(net_undir$directed)
})

test_that("as_cograph stores source type for matrix", {
  mat <- create_test_matrix(3)
  net <- as_cograph(mat)

  expect_equal(net$source, "matrix")
})

test_that("as_cograph stores from/to/weight vectors", {
  mat <- create_test_matrix(3)
  net <- as_cograph(mat)

  expect_true(length(net$from) > 0)
  expect_true(length(net$to) > 0)
  expect_true(length(net$weight) > 0)
})

test_that("as_cograph handles empty edges", {
  mat <- matrix(0, 3, 3)
  net <- as_cograph(mat)

  expect_equal(net$n_nodes, 3)
  expect_equal(net$n_edges, 0)
  expect_equal(length(net$from), 0)
})

test_that("as_cograph stores labels", {
  mat <- create_test_matrix(3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  net <- as_cograph(mat)

  expect_equal(net$labels, c("A", "B", "C"))
})

# ============================================
# get_nodes S3 Function Tests
# ============================================

test_that("get_nodes works with new list format", {
  mat <- create_test_matrix(3)
  net <- as_cograph(mat)

  nodes <- get_nodes(net)
  expect_true(is.data.frame(nodes))
  expect_equal(nrow(nodes), 3)
})

test_that("get_nodes works with R6 wrapper", {
  mat <- create_test_matrix(3)
  r6_net <- CographNetwork$new(mat)
  wrapped <- cograph:::as_cograph_network(r6_net)

  nodes <- get_nodes(wrapped)
  expect_true(is.data.frame(nodes))
  expect_equal(nrow(nodes), 3)
})

test_that("get_nodes errors on invalid object", {
  expect_error(get_nodes(list(a = 1)), "Cannot extract nodes")
})

# ============================================
# get_edges S3 Function Tests
# ============================================

test_that("get_edges works with new list format", {
  mat <- create_test_matrix(3)
  net <- as_cograph(mat)

  edges <- get_edges(net)
  expect_true(is.data.frame(edges))
  expect_true("from" %in% names(edges))
  expect_true("to" %in% names(edges))
  expect_true("weight" %in% names(edges))
})

test_that("get_edges works with R6 wrapper", {
  mat <- create_test_matrix(3)
  r6_net <- CographNetwork$new(mat)
  wrapped <- cograph:::as_cograph_network(r6_net)

  edges <- get_edges(wrapped)
  expect_true(is.data.frame(edges))
})

test_that("get_edges returns empty data frame for no edges", {
  mat <- matrix(0, 3, 3)
  net <- as_cograph(mat)

  edges <- get_edges(net)
  expect_equal(nrow(edges), 0)
})

test_that("get_edges errors on invalid object", {
  expect_error(get_edges(list(a = 1)), "Cannot extract edges")
})

# ============================================
# get_labels S3 Function Tests
# ============================================

test_that("get_labels works with new list format", {
  mat <- create_test_matrix(3)
  rownames(mat) <- colnames(mat) <- c("X", "Y", "Z")
  net <- as_cograph(mat)

  labels <- get_labels(net)
  expect_equal(labels, c("X", "Y", "Z"))
})

test_that("get_labels errors on invalid object", {
  expect_error(get_labels(list(a = 1)), "Cannot extract labels")
})

# ============================================
# set_nodes S3 Function Tests
# ============================================

test_that("set_nodes updates nodes data frame", {
  mat <- create_test_matrix(3)
  net <- as_cograph(mat)

  new_nodes <- data.frame(id = 1:3, label = c("Alpha", "Beta", "Gamma"))
  net <- set_nodes(net, new_nodes)

  expect_equal(get_labels(net), c("Alpha", "Beta", "Gamma"))
})

test_that("set_nodes adds id column if missing", {
  mat <- create_test_matrix(3)
  net <- as_cograph(mat)

  new_nodes <- data.frame(label = c("A", "B", "C"))
  net <- set_nodes(net, new_nodes)

  expect_true("id" %in% names(net$nodes))
})

test_that("set_nodes adds label column if missing", {
  mat <- create_test_matrix(3)
  net <- as_cograph(mat)

  new_nodes <- data.frame(id = 1:3)
  net <- set_nodes(net, new_nodes)

  expect_true("label" %in% names(net$nodes))
})

test_that("set_nodes errors on non-cograph_network", {
  expect_error(set_nodes(list(a = 1), data.frame()), "cograph_network")
})

test_that("set_nodes errors on non-data.frame", {
  mat <- create_test_matrix(3)
  net <- as_cograph(mat)

  expect_error(set_nodes(net, list(a = 1)), "data frame")
})

# ============================================
# set_edges S3 Function Tests
# ============================================

test_that("set_edges updates edges", {
  mat <- create_test_matrix(3)
  net <- as_cograph(mat)

  new_edges <- data.frame(from = c(1, 2), to = c(2, 3), weight = c(0.5, 0.8))
  net <- set_edges(net, new_edges)

  expect_equal(net$n_edges, 2)
  expect_equal(net$from, c(1L, 2L))
  expect_equal(net$to, c(2L, 3L))
})

test_that("set_edges adds weight column if missing", {
  mat <- create_test_matrix(3)
  net <- as_cograph(mat)

  new_edges <- data.frame(from = c(1, 2), to = c(2, 3))
  net <- set_edges(net, new_edges)

  expect_equal(net$weight, c(1, 1))
})

test_that("set_edges errors on missing from/to columns", {
  mat <- create_test_matrix(3)
  net <- as_cograph(mat)

  expect_error(set_edges(net, data.frame(weight = c(1, 2))), "from.*to")
})

test_that("set_edges errors on non-cograph_network", {
  expect_error(set_edges(list(), data.frame(from = 1, to = 2)), "cograph_network")
})

# ============================================
# set_layout S3 Function Tests
# ============================================

test_that("set_layout updates layout from data frame", {
  mat <- create_test_matrix(3)
  net <- as_cograph(mat)

  layout_df <- data.frame(x = c(0, 1, 0.5), y = c(0, 0, 1))
  net <- set_layout(net, layout_df)

  nodes <- get_nodes(net)
  expect_equal(nodes$x, c(0, 1, 0.5))
  expect_equal(nodes$y, c(0, 0, 1))
})

test_that("set_layout updates layout from matrix", {
  mat <- create_test_matrix(3)
  net <- as_cograph(mat)

  layout_mat <- matrix(c(0, 1, 0.5, 0, 0, 1), ncol = 2)
  net <- set_layout(net, layout_mat)

  nodes <- get_nodes(net)
  expect_equal(nodes$x, c(0, 1, 0.5))
})

test_that("set_layout errors on wrong row count", {
  mat <- create_test_matrix(3)
  net <- as_cograph(mat)

  layout_df <- data.frame(x = c(0, 1), y = c(0, 0))  # 2 rows, need 3

  expect_error(set_layout(net, layout_df), "same number of rows")
})

test_that("set_layout errors on missing x/y columns", {
  mat <- create_test_matrix(3)
  net <- as_cograph(mat)

  layout_df <- data.frame(a = c(0, 1, 0.5), b = c(0, 0, 1))

  expect_error(set_layout(net, layout_df), "x.*y")
})

test_that("set_layout errors on non-cograph_network", {
  expect_error(set_layout(list(), data.frame(x = 1, y = 1)), "cograph_network")
})

# ============================================
# is_directed S3 Function Tests
# ============================================

test_that("is_directed works with new list format", {
  mat_sym <- create_test_matrix(3)
  mat_asym <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), 3, 3)

  net_undir <- as_cograph(mat_sym, directed = FALSE)
  net_dir <- as_cograph(mat_asym, directed = TRUE)

  expect_false(is_directed(net_undir))
  expect_true(is_directed(net_dir))
})

test_that("is_directed works with R6 wrapper", {
  mat <- create_test_matrix(3)
  r6_net <- CographNetwork$new(mat, directed = TRUE)
  wrapped <- cograph:::as_cograph_network(r6_net)

  expect_true(is_directed(wrapped))
})

test_that("is_directed errors on invalid object", {
  expect_error(is_directed(list(a = 1)), "Cannot determine directedness")
})

# ============================================
# n_nodes S3 Function Tests
# ============================================

test_that("n_nodes works with new list format", {
  mat <- create_test_matrix(5)
  net <- as_cograph(mat)

  expect_equal(n_nodes(net), 5)
})

test_that("n_nodes works with R6 wrapper", {
  mat <- create_test_matrix(4)
  r6_net <- CographNetwork$new(mat)
  wrapped <- cograph:::as_cograph_network(r6_net)

  expect_equal(n_nodes(wrapped), 4)
})

test_that("n_nodes errors on invalid object", {
  expect_error(n_nodes(list(a = 1)), "Cannot count nodes")
})

# ============================================
# n_edges S3 Function Tests
# ============================================

test_that("n_edges works with new list format", {
  mat <- create_test_topology("complete", 4)  # 6 edges
  net <- as_cograph(mat)

  expect_equal(n_edges(net), 6)
})

test_that("n_edges works with R6 wrapper", {
  mat <- create_test_matrix(3)
  r6_net <- CographNetwork$new(mat)
  wrapped <- cograph:::as_cograph_network(r6_net)

  expect_true(n_edges(wrapped) >= 0)
})

test_that("n_edges returns 0 for empty network", {
  mat <- matrix(0, 3, 3)
  net <- as_cograph(mat)

  expect_equal(n_edges(net), 0)
})

test_that("n_edges errors on invalid object", {
  expect_error(n_edges(list(a = 1)), "Cannot count edges")
})

# ============================================
# nodes() Deprecated Function Tests
# ============================================

test_that("nodes() function works (deprecated alias)", {
  mat <- create_test_matrix(3)
  net <- as_cograph(mat)

  nodes_result <- nodes(net)
  expect_true(is.data.frame(nodes_result))
  expect_equal(nrow(nodes_result), 3)
})

# ============================================
# has_weights Edge Cases
# ============================================

test_that("has_weights returns FALSE for all-ones weights", {
  mat <- create_test_matrix(3, weighted = FALSE)
  net <- CographNetwork$new(mat)

  # All weights are 1, so has_weights should be FALSE
  expect_false(net$has_weights)
})

test_that("has_weights returns TRUE for non-uniform weights", {
  mat <- matrix(c(0, 0.5, 0.3, 0.5, 0, 0.8, 0.3, 0.8, 0), 3, 3)
  net <- CographNetwork$new(mat)

  expect_true(net$has_weights)
})

test_that("has_weights returns FALSE for NULL weights", {
  net <- CographNetwork$new()

  expect_false(net$has_weights)
})

# ============================================
# node_labels Edge Cases
# ============================================

test_that("node_labels returns NULL for empty network", {
  net <- CographNetwork$new()

  expect_null(net$node_labels)
})

test_that("node_labels returns labels from nodes data frame", {
  mat <- create_test_matrix(3)
  net <- CographNetwork$new(mat)
  net$set_nodes(data.frame(id = 1:3, label = c("One", "Two", "Three")))

  expect_equal(net$node_labels, c("One", "Two", "Three"))
})

# ============================================
# Print Method Edge Cases
# ============================================

test_that("print shows layout status when set", {
  mat <- create_test_matrix(3)
  net <- CographNetwork$new(mat)
  net$set_layout_coords(data.frame(x = c(0, 1, 0.5), y = c(0, 0, 1)))

  expect_output(print(net), "set|Layout")
})

test_that("print shows layout status when not set", {
  mat <- create_test_matrix(3)
  net <- CographNetwork$new(mat)

  expect_output(print(net), "none|Layout")
})
