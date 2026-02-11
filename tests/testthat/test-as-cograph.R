# Tests for as_cograph() and S3 getter/setter functions
# Covers: R/class-network.R (as_cograph, get_nodes, get_edges, get_labels,
#          set_nodes, set_edges, set_layout, is_directed, n_nodes, n_edges, nodes)

# ============================================
# as_cograph() Constructor Tests
# ============================================

test_that("as_cograph creates from adjacency matrix", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  expect_s3_class(net, "cograph_network")
  expect_equal(net$n_nodes, 3)
  expect_equal(net$n_edges, 3)
  expect_equal(net$source, "matrix")
})

test_that("as_cograph creates from weighted matrix", {
  mat <- matrix(c(0, 0.5, 0.3, 0.5, 0, 0.8, 0.3, 0.8, 0), nrow = 3)
  net <- as_cograph(mat)

  expect_s3_class(net, "cograph_network")
  expect_equal(net$n_nodes, 3)
  expect_true(length(net$weight) > 0)
  expect_true(all(net$weight > 0))
})

test_that("as_cograph detects undirected (symmetric) matrix", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  expect_false(net$directed)
})

test_that("as_cograph detects directed (asymmetric) matrix", {
  mat <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), nrow = 3)
  net <- as_cograph(mat)

  expect_true(net$directed)
})

test_that("as_cograph force directed parameter", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat, directed = TRUE)

  expect_true(net$directed)
})

test_that("as_cograph creates from edge list data frame", {
  edges <- data.frame(from = c("A", "B"), to = c("B", "C"))
  net <- as_cograph(edges)

  expect_s3_class(net, "cograph_network")
  expect_equal(net$source, "edgelist")
  expect_true(net$n_nodes >= 2)
})

test_that("as_cograph preserves matrix row/col names as labels", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  rownames(mat) <- colnames(mat) <- c("X", "Y", "Z")
  net <- as_cograph(mat)

  expect_equal(net$labels, c("X", "Y", "Z"))
})

test_that("as_cograph returns existing cograph_network as-is", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net1 <- as_cograph(mat)
  net2 <- as_cograph(net1)

  expect_identical(net1, net2)
})

test_that("as_cograph handles empty matrix", {
  mat <- matrix(0, 3, 3)
  net <- as_cograph(mat)

  expect_s3_class(net, "cograph_network")
  expect_equal(net$n_nodes, 3)
  expect_equal(net$n_edges, 0)
})

test_that("as_cograph stores from/to/weight vectors", {
  mat <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  expect_true(is.integer(net$from) || is.numeric(net$from))
  expect_true(is.integer(net$to) || is.numeric(net$to))
  expect_true(is.numeric(net$weight))
  expect_equal(length(net$from), length(net$to))
  expect_equal(length(net$from), length(net$weight))
})

test_that("as_cograph initializes layout fields to NULL", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  expect_null(net$layout)
  expect_null(net$layout_info)
})

# ============================================
# get_nodes() Tests
# ============================================

test_that("get_nodes returns data frame from new format", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  nodes <- get_nodes(net)
  expect_true(is.data.frame(nodes))
  expect_equal(nrow(nodes), 3)
  expect_true("label" %in% names(nodes))
  expect_true("id" %in% names(nodes))
})

test_that("get_nodes returns data frame from old R6 wrapper", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(mat)

  nodes <- get_nodes(net)
  expect_true(is.data.frame(nodes))
  expect_equal(nrow(nodes), 3)
})

test_that("get_nodes errors on non-cograph object", {
  expect_error(get_nodes(list(a = 1)), "Cannot extract nodes")
})

# ============================================
# get_edges() Tests
# ============================================

test_that("get_edges returns data frame from new format", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  edges <- get_edges(net)
  expect_true(is.data.frame(edges))
  expect_true(all(c("from", "to", "weight") %in% names(edges)))
})

test_that("get_edges returns data frame from old R6 wrapper", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(mat)

  edges <- get_edges(net)
  expect_true(is.data.frame(edges))
  expect_true(nrow(edges) > 0)
})

test_that("get_edges returns empty data frame for no edges", {
  mat <- matrix(0, 3, 3)
  net <- as_cograph(mat)

  edges <- get_edges(net)
  expect_true(is.data.frame(edges))
  expect_equal(nrow(edges), 0)
})

test_that("get_edges errors on non-cograph object", {
  expect_error(get_edges(list(a = 1)), "Cannot extract edges")
})

# ============================================
# get_labels() Tests
# ============================================

test_that("get_labels returns labels from new format", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  net <- as_cograph(mat)

  labels <- get_labels(net)
  expect_equal(labels, c("A", "B", "C"))
})

test_that("get_labels returns labels from old R6 wrapper", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  rownames(mat) <- colnames(mat) <- c("X", "Y", "Z")
  net <- cograph(mat)

  labels <- get_labels(net)
  expect_equal(labels, c("X", "Y", "Z"))
})

test_that("get_labels errors on non-cograph object", {
  expect_error(get_labels(list(a = 1)), "Cannot extract labels")
})

# ============================================
# set_nodes() Tests
# ============================================

test_that("set_nodes updates nodes in cograph_network", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  new_nodes <- data.frame(id = 1:3, label = c("A", "B", "C"))
  net <- set_nodes(net, new_nodes)

  expect_equal(get_labels(net), c("A", "B", "C"))
  expect_equal(net$n_nodes, 3)
})

test_that("set_nodes adds id column if missing", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  new_nodes <- data.frame(label = c("X", "Y", "Z"))
  net <- set_nodes(net, new_nodes)

  nodes <- get_nodes(net)
  expect_true("id" %in% names(nodes))
})

test_that("set_nodes adds label column if missing", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  new_nodes <- data.frame(id = 1:3, value = c(10, 20, 30))
  net <- set_nodes(net, new_nodes)

  nodes <- get_nodes(net)
  expect_true("label" %in% names(nodes))
})

test_that("set_nodes errors on non-cograph_network", {
  expect_error(set_nodes(list(a = 1), data.frame(id = 1)), "must be a cograph_network")
})

test_that("set_nodes errors on non-data.frame", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  expect_error(set_nodes(net, list(id = 1)), "must be a data frame")
})

# ============================================
# set_edges() Tests
# ============================================

test_that("set_edges updates edges in cograph_network", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  new_edges <- data.frame(from = c(1, 2), to = c(2, 3), weight = c(0.5, 0.8))
  net <- set_edges(net, new_edges)

  expect_equal(net$n_edges, 2)
  expect_equal(net$from, c(1L, 2L))
  expect_equal(net$to, c(2L, 3L))
  expect_equal(net$weight, c(0.5, 0.8))
})

test_that("set_edges adds default weight if missing", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  new_edges <- data.frame(from = c(1, 2), to = c(2, 3))
  net <- set_edges(net, new_edges)

  expect_equal(net$weight, c(1, 1))
})

test_that("set_edges errors on missing from/to columns", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  expect_error(set_edges(net, data.frame(a = 1, b = 2)), "must have 'from' and 'to'")
})

test_that("set_edges errors on non-cograph_network", {
  expect_error(set_edges(list(), data.frame(from = 1, to = 2)), "must be a cograph_network")
})

test_that("set_edges errors on non-data.frame", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  expect_error(set_edges(net, list(from = 1, to = 2)), "must be a data frame")
})

# ============================================
# set_layout() Tests
# ============================================

test_that("set_layout updates coordinates from data frame", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  layout_df <- data.frame(x = c(0, 1, 0.5), y = c(0, 0, 1))
  net <- set_layout(net, layout_df)

  nodes <- get_nodes(net)
  expect_equal(nodes$x, c(0, 1, 0.5))
  expect_equal(nodes$y, c(0, 0, 1))
})

test_that("set_layout updates coordinates from matrix", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  layout_mat <- matrix(c(0, 1, 0.5, 0, 0, 1), ncol = 2)
  net <- set_layout(net, layout_mat)

  nodes <- get_nodes(net)
  expect_equal(nodes$x, c(0, 1, 0.5))
  expect_equal(nodes$y, c(0, 0, 1))
})

test_that("set_layout stores layout in net$layout", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  layout_df <- data.frame(x = c(0, 1, 0.5), y = c(0, 0, 1))
  net <- set_layout(net, layout_df)

  expect_true(!is.null(net$layout))
})

test_that("set_layout errors on non-cograph_network", {
  expect_error(set_layout(list(), data.frame(x = 0, y = 0)), "must be a cograph_network")
})

test_that("set_layout errors on missing x/y columns", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  expect_error(set_layout(net, data.frame(a = 1, b = 2, c = 3)),
               "must have 'x' and 'y'")
})

test_that("set_layout errors on mismatched row count", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  expect_error(set_layout(net, data.frame(x = c(0, 1), y = c(0, 1))),
               "same number of rows")
})

# ============================================
# is_directed() S3 Function Tests
# ============================================

test_that("is_directed works with new format", {
  mat <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), nrow = 3)
  net <- as_cograph(mat)

  expect_true(is_directed(net))
})

test_that("is_directed works with symmetric matrix", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  expect_false(is_directed(net))
})

test_that("is_directed works with old R6 wrapper", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(mat)

  result <- is_directed(net)
  expect_true(is.logical(result))
})

test_that("is_directed errors on non-cograph object", {
  expect_error(is_directed(list(a = 1)), "Cannot determine directedness")
})

# ============================================
# n_nodes() / n_edges() S3 Function Tests
# ============================================

test_that("n_nodes works with new format", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  expect_equal(n_nodes(net), 3)
})

test_that("n_nodes works with old R6 wrapper", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(mat)

  expect_equal(n_nodes(net), 3)
})

test_that("n_nodes errors on non-cograph object", {
  expect_error(n_nodes(list(a = 1)), "Cannot count nodes")
})

test_that("n_edges works with new format", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  expect_equal(n_edges(net), 3)
})

test_that("n_edges works with old R6 wrapper", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(mat)

  expect_equal(n_edges(net), 3)
})

test_that("n_edges errors on non-cograph object", {
  expect_error(n_edges(list(a = 1)), "Cannot count edges")
})

# ============================================
# nodes() Deprecated Wrapper Tests
# ============================================

test_that("nodes() returns same as get_nodes()", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  result1 <- nodes(net)
  result2 <- get_nodes(net)

  expect_equal(result1, result2)
})

# ============================================
# is_cograph_network() Tests
# ============================================

test_that("is_cograph_network returns TRUE for new format", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  expect_true(cograph:::is_cograph_network(net))
})

test_that("is_cograph_network returns TRUE for old R6 wrapper", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(mat)

  expect_true(cograph:::is_cograph_network(net))
})

test_that("is_cograph_network returns TRUE for R6 object", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- CographNetwork$new(mat)

  expect_true(cograph:::is_cograph_network(net))
})

test_that("is_cograph_network returns FALSE for plain list", {
  expect_false(cograph:::is_cograph_network(list(a = 1)))
})

# ============================================
# as_cograph_network() Internal Wrapper Tests
# ============================================

test_that("as_cograph_network wraps R6 object", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  r6 <- CographNetwork$new(mat)

  net <- cograph:::as_cograph_network(r6)

  expect_s3_class(net, "cograph_network")
  expect_true(!is.null(net$network))
  expect_true(!is.null(net$nodes))
  expect_true(!is.null(net$edges))
})
