# Exhaustive tests for input-parse.R internal functions
# Covers: R/input-parse.R

# ============================================
# parse_input Tests
# ============================================

test_that("parse_input handles matrix input", {
  mat <- create_test_matrix(4)
  result <- parse_input(mat)

  expect_true(is.list(result))
  expect_true("nodes" %in% names(result))
  expect_true("edges" %in% names(result))
})

test_that("parse_input handles matrix with directed parameter", {
  mat <- create_test_matrix(4)

  result_dir <- parse_input(mat, directed = TRUE)
  result_undir <- parse_input(mat, directed = FALSE)

  expect_true(result_dir$directed)
  expect_false(result_undir$directed)
})

test_that("parse_input handles data.frame edge list", {
  edges_df <- data.frame(
    from = c(1, 2, 3),
    to = c(2, 3, 1),
    weight = c(0.5, 0.7, 0.3)
  )

  result <- parse_input(edges_df)

  expect_true(is.list(result))
  expect_true("edges" %in% names(result))
})

test_that("parse_input handles igraph objects", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(5)
  result <- parse_input(g)

  expect_true(is.list(result))
  expect_true("nodes" %in% names(result))
})

test_that("parse_input handles already parsed format", {
  parsed_input <- list(
    nodes = data.frame(id = 1:3, label = c("A", "B", "C")),
    edges = data.frame(from = c(1, 2), to = c(2, 3), weight = c(1, 1)),
    directed = FALSE
  )

  result <- parse_input(parsed_input)

  expect_equal(result, parsed_input)
})

test_that("parse_input errors on unsupported input", {
  expect_error(
    parse_input("not a valid input"),
    "Unsupported input type"
  )

  expect_error(
    parse_input(123),
    "Unsupported input type"
  )
})

# ============================================
# is_symmetric_matrix Tests
# ============================================

test_that("is_symmetric_matrix detects symmetric matrices", {
  sym_mat <- matrix(c(0, 1, 1, 0), 2, 2)
  expect_true(is_symmetric_matrix(sym_mat))
})

test_that("is_symmetric_matrix detects non-symmetric matrices", {
  asym_mat <- matrix(c(0, 1, 0, 0), 2, 2)
  expect_false(is_symmetric_matrix(asym_mat))
})

test_that("is_symmetric_matrix returns FALSE for non-matrices", {
  expect_false(is_symmetric_matrix(c(1, 2, 3)))
  expect_false(is_symmetric_matrix(data.frame(a = 1:3)))
})

test_that("is_symmetric_matrix returns FALSE for non-square matrices", {
  rect_mat <- matrix(1:6, 2, 3)
  expect_false(is_symmetric_matrix(rect_mat))
})

test_that("is_symmetric_matrix handles tolerance", {
  # Matrix with small differences
  mat <- matrix(c(0, 1, 1 + 1e-10, 0), 2, 2)
  expect_true(is_symmetric_matrix(mat))
})

test_that("is_symmetric_matrix handles larger matrices", {
  n <- 10
  mat <- matrix(runif(n^2), n, n)
  mat <- mat + t(mat)  # Make symmetric

  expect_true(is_symmetric_matrix(mat))
})

# ============================================
# create_nodes_df Tests
# ============================================

test_that("create_nodes_df creates correct structure", {
  result <- create_nodes_df(n = 4)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 4)
  expect_true("id" %in% names(result))
  expect_true("label" %in% names(result))
  expect_true("name" %in% names(result))
  expect_true("x" %in% names(result))
  expect_true("y" %in% names(result))
})

test_that("create_nodes_df uses provided labels", {
  labels <- c("A", "B", "C")
  result <- create_nodes_df(n = 3, labels = labels)

  expect_equal(result$label, labels)
})

test_that("create_nodes_df uses provided names", {
  labels <- c("A", "B", "C")
  names <- c("Node A", "Node B", "Node C")
  result <- create_nodes_df(n = 3, labels = labels, names = names)

  expect_equal(result$label, labels)
  expect_equal(result$name, names)
})

test_that("create_nodes_df defaults names to labels", {
  labels <- c("X", "Y", "Z")
  result <- create_nodes_df(n = 3, labels = labels)

  expect_equal(result$name, labels)
})

test_that("create_nodes_df generates numeric labels when not provided", {
  result <- create_nodes_df(n = 5)

  expect_equal(result$label, as.character(1:5))
})

# ============================================
# create_edges_df Tests
# ============================================

test_that("create_edges_df creates correct structure", {
  result <- create_edges_df(from = c(1, 2), to = c(2, 3))

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_true("from" %in% names(result))
  expect_true("to" %in% names(result))
  expect_true("weight" %in% names(result))
})

test_that("create_edges_df uses provided weights", {
  weights <- c(0.5, 0.7, 0.3)
  result <- create_edges_df(from = c(1, 2, 3), to = c(2, 3, 1), weight = weights)

  expect_equal(result$weight, weights)
})

test_that("create_edges_df defaults to weight 1", {
  result <- create_edges_df(from = c(1, 2), to = c(2, 3))

  expect_equal(result$weight, c(1, 1))
})

test_that("create_edges_df handles single edge", {
  result <- create_edges_df(from = 1, to = 2, weight = 0.8)

  expect_equal(nrow(result), 1)
  expect_equal(result$from, 1)
  expect_equal(result$to, 2)
  expect_equal(result$weight, 0.8)
})

# ============================================
# detect_duplicate_edges Tests
# ============================================

test_that("detect_duplicate_edges finds duplicates", {
  edges <- data.frame(
    from = c(1, 1, 2),
    to = c(2, 2, 3)
  )

  result <- detect_duplicate_edges(edges)

  expect_true(result$has_duplicates)
  expect_true(is.list(result$info))
})

test_that("detect_duplicate_edges handles no duplicates", {
  edges <- data.frame(
    from = c(1, 2, 3),
    to = c(2, 3, 4)
  )

  result <- detect_duplicate_edges(edges)

  expect_false(result$has_duplicates)
  expect_null(result$info)
})

test_that("detect_duplicate_edges handles NULL input", {
  result <- detect_duplicate_edges(NULL)

  expect_false(result$has_duplicates)
  expect_null(result$info)
})

test_that("detect_duplicate_edges handles empty data.frame", {
  edges <- data.frame(from = integer(0), to = integer(0))

  result <- detect_duplicate_edges(edges)

  expect_false(result$has_duplicates)
})

test_that("detect_duplicate_edges treats undirected edges as same", {
  # 1->2 and 2->1 should be considered duplicates
  edges <- data.frame(
    from = c(1, 2),
    to = c(2, 1),
    weight = c(0.5, 0.7)
  )

  result <- detect_duplicate_edges(edges)

  expect_true(result$has_duplicates)
  expect_equal(result$info[[1]]$count, 2)
  expect_equal(result$info[[1]]$weights, c(0.5, 0.7))
})

test_that("detect_duplicate_edges returns correct node pairs", {
  edges <- data.frame(
    from = c(3, 1, 3),
    to = c(4, 2, 4)
  )

  result <- detect_duplicate_edges(edges)

  expect_true(result$has_duplicates)
  expect_equal(result$info[[1]]$nodes, c(3, 4))
  expect_equal(result$info[[1]]$count, 2)
})

# ============================================
# aggregate_duplicate_edges Tests
# ============================================

test_that("aggregate_duplicate_edges with sum method", {
  edges <- data.frame(
    from = c(1, 1),
    to = c(2, 2),
    weight = c(0.5, 0.3)
  )

  result <- aggregate_duplicate_edges(edges, method = "sum")

  expect_equal(nrow(result), 1)
  expect_equal(result$weight, 0.8)
})

test_that("aggregate_duplicate_edges with mean method", {
  edges <- data.frame(
    from = c(1, 1),
    to = c(2, 2),
    weight = c(0.5, 0.3)
  )

  result <- aggregate_duplicate_edges(edges, method = "mean")

  expect_equal(nrow(result), 1)
  expect_equal(result$weight, 0.4)
})

test_that("aggregate_duplicate_edges with max method", {
  edges <- data.frame(
    from = c(1, 1),
    to = c(2, 2),
    weight = c(0.5, 0.3)
  )

  result <- aggregate_duplicate_edges(edges, method = "max")

  expect_equal(result$weight, 0.5)
})

test_that("aggregate_duplicate_edges with min method", {
  edges <- data.frame(
    from = c(1, 1),
    to = c(2, 2),
    weight = c(0.5, 0.3)
  )

  result <- aggregate_duplicate_edges(edges, method = "min")

  expect_equal(result$weight, 0.3)
})

test_that("aggregate_duplicate_edges with first method", {
  edges <- data.frame(
    from = c(1, 1),
    to = c(2, 2),
    weight = c(0.5, 0.3)
  )

  result <- aggregate_duplicate_edges(edges, method = "first")

  expect_equal(result$weight, 0.5)
})

test_that("aggregate_duplicate_edges with custom function", {
  edges <- data.frame(
    from = c(1, 1),
    to = c(2, 2),
    weight = c(3, 4)
  )

  result <- aggregate_duplicate_edges(edges, method = function(x) sqrt(sum(x^2)))

  expect_equal(result$weight, 5)  # sqrt(3^2 + 4^2) = 5
})

test_that("aggregate_duplicate_edges errors on unknown method", {
  edges <- data.frame(
    from = c(1, 1),
    to = c(2, 2),
    weight = c(0.5, 0.3)
  )

  expect_error(
    aggregate_duplicate_edges(edges, method = "unknown"),
    "Unknown aggregation method"
  )
})

test_that("aggregate_duplicate_edges handles NULL input", {
  result <- aggregate_duplicate_edges(NULL)
  expect_null(result)
})

test_that("aggregate_duplicate_edges handles empty data.frame", {
  edges <- data.frame(from = integer(0), to = integer(0), weight = numeric(0))
  result <- aggregate_duplicate_edges(edges)

  expect_equal(nrow(result), 0)
})

test_that("aggregate_duplicate_edges uses canonical ordering", {
  # Edges 1->2 and 2->1 should merge to 1-2
  edges <- data.frame(
    from = c(2, 1),
    to = c(1, 2),
    weight = c(0.5, 0.5)
  )

  result <- aggregate_duplicate_edges(edges, method = "sum")

  expect_equal(nrow(result), 1)
  expect_equal(result$from, 1)  # Lower index first
  expect_equal(result$to, 2)
  expect_equal(result$weight, 1.0)
})

test_that("aggregate_duplicate_edges handles multiple groups", {
  edges <- data.frame(
    from = c(1, 1, 2, 2),
    to = c(2, 2, 3, 3),
    weight = c(0.3, 0.2, 0.5, 0.4)
  )

  result <- aggregate_duplicate_edges(edges, method = "sum")

  expect_equal(nrow(result), 2)
})

# ============================================
# Integration Tests
# ============================================

test_that("parse_input with cograph works end-to-end", {
  mat <- create_test_matrix(4)
  parsed <- parse_input(mat)

  net <- cograph(mat)

  expect_true(inherits(net, "list"))
  expect_true(!is.null(net$network))
})

test_that("parse_input handles weighted matrices", {
  mat <- create_test_matrix(4, weighted = TRUE)
  result <- parse_input(mat)

  expect_true(any(result$edges$weight != 1))
})

test_that("parse_input handles asymmetric matrices for directed", {
  mat <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), 3, 3, byrow = TRUE)
  result <- parse_input(mat, directed = TRUE)

  expect_true(result$directed)
})

# ============================================
# igraph Additional Tests
# ============================================

test_that("parse_igraph handles weighted igraph", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(5)
  igraph::E(g)$weight <- c(0.5, 0.3, 0.8, 0.6, 0.4)

  result <- cograph:::parse_igraph(g)

  expect_equal(length(result$edges$weight), igraph::ecount(g))
})

test_that("parse_igraph handles igraph with no vertex names", {
  skip_if_not_installed("igraph")

  g <- igraph::graph_from_edgelist(matrix(c(1, 2, 2, 3, 3, 1), ncol = 2, byrow = TRUE))

  result <- cograph:::parse_igraph(g)

  expect_true(all(result$nodes$label == c("1", "2", "3")))
})

test_that("parse_igraph handles directed igraph", {
  skip_if_not_installed("igraph")

  g <- igraph::graph_from_edgelist(matrix(c(1, 2, 2, 3, 3, 1), ncol = 2, byrow = TRUE),
                                   directed = TRUE)

  result <- cograph:::parse_igraph(g)

  expect_true(result$directed)
})

test_that("parse_igraph extracts vertex attributes", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(3)
  igraph::V(g)$color <- c("red", "blue", "green")
  igraph::V(g)$size <- c(10, 20, 15)

  result <- cograph:::parse_igraph(g)

  expect_true("color" %in% names(result$nodes))
  expect_true("size" %in% names(result$nodes))
})

test_that("parse_igraph extracts edge attributes", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(3)
  igraph::E(g)$color <- c("red", "blue", "green")
  igraph::E(g)$width <- c(1, 2, 3)

  result <- cograph:::parse_igraph(g)

  expect_true("color" %in% names(result$edges))
  expect_true("width" %in% names(result$edges))
})

test_that("parse_igraph handles empty igraph", {
  skip_if_not_installed("igraph")

  g <- igraph::make_empty_graph(3)

  result <- cograph:::parse_igraph(g)

  expect_equal(nrow(result$nodes), 3)
  expect_equal(nrow(result$edges), 0)
})
