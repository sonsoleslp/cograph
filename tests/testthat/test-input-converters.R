# Tests for input converters
# Covers: R/input-qgraph.R, R/input-statnet.R, R/input-tna.R

# ============================================
# parse_qgraph Tests
# ============================================

test_that("parse_qgraph errors without qgraph package", {
  skip_if(requireNamespace("qgraph", quietly = TRUE),
          "Test only runs when qgraph is not installed")

  expect_error(
    cograph:::parse_qgraph(list()),
    "qgraph.*required"
  )
})

test_that("parse_qgraph validates input class", {
  skip_if_not_installed("qgraph")

  expect_error(
    cograph:::parse_qgraph(list(a = 1)),
    "must be a qgraph object"
  )
})

test_that("parse_qgraph converts qgraph object", {
  skip_if_not_installed("qgraph")

  mat <- create_test_matrix(4)
  q <- qgraph::qgraph(mat, DoNotPlot = TRUE)

  result <- cograph:::parse_qgraph(q)

  expect_true(is.list(result))
  expect_true(!is.null(result$nodes))
  expect_true(!is.null(result$edges))
  expect_true(!is.null(result$directed))
})

test_that("parse_qgraph handles directed parameter", {
  skip_if_not_installed("qgraph")

  mat <- create_test_matrix(4, symmetric = FALSE)
  q <- qgraph::qgraph(mat, DoNotPlot = TRUE)

  # Force undirected
  result <- cograph:::parse_qgraph(q, directed = FALSE)
  expect_false(result$directed)

  # Force directed
  result2 <- cograph:::parse_qgraph(q, directed = TRUE)
  expect_true(result2$directed)
})

test_that("parse_qgraph preserves layout", {
  skip_if_not_installed("qgraph")

  mat <- create_test_matrix(4)
  q <- qgraph::qgraph(mat, DoNotPlot = TRUE)

  result <- cograph:::parse_qgraph(q)

  # Layout should exist in nodes
  expect_true("x" %in% names(result$nodes) || is.null(q$layout))
})

test_that("parse_qgraph handles sparse edge list", {
  skip_if_not_installed("qgraph")

  # Create a sparse matrix with few edges
  mat <- matrix(0, 4, 4)
  mat[1, 2] <- 1
  mat[2, 1] <- 1
  q <- qgraph::qgraph(mat, DoNotPlot = TRUE)

  result <- cograph:::parse_qgraph(q)

  expect_true(is.list(result))
  # At least some nodes should exist
  expect_true(nrow(result$nodes) > 0)
})

test_that("parse_qgraph handles weighted networks", {
  skip_if_not_installed("qgraph")

  mat <- create_test_matrix(4, weighted = TRUE)
  q <- qgraph::qgraph(mat, DoNotPlot = TRUE)

  result <- cograph:::parse_qgraph(q)

  expect_true(!is.null(result$weights))
})

# ============================================
# parse_statnet Tests
# ============================================

test_that("parse_statnet errors without network package", {
  skip_if(requireNamespace("network", quietly = TRUE),
          "Test only runs when network is not installed")

  expect_error(
    cograph:::parse_statnet(list()),
    "network.*required"
  )
})

test_that("parse_statnet validates input class", {
  skip_if_not_installed("network")

  expect_error(
    cograph:::parse_statnet(list(a = 1)),
    "must be a network object"
  )
})

test_that("parse_statnet converts network object", {
  skip_if_not_installed("network")

  mat <- create_test_matrix(4)
  net_obj <- network::network(mat, directed = FALSE)

  result <- cograph:::parse_statnet(net_obj)

  expect_true(is.list(result))
  expect_true(!is.null(result$nodes))
  expect_true(!is.null(result$edges))
  expect_false(result$directed)
})

test_that("parse_statnet handles directed networks", {
  skip_if_not_installed("network")

  mat <- create_test_matrix(4, symmetric = FALSE)
  net_obj <- network::network(mat, directed = TRUE)

  result <- cograph:::parse_statnet(net_obj)
  expect_true(result$directed)
})

test_that("parse_statnet handles empty network", {
  skip_if_not_installed("network")

  net_obj <- network::network.initialize(4, directed = FALSE)

  result <- cograph:::parse_statnet(net_obj)

  expect_equal(nrow(result$nodes), 4)
  expect_equal(nrow(result$edges), 0)
})

test_that("parse_statnet handles forced directed parameter", {
  skip_if_not_installed("network")

  mat <- create_test_matrix(4)
  net_obj <- network::network(mat, directed = FALSE)

  # Force directed interpretation
  result <- cograph:::parse_statnet(net_obj, directed = TRUE)
  expect_true(result$directed)
})

test_that("parse_statnet gets vertex names", {
  skip_if_not_installed("network")

  mat <- create_test_matrix(4)
  net_obj <- network::network(mat, directed = FALSE)
  network::set.vertex.attribute(net_obj, "vertex.names", c("A", "B", "C", "D"))

  result <- cograph:::parse_statnet(net_obj)

  expect_equal(result$nodes$label[1], "A")
})

test_that("parse_statnet gets edge weights", {
  skip_if_not_installed("network")

  mat <- create_test_matrix(4, weighted = TRUE)
  net_obj <- network::network(mat, directed = FALSE)

  result <- cograph:::parse_statnet(net_obj)

  expect_true(!is.null(result$weights))
})

test_that("parse_statnet gets vertex attributes", {
  skip_if_not_installed("network")

  mat <- create_test_matrix(4)
  net_obj <- network::network(mat, directed = FALSE)
  network::set.vertex.attribute(net_obj, "color", c("red", "blue", "green", "yellow"))

  result <- cograph:::parse_statnet(net_obj)

  expect_true("color" %in% names(result$nodes))
})

# ============================================
# parse_tna Tests
# ============================================

test_that("parse_tna validates input class", {
  expect_error(
    cograph:::parse_tna(list(weights = matrix(0, 3, 3))),
    "must be a tna object"
  )
})

test_that("parse_tna converts tna object", {
  tna_obj <- structure(
    list(
      weights = matrix(c(0, 0.5, 0.3, 0.4, 0, 0.2, 0.3, 0.5, 0), 3, 3),
      labels = c("A", "B", "C"),
      inits = c(0.4, 0.3, 0.3)
    ),
    class = "tna"
  )

  result <- cograph:::parse_tna(tna_obj)

  expect_true(is.list(result))
  expect_true(!is.null(result$nodes))
  expect_true(!is.null(result$edges))
  expect_equal(nrow(result$nodes), 3)
})

test_that("parse_tna handles empty network", {
  tna_obj <- structure(
    list(
      weights = matrix(0, 3, 3),
      labels = c("A", "B", "C"),
      inits = NULL
    ),
    class = "tna"
  )

  result <- cograph:::parse_tna(tna_obj)

  expect_equal(nrow(result$nodes), 3)
  expect_equal(nrow(result$edges), 0)
})

test_that("parse_tna preserves inits as node attribute", {
  tna_obj <- structure(
    list(
      weights = matrix(c(0, 0.5, 0.5, 0), 2, 2),
      labels = c("X", "Y"),
      inits = c(0.6, 0.4)
    ),
    class = "tna"
  )

  result <- cograph:::parse_tna(tna_obj)

  expect_true("inits" %in% names(result$nodes))
  expect_equal(result$nodes$inits[1], 0.6)
})

test_that("parse_tna treats networks as directed by default", {
  tna_obj <- structure(
    list(
      weights = matrix(c(0, 0.5, 0.3, 0), 2, 2),
      labels = c("A", "B"),
      inits = NULL
    ),
    class = "tna"
  )

  result <- cograph:::parse_tna(tna_obj)
  expect_true(result$directed)
})

test_that("parse_tna respects directed parameter", {
  tna_obj <- structure(
    list(
      weights = matrix(c(0, 0.5, 0.5, 0), 2, 2),
      labels = c("A", "B"),
      inits = NULL
    ),
    class = "tna"
  )

  result <- cograph:::parse_tna(tna_obj, directed = FALSE)
  expect_false(result$directed)
})

test_that("parse_tna handles missing labels", {
  tna_obj <- structure(
    list(
      weights = matrix(c(0, 1, 1, 0), 2, 2),
      labels = NULL,
      inits = NULL
    ),
    class = "tna"
  )

  result <- cograph:::parse_tna(tna_obj)

  expect_equal(nrow(result$nodes), 2)
  expect_equal(result$nodes$label[1], "1")
})

test_that("parse_tna extracts edge weights correctly", {
  tna_obj <- structure(
    list(
      weights = matrix(c(0, 0.7, 0.3, 0), 2, 2),
      labels = c("A", "B"),
      inits = NULL
    ),
    class = "tna"
  )

  result <- cograph:::parse_tna(tna_obj)

  expect_true(0.7 %in% result$weights || 0.3 %in% result$weights)
})
