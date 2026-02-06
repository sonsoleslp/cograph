# Tests for print methods
# Covers: R/methods-print.R

# ============================================
# print.cograph_network Tests - List-based format
# ============================================

test_that("print.cograph_network works with list-based format", {
  # Create a list-based cograph_network object
  obj <- structure(
    list(
      n_nodes = 4,
      n_edges = 6,
      directed = FALSE,
      nodes = data.frame(id = 1:4, label = LETTERS[1:4]),
      edges = data.frame(from = 1:3, to = 2:4)
    ),
    class = "cograph_network"
  )

  expect_output(print(obj), "Cograph network")
  expect_output(print(obj), "4.*nodes")
  expect_output(print(obj), "6.*edges")
  expect_output(print(obj), "undirected")
})

test_that("print.cograph_network shows directed status", {
  obj <- structure(
    list(
      n_nodes = 4,
      n_edges = 6,
      directed = TRUE,
      nodes = data.frame(id = 1:4, label = LETTERS[1:4]),
      edges = data.frame(from = 1:3, to = 2:4)
    ),
    class = "cograph_network"
  )

  expect_output(print(obj), "directed")
})

test_that("print.cograph_network shows weight range", {
  obj <- structure(
    list(
      n_nodes = 4,
      n_edges = 3,
      directed = FALSE,
      weight = c(0.5, 1.0, 0.8),
      nodes = data.frame(id = 1:4, label = LETTERS[1:4]),
      edges = data.frame(from = 1:3, to = 2:4)
    ),
    class = "cograph_network"
  )

  expect_output(print(obj), "Weights")
})

test_that("print.cograph_network shows equal weights", {
  obj <- structure(
    list(
      n_nodes = 4,
      n_edges = 3,
      directed = FALSE,
      weight = c(1.0, 1.0, 1.0),
      nodes = data.frame(id = 1:4, label = LETTERS[1:4]),
      edges = data.frame(from = 1:3, to = 2:4)
    ),
    class = "cograph_network"
  )

  expect_output(print(obj), "all equal")
})

test_that("print.cograph_network shows layout status when set", {
  obj <- structure(
    list(
      n_nodes = 4,
      n_edges = 3,
      directed = FALSE,
      nodes = data.frame(id = 1:4, label = LETTERS[1:4], x = runif(4), y = runif(4)),
      edges = data.frame(from = 1:3, to = 2:4)
    ),
    class = "cograph_network"
  )

  expect_output(print(obj), "Layout.*set")
})

test_that("print.cograph_network shows layout none when not set", {
  obj <- structure(
    list(
      n_nodes = 4,
      n_edges = 3,
      directed = FALSE,
      nodes = data.frame(id = 1:4, label = LETTERS[1:4]),
      edges = data.frame(from = 1:3, to = 2:4)
    ),
    class = "cograph_network"
  )

  expect_output(print(obj), "Layout.*none")
})

# ============================================
# print.cograph_network Tests - Attr-based format
# ============================================

test_that("print.cograph_network works with attr-based format", {
  # Create an attr-based cograph_network object
  obj <- structure(
    list(),
    n_nodes = 4,
    n_edges = 6,
    directed = FALSE,
    nodes = data.frame(id = 1:4, label = LETTERS[1:4]),
    class = "cograph_network"
  )

  expect_output(print(obj), "Cograph network")
  expect_output(print(obj), "4.*nodes")
})

test_that("print.cograph_network attr-based shows directed", {
  obj <- structure(
    list(),
    n_nodes = 4,
    n_edges = 6,
    directed = TRUE,
    nodes = data.frame(id = 1:4, label = LETTERS[1:4]),
    class = "cograph_network"
  )

  expect_output(print(obj), "directed")
})

test_that("print.cograph_network attr-based shows weights", {
  obj <- structure(
    list(weight = c(0.5, 1.0, 0.8)),
    n_nodes = 4,
    n_edges = 3,
    directed = FALSE,
    nodes = data.frame(id = 1:4, label = LETTERS[1:4]),
    class = "cograph_network"
  )

  expect_output(print(obj), "Weights")
})

test_that("print.cograph_network attr-based shows layout", {
  obj <- structure(
    list(),
    n_nodes = 4,
    n_edges = 3,
    directed = FALSE,
    nodes = data.frame(id = 1:4, label = LETTERS[1:4], x = runif(4), y = runif(4)),
    class = "cograph_network"
  )

  expect_output(print(obj), "Layout.*set")
})

# ============================================
# print.cograph_network Tests - R6 wrapper format
# ============================================

test_that("print.cograph_network works with R6 wrapper format", {
  mat <- create_test_matrix(4)
  net <- cograph(mat)

  expect_output(print(net), "Cograph|cograph")
})

test_that("print.cograph_network R6 format shows node count", {
  mat <- create_test_matrix(5)
  net <- cograph(mat)

  expect_output(print(net), "5|Nodes")
})

test_that("print.cograph_network R6 format shows edge count", {
  mat <- create_test_topology("complete", 4)
  net <- cograph(mat)

  expect_output(print(net), "edge|Edge", ignore.case = TRUE)
})

test_that("print.cograph_network R6 format shows directed", {
  mat <- create_test_matrix(4, symmetric = FALSE)
  net <- cograph(mat, directed = TRUE)

  expect_output(print(net), "directed|Directed", ignore.case = TRUE)
})

# ============================================
# print.cograph_network Tests - Edge cases
# ============================================

test_that("print.cograph_network handles empty network", {
  mat <- matrix(0, 4, 4)
  net <- cograph(mat)

  expect_output(print(net))
})

test_that("print.cograph_network handles single node", {
  mat <- matrix(0, 1, 1)
  net <- cograph(mat)

  expect_output(print(net), "1")
})

test_that("print.cograph_network handles weighted network", {
  mat <- create_test_matrix(4, weighted = TRUE)
  net <- cograph(mat)

  expect_output(print(net))
})

test_that("print returns object invisibly", {
  mat <- create_test_matrix(4)
  net <- cograph(mat)

  result <- print(net)
  expect_s3_class(result, "cograph_network")
})

test_that("print.cograph_network fallback works", {
  # Create minimal cograph_network object that doesn't match other formats
  obj <- structure(list(), class = "cograph_network")

  expect_output(print(obj), "Cograph network object")
})
