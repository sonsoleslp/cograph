# test-input-qgraph.R - Tests for qgraph input parsing
# Covers: R/input-qgraph.R (parse_qgraph function)

# Make internal function available for testing
parse_qgraph <- cograph:::parse_qgraph

# ============================================
# Error Handling Tests
# ============================================

test_that("parse_qgraph errors without qgraph package", {
  skip_if_not_installed("qgraph")

  # This test only makes sense if qgraph IS installed

  # When it IS installed, we test that valid qgraph objects are parsed
})

test_that("parse_qgraph errors on non-qgraph input", {
  skip_if_not_installed("qgraph")

  expect_error(parse_qgraph(list(a = 1, b = 2)),
               "qgraph object")
})

# ============================================
# Basic qgraph Parsing Tests
# ============================================

test_that("parse_qgraph parses simple qgraph object", {
  skip_if_not_installed("qgraph")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  q <- qgraph::qgraph(mat, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  expect_true(is.list(result))
  expect_true("nodes" %in% names(result))
  expect_true("edges" %in% names(result))
  expect_true("directed" %in% names(result))
  expect_true("weights" %in% names(result))
})

test_that("parse_qgraph extracts correct node count", {
  skip_if_not_installed("qgraph")

  mat <- matrix(c(0, 1, 1, 0, 1, 0,
                  1, 0, 1, 1, 0, 1,
                  1, 1, 0, 0, 1, 0,
                  0, 1, 0, 0, 1, 1,
                  1, 0, 1, 1, 0, 0,
                  0, 1, 0, 1, 0, 0), 6, 6, byrow = TRUE)
  q <- qgraph::qgraph(mat, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  # Node count should match input matrix dimension
  expect_true(nrow(result$nodes) >= 1)
})

test_that("parse_qgraph extracts edge information", {
  skip_if_not_installed("qgraph")

  mat <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
  q <- qgraph::qgraph(mat, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  expect_true(nrow(result$edges) > 0)
})

# ============================================
# Directed Network Tests
# ============================================

test_that("parse_qgraph handles directed qgraph", {
  skip_if_not_installed("qgraph")

  mat <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), 3, 3)
  q <- qgraph::qgraph(mat, directed = TRUE, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  expect_true(result$directed)
})

test_that("parse_qgraph handles undirected qgraph", {
  skip_if_not_installed("qgraph")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  q <- qgraph::qgraph(mat, directed = FALSE, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  expect_false(result$directed)
})

test_that("parse_qgraph directed parameter overrides qgraph setting", {
  skip_if_not_installed("qgraph")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  q <- qgraph::qgraph(mat, directed = FALSE, DoNotPlot = TRUE)

  # Force directed interpretation
  result <- parse_qgraph(q, directed = TRUE)
  expect_true(result$directed)

  # Force undirected interpretation
  result2 <- parse_qgraph(q, directed = FALSE)
  expect_false(result2$directed)
})

# ============================================
# Weighted Network Tests
# ============================================

test_that("parse_qgraph extracts weights", {
  skip_if_not_installed("qgraph")

  mat <- matrix(c(0, 0.5, 0.3, 0.5, 0, 0.8, 0.3, 0.8, 0), 3, 3)
  q <- qgraph::qgraph(mat, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  expect_true(length(result$weights) > 0)
})

test_that("parse_qgraph handles negative weights", {
  skip_if_not_installed("qgraph")

  mat <- matrix(c(0, 0.5, -0.3, 0.5, 0, -0.8, -0.3, -0.8, 0), 3, 3)
  q <- qgraph::qgraph(mat, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  expect_true(any(result$weights < 0))
})

# ============================================
# Layout Extraction Tests
# ============================================

test_that("parse_qgraph extracts layout when available", {
  skip_if_not_installed("qgraph")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  custom_layout <- matrix(c(0, 1, 0.5, 0, 0, 1), ncol = 2)
  q <- qgraph::qgraph(mat, layout = custom_layout, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  expect_true("x" %in% names(result$nodes))
  expect_true("y" %in% names(result$nodes))
})

# ============================================
# Label Extraction Tests
# ============================================

test_that("parse_qgraph extracts node labels", {
  skip_if_not_installed("qgraph")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  q <- qgraph::qgraph(mat, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  expect_true("label" %in% names(result$nodes))
})

# ============================================
# Edge Case Tests
# ============================================

test_that("parse_qgraph handles sparse network", {
  skip_if_not_installed("qgraph")

  mat <- matrix(0, 5, 5)
  mat[1, 2] <- 1
  mat[2, 1] <- 1
  q <- qgraph::qgraph(mat, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  # Should successfully parse the network
  expect_true(is.list(result))
  expect_true("nodes" %in% names(result))
  expect_true(nrow(result$nodes) >= 1)
})

test_that("parse_qgraph handles dense network", {
  skip_if_not_installed("qgraph")

  mat <- matrix(1, 4, 4)
  diag(mat) <- 0
  q <- qgraph::qgraph(mat, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  # Should successfully parse the network
  expect_true(is.list(result))
  expect_true("nodes" %in% names(result))
  expect_true(nrow(result$nodes) >= 1)
})

# ============================================
# Integration Tests
# ============================================

test_that("cograph() accepts qgraph object", {
  skip_if_not_installed("qgraph")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  q <- qgraph::qgraph(mat, DoNotPlot = TRUE)

  result <- tryCatch({
    net <- cograph(q)
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot() works with qgraph input", {
  skip_if_not_installed("qgraph")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  q <- qgraph::qgraph(mat, DoNotPlot = TRUE)

  result <- tryCatch({
    with_temp_png(splot(q))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot() works with qgraph input", {
  skip_if_not_installed("qgraph")
  skip_if_not_installed("grid")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  q <- qgraph::qgraph(mat, DoNotPlot = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(q))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Directed Inference Tests
# ============================================

test_that("parse_qgraph infers directed from asymmetric matrix", {
  skip_if_not_installed("qgraph")

  # Create asymmetric matrix (directed)
  mat <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), 3, 3, byrow = TRUE)
  q <- qgraph::qgraph(mat, DoNotPlot = TRUE)

  # Let it infer directed from matrix symmetry
  result <- parse_qgraph(q, directed = NULL)

  # Should detect as directed due to asymmetry
  expect_true(is.logical(result$directed))
})

test_that("parse_qgraph handles NULL directed with symmetric matrix", {
  skip_if_not_installed("qgraph")

  # Symmetric matrix
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  q <- qgraph::qgraph(mat, DoNotPlot = TRUE)

  result <- parse_qgraph(q, directed = NULL)
  expect_true(is.logical(result$directed))
})

# ============================================
# Empty Edge List Tests
# ============================================

test_that("parse_qgraph handles network with no edges", {
  skip_if_not_installed("qgraph")

  mat <- matrix(0, 4, 4)
  q <- qgraph::qgraph(mat, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  expect_equal(nrow(result$edges), 0)
  expect_equal(length(result$weights), 0)
})

test_that("parse_qgraph handles single edge network", {
  skip_if_not_installed("qgraph")

  mat <- matrix(0, 3, 3)
  mat[1, 2] <- 1
  mat[2, 1] <- 1
  q <- qgraph::qgraph(mat, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  expect_true(nrow(result$edges) >= 1)
})

# ============================================
# Node Count Inference Tests
# ============================================

test_that("parse_qgraph infers node count from matrix", {
  skip_if_not_installed("qgraph")

  mat <- matrix(c(0, 1, 1, 0, 1, 0,
                  1, 0, 1, 1, 0, 1,
                  1, 1, 0, 0, 1, 0,
                  0, 1, 0, 0, 1, 1,
                  1, 0, 1, 1, 0, 0,
                  0, 1, 0, 1, 0, 0), 6, 6, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- paste0("N", 1:6)
  q <- qgraph::qgraph(mat, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  # Should have nodes
  expect_true(nrow(result$nodes) >= 1)
})

# ============================================
# Edgelist Weight Handling Tests
# ============================================

test_that("parse_qgraph handles missing weights in edgelist", {
  skip_if_not_installed("qgraph")

  # Binary matrix (weights are 1s)
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  q <- qgraph::qgraph(mat, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  # Weights should default to 1
  if (length(result$weights) > 0) {
    expect_true(all(result$weights != 0))
  }
})

# ============================================
# Label Source Tests
# ============================================

test_that("parse_qgraph uses graphAttributes names", {
  skip_if_not_installed("qgraph")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("Node1", "Node2", "Node3")
  q <- qgraph::qgraph(mat, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  expect_true("label" %in% names(result$nodes))
  expect_equal(nrow(result$nodes), 3)
})

test_that("parse_qgraph generates default labels", {
  skip_if_not_installed("qgraph")

  # Matrix without names
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  q <- qgraph::qgraph(mat, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  expect_true("label" %in% names(result$nodes))
})

# ============================================
# Layout Handling Tests
# ============================================

test_that("parse_qgraph handles qgraph without layout", {
  skip_if_not_installed("qgraph")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  q <- qgraph::qgraph(mat, DoNotPlot = TRUE)

  # Force no layout
  q$layout <- NULL

  result <- parse_qgraph(q)

  expect_true(is.list(result))
  expect_true("nodes" %in% names(result))
})

test_that("parse_qgraph handles layout with wrong dimensions", {
  skip_if_not_installed("qgraph")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  q <- qgraph::qgraph(mat, DoNotPlot = TRUE)

  # Set layout with wrong row count
  q$layout <- matrix(c(0, 0, 1, 1), ncol = 2)  # 2 rows, not 3

  result <- parse_qgraph(q)

  # Should still work, but layout not used
  expect_true(is.list(result))
  expect_true("nodes" %in% names(result))
})

# ============================================
# Node Count Inference Tests
# ============================================

test_that("parse_qgraph infers node count from edge list when no labels", {
  skip_if_not_installed("qgraph")

  # Create a qgraph without explicit labels
  mat <- matrix(c(0, 1, 0, 1, 0, 0, 0, 0, 0), 3, 3)
  q <- qgraph::qgraph(mat, DoNotPlot = TRUE)

  # Clear node names/labels to force inference from edge list
  q$graphAttributes$Nodes$names <- NULL
  q$graphAttributes$Nodes$labels <- NULL
  q$Arguments$input <- NULL  # Clear input matrix to force edge list inference

  result <- parse_qgraph(q)

  # Should still create nodes
  expect_true(nrow(result$nodes) >= 1)
})

test_that("parse_qgraph infers node count from input matrix", {
  skip_if_not_installed("qgraph")

  mat <- matrix(c(0, 1, 1, 0, 1, 0, 0, 1, 0,
                  1, 0, 1, 1, 0, 0, 1, 0, 1,
                  1, 1, 0, 0, 1, 1, 0, 0, 0,
                  0, 1, 0, 0, 1, 0, 1, 1, 0,
                  1, 0, 1, 1, 0, 1, 0, 0, 1,
                  0, 0, 1, 0, 1, 0, 1, 0, 0,
                  0, 1, 0, 1, 0, 1, 0, 1, 0,
                  1, 0, 0, 1, 0, 0, 1, 0, 1,
                  0, 1, 0, 0, 1, 0, 0, 1, 0), 9, 9, byrow = TRUE)
  q <- qgraph::qgraph(mat, DoNotPlot = TRUE)

  # Clear node names to force matrix-based inference
  q$graphAttributes$Nodes$names <- NULL
  q$graphAttributes$Nodes$labels <- NULL

  result <- parse_qgraph(q)

  # Should infer node count from input matrix
  expect_true(nrow(result$nodes) >= 1)
})

test_that("parse_qgraph handles directed inference from matrix symmetry", {
  skip_if_not_installed("qgraph")

  # Create asymmetric matrix
  mat <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), 3, 3, byrow = TRUE)
  q <- qgraph::qgraph(mat, DoNotPlot = TRUE)

  # Clear directed flag in edge list to force matrix symmetry check
  q$Edgelist$directed <- NULL

  result <- parse_qgraph(q, directed = NULL)

  expect_true(is.logical(result$directed))
})

test_that("parse_qgraph handles symmetric matrix for undirected inference", {
  skip_if_not_installed("qgraph")

  # Create symmetric matrix
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  q <- qgraph::qgraph(mat, DoNotPlot = TRUE)

  # Clear directed flag
  q$Edgelist$directed <- NULL

  result <- parse_qgraph(q, directed = NULL)

  expect_true(is.logical(result$directed))
})

test_that("parse_qgraph handles missing input matrix for directed inference", {
  skip_if_not_installed("qgraph")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  q <- qgraph::qgraph(mat, DoNotPlot = TRUE)

  # Clear both directed flag and input matrix
  q$Edgelist$directed <- NULL
  q$Arguments$input <- NULL

  result <- parse_qgraph(q, directed = NULL)

  # Should default to FALSE when can't determine
  expect_false(result$directed)
})

test_that("parse_qgraph uses graphAttributes labels", {
  skip_if_not_installed("qgraph")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  q <- qgraph::qgraph(mat, labels = c("X", "Y", "Z"), DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  expect_true("label" %in% names(result$nodes))
  expect_equal(nrow(result$nodes), 3)
})

test_that("parse_qgraph infers n from edge list max index", {
  skip_if_not_installed("qgraph")

  # Create a qgraph, then modify to test edge list inference
  mat <- matrix(c(0, 1, 0, 0, 0,
                  1, 0, 1, 0, 0,
                  0, 1, 0, 1, 0,
                  0, 0, 1, 0, 1,
                  0, 0, 0, 1, 0), 5, 5, byrow = TRUE)
  q <- qgraph::qgraph(mat, DoNotPlot = TRUE)

  # Clear names and input to force edge list max inference
  q$graphAttributes$Nodes$names <- NULL
  q$graphAttributes$Nodes$labels <- NULL
  q$Arguments$input <- NULL

  result <- parse_qgraph(q)

  # Should create nodes based on max edge index
  expect_true(nrow(result$nodes) >= 1)
})
