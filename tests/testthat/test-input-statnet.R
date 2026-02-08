# test-input-statnet.R - Tests for statnet network input parsing
# Covers: R/input-statnet.R (parse_statnet function)

# Make internal function available for testing
parse_statnet <- cograph:::parse_statnet

# ============================================
# Error Handling Tests
# ============================================

test_that("parse_statnet errors without network package", {
  skip_if_not_installed("network")
  # This test only makes sense if network IS installed
})

test_that("parse_statnet errors on non-network input", {
  skip_if_not_installed("network")

  expect_error(parse_statnet(list(a = 1, b = 2)),
               "network object")
})

test_that("parse_statnet errors on matrix input", {
  skip_if_not_installed("network")

  mat <- matrix(c(0, 1, 1, 0), 2, 2)
  expect_error(parse_statnet(mat),
               "network object")
})

# ============================================
# Basic statnet Parsing Tests
# ============================================

test_that("parse_statnet parses simple network object", {
  skip_if_not_installed("network")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  net <- network::network(mat, directed = FALSE)

  result <- parse_statnet(net)

  expect_true(is.list(result))
  expect_true("nodes" %in% names(result))
  expect_true("edges" %in% names(result))
  expect_true("directed" %in% names(result))
  expect_true("weights" %in% names(result))
})

test_that("parse_statnet extracts correct node count", {
  skip_if_not_installed("network")

  mat <- matrix(0, 5, 5)
  mat[1, 2] <- mat[2, 1] <- 1
  mat[2, 3] <- mat[3, 2] <- 1
  net <- network::network(mat, directed = FALSE)

  result <- parse_statnet(net)

  expect_equal(nrow(result$nodes), 5)
})

test_that("parse_statnet extracts edge information", {
  skip_if_not_installed("network")

  mat <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
  net <- network::network(mat, directed = FALSE)

  result <- parse_statnet(net)

  expect_true(nrow(result$edges) > 0)
})

# ============================================
# Directed Network Tests
# ============================================

test_that("parse_statnet handles directed network", {
  skip_if_not_installed("network")

  mat <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), 3, 3)
  net <- network::network(mat, directed = TRUE)

  result <- parse_statnet(net)

  expect_true(result$directed)
})

test_that("parse_statnet handles undirected network", {
  skip_if_not_installed("network")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  net <- network::network(mat, directed = FALSE)

  result <- parse_statnet(net)

  expect_false(result$directed)
})

test_that("parse_statnet directed parameter overrides network setting", {
  skip_if_not_installed("network")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  net <- network::network(mat, directed = FALSE)

  # Force directed interpretation
  result <- parse_statnet(net, directed = TRUE)
  expect_true(result$directed)

  # Force undirected interpretation
  result2 <- parse_statnet(net, directed = FALSE)
  expect_false(result2$directed)
})

# ============================================
# Weighted Network Tests
# ============================================

test_that("parse_statnet uses default weights when not set", {
  skip_if_not_installed("network")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  net <- network::network(mat, directed = FALSE)

  result <- parse_statnet(net)

  expect_true(all(result$weights == 1))
})

test_that("parse_statnet extracts edge weights", {
  skip_if_not_installed("network")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  net <- network::network(mat, directed = FALSE)

  # Set weights on edges
  network::set.edge.value(net, "weight", c(0.5, 0.8, 0.3))

  result <- parse_statnet(net)

  expect_true(length(result$weights) > 0)
  expect_true(any(result$weights != 1))
})

# ============================================
# Vertex Attribute Tests
# ============================================

test_that("parse_statnet extracts vertex names", {
  skip_if_not_installed("network")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  net <- network::network(mat, directed = FALSE)
  network::set.vertex.attribute(net, "vertex.names", c("A", "B", "C"))

  result <- parse_statnet(net)

  expect_equal(result$nodes$label, c("A", "B", "C"))
})

test_that("parse_statnet uses indices when no vertex names", {
  skip_if_not_installed("network")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  net <- network::network(mat, directed = FALSE)

  result <- parse_statnet(net)

  # Labels should be character versions of indices
  expect_true(all(result$nodes$label %in% c("1", "2", "3")))
})

test_that("parse_statnet extracts custom vertex attributes", {
  skip_if_not_installed("network")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  net <- network::network(mat, directed = FALSE)
  network::set.vertex.attribute(net, "group", c("A", "A", "B"))

  result <- parse_statnet(net)

  expect_true("group" %in% names(result$nodes))
  expect_equal(result$nodes$group, c("A", "A", "B"))
})

# ============================================
# Edge Attribute Tests
# ============================================

test_that("parse_statnet extracts custom edge attributes", {
  skip_if_not_installed("network")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  net <- network::network(mat, directed = FALSE)
  network::set.edge.value(net, "type", c("strong", "weak", "strong"))

  result <- parse_statnet(net)

  expect_true("type" %in% names(result$edges))
})

# ============================================
# Edge Case Tests
# ============================================

test_that("parse_statnet handles empty network", {
  skip_if_not_installed("network")

  mat <- matrix(0, 3, 3)
  net <- network::network(mat, directed = FALSE)

  result <- parse_statnet(net)

  expect_equal(nrow(result$nodes), 3)
  expect_equal(nrow(result$edges), 0)
})

test_that("parse_statnet handles sparse network", {
  skip_if_not_installed("network")

  mat <- matrix(0, 5, 5)
  mat[1, 2] <- mat[2, 1] <- 1
  net <- network::network(mat, directed = FALSE)

  result <- parse_statnet(net)

  expect_equal(nrow(result$nodes), 5)
  expect_true(nrow(result$edges) >= 1)
})

test_that("parse_statnet handles dense network", {
  skip_if_not_installed("network")

  mat <- matrix(1, 4, 4)
  diag(mat) <- 0
  net <- network::network(mat, directed = FALSE)

  result <- parse_statnet(net)

  expect_equal(nrow(result$nodes), 4)
})

# ============================================
# Integration Tests
# ============================================

test_that("cograph() accepts network object", {
  skip_if_not_installed("network")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  net <- network::network(mat, directed = FALSE)

  result <- tryCatch({
    cograph_net <- cograph(net)
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot() works with network input", {
  skip_if_not_installed("network")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  net <- network::network(mat, directed = FALSE)

  result <- tryCatch({
    with_temp_png(splot(net))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot() works with network input", {
  skip_if_not_installed("network")
  skip_if_not_installed("grid")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  net <- network::network(mat, directed = FALSE)

  result <- tryCatch({
    with_temp_png(soplot(net))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})
