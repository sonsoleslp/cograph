# test-input-tna.R - Tests for tna object input parsing
# Covers: R/input-tna.R (parse_tna function)

# Make internal function available for testing
parse_tna <- cograph:::parse_tna

# ============================================
# Helper Functions
# ============================================

# Create a mock tna object for testing
create_mock_tna <- function(n = 3, labels = NULL, inits = NULL, weighted = TRUE) {
  if (weighted) {
    weights <- matrix(runif(n * n, 0, 1), n, n)
    diag(weights) <- 0
    # Ensure some edges are zero
    weights[weights < 0.3] <- 0
  } else {
    weights <- matrix(sample(0:1, n * n, replace = TRUE), n, n)
    diag(weights) <- 0
  }

  if (is.null(labels)) {
    labels <- paste0("S", seq_len(n))
  }

  if (is.null(inits)) {
    inits <- runif(n)
    inits <- inits / sum(inits)
  }

  obj <- list(
    weights = weights,
    labels = labels,
    inits = inits
  )
  class(obj) <- "tna"
  obj
}

# ============================================
# Error Handling Tests
# ============================================

test_that("parse_tna errors on non-tna input", {
  expect_error(parse_tna(list(weights = matrix(0, 3, 3))),
               "tna object")
})

test_that("parse_tna errors on matrix input", {
  mat <- matrix(c(0, 1, 1, 0), 2, 2)
  expect_error(parse_tna(mat),
               "tna object")
})

test_that("parse_tna errors on NULL input", {
  expect_error(parse_tna(NULL),
               "tna object")
})

# ============================================
# Basic tna Parsing Tests
# ============================================

test_that("parse_tna parses simple tna object", {
  tna_obj <- create_mock_tna(3)

  result <- parse_tna(tna_obj)

  expect_true(is.list(result))
  expect_true("nodes" %in% names(result))
  expect_true("edges" %in% names(result))
  expect_true("directed" %in% names(result))
  expect_true("weights" %in% names(result))
})

test_that("parse_tna extracts correct node count", {
  tna_obj <- create_mock_tna(5)

  result <- parse_tna(tna_obj)

  expect_equal(nrow(result$nodes), 5)
})

test_that("parse_tna extracts edge information", {
  tna_obj <- create_mock_tna(3)
  # Ensure at least one edge
  tna_obj$weights[1, 2] <- 0.5

  result <- parse_tna(tna_obj)

  expect_true(nrow(result$edges) > 0)
})

# ============================================
# Directed Network Tests
# ============================================

test_that("parse_tna defaults to directed", {
  tna_obj <- create_mock_tna(3)

  result <- parse_tna(tna_obj)

  expect_true(result$directed)
})

test_that("parse_tna can be forced to undirected", {
  tna_obj <- create_mock_tna(3)

  result <- parse_tna(tna_obj, directed = FALSE)

  expect_false(result$directed)
})

test_that("parse_tna respects directed parameter", {
  tna_obj <- create_mock_tna(3)

  result_directed <- parse_tna(tna_obj, directed = TRUE)
  result_undirected <- parse_tna(tna_obj, directed = FALSE)

  expect_true(result_directed$directed)
  expect_false(result_undirected$directed)
})

# ============================================
# Label Extraction Tests
# ============================================

test_that("parse_tna extracts labels", {
  tna_obj <- create_mock_tna(3, labels = c("A", "B", "C"))

  result <- parse_tna(tna_obj)

  expect_equal(result$nodes$label, c("A", "B", "C"))
})

test_that("parse_tna uses indices when no labels", {
  tna_obj <- create_mock_tna(3)
  tna_obj$labels <- NULL

  result <- parse_tna(tna_obj)

  expect_equal(result$nodes$label, c("1", "2", "3"))
})

test_that("parse_tna handles NA labels", {
  tna_obj <- create_mock_tna(3)
  tna_obj$labels <- c(NA, NA, NA)

  result <- parse_tna(tna_obj)

  expect_equal(result$nodes$label, c("1", "2", "3"))
})

# ============================================
# Weight Extraction Tests
# ============================================

test_that("parse_tna extracts weights", {
  tna_obj <- create_mock_tna(3)
  tna_obj$weights <- matrix(c(0, 0.5, 0.3, 0.5, 0, 0.8, 0.3, 0.8, 0), 3, 3)

  result <- parse_tna(tna_obj)

  expect_true(length(result$weights) > 0)
})

test_that("parse_tna excludes zero weights from edges", {
  tna_obj <- create_mock_tna(3)
  tna_obj$weights <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), 3, 3)

  result <- parse_tna(tna_obj)

  expect_equal(nrow(result$edges), 2)  # Only 2 non-zero edges
})

# ============================================
# Initial Probabilities Tests
# ============================================

test_that("parse_tna extracts initial probabilities", {
  inits <- c(0.2, 0.3, 0.5)
  tna_obj <- create_mock_tna(3, inits = inits)

  result <- parse_tna(tna_obj)

  expect_true("inits" %in% names(result$nodes))
  expect_equal(result$nodes$inits, inits)
})

test_that("parse_tna handles missing inits", {
  tna_obj <- create_mock_tna(3)
  tna_obj$inits <- NULL

  result <- parse_tna(tna_obj)

  expect_false("inits" %in% names(result$nodes))
})

# ============================================
# Edge Case Tests
# ============================================

test_that("parse_tna handles empty network (no edges)", {
  tna_obj <- create_mock_tna(3)
  tna_obj$weights <- matrix(0, 3, 3)

  result <- parse_tna(tna_obj)

  expect_equal(nrow(result$nodes), 3)
  expect_equal(nrow(result$edges), 0)
  expect_equal(length(result$weights), 0)
})

test_that("parse_tna handles fully connected network", {
  tna_obj <- create_mock_tna(3)
  tna_obj$weights <- matrix(0.5, 3, 3)
  diag(tna_obj$weights) <- 0

  result <- parse_tna(tna_obj)

  expect_equal(nrow(result$nodes), 3)
  expect_equal(nrow(result$edges), 3)
})

test_that("parse_tna handles single node network", {
  tna_obj <- list(
    weights = matrix(0, 1, 1),
    labels = "A",
    inits = 1
  )
  class(tna_obj) <- "tna"

  result <- parse_tna(tna_obj)

  expect_equal(nrow(result$nodes), 1)
  expect_equal(nrow(result$edges), 0)
})

test_that("parse_tna handles larger network", {
  tna_obj <- create_mock_tna(10)

  result <- parse_tna(tna_obj)

  expect_equal(nrow(result$nodes), 10)
})

# ============================================
# Integration Tests
# ============================================

test_that("cograph() accepts tna object", {
  tna_obj <- create_mock_tna(3)
  tna_obj$weights[1, 2] <- 0.5  # Ensure at least one edge

  result <- tryCatch({
    cograph_net <- cograph(tna_obj)
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot() works with tna input", {
  tna_obj <- create_mock_tna(3)
  tna_obj$weights[1, 2] <- 0.5

  result <- tryCatch({
    with_temp_png(splot(tna_obj))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("soplot() works with tna input", {
  skip_if_not_installed("grid")

  tna_obj <- create_mock_tna(3)
  tna_obj$weights[1, 2] <- 0.5

  result <- tryCatch({
    with_temp_png(soplot(tna_obj))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("tplot() works with tna input", {
  tna_obj <- create_mock_tna(3)
  tna_obj$weights[1, 2] <- 0.5

  result <- tryCatch({
    with_temp_png(tplot(tna_obj))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna() works with tna input", {
  tna_obj <- create_mock_tna(3)
  tna_obj$weights[1, 2] <- 0.5

  result <- tryCatch({
    with_temp_png(plot_tna(tna_obj))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Node Attribute Tests for Donut Visualization
# ============================================

test_that("tna initial probabilities can be used for donut_values", {
  skip_if_not_installed("grid")

  tna_obj <- create_mock_tna(3, inits = c(0.2, 0.3, 0.5))
  tna_obj$weights[1, 2] <- 0.5
  tna_obj$weights[2, 3] <- 0.3

  # soplot should work with donut visualization using inits
  result <- tryCatch({
    with_temp_png(soplot(tna_obj, node_shape = "donut", layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})
