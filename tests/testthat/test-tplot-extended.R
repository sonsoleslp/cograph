# Extended tests for plot_tna() / tplot()
# Covers: R/tplot.R

# ============================================
# Basic Functionality Tests
# ============================================

test_that("plot_tna runs without error on basic matrix", {
  mat <- create_test_matrix(5, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(plot_tna(mat))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("tplot is alias for plot_tna", {
  expect_identical(tplot, plot_tna)
})

test_that("plot_tna with custom labels", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(plot_tna(mat, labels = c("A", "B", "C", "D")))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna with custom node colors", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(plot_tna(mat, color = c("red", "blue", "green", "orange")))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna with custom node size", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(plot_tna(mat, vsize = 15))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Edge Parameters Tests
# ============================================

test_that("plot_tna with edge.labels = FALSE", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(plot_tna(mat, edge.labels = FALSE))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna with custom edge.label.cex", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(plot_tna(mat, edge.label.cex = 1.5))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna with custom edge.color", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(plot_tna(mat, edge.color = "darkblue"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna with posCol and negCol", {
  mat <- create_test_matrix(4, weighted = TRUE, symmetric = FALSE)
  # Force some negative values
  mat[1, 2] <- -0.5

  result <- tryCatch({
    with_temp_png(plot_tna(mat, posCol = "green", negCol = "red"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna with minimum threshold", {
  mat <- create_test_matrix(5, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(plot_tna(mat, minimum = 0.5))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna with cut threshold", {
  mat <- create_test_matrix(5, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(plot_tna(mat, cut = 0.3))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Layout Tests
# ============================================

test_that("plot_tna with circle layout", {
  mat <- create_test_matrix(5, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(plot_tna(mat, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna with spring layout", {
  mat <- create_test_matrix(5, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(plot_tna(mat, layout = "spring"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna with custom coordinate matrix", {
  mat <- create_test_matrix(4, weighted = TRUE)
  coords <- matrix(c(0, 1, 0, 1, 0, 0, 1, 1), ncol = 2)

  result <- tryCatch({
    with_temp_png(plot_tna(mat, layout = coords))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Line Type Tests
# ============================================

test_that("plot_tna with numeric lty", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(plot_tna(mat, lty = 2))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna with string lty", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(plot_tna(mat, lty = "dashed"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Additional Parameters Tests
# ============================================

test_that("plot_tna with title", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(plot_tna(mat, title = "Test Network"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna with directed = FALSE", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(plot_tna(mat, directed = FALSE))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna with custom margins", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(plot_tna(mat, mar = c(0.5, 0.5, 0.5, 0.5)))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna with arrowAngle", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(plot_tna(mat, arrowAngle = pi / 4))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna with pie parameter", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(plot_tna(mat, pie = c(0.3, 0.5, 0.7, 0.9)))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna with edge.label.position", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(plot_tna(mat, edge.label.position = 0.5))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})
