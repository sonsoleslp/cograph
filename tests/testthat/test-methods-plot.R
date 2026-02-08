# test-methods-plot.R - Tests for S3 plot and summary methods
# Covers: R/methods-plot.R

# ============================================
# plot.cograph_network Tests
# ============================================

test_that("plot.cograph_network works with basic network", {
  mat <- create_test_matrix(3)
  net <- cograph(mat)

  result <- tryCatch({
    with_temp_png(plot(net))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot.cograph_network passes arguments to sn_render", {
  mat <- create_test_matrix(3)
  net <- cograph(mat)

  result <- tryCatch({
    with_temp_png(plot(net, node_fill = "red", edge_color = "blue"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot.cograph_network returns invisible x", {
  mat <- create_test_matrix(3)
  net <- cograph(mat)

  result <- with_temp_png(plot(net))

  expect_equal(class(result), class(net))
})

test_that("plot.cograph_network works with weighted network", {
  mat <- create_test_matrix(4, weighted = TRUE)
  net <- cograph(mat)

  result <- tryCatch({
    with_temp_png(plot(net))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot.cograph_network works with directed network", {
  mat <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), 3, 3)
  net <- cograph(mat, directed = TRUE)

  result <- tryCatch({
    with_temp_png(plot(net))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot.cograph_network works with layout specified", {
  mat <- create_test_matrix(4)
  net <- cograph(mat) |> sn_layout("circle")

  result <- tryCatch({
    with_temp_png(plot(net))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot.cograph_network works with theme applied", {
  mat <- create_test_matrix(3)
  net <- cograph(mat) |> sn_theme("dark")

  result <- tryCatch({
    with_temp_png(plot(net))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot.cograph_network works with labels", {
  mat <- create_test_matrix(3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  net <- cograph(mat)

  result <- tryCatch({
    with_temp_png(plot(net, show_labels = TRUE))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# summary.cograph_network Tests
# ============================================

test_that("summary.cograph_network prints summary", {
  mat <- create_test_matrix(3)
  net <- cograph(mat)

  output <- capture.output(summary(net))

  expect_true(any(grepl("Cograph Network Summary", output)))
  expect_true(any(grepl("Nodes:", output)))
  expect_true(any(grepl("Edges:", output)))
})

test_that("summary.cograph_network returns invisible list", {
  mat <- create_test_matrix(3)
  net <- cograph(mat)

  result <- capture.output(summary_result <- summary(net))

  expect_true(is.list(summary_result))
  expect_true("n_nodes" %in% names(summary_result))
  expect_true("n_edges" %in% names(summary_result))
  expect_true("directed" %in% names(summary_result))
})

test_that("summary.cograph_network shows correct node count", {
  mat <- create_test_matrix(5)
  net <- cograph(mat)

  output <- capture.output(summary(net))

  expect_true(any(grepl("Nodes: 5", output)))
})

test_that("summary.cograph_network shows edge statistics for weighted", {
  mat <- create_test_matrix(3, weighted = TRUE)
  net <- cograph(mat)

  output <- capture.output(summary(net))

  expect_true(any(grepl("Edge Statistics:", output)))
  expect_true(any(grepl("weight", output, ignore.case = TRUE)))
})

test_that("summary.cograph_network shows directed type", {
  mat <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), 3, 3)
  net <- cograph(mat, directed = TRUE)

  output <- capture.output(summary(net))

  expect_true(any(grepl("Type: Directed", output)))
})

test_that("summary.cograph_network shows undirected type", {
  mat <- create_test_matrix(3)
  net <- cograph(mat, directed = FALSE)

  output <- capture.output(summary(net))

  expect_true(any(grepl("Type: Undirected", output)))
})

test_that("summary.cograph_network shows node labels", {
  mat <- create_test_matrix(3)
  rownames(mat) <- colnames(mat) <- c("Alpha", "Beta", "Gamma")
  net <- cograph(mat)

  output <- capture.output(summary(net))

  expect_true(any(grepl("Node Labels:", output)))
  expect_true(any(grepl("Alpha", output)))
})

test_that("summary.cograph_network truncates many labels", {
  mat <- create_test_matrix(15)
  net <- cograph(mat)

  output <- capture.output(summary(net))

  # Should show "..." for truncated labels
  expect_true(any(grepl("\\.\\.\\.", output)))
})

test_that("summary.cograph_network shows layout status", {
  mat <- create_test_matrix(3)
  net <- cograph(mat)

  output <- capture.output(summary(net))

  expect_true(any(grepl("Layout:", output)))
})

test_that("summary.cograph_network shows layout computed after sn_layout", {
  mat <- create_test_matrix(3)
  net <- cograph(mat) |> sn_layout("circle")

  output <- capture.output(summary(net))

  expect_true(any(grepl("Layout:.*computed", output)))
})

test_that("summary.cograph_network shows theme", {
  mat <- create_test_matrix(3)
  net <- cograph(mat) |> sn_theme("classic")

  output <- capture.output(summary(net))

  expect_true(any(grepl("Theme:", output)))
})

test_that("summary.cograph_network shows theme when not set", {
  mat <- create_test_matrix(3)
  net <- cograph(mat)

  output <- capture.output(summary(net))

  # Should show theme line (may be "none" or empty)
  expect_true(any(grepl("Theme:", output)))
})

test_that("summary.cograph_network shows positive/negative edge counts", {
  mat <- matrix(c(0, 1, -0.5, 1, 0, 0.8, -0.5, 0.8, 0), 3, 3)
  net <- cograph(mat)

  output <- capture.output(summary(net))

  expect_true(any(grepl("Positive edges:", output)))
  expect_true(any(grepl("Negative edges:", output)))
})

test_that("summary.cograph_network handles empty edges gracefully", {
  mat <- matrix(0, 3, 3)
  net <- cograph(mat)

  result <- tryCatch({
    capture.output(summary(net))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("summary.cograph_network weighted flag correct", {
  mat <- create_test_matrix(3, weighted = TRUE)
  net <- cograph(mat)

  result <- capture.output(summary_result <- summary(net))

  expect_true(summary_result$weighted)
})

test_that("summary.cograph_network has_layout flag is set", {
  mat <- create_test_matrix(3)
  net <- cograph(mat)

  result <- capture.output(summary_result <- summary(net))

  # Layout may be auto-computed, just verify the flag exists
  expect_true("has_layout" %in% names(summary_result))
})

test_that("summary.cograph_network has_layout flag correct after layout", {
  mat <- create_test_matrix(3)
  net <- cograph(mat) |> sn_layout("circle")

  result <- capture.output(summary_result <- summary(net))

  expect_true(summary_result$has_layout)
})

# ============================================
# Integration Tests
# ============================================

test_that("plot and summary work on same object", {
  mat <- create_test_matrix(4, weighted = TRUE)
  net <- cograph(mat)

  # Both should work without error
  capture.output(summary_result <- summary(net))
  plot_result <- tryCatch({
    with_temp_png(plot(net))
    TRUE
  }, error = function(e) FALSE)

  expect_true(is.list(summary_result))
  expect_true(plot_result)
})

test_that("plot works after multiple pipe operations", {
  mat <- create_test_matrix(4)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  net <- cograph(mat) |>
    sn_layout("spring") |>
    sn_theme("colorblind")

  result <- tryCatch({
    with_temp_png(plot(net))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})
