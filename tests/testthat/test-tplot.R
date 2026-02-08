# test-tplot.R - Tests for TNA-style network plot functions
# Covers: R/tplot.R (plot_tna and tplot functions)

# ============================================
# Helper for tplot testing
# ============================================

with_temp_plot <- function(expr) {
  png_file <- tempfile(fileext = ".png")
  png(png_file, width = 400, height = 400)
  on.exit({
    dev.off()
    unlink(png_file)
  })
  force(expr)
}

# ============================================
# Basic Functionality Tests
# ============================================

test_that("plot_tna works with basic matrix", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(plot_tna(mat))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("tplot is alias for plot_tna", {
  expect_identical(tplot, plot_tna)
})

test_that("tplot works with basic matrix", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(tplot(mat))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Node Parameter Tests
# ============================================

test_that("plot_tna accepts color parameter", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(plot_tna(mat, color = c("red", "green", "blue", "yellow")))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna accepts labels parameter", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(plot_tna(mat, labels = c("A", "B", "C", "D")))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna accepts vsize parameter", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(plot_tna(mat, vsize = 10))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna accepts per-node vsize", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(plot_tna(mat, vsize = c(5, 8, 10, 12)))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Layout Parameter Tests
# ============================================

test_that("plot_tna accepts circle layout", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(plot_tna(mat, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna accepts spring layout", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(plot_tna(mat, layout = "spring"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna accepts oval layout (default)", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(plot_tna(mat, layout = "oval"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna accepts custom layout coordinates", {
  mat <- create_test_matrix(4)
  custom_layout <- matrix(c(0, 1, 0, 1, 0, 0, 1, 1), ncol = 2)

  result <- tryCatch({
    with_temp_plot(plot_tna(mat, layout = custom_layout))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Theme Parameter Tests
# ============================================

test_that("plot_tna accepts colorblind theme (default)", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(plot_tna(mat, theme = "colorblind"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna accepts gray theme", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(plot_tna(mat, theme = "gray"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna accepts dark theme", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(plot_tna(mat, theme = "dark"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Edge Parameter Tests
# ============================================

test_that("plot_tna accepts edge.labels parameter", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_plot(plot_tna(mat, edge.labels = TRUE))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna accepts edge.labels = FALSE", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_plot(plot_tna(mat, edge.labels = FALSE))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna accepts edge.label.position", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_plot(plot_tna(mat, edge.labels = TRUE, edge.label.position = 0.5))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna accepts edge.label.cex", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_plot(plot_tna(mat, edge.labels = TRUE, edge.label.cex = 1.2))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna accepts edge.color parameter", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(plot_tna(mat, edge.color = "darkred"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Edge Color Tests
# ============================================

test_that("plot_tna accepts posCol and negCol", {
  mat <- create_test_matrix(4, weighted = TRUE)
  mat[1, 2] <- -0.5
  mat[2, 1] <- -0.5

  result <- tryCatch({
    with_temp_plot(plot_tna(mat, posCol = "darkgreen", negCol = "darkred"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Edge Filtering Tests
# ============================================

test_that("plot_tna accepts minimum threshold", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_plot(plot_tna(mat, minimum = 0.3))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna accepts cut parameter", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_plot(plot_tna(mat, cut = 0.5))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Donut/Pie Parameter Tests (qgraph style)
# ============================================

test_that("plot_tna accepts pie parameter for donut", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(plot_tna(mat, pie = c(0.2, 0.5, 0.8, 0.3)))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna accepts pieColor parameter", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(plot_tna(mat,
                            pie = c(0.3, 0.6, 0.9, 0.4),
                            pieColor = c("red", "blue", "green", "orange")))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Line Type Tests
# ============================================

test_that("plot_tna accepts numeric lty parameter", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(plot_tna(mat, lty = 2))  # dashed
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna accepts character lty parameter", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(plot_tna(mat, lty = "dotted"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna accepts vector lty parameter", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(plot_tna(mat, lty = c(1, 2, 3, 1, 2, 3)))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Directed Parameter Tests
# ============================================

test_that("plot_tna handles directed = TRUE", {
  mat <- create_test_matrix(4, symmetric = FALSE)

  result <- tryCatch({
    with_temp_plot(plot_tna(mat, directed = TRUE))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna handles directed = FALSE", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(plot_tna(mat, directed = FALSE))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Arrow Parameter Tests
# ============================================

test_that("plot_tna accepts arrowAngle parameter", {
  mat <- create_test_matrix(4, symmetric = FALSE)

  result <- tryCatch({
    with_temp_plot(plot_tna(mat, directed = TRUE, arrowAngle = pi/4))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Title Parameter Tests
# ============================================

test_that("plot_tna accepts title parameter", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(plot_tna(mat, title = "Test Network"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Margin Parameter Tests
# ============================================

test_that("plot_tna accepts mar parameter", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_plot(plot_tna(mat, mar = c(0.2, 0.2, 0.2, 0.2)))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Return Value Tests
# ============================================

test_that("plot_tna returns cograph_network invisibly", {
  mat <- create_test_matrix(4)

  result <- NULL
  tryCatch({
    with_temp_plot({
      result <- plot_tna(mat)
    })
  }, error = function(e) NULL)

  expect_true(!is.null(result))
})

# ============================================
# Combined Parameter Tests
# ============================================

test_that("plot_tna with multiple parameters", {
  mat <- create_test_matrix(5)
  rownames(mat) <- colnames(mat) <- LETTERS[1:5]

  result <- tryCatch({
    with_temp_plot(plot_tna(mat,
                            color = c("red", "green", "blue", "yellow", "purple"),
                            labels = LETTERS[1:5],
                            vsize = c(8, 10, 12, 10, 8),
                            layout = "circle",
                            theme = "colorblind",
                            edge.labels = TRUE,
                            edge.label.cex = 0.8,
                            title = "Multi-parameter Test"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Edge Case Tests
# ============================================

test_that("plot_tna handles small network", {
  mat <- matrix(c(0, 1, 1, 0), 2, 2)

  result <- tryCatch({
    with_temp_plot(plot_tna(mat))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna handles larger network", {
  mat <- create_test_matrix(20)

  result <- tryCatch({
    with_temp_plot(plot_tna(mat))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna handles weighted network", {
  mat <- create_test_matrix(5, weighted = TRUE)

  result <- tryCatch({
    with_temp_plot(plot_tna(mat, edge.labels = TRUE))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("plot_tna handles named matrix", {
  mat <- create_test_matrix(4)
  rownames(mat) <- colnames(mat) <- c("Node1", "Node2", "Node3", "Node4")

  result <- tryCatch({
    with_temp_plot(plot_tna(mat))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})
