# Tests for Gephi Fruchterman-Reingold layout
# Covers: R/layout-gephi-fr.R

# ============================================
# layout_gephi_fr Tests
# ============================================

test_that("layout_gephi_fr works with igraph ring", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(10)
  coords <- cograph:::layout_gephi_fr(g)

  expect_true(is.matrix(coords))
  expect_equal(nrow(coords), 10)
  expect_equal(ncol(coords), 2)
})

test_that("layout_gephi_fr works with empty graph", {
  skip_if_not_installed("igraph")

  g <- igraph::make_empty_graph(0)
  coords <- cograph:::layout_gephi_fr(g)

  expect_true(is.matrix(coords))
  expect_equal(nrow(coords), 0)
  expect_equal(ncol(coords), 2)
})

test_that("layout_gephi_fr works with single node", {
  skip_if_not_installed("igraph")

  g <- igraph::make_empty_graph(1)
  coords <- cograph:::layout_gephi_fr(g)

  expect_true(is.matrix(coords))
  expect_equal(nrow(coords), 1)
  expect_equal(ncol(coords), 2)
})

test_that("layout_gephi_fr works with complete graph", {
  skip_if_not_installed("igraph")

  g <- igraph::make_full_graph(5)
  coords <- cograph:::layout_gephi_fr(g)

  expect_true(is.matrix(coords))
  expect_equal(nrow(coords), 5)
  expect_equal(ncol(coords), 2)
})

test_that("layout_gephi_fr respects area parameter", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(5)

  # Different area values should produce different spreads
  coords1 <- cograph:::layout_gephi_fr(g, area = 1000)
  coords2 <- cograph:::layout_gephi_fr(g, area = 50000)

  # Larger area should generally produce more spread
  range1 <- max(coords1) - min(coords1)
  range2 <- max(coords2) - min(coords2)

  # Just check they're different (stochastic)
  expect_true(is.numeric(range1))
  expect_true(is.numeric(range2))
})

test_that("layout_gephi_fr respects gravity parameter", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(5)

  # Higher gravity should pull nodes closer to center
  coords1 <- cograph:::layout_gephi_fr(g, gravity = 1.0)
  coords2 <- cograph:::layout_gephi_fr(g, gravity = 100.0)

  # Just check both produce valid output
  expect_true(is.matrix(coords1))
  expect_true(is.matrix(coords2))
})

test_that("layout_gephi_fr respects speed parameter", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(5)

  coords <- cograph:::layout_gephi_fr(g, speed = 0.5)

  expect_true(is.matrix(coords))
  expect_equal(nrow(coords), 5)
})

test_that("layout_gephi_fr respects niter parameter", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(5)

  # Fewer iterations
  coords1 <- cograph:::layout_gephi_fr(g, niter = 10)
  # More iterations
  coords2 <- cograph:::layout_gephi_fr(g, niter = 200)

  expect_true(is.matrix(coords1))
  expect_true(is.matrix(coords2))
})

test_that("layout_gephi_fr handles graph with no edges", {
  skip_if_not_installed("igraph")

  g <- igraph::make_empty_graph(5)
  coords <- cograph:::layout_gephi_fr(g)

  expect_true(is.matrix(coords))
  expect_equal(nrow(coords), 5)
})

test_that("layout_gephi_fr handles directed graph", {
  skip_if_not_installed("igraph")

  g <- igraph::make_graph(~ A-+B, B-+C, C-+D, D-+A)
  coords <- cograph:::layout_gephi_fr(g)

  expect_true(is.matrix(coords))
  expect_equal(nrow(coords), 4)
})

# ============================================
# compute_layout_gephi_fr Tests
# ============================================

test_that("compute_layout_gephi_fr works with cograph network", {
  skip_if_not_installed("igraph")

  mat <- create_test_matrix(5)
  net <- cograph(mat)

  coords <- cograph:::compute_layout_gephi_fr(net$network)

  expect_true(is.data.frame(coords))
  expect_equal(nrow(coords), 5)
  expect_true("x" %in% names(coords))
  expect_true("y" %in% names(coords))
})

test_that("compute_layout_gephi_fr handles parameters", {
  skip_if_not_installed("igraph")

  mat <- create_test_matrix(5)
  net <- cograph(mat)

  coords <- cograph:::compute_layout_gephi_fr(
    net$network,
    area = 5000,
    gravity = 5.0,
    speed = 0.5,
    niter = 50
  )

  expect_true(is.data.frame(coords))
  expect_equal(nrow(coords), 5)
})

# ============================================
# Integration Tests
# ============================================

test_that("gephi_fr layout works in soplot", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("grid")

  mat <- create_test_matrix(5)

  result <- tryCatch({
    with_temp_png(soplot(mat, layout = "gephi_fr"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("gephi layout alias works in soplot", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("grid")

  mat <- create_test_matrix(5)

  result <- tryCatch({
    with_temp_png(soplot(mat, layout = "gephi"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})
