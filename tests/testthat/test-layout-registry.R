# Tests for layout registry functions
# Covers: R/layout-registry.R

# ============================================
# Built-in Layouts Registration Tests
# ============================================

test_that("circle layout is registered", {
  expect_true("circle" %in% list_layouts())
})

test_that("oval layout is registered with alias", {
  layouts <- list_layouts()
  expect_true("oval" %in% layouts)
  expect_true("ellipse" %in% layouts)
})

test_that("spring layout is registered with aliases", {
  layouts <- list_layouts()
  expect_true("spring" %in% layouts)
  expect_true("fr" %in% layouts)
  expect_true("fruchterman-reingold" %in% layouts)
})

test_that("groups layout is registered", {
  expect_true("groups" %in% list_layouts())
})

test_that("grid layout is registered", {
  expect_true("grid" %in% list_layouts())
})

test_that("random layout is registered", {
  expect_true("random" %in% list_layouts())
})

test_that("star layout is registered", {
  expect_true("star" %in% list_layouts())
})

test_that("bipartite layout is registered", {
  expect_true("bipartite" %in% list_layouts())
})

test_that("custom layout is registered", {
  expect_true("custom" %in% list_layouts())
})

test_that("gephi layouts are registered", {
  layouts <- list_layouts()
  expect_true("gephi_fr" %in% layouts)
  expect_true("gephi" %in% layouts)
})

# ============================================
# Grid Layout Tests
# ============================================

test_that("grid layout works with basic network", {
  mat <- create_test_matrix(9)
  net <- cograph(mat)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "grid"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("grid layout handles single node", {
  mat <- matrix(0, 1, 1)
  net <- cograph(mat)

  # Grid layout should handle single node network
  layout_func <- get_layout("grid")
  result <- layout_func(net$network)
  expect_equal(nrow(result), 1)
})

test_that("grid layout handles two nodes", {
  mat <- matrix(c(0, 1, 1, 0), 2, 2)
  net <- cograph(mat)

  layout_func <- get_layout("grid")
  result <- layout_func(net$network)
  expect_equal(nrow(result), 2)
})

test_that("grid layout with custom ncol", {
  mat <- create_test_matrix(6)
  net <- cograph(mat)

  layout_func <- get_layout("grid")
  result <- layout_func(net$network, ncol = 2)
  expect_equal(nrow(result), 6)
})

# ============================================
# Random Layout Tests
# ============================================

test_that("random layout works with basic network", {
  mat <- create_test_matrix(4)
  net <- cograph(mat)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "random"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("random layout with seed produces consistent results", {
  mat <- create_test_matrix(4)
  net <- cograph(mat)

  layout_func <- get_layout("random")

  result1 <- layout_func(net$network, seed = 42)
  result2 <- layout_func(net$network, seed = 42)

  expect_equal(result1$x, result2$x)
  expect_equal(result1$y, result2$y)
})

# ============================================
# Star Layout Tests
# ============================================

test_that("star layout works with basic network", {
  mat <- create_test_matrix(5)
  net <- cograph(mat)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "star"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("star layout handles two nodes", {
  mat <- matrix(c(0, 1, 1, 0), 2, 2)
  net <- cograph(mat)

  layout_func <- get_layout("star")
  result <- layout_func(net$network)
  expect_equal(nrow(result), 2)
})

test_that("star layout handles single node", {
  mat <- matrix(0, 1, 1)
  net <- cograph(mat)

  layout_func <- get_layout("star")
  result <- layout_func(net$network)
  expect_equal(nrow(result), 1)
  expect_equal(result$x, 0.5)
  expect_equal(result$y, 0.5)
})

test_that("star layout with custom center", {
  mat <- create_test_matrix(5)
  net <- cograph(mat)

  layout_func <- get_layout("star")
  result <- layout_func(net$network, center = 3)
  expect_equal(nrow(result), 5)
  expect_equal(result$x[3], 0.5)
  expect_equal(result$y[3], 0.5)
})

# ============================================
# Bipartite Layout Tests
# ============================================

test_that("bipartite layout works with basic network", {
  mat <- create_test_matrix(6)
  net <- cograph(mat)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "bipartite"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("bipartite layout handles two nodes", {
  mat <- matrix(c(0, 1, 1, 0), 2, 2)
  net <- cograph(mat)

  layout_func <- get_layout("bipartite")
  result <- layout_func(net$network)
  expect_equal(nrow(result), 2)
})

test_that("bipartite layout with custom types", {
  mat <- create_test_matrix(6)
  net <- cograph(mat)

  layout_func <- get_layout("bipartite")
  result <- layout_func(net$network, types = c(0, 0, 0, 1, 1, 1))
  expect_equal(nrow(result), 6)
  # First 3 should be on left, last 3 on right
  expect_true(all(result$x[1:3] == 0.2))
  expect_true(all(result$x[4:6] == 0.8))
})

# ============================================
# Custom Layout Tests
# ============================================

test_that("custom layout works with matrix coords", {
  mat <- create_test_matrix(4)
  net <- cograph(mat)

  layout_func <- get_layout("custom")
  custom_coords <- matrix(c(0, 0, 1, 1, 0, 1, 0, 1), ncol = 2)
  result <- layout_func(net$network, coords = custom_coords)
  expect_equal(nrow(result), 4)
  expect_true("x" %in% names(result))
  expect_true("y" %in% names(result))
})

test_that("custom layout works with data frame coords", {
  mat <- create_test_matrix(4)
  net <- cograph(mat)

  layout_func <- get_layout("custom")
  custom_coords <- data.frame(x = c(0, 0, 1, 1), y = c(0, 1, 0, 1))
  result <- layout_func(net$network, coords = custom_coords)
  expect_equal(nrow(result), 4)
})

# ============================================
# Gephi FR Layout Tests
# ============================================

test_that("gephi_fr layout is available", {
  skip_if_not_installed("igraph")
  expect_true("gephi_fr" %in% list_layouts())
})

test_that("gephi_fr layout works with basic network", {
  skip_if_not_installed("igraph")

  mat <- create_test_matrix(5)
  net <- cograph(mat)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "gephi_fr"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("gephi_fr layout handles small network", {
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 1, 0), 2, 2)
  net <- cograph(mat)

  layout_func <- get_layout("gephi_fr")
  result <- layout_func(net$network)
  expect_equal(nrow(result), 2)
})

test_that("gephi layout alias works", {
  skip_if_not_installed("igraph")

  mat <- create_test_matrix(4)
  net <- cograph(mat)

  result <- tryCatch({
    with_temp_png(soplot(net, layout = "gephi"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})
