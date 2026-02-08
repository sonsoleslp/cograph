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

# ============================================
# Single Node Network Tests
# ============================================

test_that("grid layout handles single node network", {
  mat <- matrix(0, 1, 1)
  net <- cograph(mat)

  layout_func <- get_layout("grid")
  result <- layout_func(net$network)
  expect_equal(nrow(result), 1)
})

test_that("bipartite layout handles single node network", {
  mat <- matrix(0, 1, 1)
  net <- cograph(mat)

  layout_func <- get_layout("bipartite")
  result <- layout_func(net$network)
  expect_equal(nrow(result), 1)
})

test_that("gephi_fr layout handles single node network", {
  skip_if_not_installed("igraph")

  mat <- matrix(0, 1, 1)
  net <- cograph(mat)

  layout_func <- get_layout("gephi_fr")
  result <- layout_func(net$network)
  expect_equal(nrow(result), 1)
})

# ============================================
# Gephi FR Layout Parameter Tests
# ============================================

test_that("gephi_fr layout accepts custom area", {
  skip_if_not_installed("igraph")

  mat <- create_test_matrix(5)
  net <- cograph(mat)

  layout_func <- get_layout("gephi_fr")
  result <- layout_func(net$network, area = 20000)
  expect_equal(nrow(result), 5)
})

test_that("gephi_fr layout accepts custom gravity", {
  skip_if_not_installed("igraph")

  mat <- create_test_matrix(5)
  net <- cograph(mat)

  layout_func <- get_layout("gephi_fr")
  result <- layout_func(net$network, gravity = 5.0)
  expect_equal(nrow(result), 5)
})

test_that("gephi_fr layout accepts custom speed", {
  skip_if_not_installed("igraph")

  mat <- create_test_matrix(5)
  net <- cograph(mat)

  layout_func <- get_layout("gephi_fr")
  result <- layout_func(net$network, speed = 2.0)
  expect_equal(nrow(result), 5)
})

test_that("gephi_fr layout accepts custom niter", {
  skip_if_not_installed("igraph")

  mat <- create_test_matrix(5)
  net <- cograph(mat)

  layout_func <- get_layout("gephi_fr")
  result <- layout_func(net$network, niter = 50)
  expect_equal(nrow(result), 5)
})

test_that("gephi_fr layout with all custom parameters", {
  skip_if_not_installed("igraph")

  mat <- create_test_matrix(5)
  net <- cograph(mat)

  layout_func <- get_layout("gephi_fr")
  result <- layout_func(net$network,
                        area = 15000,
                        gravity = 8.0,
                        speed = 0.5,
                        niter = 75)
  expect_equal(nrow(result), 5)
})

# ============================================
# Bipartite Layout Edge Cases
# ============================================

test_that("bipartite layout handles all same type", {
  mat <- create_test_matrix(4)
  net <- cograph(mat)

  layout_func <- get_layout("bipartite")
  result <- layout_func(net$network, types = c(0, 0, 0, 0))
  expect_equal(nrow(result), 4)
  # All should be on left side
  expect_true(all(result$x == 0.2))
})

test_that("bipartite layout handles unequal split", {
  mat <- create_test_matrix(5)
  net <- cograph(mat)

  layout_func <- get_layout("bipartite")
  result <- layout_func(net$network, types = c(0, 0, 0, 0, 1))
  expect_equal(nrow(result), 5)
  expect_equal(sum(result$x == 0.2), 4)
  expect_equal(sum(result$x == 0.8), 1)
})

# ============================================
# Star Layout Edge Cases
# ============================================

test_that("star layout with center out of range handled", {
  mat <- create_test_matrix(5)
  net <- cograph(mat)

  layout_func <- get_layout("star")
  # Center = 5 is valid (last node)
  result <- layout_func(net$network, center = 5)
  expect_equal(nrow(result), 5)
  expect_equal(result$x[5], 0.5)
  expect_equal(result$y[5], 0.5)
})

# ============================================
# Grid Layout Edge Cases
# ============================================

test_that("grid layout handles non-square counts", {
  # 7 nodes - not a perfect square
  mat <- create_test_matrix(7)
  net <- cograph(mat)

  layout_func <- get_layout("grid")
  result <- layout_func(net$network)
  expect_equal(nrow(result), 7)
})

test_that("grid layout with ncol larger than n", {
  mat <- create_test_matrix(4)
  net <- cograph(mat)

  layout_func <- get_layout("grid")
  result <- layout_func(net$network, ncol = 10)
  expect_equal(nrow(result), 4)
})

# ============================================
# Random Layout Edge Cases
# ============================================

test_that("random layout produces different results with different seeds", {
  mat <- create_test_matrix(4)
  net <- cograph(mat)

  layout_func <- get_layout("random")

  result1 <- layout_func(net$network, seed = 1)
  result2 <- layout_func(net$network, seed = 2)

  # Results should be different with different seeds
  expect_false(all(result1$x == result2$x) && all(result1$y == result2$y))
})

test_that("random layout with NULL seed is reproducible within set.seed", {
  mat <- create_test_matrix(4)
  net <- cograph(mat)

  layout_func <- get_layout("random")

  set.seed(999)
  result1 <- layout_func(net$network, seed = NULL)
  set.seed(999)
  result2 <- layout_func(net$network, seed = NULL)

  # Should be the same when global seed is the same
  expect_equal(result1$x, result2$x)
  expect_equal(result1$y, result2$y)
})

# ============================================
# Direct Registration Function Tests
# ============================================

test_that("register_builtin_layouts registers all layouts", {
  # Call the registration function directly to cover it
  cograph:::register_builtin_layouts()

  # Verify all expected layouts are registered
  layouts <- list_layouts()
  expected <- c("circle", "oval", "ellipse", "spring", "fr",
                "fruchterman-reingold", "groups", "grid", "random",
                "star", "bipartite", "custom", "gephi_fr", "gephi")
  for (layout_name in expected) {
    expect_true(layout_name %in% layouts,
                info = paste("Layout", layout_name, "not registered"))
  }
})

# ============================================
# Gephi FR No Edges Tests
# ============================================

test_that("gephi_fr layout handles network with no edges", {
  skip_if_not_installed("igraph")

  # Create network with nodes but no edges
  mat <- matrix(0, 4, 4)
  net <- cograph(mat)

  layout_func <- get_layout("gephi_fr")
  result <- layout_func(net$network)
  expect_equal(nrow(result), 4)
  expect_true("x" %in% names(result))
  expect_true("y" %in% names(result))
})

test_that("gephi_fr layout handles displacement limiting", {
  skip_if_not_installed("igraph")

  # Dense network with many iterations to trigger displacement limiting
  mat <- matrix(1, 6, 6)
  diag(mat) <- 0
  net <- cograph(mat)

  layout_func <- get_layout("gephi_fr")
  result <- layout_func(net$network, niter = 200, speed = 5.0)
  expect_equal(nrow(result), 6)
})

# ============================================
# Star Layout n_others Branch Tests
# ============================================

test_that("star layout with 3 nodes positions peripheral correctly", {
  mat <- matrix(c(0, 1, 1, 1, 0, 0, 1, 0, 0), 3, 3)
  net <- cograph(mat)

  layout_func <- get_layout("star")
  result <- layout_func(net$network, center = 1)
  expect_equal(nrow(result), 3)
  # Center should be at (0.5, 0.5)
  expect_equal(result$x[1], 0.5)
  expect_equal(result$y[1], 0.5)
  # Others should be around the center
  expect_true(result$x[2] != 0.5 || result$y[2] != 0.5)
  expect_true(result$x[3] != 0.5 || result$y[3] != 0.5)
})

# ============================================
# Bipartite Layout Edge Cases
# ============================================

test_that("bipartite layout with NULL types uses alternating", {
  mat <- create_test_matrix(4)
  net <- cograph(mat)

  layout_func <- get_layout("bipartite")
  result <- layout_func(net$network, types = NULL)
  expect_equal(nrow(result), 4)
  # Should have nodes on both sides
  expect_true(0.2 %in% result$x)
  expect_true(0.8 %in% result$x)
})

test_that("bipartite layout with only type2 nodes", {
  mat <- create_test_matrix(3)
  net <- cograph(mat)

  layout_func <- get_layout("bipartite")
  # All nodes as type 1 (none as type 0)
  result <- layout_func(net$network, types = c(1, 1, 1))
  expect_equal(nrow(result), 3)
})

# ============================================
# Custom Layout Matrix Conversion
# ============================================

test_that("custom layout converts matrix columns to x and y", {
  mat <- create_test_matrix(3)
  net <- cograph(mat)

  layout_func <- get_layout("custom")
  # Matrix without column names
  custom_coords <- matrix(c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6), ncol = 2)
  result <- layout_func(net$network, coords = custom_coords)

  expect_equal(names(result)[1], "x")
  expect_equal(names(result)[2], "y")
  expect_equal(result$x, c(0.1, 0.2, 0.3))
  expect_equal(result$y, c(0.4, 0.5, 0.6))
})
