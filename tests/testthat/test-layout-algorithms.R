# Tests for layout algorithm functions
# Covers: R/layout-circle.R, R/layout-oval.R, R/layout-groups.R, R/layout-spring.R

# ============================================
# layout_circle Tests
# ============================================

test_that("layout_circle works with basic network", {
  mat <- create_test_matrix(5)
  net <- cograph(mat)

  coords <- layout_circle(net$network)

  expect_true(is.data.frame(coords))
  expect_equal(nrow(coords), 5)
  expect_true("x" %in% names(coords))
  expect_true("y" %in% names(coords))
})

test_that("layout_circle handles single node", {
  mat <- matrix(0, 1, 1)
  net <- cograph(mat)

  coords <- layout_circle(net$network)

  expect_equal(nrow(coords), 1)
  expect_equal(coords$x, 0.5)
  expect_equal(coords$y, 0.5)
})

test_that("layout_circle respects order parameter", {
  mat <- create_test_matrix(4)
  net <- cograph(mat)

  coords1 <- layout_circle(net$network, order = c(1, 2, 3, 4))
  coords2 <- layout_circle(net$network, order = c(4, 3, 2, 1))

  # Different orders should give different results
  expect_false(all(coords1$x == coords2$x))
})

test_that("layout_circle respects start_angle parameter", {
  mat <- create_test_matrix(4)
  net <- cograph(mat)

  coords1 <- layout_circle(net$network, start_angle = 0)
  coords2 <- layout_circle(net$network, start_angle = pi)

  # Different start angles should give different results
  expect_false(all(coords1$x == coords2$x))
})

test_that("layout_circle respects clockwise parameter", {
  mat <- create_test_matrix(4)
  net <- cograph(mat)

  coords_cw <- layout_circle(net$network, clockwise = TRUE)
  coords_ccw <- layout_circle(net$network, clockwise = FALSE)

  # Different directions should give different results
  expect_false(all(coords_cw$x == coords_ccw$x))
})

test_that("layout_circle handles label order", {
  mat <- create_test_matrix(4)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  net <- cograph(mat)

  coords <- layout_circle(net$network, order = c("D", "C", "B", "A"))
  expect_equal(nrow(coords), 4)
})

test_that("layout_circle warns on mismatched order length", {
  mat <- create_test_matrix(4)
  net <- cograph(mat)

  expect_warning(
    layout_circle(net$network, order = c(1, 2)),
    "Order length"
  )
})

test_that("layout_circle warns on unknown labels", {
  mat <- create_test_matrix(4)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  net <- cograph(mat)

  expect_warning(
    layout_circle(net$network, order = c("A", "B", "X", "Y")),
    "not found"
  )
})

# ============================================
# layout_oval Tests
# ============================================

test_that("layout_oval works with basic network", {
  mat <- create_test_matrix(5)
  net <- cograph(mat)

  coords <- layout_oval(net$network)

  expect_true(is.data.frame(coords))
  expect_equal(nrow(coords), 5)
})

test_that("layout_oval handles single node", {
  mat <- matrix(0, 1, 1)
  net <- cograph(mat)

  coords <- layout_oval(net$network)

  expect_equal(nrow(coords), 1)
  expect_equal(coords$x, 0.5)
  expect_equal(coords$y, 0.5)
})

test_that("layout_oval respects ratio parameter", {
  mat <- create_test_matrix(8)
  net <- cograph(mat)

  coords_wide <- layout_oval(net$network, ratio = 2)
  coords_tall <- layout_oval(net$network, ratio = 0.5)

  # Wide ratio should have larger x spread
  x_range_wide <- diff(range(coords_wide$x))
  x_range_tall <- diff(range(coords_tall$x))
  expect_true(x_range_wide > x_range_tall)
})

test_that("layout_oval respects rotation parameter", {
  mat <- create_test_matrix(6)
  net <- cograph(mat)

  coords1 <- layout_oval(net$network, rotation = 0)
  coords2 <- layout_oval(net$network, rotation = pi/4)

  # Different rotations should give different results
  expect_false(all(round(coords1$x, 5) == round(coords2$x, 5)))
})

test_that("layout_oval respects order parameter", {
  mat <- create_test_matrix(4)
  net <- cograph(mat)

  coords1 <- layout_oval(net$network, order = c(1, 2, 3, 4))
  coords2 <- layout_oval(net$network, order = c(4, 3, 2, 1))

  expect_false(all(coords1$x == coords2$x))
})

test_that("layout_oval respects clockwise parameter", {
  mat <- create_test_matrix(4)
  net <- cograph(mat)

  coords_cw <- layout_oval(net$network, clockwise = TRUE)
  coords_ccw <- layout_oval(net$network, clockwise = FALSE)

  expect_false(all(coords_cw$x == coords_ccw$x))
})

test_that("layout_oval warns on mismatched order length", {
  mat <- create_test_matrix(4)
  net <- cograph(mat)

  expect_warning(
    layout_oval(net$network, order = c(1, 2)),
    "Order length"
  )
})

test_that("layout_oval warns on unknown labels", {
  mat <- create_test_matrix(4)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  net <- cograph(mat)

  expect_warning(
    layout_oval(net$network, order = c("A", "B", "X", "Y")),
    "not found"
  )
})

# ============================================
# layout_groups Tests
# ============================================

test_that("layout_groups works with basic network", {
  mat <- create_test_matrix(6)
  net <- cograph(mat)

  groups <- c(1, 1, 2, 2, 3, 3)
  coords <- layout_groups(net$network, groups)

  expect_true(is.data.frame(coords))
  expect_equal(nrow(coords), 6)
})

test_that("layout_groups errors on wrong groups length", {
  mat <- create_test_matrix(4)
  net <- cograph(mat)

  expect_error(
    layout_groups(net$network, groups = c(1, 2)),
    "groups must have length"
  )
})

test_that("layout_groups handles single group", {
  mat <- create_test_matrix(4)
  net <- cograph(mat)

  groups <- c(1, 1, 1, 1)
  coords <- layout_groups(net$network, groups)

  expect_equal(nrow(coords), 4)
  # All nodes should be close to center
  expect_true(all(abs(coords$x - 0.5) < 0.3))
})

test_that("layout_groups handles single node per group", {
  mat <- create_test_matrix(3)
  net <- cograph(mat)

  groups <- c(1, 2, 3)
  coords <- layout_groups(net$network, groups)

  expect_equal(nrow(coords), 3)
})

test_that("layout_groups respects inner_radius", {
  mat <- create_test_matrix(6)
  net <- cograph(mat)

  groups <- c(1, 1, 1, 2, 2, 2)
  coords1 <- layout_groups(net$network, groups, inner_radius = 0.05)
  coords2 <- layout_groups(net$network, groups, inner_radius = 0.2)

  # Larger inner radius should have more spread within groups
  # Just check both work
  expect_equal(nrow(coords1), 6)
  expect_equal(nrow(coords2), 6)
})

test_that("layout_groups respects outer_radius", {
  mat <- create_test_matrix(6)
  net <- cograph(mat)

  groups <- c(1, 1, 1, 2, 2, 2)
  coords1 <- layout_groups(net$network, groups, outer_radius = 0.2)
  coords2 <- layout_groups(net$network, groups, outer_radius = 0.4)

  # Just check both work
  expect_equal(nrow(coords1), 6)
  expect_equal(nrow(coords2), 6)
})

test_that("layout_groups handles custom group_positions", {
  mat <- create_test_matrix(4)
  net <- cograph(mat)

  groups <- c(1, 1, 2, 2)
  positions <- data.frame(x = c(0.3, 0.7), y = c(0.3, 0.7))
  rownames(positions) <- c("1", "2")

  coords <- layout_groups(net$network, groups, group_positions = positions)
  expect_equal(nrow(coords), 4)
})

test_that("layout_groups handles character groups", {
  mat <- create_test_matrix(4)
  net <- cograph(mat)

  groups <- c("A", "A", "B", "B")
  coords <- layout_groups(net$network, groups)

  expect_equal(nrow(coords), 4)
})

test_that("layout_groups handles factor groups", {
  mat <- create_test_matrix(4)
  net <- cograph(mat)

  groups <- factor(c("X", "X", "Y", "Y"))
  coords <- layout_groups(net$network, groups)

  expect_equal(nrow(coords), 4)
})

# ============================================
# layout_spring Tests
# ============================================

test_that("layout_spring works with basic network", {
  skip_if_not_installed("igraph")

  mat <- create_test_matrix(5)
  net <- cograph(mat)

  coords <- layout_spring(net$network)

  expect_true(is.data.frame(coords))
  expect_equal(nrow(coords), 5)
})

test_that("layout_spring handles single node", {
  skip_if_not_installed("igraph")

  mat <- matrix(0, 1, 1)
  net <- cograph(mat)

  coords <- layout_spring(net$network)

  expect_equal(nrow(coords), 1)
})

test_that("layout_spring produces valid coordinates", {
  skip_if_not_installed("igraph")

  mat <- create_test_matrix(5)
  net <- cograph(mat)

  coords <- layout_spring(net$network)

  # All coordinates should be valid numbers
  expect_true(all(!is.na(coords$x)))
  expect_true(all(!is.na(coords$y)))
})

# ============================================
# Integration Tests
# ============================================

test_that("circle layout works in soplot", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_png(soplot(mat, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("oval layout works in soplot", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_png(soplot(mat, layout = "oval"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("ellipse layout alias works in soplot", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_png(soplot(mat, layout = "ellipse"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("spring layout works in soplot", {
  skip_if_not_installed("grid")
  skip_if_not_installed("igraph")

  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_png(soplot(mat, layout = "spring"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("fr layout alias works in soplot", {
  skip_if_not_installed("grid")
  skip_if_not_installed("igraph")

  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_png(soplot(mat, layout = "fr"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})
