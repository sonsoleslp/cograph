# Exhaustive tests for splot-polygons.R internal functions
# Covers: R/splot-polygons.R

# ============================================
# circle_vertices Tests
# ============================================

test_that("circle_vertices generates correct structure", {
  result <- circle_vertices(0.5, 0.5, 0.1)

  expect_true(is.list(result))
  expect_true("x" %in% names(result))
  expect_true("y" %in% names(result))
  expect_equal(length(result$x), length(result$y))
})

test_that("circle_vertices respects n parameter", {
  result_10 <- circle_vertices(0.5, 0.5, 0.1, n = 10)
  result_100 <- circle_vertices(0.5, 0.5, 0.1, n = 100)

  expect_equal(length(result_10$x), 10)
  expect_equal(length(result_100$x), 100)
})

test_that("circle_vertices generates points on circle", {
  result <- circle_vertices(0.5, 0.5, 0.1)

  # Check all points are at radius 0.1 from center
  distances <- sqrt((result$x - 0.5)^2 + (result$y - 0.5)^2)
  expect_true(all(abs(distances - 0.1) < 1e-10))
})

# ============================================
# square_vertices Tests
# ============================================

test_that("square_vertices generates 4 vertices", {
  result <- square_vertices(0.5, 0.5, 0.1)

  expect_equal(length(result$x), 4)
  expect_equal(length(result$y), 4)
})

test_that("square_vertices generates correct corners", {
  result <- square_vertices(0, 0, 1)

  # Should have corners at (-1,-1), (1,-1), (1,1), (-1,1)
  expect_true(-1 %in% result$x)
  expect_true(1 %in% result$x)
  expect_true(-1 %in% result$y)
  expect_true(1 %in% result$y)
})

# ============================================
# rectangle_vertices Tests
# ============================================

test_that("rectangle_vertices generates 4 vertices", {
  result <- rectangle_vertices(0.5, 0.5, 0.2, 0.1)

  expect_equal(length(result$x), 4)
  expect_equal(length(result$y), 4)
})

test_that("rectangle_vertices respects width and height", {
  result <- rectangle_vertices(0, 0, 2, 1)

  x_range <- diff(range(result$x))
  y_range <- diff(range(result$y))

  expect_equal(x_range, 4)  # Width = 2 * 2 = 4
  expect_equal(y_range, 2)  # Height = 1 * 2 = 2
})

# ============================================
# triangle_vertices Tests
# ============================================

test_that("triangle_vertices generates 3 vertices", {
  result <- triangle_vertices(0.5, 0.5, 0.1)

  expect_equal(length(result$x), 3)
  expect_equal(length(result$y), 3)
})

test_that("triangle_vertices has vertex at top", {
  result <- triangle_vertices(0.5, 0.5, 0.1)

  # First vertex should be at top (y = 0.6)
  max_y_idx <- which.max(result$y)
  expect_equal(result$x[max_y_idx], 0.5, tolerance = 1e-10)
})

# ============================================
# diamond_vertices Tests
# ============================================

test_that("diamond_vertices generates 4 vertices", {
  result <- diamond_vertices(0.5, 0.5, 0.1)

  expect_equal(length(result$x), 4)
  expect_equal(length(result$y), 4)
})

test_that("diamond_vertices has vertices on axes", {
  result <- diamond_vertices(0, 0, 1)

  # Should have vertices at (1,0), (0,1), (-1,0), (0,-1)
  expect_true(any(abs(result$x - 1) < 1e-10 & abs(result$y) < 1e-10))
  expect_true(any(abs(result$x + 1) < 1e-10 & abs(result$y) < 1e-10))
})

# ============================================
# pentagon_vertices Tests
# ============================================

test_that("pentagon_vertices generates 5 vertices", {
  result <- pentagon_vertices(0.5, 0.5, 0.1)

  expect_equal(length(result$x), 5)
  expect_equal(length(result$y), 5)
})

# ============================================
# hexagon_vertices Tests
# ============================================

test_that("hexagon_vertices generates 6 vertices", {
  result <- hexagon_vertices(0.5, 0.5, 0.1)

  expect_equal(length(result$x), 6)
  expect_equal(length(result$y), 6)
})

# ============================================
# star_vertices Tests
# ============================================

test_that("star_vertices generates correct number of vertices", {
  result <- star_vertices(0.5, 0.5, 0.1, n_points = 5)

  expect_equal(length(result$x), 10)  # 5 points * 2 (outer + inner)
})

test_that("star_vertices respects n_points parameter", {
  result_5 <- star_vertices(0.5, 0.5, 0.1, n_points = 5)
  result_6 <- star_vertices(0.5, 0.5, 0.1, n_points = 6)

  expect_equal(length(result_5$x), 10)
  expect_equal(length(result_6$x), 12)
})

test_that("star_vertices respects inner_ratio parameter", {
  result <- star_vertices(0, 0, 1, n_points = 5, inner_ratio = 0.5)

  distances <- sqrt(result$x^2 + result$y^2)
  # Should have alternating 1 and 0.5 distances
  expect_true(any(abs(distances - 1) < 1e-10))
  expect_true(any(abs(distances - 0.5) < 1e-10))
})

# ============================================
# heart_vertices Tests
# ============================================

test_that("heart_vertices generates vertices", {
  result <- heart_vertices(0.5, 0.5, 0.1)

  expect_true(is.list(result))
  expect_true(length(result$x) > 0)
  expect_equal(length(result$x), length(result$y))
})

test_that("heart_vertices respects n parameter", {
  result <- heart_vertices(0.5, 0.5, 0.1, n = 50)

  expect_equal(length(result$x), 50)
})

# ============================================
# ellipse_vertices Tests
# ============================================

test_that("ellipse_vertices generates vertices", {
  result <- ellipse_vertices(0.5, 0.5, 0.2, 0.1)

  expect_true(is.list(result))
  expect_true(length(result$x) > 0)
})

test_that("ellipse_vertices respects rx and ry", {
  result <- ellipse_vertices(0, 0, 2, 1)

  x_range <- max(result$x) - min(result$x)
  y_range <- max(result$y) - min(result$y)

  expect_equal(x_range, 4, tolerance = 0.1)  # 2 * 2 = 4
  expect_equal(y_range, 2, tolerance = 0.1)  # 2 * 1 = 2
})

# ============================================
# cross_vertices Tests
# ============================================

test_that("cross_vertices generates 12 vertices", {
  result <- cross_vertices(0.5, 0.5, 0.1)

  expect_equal(length(result$x), 12)
  expect_equal(length(result$y), 12)
})

test_that("cross_vertices respects thickness parameter", {
  result_thin <- cross_vertices(0, 0, 1, thickness = 0.2)
  result_thick <- cross_vertices(0, 0, 1, thickness = 0.5)

  # Different thicknesses should produce different shapes
  expect_false(all(result_thin$x == result_thick$x))
})

# ============================================
# regular_polygon_vertices Tests
# ============================================

test_that("regular_polygon_vertices generates correct number of vertices", {
  result_3 <- regular_polygon_vertices(0.5, 0.5, 0.1, n = 3)
  result_7 <- regular_polygon_vertices(0.5, 0.5, 0.1, n = 7)

  expect_equal(length(result_3$x), 3)
  expect_equal(length(result_7$x), 7)
})

test_that("regular_polygon_vertices respects rotation", {
  result_0 <- regular_polygon_vertices(0.5, 0.5, 0.1, n = 4, rotation = 0)
  result_45 <- regular_polygon_vertices(0.5, 0.5, 0.1, n = 4, rotation = pi/4)

  expect_false(all(result_0$x == result_45$x))
})

# ============================================
# inset_polygon_vertices Tests
# ============================================

test_that("inset_polygon_vertices creates smaller polygon", {
  outer <- circle_vertices(0.5, 0.5, 0.1)
  inner <- inset_polygon_vertices(outer, 0.5)

  outer_size <- max(outer$x) - min(outer$x)
  inner_size <- max(inner$x) - min(inner$x)

  expect_true(inner_size < outer_size)
})

test_that("inset_polygon_vertices preserves structure", {
  outer <- square_vertices(0.5, 0.5, 0.1)
  inner <- inset_polygon_vertices(outer, 0.5)

  expect_equal(length(inner$x), length(outer$x))
})

test_that("inset_polygon_vertices ratio 1 gives same polygon", {
  outer <- triangle_vertices(0.5, 0.5, 0.1)
  inner <- inset_polygon_vertices(outer, 1)

  expect_equal(inner$x, outer$x, tolerance = 1e-10)
  expect_equal(inner$y, outer$y, tolerance = 1e-10)
})

test_that("inset_polygon_vertices ratio 0 gives center point", {
  outer <- hexagon_vertices(0.5, 0.5, 0.1)
  inner <- inset_polygon_vertices(outer, 0)

  # All points should be at center
  expect_true(all(abs(inner$x - 0.5) < 1e-10))
  expect_true(all(abs(inner$y - 0.5) < 1e-10))
})

# ============================================
# get_donut_base_vertices Tests
# ============================================

test_that("get_donut_base_vertices returns circle for circle shape", {
  result <- get_donut_base_vertices("circle", 0.5, 0.5, 0.1)

  expect_true(is.list(result))
  expect_true(length(result$x) > 4)  # More than polygon vertices
})

test_that("get_donut_base_vertices returns square for square shape", {
  result <- get_donut_base_vertices("square", 0.5, 0.5, 0.1)

  expect_equal(length(result$x), 4)
})

test_that("get_donut_base_vertices returns triangle for triangle shape", {
  result <- get_donut_base_vertices("triangle", 0.5, 0.5, 0.1)

  expect_equal(length(result$x), 3)
})

test_that("get_donut_base_vertices returns pentagon for pentagon shape", {
  result <- get_donut_base_vertices("pentagon", 0.5, 0.5, 0.1)

  expect_equal(length(result$x), 5)
})

test_that("get_donut_base_vertices defaults to circle for unknown shape", {
  result <- get_donut_base_vertices("unknown_shape", 0.5, 0.5, 0.1)

  expect_true(length(result$x) > 4)
})

# ============================================
# gear_vertices Tests
# ============================================

test_that("gear_vertices generates vertices", {
  result <- gear_vertices(0.5, 0.5, 0.1)

  expect_true(is.list(result))
  expect_true(length(result$x) > 0)
})

test_that("gear_vertices respects n_teeth parameter", {
  result_6 <- gear_vertices(0.5, 0.5, 0.1, n_teeth = 6)
  result_10 <- gear_vertices(0.5, 0.5, 0.1, n_teeth = 10)

  # More teeth = more vertices
  expect_true(length(result_10$x) > length(result_6$x))
})

# ============================================
# cloud_vertices Tests
# ============================================

test_that("cloud_vertices generates vertices", {
  result <- cloud_vertices(0.5, 0.5, 0.1)

  expect_true(is.list(result))
  expect_true(length(result$x) > 0)
})

test_that("cloud_vertices respects n parameter", {
  result <- cloud_vertices(0.5, 0.5, 0.1, n = 50)

  expect_equal(length(result$x), 50)
})

# ============================================
# brain_vertices Tests
# ============================================

test_that("brain_vertices generates vertices", {
  result <- brain_vertices(0.5, 0.5, 0.1)

  expect_true(is.list(result))
  expect_true(length(result$x) > 0)
})

test_that("brain_vertices respects n parameter", {
  result <- brain_vertices(0.5, 0.5, 0.1, n = 40)

  expect_equal(length(result$x), 40)
})

# ============================================
# get_shape_vertices Tests
# ============================================

test_that("get_shape_vertices dispatches to circle", {
  result <- get_shape_vertices("circle", 0.5, 0.5, 0.1)
  expect_true(length(result$x) > 4)
})

test_that("get_shape_vertices dispatches to square", {
  result <- get_shape_vertices("square", 0.5, 0.5, 0.1)
  expect_equal(length(result$x), 4)
})

test_that("get_shape_vertices dispatches to triangle", {
  result <- get_shape_vertices("triangle", 0.5, 0.5, 0.1)
  expect_equal(length(result$x), 3)
})

test_that("get_shape_vertices dispatches to star", {
  result <- get_shape_vertices("star", 0.5, 0.5, 0.1)
  expect_equal(length(result$x), 10)  # 5 points * 2
})

test_that("get_shape_vertices dispatches to heart", {
  result <- get_shape_vertices("heart", 0.5, 0.5, 0.1)
  expect_true(length(result$x) > 4)
})

test_that("get_shape_vertices handles ellipse with r2", {
  result <- get_shape_vertices("ellipse", 0.5, 0.5, 0.2, r2 = 0.1)
  expect_true(length(result$x) > 4)
})

test_that("get_shape_vertices defaults to circle for unknown shape", {
  result <- get_shape_vertices("unknown_xyz", 0.5, 0.5, 0.1)
  expect_true(length(result$x) > 4)
})

# ============================================
# Integration Tests with splot
# ============================================

test_that("splot renders shapes correctly", {
  mat <- create_test_matrix(4)

  shapes <- c("circle", "square", "triangle", "diamond")

  result <- with_temp_png({
    splot(mat, node_shape = shapes, layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("splot renders special shapes", {
  mat <- create_test_matrix(4)

  shapes <- c("star", "heart", "hexagon", "pentagon")

  result <- with_temp_png({
    splot(mat, node_shape = shapes, layout = "circle")
    TRUE
  })

  expect_true(result)
})
