# test-splot-params.R - Tests for splot parameter resolution functions
# Covers: R/splot-params.R

# Make internal functions available
resolve_edge_colors <- cograph:::resolve_edge_colors
resolve_edge_widths <- cograph:::resolve_edge_widths
resolve_node_sizes <- cograph:::resolve_node_sizes
resolve_label_sizes <- cograph:::resolve_label_sizes
resolve_node_colors <- cograph:::resolve_node_colors
resolve_labels <- cograph:::resolve_labels
resolve_edge_labels <- cograph:::resolve_edge_labels
resolve_shapes <- cograph:::resolve_shapes
resolve_curvatures <- cograph:::resolve_curvatures
resolve_loop_rotation <- cograph:::resolve_loop_rotation
filter_edges_by_weight <- cograph:::filter_edges_by_weight
get_edge_order <- cograph:::get_edge_order
get_node_order <- cograph:::get_node_order

# ============================================
# resolve_edge_colors Tests
# ============================================

test_that("resolve_edge_colors returns empty for no edges", {
  edges <- data.frame(from = integer(0), to = integer(0), weight = numeric(0))
  result <- resolve_edge_colors(edges)
  expect_equal(length(result), 0)
})
test_that("resolve_edge_colors uses explicit colors", {
  edges <- data.frame(from = 1:3, to = 2:4, weight = c(1, 2, 3))
  result <- resolve_edge_colors(edges, edge.color = "red")
  expect_equal(result, rep("red", 3))
})

test_that("resolve_edge_colors recycles explicit colors", {
  edges <- data.frame(from = 1:4, to = 2:5, weight = 1:4)
  result <- resolve_edge_colors(edges, edge.color = c("red", "blue"))
  expect_equal(result, c("red", "blue", "red", "blue"))
})

test_that("resolve_edge_colors colors by weight sign", {
  edges <- data.frame(from = 1:3, to = 2:4, weight = c(1, -1, 0))
  result <- resolve_edge_colors(edges, posCol = "green", negCol = "red")
  expect_equal(result[1], "green")
  expect_equal(result[2], "red")
  expect_equal(result[3], "gray50")  # default
})

test_that("resolve_edge_colors uses default for no weight", {
  edges <- data.frame(from = 1:3, to = 2:4)
  result <- resolve_edge_colors(edges, default_col = "purple")
  expect_equal(result, rep("purple", 3))
})

# ============================================
# resolve_edge_widths Tests
# ============================================

test_that("resolve_edge_widths returns empty for no edges", {
  edges <- data.frame(from = integer(0), to = integer(0))
  result <- resolve_edge_widths(edges)
  expect_equal(length(result), 0)
})

test_that("resolve_edge_widths uses explicit widths", {
  edges <- data.frame(from = 1:3, to = 2:4, weight = c(1, 2, 3))
  result <- resolve_edge_widths(edges, edge.width = 2.5)
  expect_equal(result, rep(2.5, 3))
})

test_that("resolve_edge_widths recycles explicit widths", {
  edges <- data.frame(from = 1:4, to = 2:5, weight = 1:4)
  result <- resolve_edge_widths(edges, edge.width = c(1, 2))
  expect_equal(result, c(1, 2, 1, 2))
})

test_that("resolve_edge_widths scales by weight", {
  edges <- data.frame(from = 1:3, to = 2:4, weight = c(0.1, 0.5, 1.0))
  result <- resolve_edge_widths(edges)

  expect_true(result[1] < result[2])
  expect_true(result[2] < result[3])
})

test_that("resolve_edge_widths uses default width when no weights", {
  edges <- data.frame(from = 1:3, to = 2:4)
  result <- resolve_edge_widths(edges)

  # All should be the same default
  expect_true(all(result == result[1]))
})

test_that("resolve_edge_widths respects edge_width_range", {
  edges <- data.frame(from = 1:3, to = 2:4, weight = c(0.1, 0.5, 1.0))
  result <- resolve_edge_widths(edges, edge_width_range = c(1, 5))

  expect_true(all(result >= 1))
  expect_true(all(result <= 5))
})

test_that("resolve_edge_widths respects esize parameter", {
  edges <- data.frame(from = 1:3, to = 2:4, weight = c(0.1, 0.5, 1.0))
  result <- resolve_edge_widths(edges, esize = 3)

  expect_equal(max(result), 3)
})

# ============================================
# resolve_node_sizes Tests
# ============================================

test_that("resolve_node_sizes uses default when NULL", {
  result <- resolve_node_sizes(NULL, n = 5)

  expect_equal(length(result), 5)
  expect_true(all(result == result[1]))
})

test_that("resolve_node_sizes applies scale factor", {
  result <- resolve_node_sizes(10, n = 3)

  # Default scale factor is 0.015
  expect_equal(result, rep(10 * 0.015, 3))
})

test_that("resolve_node_sizes recycles values", {
  result <- resolve_node_sizes(c(5, 10), n = 4)

  expect_equal(length(result), 4)
})

test_that("resolve_node_sizes respects custom scale factor", {
  result <- resolve_node_sizes(10, n = 3, scale_factor = 0.05)

  expect_equal(result, rep(0.5, 3))
})

test_that("resolve_node_sizes respects custom default", {
  result <- resolve_node_sizes(NULL, n = 3, default_size = 8)

  expect_true(all(result > 0))
})

test_that("resolve_node_sizes works with legacy scaling", {
  result_default <- resolve_node_sizes(10, n = 3, scaling = "default")
  result_legacy <- resolve_node_sizes(10, n = 3, scaling = "legacy")

  # Legacy has different scale factor
  expect_false(result_default[1] == result_legacy[1])
})

# ============================================
# resolve_label_sizes Tests
# ============================================

test_that("resolve_label_sizes uses explicit values", {
  result <- resolve_label_sizes(1.5, node_size_usr = rep(0.1, 3), n = 3)

  expect_equal(result, rep(1.5, 3))
})

test_that("resolve_label_sizes recycles explicit values", {
  result <- resolve_label_sizes(c(1, 2), node_size_usr = rep(0.1, 4), n = 4)

  expect_equal(result, c(1, 2, 1, 2))
})

test_that("resolve_label_sizes uses default when NULL (default mode)", {
  result <- resolve_label_sizes(NULL, node_size_usr = rep(0.1, 3), n = 3,
                                scaling = "default")

  # Default mode uses independent label size (1)
  expect_equal(result, rep(1, 3))
})

test_that("resolve_label_sizes couples to node size (legacy mode)", {
  result <- resolve_label_sizes(NULL, node_size_usr = rep(0.1, 3), n = 3,
                                scaling = "legacy")

  # Legacy mode: vsize_usr * 8, capped at 1
  expected <- pmin(1, 0.1 * 8)
  expect_equal(result, rep(expected, 3))
})

# ============================================
# resolve_node_colors Tests
# ============================================

test_that("resolve_node_colors uses explicit colors", {
  result <- resolve_node_colors("red", n = 3)

  expect_equal(result, rep("red", 3))
})

test_that("resolve_node_colors recycles colors", {
  result <- resolve_node_colors(c("red", "blue"), n = 4)

  expect_equal(result, c("red", "blue", "red", "blue"))
})

test_that("resolve_node_colors uses groups for coloring", {
  result <- resolve_node_colors(NULL, n = 4, groups = c(1, 1, 2, 2))

  # Group 1 and 2 should have different colors
  expect_false(result[1] == result[3])
  expect_equal(result[1], result[2])
  expect_equal(result[3], result[4])
})

test_that("resolve_node_colors uses node data color column", {
  nodes <- data.frame(id = 1:3, color = c("red", "green", "blue"))
  result <- resolve_node_colors(NULL, n = 3, nodes = nodes)

  expect_equal(result, c("red", "green", "blue"))
})

test_that("resolve_node_colors uses default when nothing specified", {
  result <- resolve_node_colors(NULL, n = 3, default_col = "purple")

  expect_equal(result, rep("purple", 3))
})

# ============================================
# resolve_labels Tests
# ============================================

test_that("resolve_labels returns NULL for NULL input", {
  result <- resolve_labels(NULL, nodes = NULL, n = 3)
  expect_null(result)
})

test_that("resolve_labels returns NULL for FALSE input", {
  result <- resolve_labels(FALSE, nodes = NULL, n = 3)
  expect_null(result)
})

test_that("resolve_labels uses node labels for TRUE", {
  nodes <- data.frame(id = 1:3, label = c("A", "B", "C"))
  result <- resolve_labels(TRUE, nodes = nodes, n = 3)

  expect_equal(result, c("A", "B", "C"))
})

test_that("resolve_labels uses indices for TRUE when no labels", {
  result <- resolve_labels(TRUE, nodes = NULL, n = 3)

  expect_equal(result, c("1", "2", "3"))
})

test_that("resolve_labels uses explicit labels", {
  result <- resolve_labels(c("X", "Y", "Z"), nodes = NULL, n = 3)

  expect_equal(result, c("X", "Y", "Z"))
})

test_that("resolve_labels recycles labels", {
  result <- resolve_labels(c("A", "B"), nodes = NULL, n = 4)

  expect_equal(result, c("A", "B", "A", "B"))
})

# ============================================
# resolve_edge_labels Tests
# ============================================

test_that("resolve_edge_labels returns NULL for NULL input", {
  edges <- data.frame(from = 1:3, to = 2:4)
  result <- resolve_edge_labels(NULL, edges = edges, m = 3)
  expect_null(result)
})

test_that("resolve_edge_labels returns NULL for FALSE input", {
  edges <- data.frame(from = 1:3, to = 2:4)
  result <- resolve_edge_labels(FALSE, edges = edges, m = 3)
  expect_null(result)
})

test_that("resolve_edge_labels uses weights for TRUE", {
  edges <- data.frame(from = 1:3, to = 2:4, weight = c(0.123, 0.456, 0.789))
  result <- resolve_edge_labels(TRUE, edges = edges, m = 3)

  expect_equal(result, c("0.12", "0.46", "0.79"))
})

test_that("resolve_edge_labels returns empty strings for TRUE when no weights", {
  edges <- data.frame(from = 1:3, to = 2:4)
  result <- resolve_edge_labels(TRUE, edges = edges, m = 3)

  expect_equal(result, rep("", 3))
})

test_that("resolve_edge_labels uses explicit labels", {
  edges <- data.frame(from = 1:3, to = 2:4)
  result <- resolve_edge_labels(c("a", "b", "c"), edges = edges, m = 3)

  expect_equal(result, c("a", "b", "c"))
})

# ============================================
# resolve_shapes Tests
# ============================================

test_that("resolve_shapes uses circle as default", {
  result <- resolve_shapes(NULL, n = 3)

  expect_equal(result, rep("circle", 3))
})

test_that("resolve_shapes uses explicit shape", {
  result <- resolve_shapes("square", n = 3)

  expect_equal(result, rep("square", 3))
})

test_that("resolve_shapes recycles shapes", {
  result <- resolve_shapes(c("circle", "square"), n = 4)

  expect_equal(result, c("circle", "square", "circle", "square"))
})

# ============================================
# resolve_curvatures Tests
# ============================================

test_that("resolve_curvatures returns empty for no edges", {
  edges <- data.frame(from = integer(0), to = integer(0))
  result <- resolve_curvatures(0, edges)
  expect_equal(length(result), 0)
})

test_that("resolve_curvatures recycles values", {
  edges <- data.frame(from = 1:4, to = 2:5)
  result <- resolve_curvatures(c(0.1, 0.2), edges)

  expect_equal(result, c(0.1, 0.2, 0.1, 0.2))
})

test_that("resolve_curvatures applies curvature for reciprocal edges", {
  # Create edges with reciprocal pair
  edges <- data.frame(from = c(1, 2), to = c(2, 1))
  result <- resolve_curvatures(0, edges, curveScale = TRUE)

  # Both edges should get default curvature
  expect_true(all(result != 0))
})

test_that("resolve_curvatures skips self-loops", {
  edges <- data.frame(from = c(1, 2), to = c(1, 3))  # first is self-loop
  result <- resolve_curvatures(0, edges, curveScale = TRUE)

  # Self-loop should keep original curvature
  expect_equal(result[1], 0)
})

test_that("resolve_curvatures respects curveScale = FALSE", {
  edges <- data.frame(from = c(1, 2), to = c(2, 1))
  result <- resolve_curvatures(0, edges, curveScale = FALSE)

  # Should not modify for reciprocals
  expect_equal(result, c(0, 0))
})

# ============================================
# resolve_loop_rotation Tests
# ============================================

test_that("resolve_loop_rotation returns empty for no edges", {
  edges <- data.frame(from = integer(0), to = integer(0))
  result <- resolve_loop_rotation(NULL, edges)
  expect_equal(length(result), 0)
})

test_that("resolve_loop_rotation uses default pi/2 rotation", {
  edges <- data.frame(from = c(1, 2), to = c(1, 3))  # first is self-loop
  result <- resolve_loop_rotation(NULL, edges)

  expect_equal(result[1], pi/2)
})

test_that("resolve_loop_rotation uses explicit values", {
  edges <- data.frame(from = c(1, 2), to = c(1, 3))
  result <- resolve_loop_rotation(c(0, pi), edges)

  expect_equal(result, c(0, pi))
})

test_that("resolve_loop_rotation calculates from layout", {
  edges <- data.frame(from = c(1, 2), to = c(1, 2))  # both self-loops
  layout <- matrix(c(0, 1, 0, 1), ncol = 2)  # nodes at (0,0) and (1,1)

  result <- resolve_loop_rotation(NULL, edges, layout = layout)

  # Rotations should point away from center (0.5, 0.5)
  expect_true(abs(result[1] - result[2]) > 0.1)  # Different angles
})

# ============================================
# filter_edges_by_weight Tests
# ============================================

test_that("filter_edges_by_weight returns all for minimum = 0", {
  edges <- data.frame(from = 1:3, to = 2:4, weight = c(0.1, 0.5, 1.0))
  result <- filter_edges_by_weight(edges, minimum = 0)

  expect_equal(nrow(result), 3)
})

test_that("filter_edges_by_weight filters below threshold", {
  edges <- data.frame(from = 1:3, to = 2:4, weight = c(0.1, 0.5, 1.0))
  result <- filter_edges_by_weight(edges, minimum = 0.3)

  expect_equal(nrow(result), 2)
  expect_true(all(abs(result$weight) >= 0.3))
})

test_that("filter_edges_by_weight uses absolute weight", {
  edges <- data.frame(from = 1:3, to = 2:4, weight = c(-1.0, -0.1, 0.5))
  result <- filter_edges_by_weight(edges, minimum = 0.3)

  expect_equal(nrow(result), 2)
})

test_that("filter_edges_by_weight returns all if no weight column", {
  edges <- data.frame(from = 1:3, to = 2:4)
  result <- filter_edges_by_weight(edges, minimum = 0.5)

  expect_equal(nrow(result), 3)
})

# ============================================
# get_edge_order Tests
# ============================================

test_that("get_edge_order returns indices for no weights", {
  edges <- data.frame(from = 1:3, to = 2:4)
  result <- get_edge_order(edges)

  expect_equal(result, 1:3)
})

test_that("get_edge_order returns empty for empty edges", {
  edges <- data.frame(from = integer(0), to = integer(0))
  result <- get_edge_order(edges)

  expect_equal(length(result), 0)
})

test_that("get_edge_order orders weakest to strongest", {
  edges <- data.frame(from = 1:3, to = 2:4, weight = c(1.0, 0.1, 0.5))
  result <- get_edge_order(edges)

  # Weakest (0.1 at index 2) should come first
  expect_equal(result[1], 2)
  expect_equal(result[3], 1)  # Strongest (1.0) last
})

test_that("get_edge_order uses absolute weights", {
  edges <- data.frame(from = 1:3, to = 2:4, weight = c(-1.0, 0.1, -0.5))
  result <- get_edge_order(edges)

  # Order: 0.1, -0.5, -1.0
  expect_equal(result[1], 2)
  expect_equal(result[3], 1)
})

# ============================================
# get_node_order Tests
# ============================================

test_that("get_node_order orders largest to smallest", {
  sizes <- c(0.1, 0.5, 0.3)
  result <- get_node_order(sizes)

  # Largest (0.5 at index 2) should come first
  expect_equal(result[1], 2)
  expect_equal(result[3], 1)  # Smallest last
})

test_that("get_node_order handles equal sizes", {
  sizes <- c(0.5, 0.5, 0.5)
  result <- get_node_order(sizes)

  # Should return some valid ordering
  expect_equal(length(result), 3)
  expect_true(all(result %in% 1:3))
})

test_that("get_node_order handles single element", {
  sizes <- c(0.5)
  result <- get_node_order(sizes)

  expect_equal(result, 1)
})

# ============================================
# Integration Tests
# ============================================

test_that("resolve functions work in splot pipeline", {
  mat <- create_test_matrix(4, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(splot(mat,
                        node_size = 8,
                        node_fill = c("red", "blue", "green", "yellow"),
                        edge_width_range = c(0.5, 3),
                        labels = TRUE))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("resolve functions handle NA values gracefully", {
  edges <- data.frame(from = 1:3, to = 2:4, weight = c(1, NA, 0.5))

  # Should not error
  colors <- resolve_edge_colors(edges)
  expect_equal(length(colors), 3)
})
