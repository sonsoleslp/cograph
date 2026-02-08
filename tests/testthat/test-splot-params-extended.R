# Extended tests for splot parameter resolution functions
# Covers: R/splot-params.R

# ============================================
# resolve_edge_colors Tests
# ============================================

test_that("resolve_edge_colors returns empty for no edges", {
  edges <- data.frame(from = integer(0), to = integer(0), weight = numeric(0))
  result <- cograph:::resolve_edge_colors(edges)
  expect_equal(length(result), 0)
})

test_that("resolve_edge_colors uses user-specified colors", {
  edges <- data.frame(from = 1:3, to = 2:4, weight = c(0.5, -0.3, 0.8))
  result <- cograph:::resolve_edge_colors(edges, edge.color = "red")
  expect_equal(result, rep("red", 3))
})

test_that("resolve_edge_colors recycles user colors", {
  edges <- data.frame(from = 1:4, to = 2:5, weight = rep(1, 4))
  result <- cograph:::resolve_edge_colors(edges, edge.color = c("red", "blue"))
  expect_equal(result, c("red", "blue", "red", "blue"))
})

test_that("resolve_edge_colors colors by weight sign", {
  edges <- data.frame(from = 1:3, to = 2:4, weight = c(0.5, -0.3, 0))
  result <- cograph:::resolve_edge_colors(edges, posCol = "green", negCol = "red")

  expect_equal(result[1], "green")
  expect_equal(result[2], "red")
  expect_equal(result[3], "gray50")  # zero weight = default
})

test_that("resolve_edge_colors uses default when no weights", {
  edges <- data.frame(from = 1:3, to = 2:4)
  result <- cograph:::resolve_edge_colors(edges)
  expect_equal(result, rep("gray50", 3))
})

test_that("resolve_edge_colors uses custom default color", {
  edges <- data.frame(from = 1:3, to = 2:4)
  result <- cograph:::resolve_edge_colors(edges, default_col = "black")
  expect_equal(result, rep("black", 3))
})

# ============================================
# resolve_edge_widths Tests
# ============================================

test_that("resolve_edge_widths returns empty for no edges", {
  edges <- data.frame(from = integer(0), to = integer(0), weight = numeric(0))
  result <- cograph:::resolve_edge_widths(edges)
  expect_equal(length(result), 0)
})

test_that("resolve_edge_widths uses explicit widths", {
  edges <- data.frame(from = 1:3, to = 2:4, weight = c(0.5, 0.3, 0.8))
  result <- cograph:::resolve_edge_widths(edges, edge.width = c(1, 2, 3))
  expect_equal(result, c(1, 2, 3))
})

test_that("resolve_edge_widths recycles explicit widths", {
  edges <- data.frame(from = 1:4, to = 2:5, weight = rep(1, 4))
  result <- cograph:::resolve_edge_widths(edges, edge.width = c(1, 2))
  expect_equal(result, c(1, 2, 1, 2))
})

test_that("resolve_edge_widths scales by weight", {
  edges <- data.frame(from = 1:3, to = 2:4, weight = c(0.2, 0.5, 1.0))
  result <- cograph:::resolve_edge_widths(edges)

  expect_equal(length(result), 3)
  expect_true(all(result > 0))
  # Stronger weight should have larger width
  expect_true(result[3] >= result[1])
})

test_that("resolve_edge_widths uses default width without weights", {
  edges <- data.frame(from = 1:3, to = 2:4)
  result <- cograph:::resolve_edge_widths(edges)

  expect_equal(length(result), 3)
  expect_true(all(result > 0))
})

# ============================================
# resolve_node_sizes Tests
# ============================================

test_that("resolve_node_sizes uses default when vsize is NULL", {
  result <- cograph:::resolve_node_sizes(NULL, n = 5)

  expect_equal(length(result), 5)
  expect_true(all(result > 0))
})

test_that("resolve_node_sizes uses scalar vsize", {
  result <- cograph:::resolve_node_sizes(10, n = 3)

  expect_equal(length(result), 3)
  expect_true(all(result > 0))
})

test_that("resolve_node_sizes uses vector vsize", {
  result <- cograph:::resolve_node_sizes(c(5, 10, 15), n = 3)

  expect_equal(length(result), 3)
  expect_true(result[1] < result[2])
  expect_true(result[2] < result[3])
})

test_that("resolve_node_sizes recycles vsize", {
  result <- cograph:::resolve_node_sizes(c(5, 10), n = 4)

  expect_equal(length(result), 4)
})

# ============================================
# resolve_node_colors Tests
# ============================================

test_that("resolve_node_colors uses specified colors", {
  result <- cograph:::resolve_node_colors(c("red", "blue", "green"), n = 3)

  expect_equal(result, c("red", "blue", "green"))
})

test_that("resolve_node_colors recycles colors", {
  result <- cograph:::resolve_node_colors(c("red", "blue"), n = 4)

  expect_equal(result, c("red", "blue", "red", "blue"))
})

test_that("resolve_node_colors uses default when NULL", {
  result <- cograph:::resolve_node_colors(NULL, n = 3)

  expect_equal(length(result), 3)
  expect_equal(result, rep("#4A90D9", 3))
})

test_that("resolve_node_colors colors by groups", {
  result <- cograph:::resolve_node_colors(
    NULL, n = 4, groups = c("A", "A", "B", "B")
  )

  expect_equal(length(result), 4)
  expect_equal(result[1], result[2])  # same group = same color
  expect_equal(result[3], result[4])  # same group = same color
  expect_false(result[1] == result[3])  # different groups = different colors
})

test_that("resolve_node_colors uses node data color column", {
  nodes <- data.frame(id = 1:3, color = c("red", "green", "blue"))
  result <- cograph:::resolve_node_colors(NULL, n = 3, nodes = nodes)

  expect_equal(result, c("red", "green", "blue"))
})

# ============================================
# resolve_labels Tests
# ============================================

test_that("resolve_labels returns NULL for FALSE", {
  result <- cograph:::resolve_labels(FALSE, NULL, 5)
  expect_null(result)
})

test_that("resolve_labels returns NULL for NULL", {
  result <- cograph:::resolve_labels(NULL, NULL, 5)
  expect_null(result)
})

test_that("resolve_labels uses node labels for TRUE", {
  nodes <- data.frame(id = 1:3, label = c("A", "B", "C"))
  result <- cograph:::resolve_labels(TRUE, nodes, 3)
  expect_equal(result, c("A", "B", "C"))
})

test_that("resolve_labels uses indices when no node labels", {
  result <- cograph:::resolve_labels(TRUE, NULL, 4)
  expect_equal(result, c("1", "2", "3", "4"))
})

test_that("resolve_labels uses user-provided labels", {
  result <- cograph:::resolve_labels(c("X", "Y", "Z"), NULL, 3)
  expect_equal(result, c("X", "Y", "Z"))
})

test_that("resolve_labels recycles user labels", {
  result <- cograph:::resolve_labels(c("A", "B"), NULL, 4)
  expect_equal(result, c("A", "B", "A", "B"))
})

# ============================================
# resolve_edge_labels Tests
# ============================================

test_that("resolve_edge_labels returns NULL for FALSE", {
  result <- cograph:::resolve_edge_labels(FALSE, NULL, 5)
  expect_null(result)
})

test_that("resolve_edge_labels returns NULL for NULL", {
  result <- cograph:::resolve_edge_labels(NULL, NULL, 5)
  expect_null(result)
})

test_that("resolve_edge_labels uses weights for TRUE", {
  edges <- data.frame(from = 1:3, to = 2:4, weight = c(0.123, 0.456, 0.789))
  result <- cograph:::resolve_edge_labels(TRUE, edges, 3)

  expect_equal(result, c("0.12", "0.46", "0.79"))
})

test_that("resolve_edge_labels returns empty strings when no weights", {
  edges <- data.frame(from = 1:3, to = 2:4)
  result <- cograph:::resolve_edge_labels(TRUE, edges, 3)

  expect_equal(result, rep("", 3))
})

test_that("resolve_edge_labels uses user-provided labels", {
  result <- cograph:::resolve_edge_labels(c("a", "b", "c"), NULL, 3)
  expect_equal(result, c("a", "b", "c"))
})

# ============================================
# resolve_shapes Tests
# ============================================

test_that("resolve_shapes defaults to circle", {
  result <- cograph:::resolve_shapes(NULL, 3)
  expect_equal(result, rep("circle", 3))
})

test_that("resolve_shapes recycles shapes", {
  result <- cograph:::resolve_shapes(c("circle", "square"), 4)
  expect_equal(result, c("circle", "square", "circle", "square"))
})

test_that("resolve_shapes uses scalar shape", {
  result <- cograph:::resolve_shapes("diamond", 3)
  expect_equal(result, rep("diamond", 3))
})

# ============================================
# resolve_curvatures Tests
# ============================================

test_that("resolve_curvatures returns empty for no edges", {
  edges <- data.frame(from = integer(0), to = integer(0))
  result <- cograph:::resolve_curvatures(0, edges)
  expect_equal(length(result), 0)
})

test_that("resolve_curvatures recycles scalar", {
  edges <- data.frame(from = 1:3, to = 2:4)
  result <- cograph:::resolve_curvatures(0.3, edges)
  expect_equal(result, rep(0.3, 3))
})

test_that("resolve_curvatures detects reciprocal edges", {
  edges <- data.frame(from = c(1, 2), to = c(2, 1))
  result <- cograph:::resolve_curvatures(0, edges, curveScale = TRUE)

  # Both should get curvature for reciprocal detection
  expect_true(all(result > 0))
})

test_that("resolve_curvatures skips self-loops in reciprocal check", {
  edges <- data.frame(from = c(1, 1), to = c(1, 2))
  result <- cograph:::resolve_curvatures(0, edges, curveScale = TRUE)

  # Self-loop should keep 0 curvature (no reciprocal detection)
  expect_equal(result[1], 0)
})

test_that("resolve_curvatures respects curveScale = FALSE", {
  edges <- data.frame(from = c(1, 2), to = c(2, 1))
  result <- cograph:::resolve_curvatures(0, edges, curveScale = FALSE)

  expect_equal(result, c(0, 0))
})

# ============================================
# resolve_loop_rotation Tests
# ============================================

test_that("resolve_loop_rotation returns empty for no edges", {
  edges <- data.frame(from = integer(0), to = integer(0))
  result <- cograph:::resolve_loop_rotation(NULL, edges)
  expect_equal(length(result), 0)
})

test_that("resolve_loop_rotation defaults to pi/2", {
  edges <- data.frame(from = c(1, 1), to = c(1, 2))
  result <- cograph:::resolve_loop_rotation(NULL, edges)

  expect_equal(result[1], pi / 2)
})

test_that("resolve_loop_rotation uses layout for auto rotation", {
  edges <- data.frame(from = c(1, 2), to = c(1, 2))
  layout <- matrix(c(0, 1, 0, 1), ncol = 2)  # nodes at corners
  result <- cograph:::resolve_loop_rotation(NULL, edges, layout = layout)

  expect_equal(length(result), 2)
})

test_that("resolve_loop_rotation uses user-specified rotation", {
  edges <- data.frame(from = c(1, 2), to = c(1, 2))
  result <- cograph:::resolve_loop_rotation(c(0, pi), edges)

  expect_equal(result, c(0, pi))
})

# ============================================
# resolve_label_sizes Tests
# ============================================

test_that("resolve_label_sizes uses user-specified sizes", {
  result <- cograph:::resolve_label_sizes(c(0.5, 0.8), NULL, 2)
  expect_equal(result, c(0.5, 0.8))
})

test_that("resolve_label_sizes uses default when NULL", {
  result <- cograph:::resolve_label_sizes(NULL, rep(0.05, 3), 3)
  expect_equal(length(result), 3)
  expect_true(all(result > 0))
})

# ============================================
# filter_edges_by_weight Tests
# ============================================

test_that("filter_edges_by_weight returns all when minimum = 0", {
  edges <- data.frame(from = 1:3, to = 2:4, weight = c(0.1, 0.5, 0.9))
  result <- cograph:::filter_edges_by_weight(edges, minimum = 0)
  expect_equal(nrow(result), 3)
})

test_that("filter_edges_by_weight filters by threshold", {
  edges <- data.frame(from = 1:3, to = 2:4, weight = c(0.1, 0.5, 0.9))
  result <- cograph:::filter_edges_by_weight(edges, minimum = 0.3)
  expect_equal(nrow(result), 2)
})

test_that("filter_edges_by_weight uses absolute value", {
  edges <- data.frame(from = 1:3, to = 2:4, weight = c(-0.5, 0.1, 0.3))
  result <- cograph:::filter_edges_by_weight(edges, minimum = 0.3)
  expect_equal(nrow(result), 2)  # -0.5 and 0.3 both have abs >= 0.3
})

test_that("filter_edges_by_weight returns all when no weight column", {
  edges <- data.frame(from = 1:3, to = 2:4)
  result <- cograph:::filter_edges_by_weight(edges, minimum = 0.5)
  expect_equal(nrow(result), 3)
})

# ============================================
# get_edge_order / get_node_order Tests
# ============================================

test_that("get_edge_order returns weakest-to-strongest order", {
  edges <- data.frame(from = 1:3, to = 2:4, weight = c(0.5, 0.1, 0.9))
  result <- cograph:::get_edge_order(edges)

  expect_equal(result[1], 2)  # weakest first
  expect_equal(result[3], 3)  # strongest last
})

test_that("get_edge_order returns sequential for no weights", {
  edges <- data.frame(from = 1:3, to = 2:4)
  result <- cograph:::get_edge_order(edges)

  expect_equal(result, 1:3)
})

test_that("get_edge_order returns empty for no edges", {
  edges <- data.frame(from = integer(0), to = integer(0))
  result <- cograph:::get_edge_order(edges)

  expect_equal(length(result), 0)
})

test_that("get_node_order returns largest-to-smallest order", {
  sizes <- c(0.5, 0.1, 0.9, 0.3)
  result <- cograph:::get_node_order(sizes)

  expect_equal(result[1], 3)  # largest first
  expect_equal(result[4], 2)  # smallest last
})
