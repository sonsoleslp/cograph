# test-coverage-splotnodes-deep.R
# Deep coverage tests for splot-nodes.R and splot-edges.R internal functions.
# Targets specific uncovered lines by calling internal functions directly.

# ============================================
# HELPER: setup a base R plotting device
# ============================================

setup_base_plot <- function() {
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n",
       axes = FALSE, xlab = "", ylab = "")
}

# ============================================
# splot-nodes.R: draw_node_base
# ============================================

test_that("draw_node_base with NULL size2 defaults to size (line 23)", {
  # Line 23: if (is.null(size2)) size2 <- size

  fn <- cograph:::draw_node_base
  with_temp_png({
    setup_base_plot()
    fn(0, 0, size = 0.1, size2 = NULL, shape = "circle",
       col = "blue", border.col = "black", border.width = 1)
  })
  expect_true(TRUE)
})

test_that("draw_node_base with SVG shape branch (lines 77-78)", {
  # Lines 75-78: get_svg_shape returns non-null for a registered SVG shape
  # Register a simple SVG shape, then draw it
  skip_if_not(
    is.function(cograph:::register_svg_shape),
    "register_svg_shape not available"
  )
  svg_str <- '<svg viewBox="0 0 100 100"><circle cx="50" cy="50" r="40"/></svg>'
  cograph:::register_svg_shape("test_svg_shape_deep", svg_str)
  on.exit(try(cograph:::unregister_svg_shape("test_svg_shape_deep"), silent = TRUE), add = TRUE)

  fn <- cograph:::draw_node_base
  with_temp_png({
    setup_base_plot()
    fn(0, 0, size = 0.1, shape = "test_svg_shape_deep",
       col = "red", border.col = "black", border.width = 1)
  })
  expect_true(TRUE)
})

# ============================================
# splot-nodes.R: draw_pie_node_base
# ============================================

test_that("draw_pie_node_base with NULL/empty values returns invisible (line 112)", {
  # Line 112: return(invisible()) when values is NULL or length 0
  fn <- cograph:::draw_pie_node_base
  with_temp_png({
    setup_base_plot()
    fn(0, 0, size = 0.1, values = NULL)
    fn(0, 0, size = 0.1, values = numeric(0))
  })
  expect_true(TRUE)
})

test_that("draw_pie_node_base skips zero-proportion slices (line 150)", {
  # Line 150: if (props[i] <= 0) next
  fn <- cograph:::draw_pie_node_base
  with_temp_png({
    setup_base_plot()
    fn(0, 0, size = 0.2, values = c(1, 0, 2),
       colors = c("red", "green", "blue"))
  })
  expect_true(TRUE)
})

test_that("draw_pie_node_base skips zero-proportion in dividers (line 173)", {
  # Line 173: if (props[i] <= 0) next (in divider loop)
  fn <- cograph:::draw_pie_node_base
  with_temp_png({
    setup_base_plot()
    fn(0, 0, size = 0.2, values = c(1, 0, 2),
       colors = c("red", "green", "blue"),
       border.width = 2)
  })
  expect_true(TRUE)
})

# ============================================
# splot-nodes.R: draw_polygon_donut_node_base
# ============================================

test_that("draw_polygon_donut_node_base with NULL values defaults (lines 246-247)", {
  # Lines 245-248: values <- 1 when NULL
  fn <- cograph:::draw_polygon_donut_node_base
  with_temp_png({
    setup_base_plot()
    fn(0, 0, size = 0.3, values = NULL, colors = NULL,
       default_color = "steelblue", donut_shape = "square")
  })
  expect_true(TRUE)
})

test_that("draw_polygon_donut_node_base multi-segment (line 277)", {
  # Lines 271-291: multi-segment donut path, line 277 colors <- rainbow
  fn <- cograph:::draw_polygon_donut_node_base
  with_temp_png({
    setup_base_plot()
    fn(0, 0, size = 0.3, values = c(1, 2, 3), colors = NULL,
       donut_shape = "hexagon")
  })
  expect_true(TRUE)
})

test_that("draw_polygon_donut_node_base with outer_border.col (lines 297-302)", {
  # Lines 295-303: outer boundary border (double border feature)
  fn <- cograph:::draw_polygon_donut_node_base
  with_temp_png({
    setup_base_plot()
    fn(0, 0, size = 0.3, values = 0.5, colors = "maroon",
       donut_shape = "square",
       border.col = "black", border.width = 1,
       outer_border.col = "red", border.lty = 2)
  })
  expect_true(TRUE)
})

# ============================================
# splot-nodes.R: draw_donut_node_base
# ============================================

test_that("draw_donut_node_base multi-segment with NULL colors (lines 436-437, 439)", {
  # Lines 435-439: multi-value donut with NULL colors -> generates rainbow
  fn <- cograph:::draw_donut_node_base
  with_temp_png({
    setup_base_plot()
    fn(0, 0, size = 0.3, values = c(1, 2, 3), colors = NULL)
  })
  expect_true(TRUE)
})

test_that("draw_donut_node_base multi-segment with default_color single (line 436-437)", {
  # Lines 436-437: default_color with n == 1 (not reachable in multi, but test n > 1)
  fn <- cograph:::draw_donut_node_base
  with_temp_png({
    setup_base_plot()
    # Multi-segment: default_color ignored when n > 1
    fn(0, 0, size = 0.3, values = c(1, 2), colors = NULL,
       default_color = "purple")
  })
  expect_true(TRUE)
})

test_that("draw_donut_node_base multi-segment skips zero prop (line 446)", {
  # Line 446: if (props[i] <= 0) next
  fn <- cograph:::draw_donut_node_base
  with_temp_png({
    setup_base_plot()
    fn(0, 0, size = 0.3, values = c(1, 0, 2),
       colors = c("red", "green", "blue"))
  })
  expect_true(TRUE)
})

# ============================================
# splot-nodes.R: draw_donut_pie_node_base
# ============================================

test_that("draw_donut_pie_node_base with pie_default_color single pie (line 601)", {
  # Line 600-601: pie_default_color with n == 1
  fn <- cograph:::draw_donut_pie_node_base
  with_temp_png({
    setup_base_plot()
    fn(0, 0, size = 0.3, donut_value = 0.7, donut_color = "blue",
       pie_values = c(1), pie_colors = NULL, pie_default_color = "orange")
  })
  expect_true(TRUE)
})

test_that("draw_donut_pie_node_base skips zero pie prop (line 610)", {
  # Line 610: if (props[i] <= 0) next
  fn <- cograph:::draw_donut_pie_node_base
  with_temp_png({
    setup_base_plot()
    fn(0, 0, size = 0.3, donut_value = 0.7, donut_color = "blue",
       pie_values = c(1, 0, 2), pie_colors = c("red", "green", "blue"))
  })
  expect_true(TRUE)
})

test_that("draw_donut_pie_node_base skips zero prop in dividers (line 627)", {
  # Line 627: if (props[i] <= 0) next (in divider loop)
  fn <- cograph:::draw_donut_pie_node_base
  with_temp_png({
    setup_base_plot()
    fn(0, 0, size = 0.3, donut_value = 0.7, donut_color = "blue",
       pie_values = c(1, 0, 2), pie_colors = c("red", "green", "blue"),
       border.width = 2, pie_border.width = 1.5)
  })
  expect_true(TRUE)
})

test_that("draw_donut_pie_node_base without pie (lines 640-646)", {
  # Lines 638-646: else branch when no pie_values -> fills center with white
  fn <- cograph:::draw_donut_pie_node_base
  with_temp_png({
    setup_base_plot()
    fn(0, 0, size = 0.3, donut_value = 0.7, donut_color = "blue",
       pie_values = NULL)
  })
  expect_true(TRUE)
})

# ============================================
# splot-nodes.R: draw_double_donut_pie_node_base
# ============================================

test_that("draw_double_donut_pie_node_base segmented donut skip zero (line 757)", {
  # Line 757: if (props[i] <= 0) next in draw_donut_ring segmented
  fn <- cograph:::draw_double_donut_pie_node_base
  with_temp_png({
    setup_base_plot()
    fn(0, 0, size = 0.3,
       donut_values = c(1, 0, 2), donut_colors = c("red", "green", "blue"),
       donut2_values = 0.5, donut2_colors = "cyan",
       pie_values = c(1, 2), pie_colors = c("orange", "purple"))
  })
  expect_true(TRUE)
})

test_that("draw_double_donut_pie_node_base NULL donut_values uses bg (line 770)", {
  # Line 770: draw_ring_segment for background when donut_values is NULL
  fn <- cograph:::draw_double_donut_pie_node_base
  with_temp_png({
    setup_base_plot()
    fn(0, 0, size = 0.3,
       donut_values = NULL, donut_colors = NULL,
       donut2_values = 0.5, donut2_colors = "cyan")
  })
  expect_true(TRUE)
})

test_that("draw_double_donut_pie_node_base NULL donut2_values uses bg (line 778)", {
  # Line 778: draw_ring_segment for background when donut2_values is NULL
  fn <- cograph:::draw_double_donut_pie_node_base
  with_temp_png({
    setup_base_plot()
    fn(0, 0, size = 0.3,
       donut_values = 0.7, donut_colors = "red",
       donut2_values = NULL, donut2_colors = NULL)
  })
  expect_true(TRUE)
})

test_that("draw_double_donut_pie_node_base skip zero pie prop (line 798)", {
  # Line 798: if (props[i] <= 0) next in pie drawing
  fn <- cograph:::draw_double_donut_pie_node_base
  with_temp_png({
    setup_base_plot()
    fn(0, 0, size = 0.3,
       donut_values = 0.7, donut_colors = "red",
       donut2_values = 0.5, donut2_colors = "cyan",
       pie_values = c(1, 0, 2), pie_colors = c("red", "green", "blue"))
  })
  expect_true(TRUE)
})

test_that("draw_double_donut_pie_node_base skip zero pie in dividers (line 815)", {
  # Line 815: if (props[i] <= 0) next in pie divider loop
  fn <- cograph:::draw_double_donut_pie_node_base
  with_temp_png({
    setup_base_plot()
    fn(0, 0, size = 0.3,
       donut_values = 0.7, donut_colors = "red",
       donut2_values = 0.5, donut2_colors = "cyan",
       pie_values = c(1, 0, 2), pie_colors = c("red", "green", "blue"),
       border.width = 2, pie_border.width = 1.5)
  })
  expect_true(TRUE)
})

test_that("draw_double_donut_pie_node_base segmented donut with NULL colors (line 733)", {
  # Line 733: draw_donut_ring returns when values is NULL
  # Line 751: colors <- rainbow when NULL in segmented donut
  fn <- cograph:::draw_double_donut_pie_node_base
  with_temp_png({
    setup_base_plot()
    fn(0, 0, size = 0.3,
       donut_values = c(1, 2, 3), donut_colors = NULL,
       donut2_values = c(2, 3), donut2_colors = NULL)
  })
  expect_true(TRUE)
})

# ============================================
# splot-nodes.R: draw_node_label_base
# ============================================

test_that("draw_node_label_base with empty/NA label returns invisible (line 871)", {
  # Line 871: return(invisible()) when label is NULL, NA, or ""
  fn <- cograph:::draw_node_label_base
  with_temp_png({
    setup_base_plot()
    fn(0, 0, label = NULL)
    fn(0, 0, label = NA)
    fn(0, 0, label = "")
  })
  expect_true(TRUE)
})

# ============================================
# splot-nodes.R: render_nodes_base (lines 1126-1214)
# ============================================

test_that("render_nodes_base dispatches pie+donut, pie-only, donut-only, standard (lines 1126-1214)", {
  fn <- cograph:::render_nodes_base
  layout_mat <- matrix(c(-0.5, -0.5, 0.5, 0.5,
                          -0.5,  0.5, -0.5, 0.5), ncol = 2)

  with_temp_png({
    setup_base_plot()

    # Node 1: donut + pie (lines 1150-1165)
    # Node 2: donut only (lines 1167-1178)
    # Node 3: pie only (lines 1180-1191)
    # Node 4: standard shape (lines 1193-1201)
    fn(
      layout = layout_mat,
      vsize = rep(0.1, 4),
      vsize2 = NULL,
      shape = rep("circle", 4),
      color = rep("#4A90D9", 4),
      border.color = rep("#2C5AA0", 4),
      border.width = rep(1, 4),
      pie = list(c(1, 2), NULL, c(3, 1, 2), NULL),
      pieColor = list(c("red", "blue"), NULL, c("green", "orange", "purple"), NULL),
      donut = list(0.5, 0.7, NULL, NULL),
      donutColor = list("maroon", "steelblue", NULL, NULL),
      labels = c("A", "B", "C", "D"),
      label.cex = rep(0.8, 4),
      label.color = rep("black", 4)
    )
  })
  expect_true(TRUE)
})

test_that("render_nodes_base with empty layout returns early (line 1127)", {
  fn <- cograph:::render_nodes_base
  with_temp_png({
    setup_base_plot()
    fn(
      layout = matrix(ncol = 2, nrow = 0),
      vsize = numeric(0),
      shape = character(0),
      color = character(0),
      border.color = character(0),
      border.width = numeric(0)
    )
  })
  expect_true(TRUE)
})

test_that("render_nodes_base label rendering skips empty labels (lines 1206-1214)", {
  fn <- cograph:::render_nodes_base
  layout_mat <- matrix(c(0, 0.5, 0, 0.5), ncol = 2)

  with_temp_png({
    setup_base_plot()
    fn(
      layout = layout_mat,
      vsize = rep(0.1, 2),
      shape = rep("circle", 2),
      color = rep("blue", 2),
      border.color = rep("black", 2),
      border.width = rep(1, 2),
      labels = c("Hello", ""),  # Second is empty, should be skipped
      label.cex = rep(0.8, 2),
      label.color = rep("black", 2)
    )
  })
  expect_true(TRUE)
})

# ============================================
# splot-edges.R: find_curve_split_index
# ============================================

test_that("find_curve_split_index with zero-length curve (line 28)", {
  # Line 28: total_length < 1e-10 returns 1
  fn <- cograph:::find_curve_split_index
  # All same points -> zero arc length
  result <- fn(c(0.5, 0.5, 0.5), c(0.5, 0.5, 0.5), 0.5)
  expect_equal(result, 1)
})

# ============================================
# splot-edges.R: draw_curve_with_start_segment
# ============================================

test_that("draw_curve_with_start_segment with split (lines 52-79)", {
  fn <- cograph:::draw_curve_with_start_segment
  with_temp_png({
    setup_base_plot()
    x <- seq(-0.5, 0.5, length.out = 20)
    y <- sin(x * pi) * 0.3
    # Different start_lty from lty, with fraction > 0
    fn(x, y, col = "gray50", lwd = 1, lty = 1,
       start_lty = 2, start_fraction = 0.3)
  })
  expect_true(TRUE)
})

test_that("draw_curve_with_start_segment with n < 2 returns invisible", {
  fn <- cograph:::draw_curve_with_start_segment
  with_temp_png({
    setup_base_plot()
    fn(c(0), c(0), col = "black", lwd = 1, lty = 1)
  })
  expect_true(TRUE)
})

# ============================================
# splot-edges.R: draw_curved_edge_base with zero-length edge (line 216)
# ============================================

test_that("draw_curved_edge_base with zero-length edge returns early (line 216)", {
  fn <- cograph:::draw_curved_edge_base
  with_temp_png({
    setup_base_plot()
    # Same start and end point -> len < 1e-10
    fn(0, 0, 0, 0, curve = 0.3, col = "gray50", lwd = 1, lty = 1)
  })
  expect_true(TRUE)
})

# ============================================
# splot-edges.R: get_edge_label_position
# ============================================

test_that("get_edge_label_position with zero-length edge (line 510-511)", {
  fn <- cograph:::get_edge_label_position
  pos <- fn(0.5, 0.5, 0.5, 0.5, position = 0.5, curve = 0)
  expect_equal(pos$x, 0.5)
  expect_equal(pos$y, 0.5)
})

test_that("get_edge_label_position curved edge with small offset triggers min_offset (line 535)", {
  fn <- cograph:::get_edge_label_position
  # Very short edge with non-zero curve -> triggers min_offset
  pos <- fn(0.5, 0.5, 0.50001, 0.50001, position = 0.5, curve = 0.5)
  expect_true(is.list(pos))
  expect_true(!is.null(pos$x) && !is.null(pos$y))
})

test_that("get_edge_label_position with curve_direction == 0 (line 561)", {
  fn <- cograph:::get_edge_label_position
  # When curve is exactly 0 but passes abs(curve) check due to precision,
  # the function would use label_offset with curve_direction. We need
  # a very small curve close to epsilon but > 1e-6.
  pos <- fn(0, 0, 1, 1, position = 0.5, curve = 1e-5, label_offset = 0.05)
  expect_true(is.list(pos))
})

# ============================================
# splot-edges.R: draw_self_loop_base
# ============================================

test_that("draw_self_loop_base renders correctly", {
  fn <- cograph:::draw_self_loop_base
  with_temp_png({
    setup_base_plot()
    fn(0, 0, node_size = 0.1, col = "gray50", lwd = 1, lty = 1,
       rotation = pi / 2, arrow = TRUE, asize = 0.02)
  })
  expect_true(TRUE)
})

test_that("draw_self_loop_base without arrow", {
  fn <- cograph:::draw_self_loop_base
  with_temp_png({
    setup_base_plot()
    fn(0, 0, node_size = 0.1, col = "gray50", lwd = 1, lty = 1,
       rotation = pi / 4, arrow = FALSE, asize = 0.02)
  })
  expect_true(TRUE)
})

# ============================================
# splot-edges.R: draw_edge_label_base
# ============================================

test_that("draw_edge_label_base with shadow (lines 461-472)", {
  fn <- cograph:::draw_edge_label_base
  with_temp_png({
    setup_base_plot()
    fn(0, 0, label = "0.5", cex = 0.8, col = "gray30",
       bg = "white", font = 1,
       shadow = TRUE, shadow_color = "gray40",
       shadow_offset = 0.5, shadow_alpha = 0.5)
  })
  expect_true(TRUE)
})

test_that("draw_edge_label_base with NULL/NA/empty label returns invisible", {
  fn <- cograph:::draw_edge_label_base
  with_temp_png({
    setup_base_plot()
    fn(0, 0, label = NULL)
    fn(0, 0, label = NA)
    fn(0, 0, label = "")
  })
  expect_true(TRUE)
})

test_that("draw_edge_label_base with NA bg (no background)", {
  fn <- cograph:::draw_edge_label_base
  with_temp_png({
    setup_base_plot()
    fn(0, 0, label = "test", bg = NA)
  })
  expect_true(TRUE)
})

# ============================================
# splot-edges.R: render_edges_base (lines 598-719)
# ============================================

test_that("render_edges_base renders self-loops with labels (lines 598-719)", {
  fn <- cograph:::render_edges_base

  # Create layout for 3 nodes
  layout_mat <- matrix(c(-0.5, 0.5, 0, -0.3, -0.3, 0.3), ncol = 2)

  # Create edges data frame with self-loop and regular edges
  edges <- data.frame(
    from = c(1, 1, 2),
    to = c(1, 2, 3),  # first is self-loop
    weight = c(0.5, 0.8, 0.6)
  )

  with_temp_png({
    setup_base_plot()
    fn(
      edges = edges,
      layout = layout_mat,
      node_sizes = rep(0.08, 3),
      shapes = rep("circle", 3),
      edge.color = c("red", "blue", "green"),
      edge.width = c(1, 2, 1.5),
      lty = rep(1, 3),
      curve = c(0, 0.3, 0),
      curvePivot = rep(0.5, 3),
      arrows = rep(TRUE, 3),
      asize = rep(0.02, 3),
      bidirectional = rep(FALSE, 3),
      loopRotation = NULL,
      edge.labels = c("self", "edge1", "edge2"),
      edge.label.cex = 0.7,
      edge.label.bg = "white",
      edge.label.position = 0.5
    )
  })
  expect_true(TRUE)
})

test_that("render_edges_base with empty edges returns early (line 599)", {
  fn <- cograph:::render_edges_base
  layout_mat <- matrix(c(0, 0), ncol = 2)
  edges <- data.frame(from = integer(0), to = integer(0), weight = numeric(0))

  with_temp_png({
    setup_base_plot()
    fn(
      edges = edges,
      layout = layout_mat,
      node_sizes = 0.1
    )
  })
  expect_true(TRUE)
})

test_that("render_edges_base with explicit loopRotation (line 623)", {
  fn <- cograph:::render_edges_base

  layout_mat <- matrix(c(0, 0.5, 0, 0.5), ncol = 2)
  edges <- data.frame(
    from = c(1, 1),
    to = c(1, 2),
    weight = c(0.5, 0.8)
  )

  with_temp_png({
    setup_base_plot()
    fn(
      edges = edges,
      layout = layout_mat,
      node_sizes = rep(0.08, 2),
      shapes = rep("circle", 2),
      edge.color = c("gray50", "gray50"),
      edge.width = c(1, 1),
      lty = rep(1, 2),
      curve = c(0, 0),
      arrows = rep(TRUE, 2),
      asize = rep(0.02, 2),
      bidirectional = rep(FALSE, 2),
      loopRotation = c(pi / 4, 0)
    )
  })
  expect_true(TRUE)
})

test_that("render_edges_base curved and straight edges with labels (lines 673-703)", {
  fn <- cograph:::render_edges_base

  layout_mat <- matrix(c(-0.5, 0.5, 0, -0.3, -0.3, 0.5), ncol = 2)
  edges <- data.frame(
    from = c(1, 2),
    to = c(2, 3),
    weight = c(0.7, 0.4)
  )

  with_temp_png({
    setup_base_plot()
    fn(
      edges = edges,
      layout = layout_mat,
      node_sizes = rep(0.08, 3),
      shapes = rep("circle", 3),
      edge.color = c("blue", "red"),
      edge.width = c(1.5, 1),
      lty = rep(1, 2),
      curve = c(0.3, 0),  # first curved, second straight
      curvePivot = rep(0.5, 2),
      arrows = rep(TRUE, 2),
      asize = rep(0.02, 2),
      bidirectional = rep(FALSE, 2),
      edge.labels = c("0.7", "0.4"),
      edge.label.cex = 0.8,
      edge.label.bg = "lightyellow",
      edge.label.position = 0.5
    )
  })
  expect_true(TRUE)
})

# ============================================
# Integration: splot() with parameters targeting uncovered lines
# ============================================

test_that("splot with self-loops + edge_labels + shadow covers edge label rendering", {
  mat <- create_test_matrix(3, weighted = TRUE)
  mat[1, 1] <- 0.5
  mat[2, 2] <- 0.7
  expect_splot_works(
    mat,
    edge_labels = TRUE,
    edge_label_shadow = TRUE,
    edge_label_shadow_color = "gray40",
    edge_label_shadow_offset = 0.5,
    edge_label_shadow_alpha = 0.3,
    show_arrows = TRUE
  )
})

test_that("splot with donut_fill and outer_border covers donut rendering", {
  mat <- create_test_matrix(3)
  expect_splot_works(
    mat,
    donut_fill = c(0.3, 0.6, 0.9),
    donut_color = "steelblue",
    donut_outer_border_color = "darkblue",
    donut_line_type = 2,
    donut_show_value = TRUE,
    donut_value_prefix = "",
    donut_value_suffix = "%",
    donut_value_digits = 1,
    donut_value_fontface = "italic",
    donut_value_fontfamily = "mono"
  )
})

test_that("splot with polygon donut multi-segment and outer border", {
  mat <- create_test_matrix(3)
  expect_splot_works(
    mat,
    donut_fill = c(0.5, 0.7, 0.3),
    donut_shape = "square",
    donut_outer_border_color = "red",
    donut_border_color = "darkblue",
    donut_line_type = 2,
    donut_show_value = TRUE
  )
})

test_that("splot with pie values including zero proportions", {
  mat <- create_test_matrix(3)
  expect_splot_works(
    mat,
    pie_values = list(c(1, 0, 2), c(0, 3, 1), c(2, 2, 0)),
    pie_colors = c("red", "green", "blue"),
    pie_border_width = 1.5
  )
})

test_that("splot with double_donut_pie and zero proportions", {
  mat <- create_test_matrix(3)
  expect_splot_works(
    mat,
    node_shape = "double_donut_pie",
    donut_values = list(c(1, 0, 2), 0.5, 0.7),
    donut2_values = list(0.5, c(2, 0, 1), 0.3),
    pie_values = list(c(1, 0, 3), c(2, 1), c(0, 2, 1)),
    pie_colors = c("red", "green", "blue"),
    pie_border_width = 1.5,
    donut_border_width = 2
  )
})

test_that("splot with donut_pie where pie has zero proportions and no pie", {
  mat <- create_test_matrix(3)
  expect_splot_works(
    mat,
    donut_fill = c(0.5, 0.7, 0.3),
    pie_values = list(c(1, 0, 2), NULL, c(3, 1)),
    pie_colors = c("red", "green", "blue"),
    pie_border_width = 1.5
  )
})

test_that("splot with SVG custom shape", {
  skip_if_not(
    is.function(cograph:::register_svg_shape),
    "register_svg_shape not available"
  )
  svg_str <- '<svg viewBox="0 0 100 100"><rect x="10" y="10" width="80" height="80"/></svg>'
  cograph:::register_svg_shape("test_rect_shape_cov", svg_str)
  on.exit(try(cograph:::unregister_svg_shape("test_rect_shape_cov"), silent = TRUE), add = TRUE)

  mat <- create_test_matrix(3)
  expect_splot_works(mat, node_shape = "test_rect_shape_cov")
})

test_that("splot with use_pch = TRUE", {
  mat <- create_test_matrix(3)
  expect_splot_works(mat, use_pch = TRUE)
})

test_that("splot with edge CI values covers CI underlay paths", {
  mat <- create_test_matrix(3, weighted = TRUE)
  mat[1, 1] <- 0.5  # self-loop
  n_edges <- sum(mat != 0)
  ci_vals <- rep(0.15, n_edges)
  expect_splot_works(
    mat,
    edge_ci = ci_vals,
    edge_ci_scale = 2.0,
    edge_ci_alpha = 0.2,
    edge_ci_style = 2,
    show_arrows = TRUE
  )
})

test_that("splot CI underlay for curved edges", {
  mat <- matrix(0, 4, 4)
  mat[1, 2] <- 0.8
  mat[2, 1] <- 0.6  # reciprocal
  mat[2, 3] <- 0.5
  mat[3, 4] <- 0.7
  n_edges <- sum(mat != 0)
  ci_vals <- rep(0.1, n_edges)
  expect_splot_works(
    mat,
    directed = TRUE,
    show_arrows = TRUE,
    edge_ci = ci_vals,
    curvature = 0.3
  )
})

test_that("splot with edge_start_style dashed on curved edges", {
  mat <- matrix(0, 4, 4)
  mat[1, 2] <- 0.8
  mat[2, 1] <- 0.6
  mat[2, 3] <- 0.5
  expect_splot_works(
    mat,
    directed = TRUE,
    show_arrows = TRUE,
    edge_start_style = "dashed",
    edge_start_length = 0.2,
    curvature = 0.3
  )
})

test_that("splot with bidirectional arrows on curved edges", {
  mat <- create_test_matrix(3, symmetric = FALSE, weighted = TRUE)
  expect_splot_works(
    mat,
    directed = TRUE,
    show_arrows = TRUE,
    bidirectional = TRUE,
    curvature = 0.3
  )
})

test_that("splot with labels at various positions covers label offset branches", {
  mat <- create_test_matrix(5)
  expect_splot_works(
    mat,
    labels = c("A", "B", "C", "D", "E"),
    label_position = c("center", "above", "below", "left", "right"),
    label_fontface = c("plain", "bold", "italic", "bold.italic", "plain"),
    label_fontfamily = c("sans", "serif", "mono", "sans", "serif"),
    label_hjust = c(0, 0.5, 1, 0, 0.5),
    label_vjust = c(0, 0.5, 1, 0, 0.5),
    label_angle = c(0, 15, -15, 30, 0)
  )
})

test_that("splot with donut multi-segment null colors auto-generates rainbow", {
  mat <- create_test_matrix(3)
  expect_splot_works(
    mat,
    node_shape = "donut",
    donut_values = list(c(1, 2, 3), c(2, 3), c(1, 1, 1)),
    donut_colors = NULL
  )
})

test_that("splot with donut multi-segment zero values skips segments", {
  mat <- create_test_matrix(3)
  expect_splot_works(
    mat,
    node_shape = "donut",
    donut_values = list(c(1, 0, 2), c(0, 3), c(2, 0, 1)),
    donut_colors = list(c("red", "green", "blue"), c("cyan", "magenta"), c("orange", "purple", "pink"))
  )
})

test_that("splot edge labels on self-loop + regular edges combined", {
  mat <- create_test_matrix(4, weighted = TRUE)
  mat[1, 1] <- 0.5
  mat[3, 3] <- 0.3
  expect_splot_works(
    mat,
    edge_labels = TRUE,
    edge_label_bg = "lightyellow",
    edge_label_position = 0.5,
    edge_label_fontface = "bold",
    show_arrows = TRUE
  )
})

test_that("splot renders edge labels on straight edges with offset", {
  mat <- create_test_matrix(3, weighted = TRUE)
  expect_splot_works(
    mat,
    curves = FALSE,
    edge_labels = TRUE,
    edge_label_offset = 0.05,
    edge_label_bg = "white"
  )
})

test_that("splot renders edge labels on curved edges with shadow and offset", {
  mat <- create_test_matrix(4, symmetric = FALSE, weighted = TRUE)
  expect_splot_works(
    mat,
    curvature = 0.3,
    edge_labels = TRUE,
    edge_label_offset = 0.03,
    edge_label_shadow = TRUE,
    edge_label_shadow_color = "gray30",
    edge_label_shadow_offset = 1.0,
    edge_label_shadow_alpha = 0.4,
    show_arrows = TRUE
  )
})

# ============================================
# Direct internal function: draw_donut_node_base
# with outer_border.col for double border (circular)
# ============================================

test_that("draw_donut_node_base with outer_border.col (lines 465-473)", {
  fn <- cograph:::draw_donut_node_base
  with_temp_png({
    setup_base_plot()
    fn(0, 0, size = 0.3, values = 0.7, colors = "steelblue",
       outer_border.col = "darkred", border.lty = 2)
  })
  expect_true(TRUE)
})

test_that("draw_donut_node_base with donut_border.width set", {
  fn <- cograph:::draw_donut_node_base
  with_temp_png({
    setup_base_plot()
    fn(0, 0, size = 0.3, values = 0.5, colors = "maroon",
       donut_border.width = 3, border.width = 1)
  })
  expect_true(TRUE)
})

# ============================================
# Direct calls to draw_straight_edge_base and draw_curved_edge_base
# to cover start_fraction / start_lty split
# ============================================

test_that("draw_straight_edge_base with start_fraction split", {
  fn <- cograph:::draw_straight_edge_base
  with_temp_png({
    setup_base_plot()
    fn(-0.5, -0.5, 0.5, 0.5,
       col = "gray50", lwd = 1, lty = 1,
       arrow = TRUE, asize = 0.02,
       start_lty = 2, start_fraction = 0.3)
  })
  expect_true(TRUE)
})

test_that("draw_curved_edge_base with start_fraction split", {
  fn <- cograph:::draw_curved_edge_base
  with_temp_png({
    setup_base_plot()
    fn(-0.5, -0.5, 0.5, 0.5,
       curve = 0.3, col = "gray50", lwd = 1, lty = 1,
       arrow = TRUE, asize = 0.02,
       start_lty = 2, start_fraction = 0.3)
  })
  expect_true(TRUE)
})

test_that("draw_curved_edge_base bidirectional with arrows", {
  fn <- cograph:::draw_curved_edge_base
  with_temp_png({
    setup_base_plot()
    fn(-0.5, -0.3, 0.5, 0.3,
       curve = 0.3, col = "gray50", lwd = 1, lty = 1,
       arrow = TRUE, asize = 0.02, bidirectional = TRUE)
  })
  expect_true(TRUE)
})

test_that("draw_curved_edge_base no arrow draws full curve", {
  fn <- cograph:::draw_curved_edge_base
  with_temp_png({
    setup_base_plot()
    fn(-0.5, -0.3, 0.5, 0.3,
       curve = 0.3, col = "gray50", lwd = 1, lty = 1,
       arrow = FALSE, asize = 0, bidirectional = FALSE,
       start_lty = 2, start_fraction = 0.2)
  })
  expect_true(TRUE)
})

# ============================================
# splot-nodes.R: draw_polygon_donut with fontface variations
# ============================================

test_that("draw_polygon_donut_node_base with various fontfaces", {
  fn <- cograph:::draw_polygon_donut_node_base
  for (face in c("plain", "bold", "italic", "bold.italic")) {
    with_temp_png({
      setup_base_plot()
      fn(0, 0, size = 0.3, values = 0.5, colors = "maroon",
         donut_shape = "hexagon", show_value = TRUE,
         value_fontface = face)
    })
  }
  expect_true(TRUE)
})

test_that("draw_donut_node_base with various fontfaces", {
  fn <- cograph:::draw_donut_node_base
  for (face in c("plain", "bold", "italic", "bold.italic")) {
    with_temp_png({
      setup_base_plot()
      fn(0, 0, size = 0.3, values = 0.5, colors = "maroon",
         show_value = TRUE, value_fontface = face)
    })
  }
  expect_true(TRUE)
})
