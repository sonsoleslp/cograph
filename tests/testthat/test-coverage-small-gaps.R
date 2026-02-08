# Tests to cover small gaps in code coverage across multiple source files.
# Uses project helpers: create_test_matrix(), with_temp_png(), expect_splot_works()

# =============================================================================
# 1. R/splot-params.R line 167 - resolve_label_sizes legacy coupled mode
# =============================================================================

test_that("resolve_label_sizes legacy coupled mode", {
  result <- cograph:::resolve_label_sizes(NULL, rep(0.05, 3), 3, scaling = "legacy")
  expect_equal(length(result), 3)
  expect_true(all(result > 0))
})

# =============================================================================
# 2. R/splot-polygons.R line 244 - get_donut_base_vertices rectangle shape
# =============================================================================

test_that("donut with rectangle base shape", {
  mat <- create_test_matrix(3, weighted = TRUE)
  expect_splot_works(mat, donut_fill = c(0.3, 0.6, 0.9), donut_shape = "rectangle")
})

# =============================================================================
# 3. R/class-layout.R line 116 - CographLayout print with long vector param
# =============================================================================

test_that("CographLayout print with long vector param", {
  layout <- CographLayout$new("custom", coords = c(1, 2, 3, 4, 5))
  expect_output(print(layout), "\\.\\.\\.")
})

# =============================================================================
# 4. R/methods-print.R lines 20-21, 53-54, 57, 63 - print.cograph_network
# =============================================================================

test_that("print for new format directed network", {
  mat <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), nrow = 3)
  net <- as_cograph(mat)
  expect_output(print(net), "directed")
})

test_that("print for new format with equal weights", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)
  expect_output(print(net), "all equal")
})

test_that("print for new format shows layout none", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)
  expect_output(print(net), "none")
})

# =============================================================================
# 5. R/methods-plot.R lines 20-21, 53-54, 63 - plot.cograph_network & summary
# =============================================================================

test_that("plot.cograph_network renders without error", {
  mat <- create_test_matrix(4)
  net <- cograph(mat)
  result <- tryCatch({
    with_temp_png(plot(net))
    TRUE
  }, error = function(e) FALSE)
  expect_true(result)
})

test_that("summary.cograph_network prints summary info", {
  mat <- create_test_matrix(4, weighted = TRUE)
  net <- cograph(mat)
  expect_output(summary(net), "Cograph Network Summary")
  expect_output(summary(net), "Nodes:")
  expect_output(summary(net), "Edges:")
})

test_that("summary.cograph_network with many nodes truncates labels", {
  mat <- create_test_matrix(12)
  net <- cograph(mat)
  expect_output(summary(net), "\\.\\.\\.")
})

test_that("summary.cograph_network with negative edges", {
  mat <- create_test_matrix(4, weighted = TRUE, symmetric = FALSE, seed = 99)
  net <- cograph(mat)
  out <- capture.output(summary(net))
  # Should have output about the network

  expect_true(length(out) > 0)
})

# =============================================================================
# 6. R/tplot.R line 81 - plot_tna with pieColor parameter
# =============================================================================

test_that("plot_tna with pieColor", {
  mat <- create_test_matrix(4, weighted = TRUE)
  result <- tryCatch({
    with_temp_png(plot_tna(mat,
      pie = c(0.3, 0.5, 0.7, 0.9),
      pieColor = c("red", "blue", "green", "orange")
    ))
    TRUE
  }, error = function(e) FALSE)
  expect_true(result)
})

# =============================================================================
# 7. R/layout-oval.R line 30 - oval layout with 0 nodes
# =============================================================================

test_that("oval layout with 0 nodes", {
  net <- CographNetwork$new()
  fn <- get_layout("oval")
  result <- fn(net)
  expect_equal(nrow(result), 0)
})

# =============================================================================
# 8. R/input-matrix.R line 19 - parse_matrix with non-matrix input
# =============================================================================

test_that("parse_matrix errors on non-matrix", {
  expect_error(cograph:::parse_matrix("not a matrix"), "must be a matrix")
})

# =============================================================================
# 9. R/layout-circle.R line 27 - circle layout with 0 nodes
# =============================================================================

test_that("circle layout with 0 nodes", {
  net <- CographNetwork$new()
  fn <- get_layout("circle")
  result <- fn(net)
  expect_equal(nrow(result), 0)
})

# =============================================================================
# 10. R/splot-labels.R lines 107, 165, 175
# =============================================================================

test_that("resolve_stars with non-list non-char input returns empty", {
  result <- cograph:::resolve_stars(list(a = 1), n = 3)
  expect_equal(result, rep("", 3))
})

test_that("format_edge_label_template with low and up NA", {
  result <- cograph:::format_edge_label_template(
    "{low}-{up}",
    weight = 0.5,
    ci_lower = NA,
    ci_upper = NA
  )
  expect_true(is.character(result))
})

test_that("format_edge_label_template with low and up values", {
  result <- cograph:::format_edge_label_template(
    "{low}-{up}",
    weight = 0.5,
    ci_lower = 0.2,
    ci_upper = 0.8
  )
  expect_true(grepl("0.20", result))
  expect_true(grepl("0.80", result))
})

# =============================================================================
# 11. R/input-parse.R lines 24, 26, 28 - parse_input for unsupported types
# =============================================================================

test_that("parse_input errors on unsupported type", {
  expect_error(cograph:::parse_input(42), "Unsupported input")
})

# =============================================================================
# 12. R/input-edgelist.R lines 19, 30, 34 - parse_edgelist validation
# =============================================================================

test_that("parse_edgelist errors on non-data.frame", {
  expect_error(cograph:::parse_edgelist("not a df"), "must be a data frame")
})

test_that("parse_edgelist auto-detects column names", {
  df <- data.frame(source = c(1, 2), target = c(2, 3), w = c(0.5, 0.8))
  result <- cograph:::parse_edgelist(df)
  expect_true(!is.null(result$edges))
})

# =============================================================================
# 13. R/layout-groups.R lines 37, 69, 82
# =============================================================================

test_that("groups layout with 0 nodes", {
  net <- CographNetwork$new()
  result <- cograph:::layout_groups(net, character(0))
  expect_equal(nrow(result), 0)
})

test_that("groups layout with custom group positions", {
  mat <- create_test_matrix(6)
  net <- CographNetwork$new(mat)
  groups <- c("A", "A", "A", "B", "B", "B")
  positions <- data.frame(x = c(0.3, 0.7), y = c(0.5, 0.5))
  result <- cograph:::layout_groups(net, groups, group_positions = positions)
  expect_equal(nrow(result), 6)
})

# =============================================================================
# 14. R/layout-spring.R lines 33, 47-48, 50
# =============================================================================

test_that("spring layout with 0 nodes", {
  net <- CographNetwork$new()
  result <- cograph:::layout_spring(net)
  expect_equal(nrow(result), 0)
})

test_that("spring layout with initial positions as matrix", {
  mat <- create_test_matrix(4)
  net <- CographNetwork$new(mat)
  initial <- matrix(runif(8), ncol = 2)
  result <- cograph:::layout_spring(net, initial = initial)
  expect_equal(nrow(result), 4)
})

test_that("spring layout with initial positions as data frame", {
  mat <- create_test_matrix(4)
  net <- CographNetwork$new(mat)
  initial <- data.frame(x = runif(4), y = runif(4))
  result <- cograph:::layout_spring(net, initial = initial)
  expect_equal(nrow(result), 4)
})

# =============================================================================
# 15. R/scale-constants.R lines 205, 207-208, 211, 274, 280, 304, 318
# =============================================================================

test_that("compute_adaptive_esize directed", {
  result <- cograph:::compute_adaptive_esize(10, directed = TRUE)
  expect_true(result > 0)
})

test_that("scale_edge_widths with log mode", {
  result <- cograph:::scale_edge_widths(c(0.1, 0.5, 1.0), mode = "log")
  expect_equal(length(result), 3)
})

test_that("scale_edge_widths with sqrt mode", {
  result <- cograph:::scale_edge_widths(c(0.1, 0.5, 1.0), mode = "sqrt")
  expect_equal(length(result), 3)
})

test_that("scale_edge_widths with rank mode", {
  result <- cograph:::scale_edge_widths(c(0.1, 0.5, 1.0), mode = "rank", cut = 0)
  expect_equal(length(result), 3)
})

test_that("scale_edge_widths with invalid mode errors", {
  expect_error(
    cograph:::scale_edge_widths(c(0.1, 0.5), mode = "invalid"),
    "edge_scale_mode"
  )
})

test_that("scale_edge_widths empty", {
  result <- cograph:::scale_edge_widths(numeric(0))
  expect_equal(length(result), 0)
})

# =============================================================================
# 16. R/mlna.R lines 122, 125, 286-287, 328, 494, 503
# =============================================================================

test_that("plot_mlna with unnamed matrix (line 122)", {
  m <- matrix(runif(9, 0, 0.3), 3, 3)
  diag(m) <- 0
  rownames(m) <- colnames(m) <- c("1", "2", "3")
  layers <- list(L1 = c("1", "2"), L2 = c("3"))
  result <- tryCatch({
    with_temp_png(plot_mlna(m, layers, minimum = 0, legend = FALSE))
    TRUE
  }, error = function(e) FALSE)
  expect_true(result)
})

test_that("plot_mlna errors on invalid model (line 125)", {
  # layer_list must have 2+ elements to reach the model check
  expect_error(
    plot_mlna("invalid", list(c("A"), c("B"))),
    "must be a tna object or matrix"
  )
})

test_that("plot_mlna with legend and unnamed layers (lines 494, 503)", {
  m <- matrix(runif(9, 0, 0.3), 3, 3)
  diag(m) <- 0
  rownames(m) <- colnames(m) <- c("A", "B", "C")
  layers <- list(c("A", "B"), c("C"))
  result <- tryCatch({
    with_temp_png(plot_mlna(m, layers, minimum = 0, legend = TRUE))
    TRUE
  }, error = function(e) FALSE)
  expect_true(result)
})

test_that("plot_mlna with matrix without colnames (line 122 seq_len)", {
  m <- matrix(runif(9, 0, 0.3), 3, 3)
  diag(m) <- 0
  # Deliberately no colnames
  layers <- list(L1 = c("1", "2"), L2 = c("3"))
  result <- tryCatch({
    with_temp_png(plot_mlna(m, layers, minimum = 0, legend = FALSE))
    TRUE
  }, error = function(e) FALSE)
  expect_true(result)
})

test_that("plot_mlna with single-node layer (lines 286-287)", {
  m <- matrix(runif(9, 0.1, 0.5), 3, 3)
  diag(m) <- 0
  rownames(m) <- colnames(m) <- c("A", "B", "C")
  # One layer has a single node
  layers <- list(L1 = c("A", "B"), L2 = c("C"))
  result <- tryCatch({
    with_temp_png(plot_mlna(m, layers, minimum = 0, legend = TRUE))
    TRUE
  }, error = function(e) FALSE)
  expect_true(result)
})

# =============================================================================
# 17. R/sonplot-qgraph-geometry.R lines 226, 232, 241-243
# =============================================================================

test_that("qgraph_cent_to_edge_simple square shape down direction (line 226)", {
  # sin(a) < 0 with cos(a) ~= 0: angle = 3*pi/2 (pointing down)
  result <- cograph:::qgraph_cent_to_edge_simple(0, 0, 3 * pi / 2, 1, shape = "square")
  expect_equal(result$x, 0, tolerance = 1e-8)
  expect_equal(result$y, -1, tolerance = 1e-8)
})

test_that("qgraph_cent_to_edge_simple square shape left direction (line 232)", {
  # cos(a) < 0 with sin(a) ~= 0: angle = pi (pointing left)
  result <- cograph:::qgraph_cent_to_edge_simple(0, 0, pi, 1, shape = "square")
  expect_equal(result$x, -1, tolerance = 1e-8)
  expect_equal(result$y, 0, tolerance = 1e-8)
})

test_that("qgraph_cent_to_edge_simple square diagonal hits top/bottom (lines 241-243)", {
  # Steep angle that exceeds the horizontal edge -> hits vertical edge
  # angle ~= 70 degrees (more vertical than horizontal in square)
  angle <- 70 * pi / 180
  result <- cograph:::qgraph_cent_to_edge_simple(0, 0, angle, 1, shape = "square")
  expect_true(!is.null(result$x))
  expect_true(!is.null(result$y))
  # The y coordinate should be capped at hw = 1
  expect_equal(abs(result$y), 1, tolerance = 1e-8)
})

# =============================================================================
# 18. R/class-network.R - S3 fallback paths (attr-based format)
# =============================================================================

test_that("CographNetwork clone with plot_params (line 86)", {
  mat <- create_test_matrix(3)
  net <- CographNetwork$new(mat)
  net$set_plot_params(list(node_size = 5))
  cloned <- net$clone_network()
  expect_equal(cloned$get_plot_params()$node_size, 5)
})

test_that("CographNetwork set_layout_coords with matrix (lines 123-128)", {
  net <- CographNetwork$new(create_test_matrix(3))
  coords_mat <- matrix(c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6), ncol = 2)
  net$set_layout_coords(coords_mat)
  layout <- net$get_layout()
  expect_true(is.data.frame(layout))
  expect_equal(nrow(layout), 3)
})

test_that("n_nodes active binding returns 0 for empty network (line 236)", {
  net <- CographNetwork$new()
  expect_equal(net$n_nodes, 0L)
})

test_that("n_edges active binding returns 0 for empty network (line 241)", {
  net <- CographNetwork$new()
  expect_equal(net$n_edges, 0L)
})

test_that("node_labels returns NULL for empty network (line 256)", {
  net <- CographNetwork$new()
  expect_null(net$node_labels)
})

test_that("get_nodes with attr-based format (lines 329-331)", {
  obj <- structure(list(), class = c("cograph_network", "list"))
  attr(obj, "nodes") <- data.frame(id = 1:3, label = c("A", "B", "C"))
  nodes <- get_nodes(obj)
  expect_equal(nrow(nodes), 3)
})

test_that("get_nodes with R6 wrapper format (lines 334-335)", {
  mat <- create_test_matrix(3)
  net_r6 <- CographNetwork$new(mat)
  obj <- structure(
    list(network = net_r6),
    class = c("cograph_network", "list")
  )
  nodes <- get_nodes(obj)
  expect_true(is.data.frame(nodes))
})

test_that("get_edges with R6 wrapper format (lines 378-379)", {
  mat <- create_test_matrix(3)
  net_r6 <- CographNetwork$new(mat)
  obj <- structure(
    list(network = net_r6),
    class = c("cograph_network", "list")
  )
  edges <- get_edges(obj)
  expect_true(is.data.frame(edges))
})

test_that("get_labels with attr-based format (line 409)", {
  obj <- structure(list(), class = c("cograph_network", "list"))
  attr(obj, "labels") <- c("X", "Y", "Z")
  labels <- get_labels(obj)
  expect_equal(labels, c("X", "Y", "Z"))
})

test_that("as_cograph source_type detection for various types (lines 671-679)", {
  # matrix type
  mat <- matrix(c(0, 1, 1, 0), 2, 2)
  net <- as_cograph(mat)
  expect_s3_class(net, "cograph_network")

  # data frame type (edgelist)
  df <- data.frame(from = c(1, 2), to = c(2, 3))
  net2 <- as_cograph(df)
  expect_s3_class(net2, "cograph_network")
})

test_that("is_directed with attr-based format (lines 778-779)", {
  obj <- structure(list(), class = c("cograph_network", "list"))
  attr(obj, "directed") <- TRUE
  expect_true(is_directed(obj))
})

test_that("n_nodes with attr-based format (lines 812-813)", {
  obj <- structure(list(), class = c("cograph_network", "list"))
  attr(obj, "n_nodes") <- 5L
  expect_equal(n_nodes(obj), 5L)
})

test_that("n_edges with attr-based format (lines 846-847)", {
  obj <- structure(list(), class = c("cograph_network", "list"))
  attr(obj, "n_edges") <- 10L
  expect_equal(n_edges(obj), 10L)
})

# =============================================================================
# 19. R/cograph.R lines 73-77, 82-86, 311-314 - igraph layout paths
# =============================================================================

test_that("cograph with igraph layout codes", {
  skip_if_not_installed("igraph")
  mat <- create_test_matrix(5)
  net <- cograph(mat, layout = "kk")
  expect_s3_class(net, "cograph_network")
})
