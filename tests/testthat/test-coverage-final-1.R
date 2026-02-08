# test-coverage-final-1.R
# Coverage gap tests for layout-registry.R, render-edges.R, render-grid.R,
# cograph.R, and input-igraph.R

# ============================================
# 1. layout-registry.R - Grid/Star/Bipartite edge cases + Gephi FR
# ============================================

test_that("grid layout returns empty df for 0-node network", {
  # Lines 31-32: grid layout with n==0

  grid_fn <- get_layout("grid")
  expect_false(is.null(grid_fn))

  mock_net_0 <- list(n_nodes = 0L)
  coords <- grid_fn(mock_net_0)
  expect_equal(nrow(coords), 0)
  expect_true("x" %in% names(coords))
  expect_true("y" %in% names(coords))
})

test_that("grid layout returns (0.5, 0.5) for 1-node network", {
  grid_fn <- get_layout("grid")

  mock_net_1 <- list(n_nodes = 1L)
  coords <- grid_fn(mock_net_1)
  expect_equal(nrow(coords), 1)
  expect_equal(coords$x, 0.5)
  expect_equal(coords$y, 0.5)
})

test_that("star layout returns empty df for 0-node network", {
  # Lines 55-56: star layout with n==0 and n==1
  star_fn <- get_layout("star")
  expect_false(is.null(star_fn))

  mock_net_0 <- list(n_nodes = 0L)
  coords <- star_fn(mock_net_0)
  expect_equal(nrow(coords), 0)
  expect_true("x" %in% names(coords))
  expect_true("y" %in% names(coords))
})

test_that("star layout returns (0.5, 0.5) for 1-node network", {
  star_fn <- get_layout("star")

  mock_net_1 <- list(n_nodes = 1L)
  coords <- star_fn(mock_net_1)
  expect_equal(nrow(coords), 1)
  expect_equal(coords$x, 0.5)
  expect_equal(coords$y, 0.5)
})

test_that("bipartite layout returns empty df for 0-node network", {
  # Line 78: bipartite layout with n==0
  bipartite_fn <- get_layout("bipartite")
  expect_false(is.null(bipartite_fn))

  mock_net_0 <- list(n_nodes = 0L)
  coords <- bipartite_fn(mock_net_0)
  expect_equal(nrow(coords), 0)
  expect_true("x" %in% names(coords))
  expect_true("y" %in% names(coords))
})

test_that("gephi_fr layout computes valid coordinates", {
  # Lines 107-201: entire Gephi Fruchterman-Reingold algorithm
  skip_if_not_installed("igraph")

  gephi_fn <- get_layout("gephi_fr")
  expect_false(is.null(gephi_fn))

  # Create a 5-node weighted network with edges
  mat <- create_test_matrix(5, weighted = TRUE)
  net <- CographNetwork$new(mat)

  # Run with fewer iterations for speed
  set.seed(42)
  coords <- gephi_fn(net, niter = 10)

  expect_equal(nrow(coords), 5)
  expect_true("x" %in% names(coords))
  expect_true("y" %in% names(coords))
  expect_true(all(is.finite(coords$x)))
  expect_true(all(is.finite(coords$y)))
})

test_that("gephi layout alias works identically to gephi_fr", {
  skip_if_not_installed("igraph")

  gephi_alias_fn <- get_layout("gephi")
  gephi_fn <- get_layout("gephi_fr")
  expect_false(is.null(gephi_alias_fn))

  # Both should be the same function
  mat <- create_test_matrix(4, weighted = TRUE)
  net <- CographNetwork$new(mat)

  set.seed(99)
  coords1 <- gephi_fn(net, niter = 5)
  set.seed(99)
  coords2 <- gephi_alias_fn(net, niter = 5)
  expect_equal(coords1, coords2)
})

test_that("gephi_fr handles 0-node network", {
  skip_if_not_installed("igraph")

  gephi_fn <- get_layout("gephi_fr")

  # Create an empty network
  empty_net <- CographNetwork$new()
  empty_net$set_nodes(data.frame(label = character(0), x = numeric(0), y = numeric(0)))
  empty_net$set_edges(data.frame(from = integer(0), to = integer(0), weight = numeric(0)))
  empty_net$set_directed(FALSE)

  coords <- gephi_fn(empty_net, niter = 5)
  expect_equal(nrow(coords), 0)
})

test_that("gephi_fr exercises attraction and displacement code paths", {
  skip_if_not_installed("igraph")

  gephi_fn <- get_layout("gephi_fr")

  # Create a larger, denser network to exercise all internal paths
  mat <- create_test_matrix(8, density = 0.7, weighted = TRUE)
  net <- CographNetwork$new(mat)

  set.seed(123)
  coords <- gephi_fn(net, niter = 20, area = 5000, gravity = 5.0, speed = 2.0)

  expect_equal(nrow(coords), 8)
  expect_true(all(is.finite(coords$x)))
  expect_true(all(is.finite(coords$y)))
  # Coordinates should not all be identical

  expect_true(length(unique(coords$x)) > 1)
  expect_true(length(unique(coords$y)) > 1)
})

# ============================================
# 2. render-edges.R - Edge width scaling, color by weight sign,
#    cut threshold, and edge label null path
# ============================================

test_that("soplot renders edges with weight-based auto-scaling (no explicit width)", {
  # Lines 30-42: scale_edge_widths() path in render_edges_grid()
  # Need weighted edges without explicit edge_width
  skip_if_not_installed("grid")

  adj <- matrix(c(
    0,   0.8, -0.3,  0,
    0.8, 0,    0.5,  0.1,
   -0.3, 0.5,  0,    0.7,
    0,   0.1,  0.7,  0
  ), nrow = 4, byrow = TRUE)

  # edge_scale_mode triggers the weight-based scaling path
  result <- safe_plot(soplot(adj, edge_scale_mode = "linear"))
  expect_true(result$success, info = result$error)
})

test_that("soplot renders edges without weights using default width", {
  # Line 45: default edge width when no weights
  skip_if_not_installed("grid")

  # Unweighted binary adjacency matrix
  adj <- matrix(c(0, 1, 1, 1, 0, 0, 1, 0, 0), nrow = 3)

  # Create network and explicitly set edge aes without width or weights
  net <- cograph(adj)
  # Remove the width from edge aes so it falls through to the default path
  net$network$set_edge_aes(list(width = NULL))

  result <- safe_plot(soplot(net))
  expect_true(result$success, info = result$error)
})

test_that("soplot renders edge colors by weight sign (positive/negative/zero)", {
  # Lines 58-65: color by weight sign without explicit color
  skip_if_not_installed("grid")

  # Create matrix with positive, negative, and zero weights
  adj <- matrix(c(
    0,    0.5, -0.3,
    0.5,  0,    0,
   -0.3,  0,    0
  ), nrow = 3, byrow = TRUE)

  # No explicit edge_color -> triggers the weight sign color path
  result <- safe_plot(soplot(adj))
  expect_true(result$success, info = result$error)
})

test_that("soplot applies cut threshold for edge transparency", {
  # Lines 81-82: cut threshold triggers fade_factor for below-cut edges
  skip_if_not_installed("grid")

  adj <- matrix(c(
    0,   0.8, 0.05, 0.3,
    0.8, 0,   0.1,  0.02,
    0.05, 0.1, 0,   0.6,
    0.3, 0.02, 0.6, 0
  ), nrow = 4, byrow = TRUE)

  # edge_cutoff sets aes$cut which triggers the transparency path
  result <- safe_plot(soplot(adj, edge_cutoff = 0.2))
  expect_true(result$success, info = result$error)
})

test_that("render_edge_labels_grid returns empty gList when template produces NULL labels", {
  # Line 555: is.null(labels) path
  skip_if_not_installed("grid")

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  # Set label_style to "none" via edge aes to trigger the template path
  # that returns NULL from build_edge_labels_from_template
  net$network$set_edge_aes(list(label_style = "none", label_template = NULL))

  # Call render_edge_labels_grid directly inside a graphics device
  result <- with_temp_png({
    grid::grid.newpage()
    vp <- grid::viewport()
    grid::pushViewport(vp)
    grobs <- cograph:::render_edge_labels_grid(net$network)
    grid::popViewport()
    grobs
  })

  # Should return a gList (empty) since labels are NULL
  expect_true(inherits(result, "gList") || is.null(result))
})

# ============================================
# 3. render-grid.R - TNA handling, same-coord normalization, empty legend
# ============================================

test_that("soplot handles tna object directly", {
  # Lines 281-299: TNA handling in soplot()
  skip_if_not_installed("tna")
  skip_if_not_installed("grid")

  # Create a tna object from a transition matrix
  trans_mat <- matrix(c(
    0.7, 0.2, 0.1,
    0.1, 0.6, 0.3,
    0.3, 0.1, 0.6
  ), nrow = 3, byrow = TRUE)
  rownames(trans_mat) <- colnames(trans_mat) <- c("A", "B", "C")

  tna_obj <- tna::tna(trans_mat)

  result <- safe_plot(soplot(tna_obj))
  expect_true(result$success, info = result$error)
})

test_that("soplot handles all-same coordinate normalization", {
  # Lines 611-612: all nodes at same position -> center at 0.5
  skip_if_not_installed("grid")

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  # Set all nodes to the exact same position
  nodes <- net$network$get_nodes()
  nodes$x <- rep(5.0, nrow(nodes))
  nodes$y <- rep(5.0, nrow(nodes))
  net$network$set_nodes(nodes)

  # Also update the layout coords
  net$network$set_layout_coords(data.frame(x = rep(5.0, 3), y = rep(5.0, 3)))

  result <- safe_plot(soplot(net))
  expect_true(result$success, info = result$error)
})

test_that("render_legend_grid returns empty gList for 0 unique items", {
  # Line 818: legend with 0 unique items -> returns empty gList
  skip_if_not_installed("grid")

  # Create a network with no nodes
  empty_net <- CographNetwork$new()
  empty_net$set_nodes(data.frame(label = character(0), x = numeric(0), y = numeric(0)))
  empty_net$set_edges(data.frame(from = integer(0), to = integer(0)))
  empty_net$set_directed(FALSE)
  empty_net$set_theme(get_theme("classic"))
  empty_net$set_node_aes(list(fill = character(0)))

  result <- with_temp_png({
    grid::grid.newpage()
    vp <- grid::viewport()
    grid::pushViewport(vp)
    grobs <- cograph:::render_legend_grid(empty_net, position = "topright")
    grid::popViewport()
    grobs
  })

  expect_true(inherits(result, "gList"))
})

# ============================================
# 4. cograph.R - igraph layout function and igraph layout name
# ============================================

test_that("cograph() works with igraph layout function", {
  # Lines 73-77 / 202-204: cograph() with a function as layout
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(mat, layout = igraph::layout_with_fr)

  expect_s3_class(net, "cograph_network")
  expect_equal(net$network$n_nodes, 3)

  layout <- net$network$get_layout()
  expect_false(is.null(layout))
  expect_equal(nrow(layout), 3)
  expect_true("x" %in% names(layout))
  expect_true("y" %in% names(layout))

  # Layout info should show custom function
  info <- net$network$get_layout_info()
  expect_equal(info$name, "custom_function")
})

test_that("cograph() works with igraph layout name string (2-letter code)", {
  # Lines 82-86 / 205-209: cograph() with igraph layout name
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(mat, layout = "kk")

  expect_s3_class(net, "cograph_network")
  expect_equal(net$network$n_nodes, 3)

  layout <- net$network$get_layout()
  expect_false(is.null(layout))
  expect_equal(nrow(layout), 3)
  expect_true("x" %in% names(layout))
  expect_true("y" %in% names(layout))
})

test_that("cograph() works with full igraph layout name", {
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(mat, layout = "layout_with_fr")

  expect_s3_class(net, "cograph_network")
  layout <- net$network$get_layout()
  expect_false(is.null(layout))
  expect_equal(nrow(layout), 3)
})

test_that("cograph() works with igraph_ prefixed layout name", {
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(mat, layout = "igraph_kk")

  expect_s3_class(net, "cograph_network")
  layout <- net$network$get_layout()
  expect_false(is.null(layout))
  expect_equal(nrow(layout), 3)
})

# ============================================
# 5. input-igraph.R - Successful path tests
# ============================================

test_that("apply_igraph_layout works with igraph layout function", {
  # Lines 91-108: apply_igraph_layout successful path
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- CographNetwork$new(mat)

  coords <- cograph:::apply_igraph_layout(net, igraph::layout_in_circle)
  expect_equal(nrow(coords), 3)
  expect_true("x" %in% names(coords))
  expect_true("y" %in% names(coords))
  expect_true(all(coords$x >= 0 & coords$x <= 1))
  expect_true(all(coords$y >= 0 & coords$y <= 1))
})

test_that("apply_igraph_layout_by_name works with two-letter code", {
  # Lines 120-186: apply_igraph_layout_by_name successful path
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- CographNetwork$new(mat)

  coords <- cograph:::apply_igraph_layout_by_name(net, "ci", seed = 42)
  expect_equal(nrow(coords), 3)
  expect_true("x" %in% names(coords))
  expect_true("y" %in% names(coords))
})

test_that("apply_igraph_layout_by_name errors on unknown layout", {
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- CographNetwork$new(mat)

  expect_error(
    cograph:::apply_igraph_layout_by_name(net, "zzz_nonexistent"),
    "Unknown igraph layout"
  )
})

test_that("network_to_igraph converts network correctly", {
  skip_if_not_installed("igraph")

  mat <- create_test_matrix(4, weighted = TRUE)
  net <- CographNetwork$new(mat)

  g <- cograph:::network_to_igraph(net)
  expect_true(inherits(g, "igraph"))
  expect_equal(igraph::vcount(g), 4)
  expect_true(igraph::ecount(g) > 0)
})

test_that("network_to_igraph handles empty network", {
  skip_if_not_installed("igraph")

  net <- CographNetwork$new()
  net$set_nodes(data.frame(label = c("A", "B"), x = c(0.3, 0.7), y = c(0.3, 0.7)))
  net$set_edges(data.frame(from = integer(0), to = integer(0), weight = numeric(0)))
  net$set_directed(FALSE)

  g <- cograph:::network_to_igraph(net)
  expect_true(inherits(g, "igraph"))
  expect_equal(igraph::vcount(g), 2)
  expect_equal(igraph::ecount(g), 0)
})

# ============================================
# 6. Additional rendering edge cases
# ============================================

test_that("soplot with weighted edges and edge_width_range triggers scale_edge_widths", {
  # Additional test for Lines 30-42 with explicit edge_width_range
  skip_if_not_installed("grid")

  adj <- create_test_matrix(5, weighted = TRUE)
  result <- safe_plot(soplot(adj, edge_width_range = c(0.5, 5)))
  expect_true(result$success, info = result$error)
})

test_that("soplot with edge_size parameter triggers weight scaling", {
  # Tests esize parameter in scale_edge_widths
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, weighted = TRUE)
  result <- safe_plot(soplot(adj, edge_size = 3))
  expect_true(result$success, info = result$error)
})

test_that("soplot with edge_cutoff and weighted edges renders transparency correctly", {
  # More targeted test for cut threshold code paths
  skip_if_not_installed("grid")

  # Matrix with some weights well below the cutoff
  adj <- matrix(c(
    0,   0.01, 0.9,
    0.01, 0,   0.5,
    0.9,  0.5, 0
  ), nrow = 3, byrow = TRUE)

  result <- safe_plot(soplot(adj, edge_cutoff = 0.3))
  expect_true(result$success, info = result$error)
})

test_that("soplot with legend renders correctly", {
  # Exercise the legend rendering path with actual items
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, legend = TRUE, legend_position = "topleft"))
  expect_true(result$success, info = result$error)
})

test_that("compute_layout_for_cograph handles igraph function layout", {
  # Lines 73-77 in compute_layout_for_cograph
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0), nrow = 4)
  net <- cograph(mat, layout = igraph::layout_with_kk)

  expect_s3_class(net, "cograph_network")
  layout <- net$network$get_layout()
  expect_equal(nrow(layout), 4)
  expect_true(all(is.finite(layout$x)))
  expect_true(all(is.finite(layout$y)))
})

test_that("compute_layout_for_cograph handles igraph name layout", {
  # Lines 82-86 in compute_layout_for_cograph
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0), nrow = 4)

  # Use the fr two-letter code
  net <- cograph(mat, layout = "fr")
  expect_s3_class(net, "cograph_network")
  layout <- net$network$get_layout()
  expect_equal(nrow(layout), 4)
})

test_that("gephi_fr layout handles network with nodes at same positions", {
  # Edge case: ensure displacement code handles edge cases
  skip_if_not_installed("igraph")

  gephi_fn <- get_layout("gephi_fr")

  # Complete graph (dense)
  mat <- create_test_topology("complete", n = 5)
  net <- CographNetwork$new(mat)

  set.seed(1)
  coords <- gephi_fn(net, niter = 15)
  expect_equal(nrow(coords), 5)
  expect_true(all(is.finite(coords$x)))
  expect_true(all(is.finite(coords$y)))
})

test_that("soplot renders directed edges with weight sign coloring", {
  # Test color by weight sign in a directed network
  skip_if_not_installed("grid")

  # Directed weighted matrix with positive, negative, and zero
  adj <- matrix(c(
    0,    0.5, -0.3,  0,
    0,    0,    0.7, -0.1,
    0.2,  0,    0,    0,
    0,    0.4,  0,    0
  ), nrow = 4, byrow = TRUE)

  result <- safe_plot(soplot(adj, show_arrows = TRUE))
  expect_true(result$success, info = result$error)
})

test_that("normalize_coords handles single node", {
  skip_if_not_installed("igraph")

  coords <- matrix(c(3.5, 7.2), nrow = 1)
  result <- cograph:::normalize_coords(coords)
  expect_equal(nrow(result), 1)
  expect_equal(result[1, 1], 0.5)
  expect_equal(result[1, 2], 0.5)
})

test_that("normalize_coords handles constant coordinates", {
  skip_if_not_installed("igraph")

  # All nodes at same position
  coords <- matrix(c(1, 1, 1, 2, 2, 2), ncol = 2)
  result <- cograph:::normalize_coords(coords)
  expect_equal(nrow(result), 3)
  expect_true(all(result[, 1] == 0.5))
  # y should be normalized (has range)
})
