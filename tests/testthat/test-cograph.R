test_that("cograph() creates network from matrix", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  expect_s3_class(net, "cograph_network")
  expect_equal(net$network$n_nodes, 3)
  expect_equal(net$network$n_edges, 3)
})

test_that("cograph() creates network from edge list", {
  edges <- data.frame(from = c("A", "B"), to = c("B", "C"))
  net <- cograph(edges)

  expect_s3_class(net, "cograph_network")
  expect_equal(net$network$n_nodes, 3)
})

test_that("cograph() applies default layout", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  layout <- net$network$get_layout()
  expect_false(is.null(layout))
  expect_equal(nrow(layout), 3)
})

test_that("sn_layout() changes layout", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net1 <- cograph(adj, layout = "spring", seed = 42)
  net2 <- net1 |> sn_layout("circle")

  coords1 <- net1$network$get_layout()
  coords2 <- net2$network$get_layout()

  # Layouts should be different
  expect_false(all(coords1$x == coords2$x))
})

test_that("sn_nodes() modifies node aesthetics", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj) |>
    sn_nodes(size = 0.1, fill = "red")

  aes <- net$network$get_node_aes()
  expect_true(all(aes$size == 0.1))
  expect_true(all(aes$fill == "red"))
})

test_that("sn_edges() modifies edge aesthetics", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj) |>
    sn_edges(width = 2, color = "blue")

  aes <- net$network$get_edge_aes()
  expect_true(all(aes$width == 2))
  expect_true(all(aes$color == "blue"))
})

test_that("sn_theme() applies theme", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj) |> sn_theme("dark")

  theme <- net$network$get_theme()
  expect_equal(theme$name, "dark")
  expect_equal(theme$get("background"), "#1a1a2e")
})

test_that("pipe chain works correctly", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- adj |>
    cograph(layout = "circle") |>
    sn_nodes(size = 0.08, fill = "steelblue") |>
    sn_edges(width = 1.5) |>
    sn_theme("minimal")

  expect_s3_class(net, "cograph_network")
  expect_true(all(net$network$get_node_aes()$fill == "steelblue"))
  expect_equal(net$network$get_theme()$name, "minimal")
})

test_that("print method works", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  expect_output(print(net), "Cograph Network")
  expect_output(print(net), "Nodes: 3")
})

test_that("summary method works", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  expect_output(summary(net), "Cograph Network Summary")
  result <- summary(net)
  expect_equal(result$n_nodes, 3)
})

# ============================================
# cograph() Extended Tests
# ============================================

test_that("cograph() works with node_labels", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj, node_labels = c("A", "B", "C"))

  expect_s3_class(net, "cograph_network")
  labels <- net$network$node_labels
  expect_equal(labels, c("A", "B", "C"))
})

test_that("cograph() works with seed = NULL", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj, seed = NULL)

  expect_s3_class(net, "cograph_network")
  expect_equal(net$network$n_nodes, 3)
})

test_that("cograph() works with directed = TRUE", {
  adj <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), nrow = 3)
  net <- cograph(adj, directed = TRUE)

  expect_s3_class(net, "cograph_network")
  expect_true(net$network$is_directed)
})

test_that("cograph() works with directed = FALSE", {
  adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), nrow = 3)
  net <- cograph(adj, directed = FALSE)

  expect_s3_class(net, "cograph_network")
  expect_false(net$network$is_directed)
})

test_that("cograph() works with matrix layout coordinates", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  coords <- matrix(c(0, 1, 0.5, 0, 0, 1), ncol = 2, byrow = TRUE)
  net <- cograph(adj, layout = coords)

  expect_s3_class(net, "cograph_network")
  layout <- net$network$get_layout()
  # Layout should have 3 rows and x, y columns
  expect_equal(nrow(layout), 3)
  expect_true("x" %in% names(layout))
  expect_true("y" %in% names(layout))
})

test_that("cograph() works with data.frame layout coordinates", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  coords <- data.frame(x = c(0, 1, 0.5), y = c(0, 0, 1))
  net <- cograph(adj, layout = coords)

  expect_s3_class(net, "cograph_network")
})

test_that("cograph() works with circle layout", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj, layout = "circle")

  expect_s3_class(net, "cograph_network")
  layout <- net$network$get_layout()
  expect_equal(nrow(layout), 3)
})

test_that("cograph() works with grid layout", {
  adj <- matrix(c(0, 1, 1, 0,
                  1, 0, 0, 1,
                  1, 0, 0, 1,
                  0, 1, 1, 0), nrow = 4)
  net <- cograph(adj, layout = "grid")

  expect_s3_class(net, "cograph_network")
})

test_that("cograph() works with random layout", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj, layout = "random")

  expect_s3_class(net, "cograph_network")
})

# ============================================
# sn_layout() Extended Tests
# ============================================

test_that("sn_layout() works with custom matrix coordinates", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)
  coords <- matrix(c(0, 1, 0.5, 0, 0, 1), ncol = 2, byrow = TRUE)
  net2 <- sn_layout(net, coords)

  expect_s3_class(net2, "cograph_network")
})

test_that("sn_layout() works with custom data.frame coordinates", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)
  coords <- data.frame(x = c(0, 1, 0.5), y = c(0, 0, 1))
  net2 <- sn_layout(net, coords)

  expect_s3_class(net2, "cograph_network")
})

test_that("sn_layout() works with CographLayout object", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)
  layout_obj <- CographLayout$new("circle")
  net2 <- sn_layout(net, layout_obj)

  expect_s3_class(net2, "cograph_network")
})

test_that("sn_layout() works with seed = NULL", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)
  net2 <- sn_layout(net, "spring", seed = NULL)

  expect_s3_class(net2, "cograph_network")
})

test_that("sn_layout() works directly on matrix", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- sn_layout(adj, "circle")

  expect_s3_class(net, "cograph_network")
})

test_that("sn_layout() errors on invalid layout type", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  expect_error(sn_layout(net, 123))
})

# ============================================
# sn_theme() Extended Tests
# ============================================

test_that("sn_theme() works with CographTheme object", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)
  theme_obj <- CographTheme$new(node_fill = "purple")
  net2 <- sn_theme(net, theme_obj)

  expect_s3_class(net2, "cograph_network")
})

test_that("sn_theme() works with overrides", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)
  net2 <- sn_theme(net, "classic", background = "lightgray")

  expect_s3_class(net2, "cograph_network")
  theme <- net2$network$get_theme()
  expect_equal(theme$get("background"), "lightgray")
})

test_that("sn_theme() works directly on matrix", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- sn_theme(adj, "dark")

  expect_s3_class(net, "cograph_network")
})

test_that("sn_theme() errors on unknown theme", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  expect_error(sn_theme(net, "nonexistent_theme_xyz"))
})

test_that("sn_theme() errors on invalid theme type", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  expect_error(sn_theme(net, 123))
})

# ============================================
# sn_palette() Extended Tests
# ============================================

test_that("sn_palette() works with function palette", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)
  my_pal <- function(n) rainbow(n, s = 0.7)
  net2 <- sn_palette(net, my_pal)

  expect_s3_class(net2, "cograph_network")
})

test_that("sn_palette() works with target = edges", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)
  net2 <- sn_palette(net, "viridis", target = "edges")

  expect_s3_class(net2, "cograph_network")
})

test_that("sn_palette() works with target = both", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)
  net2 <- sn_palette(net, "viridis", target = "both")

  expect_s3_class(net2, "cograph_network")
})

test_that("sn_palette() works with by parameter", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)
  # Add a group column to nodes
  nodes <- net$network$get_nodes()
  nodes$group <- c("A", "B", "A")
  net$network$set_nodes(nodes)

  net2 <- sn_palette(net, "viridis", by = "group")

  expect_s3_class(net2, "cograph_network")
})

test_that("sn_palette() works directly on matrix", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- sn_palette(adj, "viridis")

  expect_s3_class(net, "cograph_network")
})

test_that("sn_palette() errors on unknown palette", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  expect_error(sn_palette(net, "nonexistent_palette_xyz"))
})

test_that("sn_palette() errors on invalid palette type", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  expect_error(sn_palette(net, 123))
})

# ============================================
# ensure_cograph_network() Tests
# ============================================

test_that("ensure_cograph_network handles cograph_network input", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  result <- cograph:::ensure_cograph_network(net)
  expect_s3_class(result, "cograph_network")
})

test_that("ensure_cograph_network converts matrix input", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)

  result <- cograph:::ensure_cograph_network(adj)
  expect_s3_class(result, "cograph_network")
})

test_that("ensure_cograph_network converts data.frame input", {
  edges <- data.frame(from = c("A", "B"), to = c("B", "C"))

  result <- cograph:::ensure_cograph_network(edges)
  expect_s3_class(result, "cograph_network")
})

test_that("ensure_cograph_network errors on invalid input", {
  expect_error(cograph:::ensure_cograph_network("invalid"))
})

# ============================================
# igraph Layout Tests (if igraph available)
# ============================================

test_that("cograph() works with igraph two-letter layout codes", {
  skip_if_not_installed("igraph")

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)

  # Test "kk" (Kamada-Kawai)
  net_kk <- cograph(adj, layout = "kk")
  expect_s3_class(net_kk, "cograph_network")

  # Test "fr" (Fruchterman-Reingold)
  net_fr <- cograph(adj, layout = "fr")
  expect_s3_class(net_fr, "cograph_network")

  # Test "ci" (circle - igraph version)
  net_ci <- cograph(adj, layout = "ci")
  expect_s3_class(net_ci, "cograph_network")
})

test_that("cograph() works with igraph layout function", {
  skip_if_not_installed("igraph")

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj, layout = igraph::layout_in_circle)

  expect_s3_class(net, "cograph_network")
})

test_that("sn_layout() works with igraph two-letter codes", {
  skip_if_not_installed("igraph")

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)
  net2 <- sn_layout(net, "kk")

  expect_s3_class(net2, "cograph_network")
})

test_that("sn_layout() works with igraph layout function", {
  skip_if_not_installed("igraph")

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)
  net2 <- sn_layout(net, igraph::layout_with_kk)

  expect_s3_class(net2, "cograph_network")
})

test_that("sn_layout() works with igraph layout_ prefix names", {
  skip_if_not_installed("igraph")

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)
  net2 <- sn_layout(net, "layout_with_kk")

  expect_s3_class(net2, "cograph_network")
})

# ============================================
# ensure_cograph_network Extended Tests
# ============================================

test_that("ensure_cograph_network handles cograph_network without layout", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  # Clear the layout coordinates
  nodes <- net$network$get_nodes()
  nodes$x <- NA
  nodes$y <- NA
  net$network$set_nodes(nodes)

  # ensure_cograph_network should recompute layout
  result <- cograph:::ensure_cograph_network(net)
  expect_s3_class(result, "cograph_network")
})

test_that("ensure_cograph_network handles igraph input", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(5)
  result <- cograph:::ensure_cograph_network(g)
  expect_s3_class(result, "cograph_network")
})

test_that("ensure_cograph_network handles network/statnet input", {
  skip_if_not_installed("network")

  net_statnet <- network::network(matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3))
  result <- cograph:::ensure_cograph_network(net_statnet)
  expect_s3_class(result, "cograph_network")
})

test_that("ensure_cograph_network handles qgraph input", {
  skip_if_not_installed("qgraph")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  q <- qgraph::qgraph(mat, DoNotPlot = TRUE)
  result <- cograph:::ensure_cograph_network(q)
  expect_s3_class(result, "cograph_network")
})

# ============================================
# compute_layout_for_cograph Tests
# ============================================

test_that("compute_layout_for_cograph handles function layout", {
  skip_if_not_installed("igraph")

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  # Clear layout
  nodes <- net$network$get_nodes()
  nodes$x <- NA
  nodes$y <- NA
  net$network$set_nodes(nodes)

  # Use ensure_cograph_network with a function layout
  result <- cograph:::ensure_cograph_network(net, layout = igraph::layout_in_circle)
  expect_s3_class(result, "cograph_network")
})

test_that("compute_layout_for_cograph handles igraph code layout", {
  skip_if_not_installed("igraph")

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  # Clear layout
  nodes <- net$network$get_nodes()
  nodes$x <- NA
  nodes$y <- NA
  net$network$set_nodes(nodes)

  # Use ensure_cograph_network with igraph code layout
  result <- cograph:::ensure_cograph_network(net, layout = "fr")
  expect_s3_class(result, "cograph_network")
})

test_that("compute_layout_for_cograph handles matrix layout", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  # Clear layout
  nodes <- net$network$get_nodes()
  nodes$x <- NA
  nodes$y <- NA
  net$network$set_nodes(nodes)

  # Use ensure_cograph_network with matrix layout
  custom_coords <- matrix(c(0, 1, 0.5, 0, 0, 1), ncol = 2)
  result <- cograph:::ensure_cograph_network(net, layout = custom_coords)
  expect_s3_class(result, "cograph_network")
})

test_that("compute_layout_for_cograph handles NULL seed", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj, seed = NULL)

  # Clear layout
  nodes <- net$network$get_nodes()
  nodes$x <- NA
  nodes$y <- NA
  net$network$set_nodes(nodes)

  # Use ensure_cograph_network
  result <- cograph:::ensure_cograph_network(net, seed = NULL)
  expect_s3_class(result, "cograph_network")
})

# ============================================
# compute_layout_for_cograph Extended Tests
# ============================================

test_that("compute_layout_for_cograph handles builtin layout", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  # Clear layout
  nodes <- net$network$get_nodes()
  nodes$x <- NA
  nodes$y <- NA
  net$network$set_nodes(nodes)

  # Use ensure_cograph_network with builtin layout
  result <- cograph:::ensure_cograph_network(net, layout = "circle")
  expect_s3_class(result, "cograph_network")

  # Verify layout was computed
  new_nodes <- get_nodes(result)
  expect_false(all(is.na(new_nodes$x)))
})

test_that("compute_layout_for_cograph handles data.frame layout", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  # Clear layout
  nodes <- net$network$get_nodes()
  nodes$x <- NA
  nodes$y <- NA
  net$network$set_nodes(nodes)

  # Use ensure_cograph_network with data.frame layout
  coords <- data.frame(x = c(0, 1, 0.5), y = c(0, 0, 1))
  result <- cograph:::ensure_cograph_network(net, layout = coords)
  expect_s3_class(result, "cograph_network")
})

test_that("compute_layout_for_cograph handles seed parameter", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  # Clear layout
  nodes <- net$network$get_nodes()
  nodes$x <- NA
  nodes$y <- NA
  net$network$set_nodes(nodes)

  # Use ensure_cograph_network with specific seed
  result <- cograph:::ensure_cograph_network(net, layout = "spring", seed = 123)
  expect_s3_class(result, "cograph_network")
})

test_that("compute_layout_for_cograph updates network layout info", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  # Clear layout
  nodes <- net$network$get_nodes()
  nodes$x <- NA
  nodes$y <- NA
  net$network$set_nodes(nodes)

  # Use ensure_cograph_network
  result <- cograph:::ensure_cograph_network(net, layout = "grid")
  expect_s3_class(result, "cograph_network")
})

test_that("compute_layout_for_cograph handles directed network", {
  adj <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), nrow = 3)
  net <- cograph(adj, directed = TRUE)

  # Clear layout
  nodes <- net$network$get_nodes()
  nodes$x <- NA
  nodes$y <- NA
  net$network$set_nodes(nodes)

  result <- cograph:::ensure_cograph_network(net, layout = "circle")
  expect_s3_class(result, "cograph_network")
})

test_that("compute_layout_for_cograph uses edges for layout", {
  adj <- matrix(c(0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0), nrow = 4)
  net <- cograph(adj)

  # Clear layout
  nodes <- net$network$get_nodes()
  nodes$x <- NA
  nodes$y <- NA
  net$network$set_nodes(nodes)

  result <- cograph:::ensure_cograph_network(net, layout = "spring")
  expect_s3_class(result, "cograph_network")

  # Verify the layout has valid coordinates
  new_nodes <- get_nodes(result)
  expect_true(all(!is.na(new_nodes$x)))
  expect_true(all(!is.na(new_nodes$y)))
})

# ============================================
# as_cograph and Lightweight Format Tests
# ============================================

test_that("as_cograph creates lightweight network without layout", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(adj)

  expect_s3_class(net, "cograph_network")
  expect_true("nodes" %in% names(net))
  expect_null(net$layout)
})

test_that("ensure_cograph_network triggers compute_layout_for_cograph for lightweight format", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(adj)

  # Lightweight format has no x column initially
  expect_true(!"x" %in% names(net$nodes) || all(is.na(net$nodes$x)))

  # ensure_cograph_network should compute layout
  result <- cograph:::ensure_cograph_network(net, layout = "circle")
  expect_s3_class(result, "cograph_network")
})

test_that("compute_layout_for_cograph handles builtin layout via lightweight format", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(adj)

  # Trigger compute_layout_for_cograph with builtin layout
  result <- cograph:::ensure_cograph_network(net, layout = "grid")
  expect_s3_class(result, "cograph_network")
})

test_that("compute_layout_for_cograph handles spring layout via lightweight format", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(adj)

  result <- cograph:::ensure_cograph_network(net, layout = "spring", seed = 42)
  expect_s3_class(result, "cograph_network")
})

test_that("compute_layout_for_cograph handles matrix layout via lightweight format", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(adj)

  coords <- matrix(c(0, 1, 0.5, 0, 0, 1), ncol = 2)
  result <- cograph:::ensure_cograph_network(net, layout = coords)
  expect_s3_class(result, "cograph_network")
})

test_that("compute_layout_for_cograph handles NULL seed via lightweight format", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(adj)

  result <- cograph:::ensure_cograph_network(net, layout = "random", seed = NULL)
  expect_s3_class(result, "cograph_network")
})

test_that("compute_layout_for_cograph handles function layout via lightweight format", {
  skip_if_not_installed("igraph")

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(adj)

  result <- cograph:::ensure_cograph_network(net, layout = igraph::layout_in_circle)
  expect_s3_class(result, "cograph_network")
})

test_that("compute_layout_for_cograph handles igraph code via lightweight format", {
  skip_if_not_installed("igraph")

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(adj)

  result <- cograph:::ensure_cograph_network(net, layout = "kk")
  expect_s3_class(result, "cograph_network")
})

test_that("compute_layout_for_cograph handles directed lightweight network", {
  adj <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), nrow = 3)
  net <- as_cograph(adj, directed = TRUE)

  result <- cograph:::ensure_cograph_network(net, layout = "circle")
  expect_s3_class(result, "cograph_network")
})

# ============================================
# Edge List Input Tests
# ============================================

test_that("cograph() handles weighted edge list", {
  edges <- data.frame(
    from = c("A", "B", "C"),
    to = c("B", "C", "A"),
    weight = c(0.5, 0.8, 0.3)
  )
  net <- cograph(edges)

  expect_s3_class(net, "cograph_network")
  expect_equal(net$network$n_nodes, 3)
})

test_that("cograph() handles edge list with extra columns", {
  edges <- data.frame(
    from = c("A", "B"),
    to = c("B", "C"),
    weight = c(1, 2),
    color = c("red", "blue")
  )
  net <- cograph(edges)

  expect_s3_class(net, "cograph_network")
})

# ============================================
# Additional Validation Tests
# ============================================

test_that("cograph() handles asymmetric matrix as directed", {
  adj <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), nrow = 3)
  net <- cograph(adj, directed = TRUE)

  expect_true(net$network$is_directed)
})

test_that("cograph() normalizes node coordinates", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  layout <- net$network$get_layout()
  # Coordinates should be in a reasonable range
  expect_true(all(layout$x >= 0 & layout$x <= 1))
  expect_true(all(layout$y >= 0 & layout$y <= 1))
})
