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
