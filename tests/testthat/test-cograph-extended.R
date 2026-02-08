# Extended tests for cograph() main entry point and helpers
# Covers: R/cograph.R (sn_palette, sn_theme errors, ensure_cograph_network,
#          compute_layout_for_cograph, cograph with custom layouts)

# ============================================
# ensure_cograph_network Tests
# ============================================

test_that("ensure_cograph_network returns cograph_network as-is with layout", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(mat)

  result <- cograph:::ensure_cograph_network(net)
  expect_s3_class(result, "cograph_network")
})

test_that("ensure_cograph_network converts matrix", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)

  result <- cograph:::ensure_cograph_network(mat)
  expect_s3_class(result, "cograph_network")
})

test_that("ensure_cograph_network converts data.frame", {
  edges <- data.frame(from = c("A", "B"), to = c("B", "C"))

  result <- cograph:::ensure_cograph_network(edges)
  expect_s3_class(result, "cograph_network")
})

test_that("ensure_cograph_network errors on invalid input", {
  expect_error(cograph:::ensure_cograph_network("invalid"), "must be a matrix")
})

test_that("ensure_cograph_network computes layout for new format without coords", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  result <- cograph:::ensure_cograph_network(net, layout = "circle")

  nodes <- get_nodes(result)
  expect_true("x" %in% names(nodes))
  expect_false(all(is.na(nodes$x)))
})

# ============================================
# compute_layout_for_cograph Tests
# ============================================

test_that("compute_layout_for_cograph adds layout to new format", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  result <- cograph:::compute_layout_for_cograph(net, layout = "circle")

  expect_true(!is.null(result$layout))
  expect_true(!is.null(result$layout_info))
  nodes <- get_nodes(result)
  expect_true("x" %in% names(nodes))
  expect_false(all(is.na(nodes$x)))
})

test_that("compute_layout_for_cograph with spring layout", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  result <- cograph:::compute_layout_for_cograph(net, layout = "spring", seed = 42)

  expect_equal(result$layout_info$name, "spring")
  expect_equal(result$layout_info$seed, 42)
})

test_that("compute_layout_for_cograph with custom coordinate matrix", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)
  coords <- matrix(c(0, 1, 0.5, 0, 0, 1), ncol = 2)

  result <- cograph:::compute_layout_for_cograph(net, layout = coords)

  nodes <- get_nodes(result)
  expect_equal(nodes$x, c(0, 1, 0.5))
  expect_equal(nodes$y, c(0, 0, 1))
})

test_that("compute_layout_for_cograph with custom coordinate data.frame", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)
  coords <- data.frame(x = c(0, 1, 0.5), y = c(0, 0, 1))

  result <- cograph:::compute_layout_for_cograph(net, layout = coords)

  nodes <- get_nodes(result)
  expect_equal(nodes$x, c(0, 1, 0.5))
})

# ============================================
# cograph() Extended Tests
# ============================================

test_that("cograph with circle layout", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(mat, layout = "circle")

  expect_s3_class(net, "cograph_network")
  layout <- net$network$get_layout()
  expect_equal(nrow(layout), 3)
})

test_that("cograph with grid layout", {
  mat <- create_test_matrix(9)
  net <- cograph(mat, layout = "grid")

  expect_s3_class(net, "cograph_network")
  expect_equal(net$network$n_nodes, 9)
})

test_that("cograph with random layout", {
  mat <- create_test_matrix(5)
  net <- cograph(mat, layout = "random")

  expect_s3_class(net, "cograph_network")
})

test_that("cograph with custom coordinate matrix", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  coords <- matrix(c(0, 1, 0.5, 0, 0, 1), ncol = 2)

  net <- cograph(mat, layout = coords)

  expect_s3_class(net, "cograph_network")
  layout <- net$network$get_layout()
  expect_equal(layout$x, c(0, 1, 0.5))
})

test_that("cograph with custom coordinate data frame", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  coords <- data.frame(x = c(0, 1, 0.5), y = c(0, 0, 1))

  net <- cograph(mat, layout = coords)

  expect_s3_class(net, "cograph_network")
})

test_that("cograph with custom node labels", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(mat, node_labels = c("A", "B", "C"))

  expect_equal(net$network$node_labels, c("A", "B", "C"))
})

test_that("cograph with NULL seed", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(mat, seed = NULL)

  expect_s3_class(net, "cograph_network")
})

test_that("cograph stores layout info", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(mat, layout = "circle", seed = 42)

  info <- net$network$get_layout_info()
  expect_equal(info$name, "circle")
  expect_equal(info$seed, 42)
})

# ============================================
# sn_layout Extended Tests
# ============================================

test_that("sn_layout with CographLayout object", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(mat)

  layout_obj <- CographLayout$new("grid")
  result <- sn_layout(net, layout_obj)

  expect_s3_class(result, "cograph_network")
})

test_that("sn_layout with custom coordinate matrix", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(mat)
  coords <- matrix(c(0, 1, 0.5, 0, 0, 1), ncol = 2)

  result <- sn_layout(net, coords)

  expect_s3_class(result, "cograph_network")
})

test_that("sn_layout with matrix input auto-converts", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)

  result <- sn_layout(mat, "circle")

  expect_s3_class(result, "cograph_network")
})

test_that("sn_layout errors on invalid layout type", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(mat)

  expect_error(sn_layout(net, 42), "layout must be")
})

# ============================================
# sn_theme Extended Tests
# ============================================

test_that("sn_theme with matrix input auto-converts", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)

  result <- sn_theme(mat, "dark")

  expect_s3_class(result, "cograph_network")
})

test_that("sn_theme errors on invalid theme name", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(mat)

  expect_error(sn_theme(net, "nonexistent_theme"), "Unknown theme")
})

test_that("sn_theme errors on invalid theme type", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(mat)

  expect_error(sn_theme(net, 42), "must be a string or CographTheme")
})

test_that("sn_theme with CographTheme object", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(mat)
  theme <- CographTheme$new(background = "navy")

  result <- sn_theme(net, theme)

  expect_s3_class(result, "cograph_network")
  result_theme <- result$network$get_theme()
  expect_equal(result_theme$get("background"), "navy")
})

test_that("sn_theme with overrides", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(mat)

  result <- sn_theme(net, "classic", background = "lightgray")

  theme <- result$network$get_theme()
  expect_equal(theme$get("background"), "lightgray")
})

# ============================================
# sn_palette Tests
# ============================================

test_that("sn_palette applies to nodes", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(mat)

  result <- sn_palette(net, "viridis", target = "nodes")

  expect_s3_class(result, "cograph_network")
  aes <- result$network$get_node_aes()
  expect_true(!is.null(aes$fill))
})

test_that("sn_palette applies to edges", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(mat)

  result <- sn_palette(net, "viridis", target = "edges")

  expect_s3_class(result, "cograph_network")
  aes <- result$network$get_edge_aes()
  expect_true(!is.null(aes$positive_color))
  expect_true(!is.null(aes$negative_color))
})

test_that("sn_palette applies to both", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(mat)

  result <- sn_palette(net, "viridis", target = "both")

  expect_s3_class(result, "cograph_network")
})

test_that("sn_palette with custom function", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(mat)

  my_pal <- function(n) rainbow(n, s = 0.7)
  result <- sn_palette(net, my_pal)

  expect_s3_class(result, "cograph_network")
})

test_that("sn_palette with matrix input auto-converts", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)

  result <- sn_palette(mat, "viridis")

  expect_s3_class(result, "cograph_network")
})

test_that("sn_palette errors on invalid palette name", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(mat)

  expect_error(sn_palette(net, "nonexistent_palette"), "Unknown palette")
})

test_that("sn_palette errors on invalid palette type", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(mat)

  expect_error(sn_palette(net, 42), "must be a string or function")
})

test_that("sn_palette with by parameter", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(mat)
  # Manually set a group column in nodes
  nodes <- net$network$get_nodes()
  nodes$group <- c("A", "B", "A")
  net$network$set_nodes(nodes)

  result <- sn_palette(net, "viridis", by = "group")

  expect_s3_class(result, "cograph_network")
})
