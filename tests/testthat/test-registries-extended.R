# Extended tests for registry functions
# Covers: R/layout-registry.R, R/shapes-registry.R, R/themes-registry.R, R/zzz.R

# ============================================
# Direct Registration Function Calls (for covr tracking)
# ============================================

test_that("register_builtin_shapes runs without error", {
  expect_no_error(cograph:::register_builtin_shapes())
})

test_that("register_builtin_layouts runs without error", {
  expect_no_error(cograph:::register_builtin_layouts())
})

test_that("register_builtin_themes runs without error", {
  expect_no_error(cograph:::register_builtin_themes())
})

test_that("init_registries runs without error", {
  expect_no_error(cograph:::init_registries())
})

test_that("register_builtin_palettes runs without error", {
  expect_no_error(cograph:::register_builtin_palettes())
})

test_that(".onLoad can be called directly", {
  # Calling .onLoad again should not error (re-initializes registries)
  expect_no_error(cograph:::.onLoad(NULL, "cograph"))
})

# ============================================
# Layout Registry Extended Tests
# ============================================

test_that("all built-in layouts are registered", {
  layouts <- list_layouts()

  expected <- c("circle", "oval", "ellipse", "spring", "fr",
                "fruchterman-reingold", "groups", "grid", "random",
                "star", "bipartite", "custom", "gephi_fr", "gephi")
  for (name in expected) {
    expect_true(name %in% layouts, info = paste("Missing layout:", name))
  }
})

test_that("get_layout returns function for each built-in layout", {
  builtins <- c("circle", "spring", "groups", "grid", "random",
                "star", "bipartite", "custom")

  for (name in builtins) {
    fn <- get_layout(name)
    expect_true(is.function(fn), info = paste("get_layout failed for:", name))
  }
})

test_that("get_layout returns NULL for nonexistent layout", {
  result <- get_layout("nonexistent_layout_xyz")
  expect_null(result)
})

test_that("register_layout works for custom layout", {
  my_layout <- function(network, ...) {
    n <- network$n_nodes
    data.frame(x = seq(0, 1, length.out = n), y = rep(0.5, n))
  }

  register_layout("test_custom_layout", my_layout)
  expect_true("test_custom_layout" %in% list_layouts())

  retrieved <- get_layout("test_custom_layout")
  expect_true(is.function(retrieved))
})

test_that("oval layout alias works", {
  oval_fn <- get_layout("oval")
  ellipse_fn <- get_layout("ellipse")

  # Both should be non-null functions
  expect_true(is.function(oval_fn))
  expect_true(is.function(ellipse_fn))
})

test_that("spring layout aliases work", {
  spring_fn <- get_layout("spring")
  fr_fn <- get_layout("fr")
  long_fn <- get_layout("fruchterman-reingold")

  expect_true(is.function(spring_fn))
  expect_true(is.function(fr_fn))
  expect_true(is.function(long_fn))
})

# ============================================
# Layout Computation Tests (covers layout functions)
# ============================================

test_that("circle layout produces correct number of points", {
  mat <- create_test_matrix(8)
  net <- CographNetwork$new(mat)
  layout_fn <- get_layout("circle")
  coords <- layout_fn(net)

  expect_equal(nrow(coords), 8)
  expect_true("x" %in% names(coords))
  expect_true("y" %in% names(coords))
})

test_that("spring layout produces correct number of points", {
  mat <- create_test_matrix(6)
  net <- CographNetwork$new(mat)
  layout_fn <- get_layout("spring")
  coords <- layout_fn(net)

  expect_equal(nrow(coords), 6)
})

test_that("random layout with different seeds gives different results", {
  mat <- create_test_matrix(5)
  net <- CographNetwork$new(mat)
  layout_fn <- get_layout("random")

  coords1 <- layout_fn(net, seed = 1)
  coords2 <- layout_fn(net, seed = 99)

  expect_false(all(coords1$x == coords2$x))
})

# ============================================
# Shapes Registry Extended Tests
# ============================================

test_that("all expected shapes are registered", {
  shapes <- list_shapes()

  expected <- c("circle", "square", "triangle", "diamond", "pentagon",
                "hexagon", "ellipse", "heart", "star", "pie", "donut",
                "polygon_donut", "donut_pie", "double_donut_pie",
                "cross", "plus", "none", "rectangle",
                "neural", "chip", "robot", "brain", "network",
                "database", "cloud", "gear")

  for (name in expected) {
    expect_true(name %in% shapes, info = paste("Missing shape:", name))
  }
})

test_that("get_shape returns function for basic shapes", {
  basic <- c("circle", "square", "triangle", "diamond")

  for (name in basic) {
    fn <- get_shape(name)
    expect_true(is.function(fn), info = paste("get_shape failed for:", name))
  }
})

test_that("shape count matches README claim (25+)", {
  shapes <- list_shapes()
  expect_true(length(shapes) >= 25)
})

# ============================================
# Themes Registry Extended Tests
# ============================================

test_that("all expected themes are registered", {
  themes <- list_themes()

  expected <- c("classic", "dark", "colorblind", "gray", "grey",
                "minimal", "viridis", "nature")
  for (name in expected) {
    expect_true(name %in% themes, info = paste("Missing theme:", name))
  }
})

test_that("get_theme returns CographTheme for each built-in", {
  theme_names <- c("classic", "dark", "colorblind", "gray", "minimal",
                   "viridis", "nature")

  for (name in theme_names) {
    theme <- get_theme(name)
    expect_true(inherits(theme, "CographTheme"),
                info = paste("get_theme returned wrong type for:", name))
  }
})

test_that("grey alias returns same as gray", {
  gray_theme <- get_theme("gray")
  grey_theme <- get_theme("grey")

  expect_equal(gray_theme$get("background"), grey_theme$get("background"))
  expect_equal(gray_theme$get("node_fill"), grey_theme$get("node_fill"))
})

test_that("each theme has valid colors", {
  theme_names <- c("classic", "dark", "colorblind", "minimal", "viridis", "nature")

  for (name in theme_names) {
    theme <- get_theme(name)
    bg <- theme$get("background")
    fill <- theme$get("node_fill")

    # Colors should be parseable
    expect_false(is.null(tryCatch(grDevices::col2rgb(bg), error = function(e) NULL)),
                 info = paste("Invalid background color in theme:", name))
    expect_false(is.null(tryCatch(grDevices::col2rgb(fill), error = function(e) NULL)),
                 info = paste("Invalid node_fill color in theme:", name))
  }
})

# ============================================
# .onLoad / .onAttach Tests (zzz.R)
# ============================================

test_that("registries are initialized after package load", {
  env <- cograph:::.cograph_env

  expect_true(!is.null(env$shapes))
  expect_true(!is.null(env$layouts))
  expect_true(!is.null(env$themes))
  expect_true(!is.null(env$palettes))
})

test_that("registries contain expected number of entries", {
  expect_true(length(list_shapes()) >= 25)
  expect_true(length(list_layouts()) >= 10)
  expect_true(length(list_themes()) >= 7)
})

test_that("palettes are registered", {
  palettes <- list_palettes()
  expect_true(length(palettes) > 0)
})

test_that("package version is accessible", {
  ver <- packageVersion("cograph")
  expect_true(!is.null(ver))
  expect_true(is.character(as.character(ver)))
})
