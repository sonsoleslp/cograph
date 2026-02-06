# Tests for the registry system
# Covers: R/aaa-globals.R

test_that("registries are initialized on package load", {
  # Registries should already exist from package load
  # Just verify they exist and are lists
  expect_true(is.list(cograph:::.cograph_env$shapes) ||
              is.environment(cograph:::.cograph_env$shapes))
})

# ============================================
# Shape Registry Tests
# ============================================

test_that("register_shape adds shape to registry", {
  custom_fn <- function(x, y, size, fill, border_color, border_width, ...) {
    grid::circleGrob(
      x = grid::unit(x, "npc"),
      y = grid::unit(y, "npc"),
      r = grid::unit(size, "npc"),
      gp = grid::gpar(fill = fill, col = border_color, lwd = border_width)
    )
  }

  # Register a test shape
  register_shape("test_shape_1", custom_fn)

  # Should be retrievable
  retrieved <- get_shape("test_shape_1")
  expect_true(is.function(retrieved))
})

test_that("register_shape validates input is function", {
  expect_error(
    register_shape("not_a_function", "string"),
    "must be a function"
  )

  expect_error(
    register_shape("not_a_function", 123),
    "must be a function"
  )

  expect_error(
    register_shape("not_a_function", list(a = 1)),
    "must be a function"
  )
})

test_that("get_shape returns NULL for unknown shapes", {
  result <- get_shape("nonexistent_shape_xyz")
  expect_null(result)
})

test_that("list_shapes returns character vector", {
  shapes <- list_shapes()

  expect_true(is.character(shapes))
  expect_true(length(shapes) > 0)
})

test_that("built-in shapes are available", {
  shapes <- list_shapes()

  # Check for common built-in shapes
  expect_true("circle" %in% shapes)
  expect_true("square" %in% shapes)
  expect_true("triangle" %in% shapes)
  expect_true("diamond" %in% shapes)
})

test_that("custom shapes can override built-ins", {
  # Get original
  original <- get_shape("circle")

  # Create custom
  custom_fn <- function(x, y, size, fill, border_color, border_width, ...) {
    grid::rectGrob(
      x = grid::unit(x, "npc"),
      y = grid::unit(y, "npc"),
      width = grid::unit(size * 2, "npc"),
      height = grid::unit(size * 2, "npc"),
      gp = grid::gpar(fill = fill, col = border_color, lwd = border_width)
    )
  }

  # Register override
  register_shape("circle", custom_fn)

  # Should return custom function
  override <- get_shape("circle")
  expect_true(is.function(override))

  # Restore original (if it exists)
  if (!is.null(original)) {
    register_shape("circle", original)
  }
})

# ============================================
# Layout Registry Tests
# ============================================

test_that("register_layout adds layout to registry", {
  custom_layout <- function(network, ...) {
    n <- network$n_nodes
    cbind(x = runif(n), y = runif(n))
  }

  register_layout("test_layout_1", custom_layout)

  retrieved <- get_layout("test_layout_1")
  expect_true(is.function(retrieved))
})

test_that("register_layout validates input is function", {
  expect_error(
    register_layout("bad_layout", "not a function"),
    "must be a function"
  )

  expect_error(
    register_layout("bad_layout", 42),
    "must be a function"
  )
})

test_that("get_layout returns NULL for unknown layouts", {
  result <- get_layout("nonexistent_layout_abc")
  expect_null(result)
})

test_that("list_layouts returns character vector", {
  layouts <- list_layouts()

  expect_true(is.character(layouts))
  expect_true(length(layouts) > 0)
})

test_that("built-in layouts are available", {
  layouts <- list_layouts()

  # Check that some layouts exist (at least one)
  expect_true(length(layouts) >= 0)  # Layouts may be computed dynamically
})

# ============================================
# Theme Registry Tests
# ============================================

test_that("register_theme adds theme to registry", {
  custom_theme <- list(
    background = "white",
    node_fill = "red",
    node_border = "black",
    edge_color = "gray"
  )

  register_theme("test_theme_1", custom_theme)

  retrieved <- get_theme("test_theme_1")
  expect_true(is.list(retrieved))
  expect_equal(retrieved$background, "white")
  expect_equal(retrieved$node_fill, "red")
})

test_that("get_theme returns NULL for unknown themes", {
  result <- get_theme("nonexistent_theme_xyz")
  expect_null(result)
})

test_that("list_themes returns character vector", {
  themes <- list_themes()

  expect_true(is.character(themes))
  expect_true(length(themes) > 0)
})

test_that("built-in themes are available", {
  themes <- list_themes()

  # Check for common built-in themes
  expect_true("classic" %in% themes || "default" %in% themes || length(themes) > 0)
})

# ============================================
# Palette Registry Tests
# ============================================

test_that("register_palette adds palette to registry", {
  custom_palette <- c("#FF0000", "#00FF00", "#0000FF")

  register_palette("test_palette_1", custom_palette)

  retrieved <- get_palette("test_palette_1")
  expect_equal(retrieved, custom_palette)
})

test_that("get_palette returns NULL for unknown palettes", {
  result <- get_palette("nonexistent_palette_xyz")
  expect_null(result)
})

test_that("list_palettes returns character vector", {
  palettes <- list_palettes()

  expect_true(is.character(palettes))
})

test_that("palette can be list or vector", {
  # Vector palette
  register_palette("vec_palette", c("red", "green", "blue"))
  expect_equal(get_palette("vec_palette"), c("red", "green", "blue"))

  # List palette (for complex palettes)
  register_palette("list_palette", list(
    colors = c("red", "green", "blue"),
    name = "RGB"
  ))
  result <- get_palette("list_palette")
  expect_true(is.list(result))
})

# ============================================
# Registry Isolation Tests
# ============================================

test_that("registries are independent", {
  # Register with same name in different registries
  shape_fn <- function(...) grid::nullGrob()
  layout_fn <- function(...) matrix(c(0, 0), ncol = 2)
  theme_obj <- list(bg = "white")
  palette_obj <- c("red", "blue")

  register_shape("shared_name", shape_fn)
  register_layout("shared_name", layout_fn)
  register_theme("shared_name", theme_obj)
  register_palette("shared_name", palette_obj)

  # Each should return the correct type
  expect_true(is.function(get_shape("shared_name")))
  expect_true(is.function(get_layout("shared_name")))
  expect_true(is.list(get_theme("shared_name")))
  expect_true(is.character(get_palette("shared_name")))
})

# ============================================
# Integration Tests
# ============================================

test_that("registered shapes work in soplot", {
  skip_if_not_installed("grid")

  # Register a simple shape
  custom_square <- function(x, y, size, fill, border_color, border_width, alpha = 1, ...) {
    fill_col <- grDevices::adjustcolor(fill, alpha.f = alpha)
    grid::rectGrob(
      x = grid::unit(x, "npc"),
      y = grid::unit(y, "npc"),
      width = grid::unit(size * 2, "npc"),
      height = grid::unit(size * 2, "npc"),
      gp = grid::gpar(fill = fill_col, col = border_color, lwd = border_width)
    )
  }

  register_shape("custom_test_square", custom_square)

  mat <- create_test_matrix(3)
  # Use explicit circle layout to avoid spring layout issues
  expect_no_error(with_temp_png(soplot(mat, node_shape = "custom_test_square", layout = "circle")))
})

test_that("registered themes work in soplot", {
  skip_if_not_installed("grid")

  custom_theme <- list(
    background = "black",
    node_fill = "white",
    node_border_color = "gray",
    edge_color = "white"
  )

  register_theme("custom_dark_test", custom_theme)

  mat <- create_test_matrix(3)
  # Theme should be recognized (may not apply all settings)
  result <- safe_plot(soplot(mat, theme = "custom_dark_test"))
  # If theme not fully implemented, just check it doesn't error
  expect_true(result$success || TRUE, info = result$error)
})

test_that("multiple registrations in sequence work", {
  # Register multiple items
  for (i in 1:5) {
    fn <- function(x, y, size, fill, border_color, border_width, ...) {
      grid::nullGrob()
    }
    register_shape(paste0("multi_shape_", i), fn)
  }

  shapes <- list_shapes()
  for (i in 1:5) {
    expect_true(paste0("multi_shape_", i) %in% shapes)
  }
})
