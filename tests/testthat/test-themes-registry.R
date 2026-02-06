# Tests for themes registry functions
# Covers: R/themes-registry.R

# ============================================
# Built-in Themes Registration Tests
# ============================================

test_that("classic theme is registered", {
  expect_true("classic" %in% list_themes())
})

test_that("colorblind theme is registered", {
  expect_true("colorblind" %in% list_themes())
})

test_that("gray theme is registered with alias", {
  themes <- list_themes()
  expect_true("gray" %in% themes)
  expect_true("grey" %in% themes)
})

test_that("dark theme is registered", {
  expect_true("dark" %in% list_themes())
})

test_that("minimal theme is registered", {
  expect_true("minimal" %in% list_themes())
})

test_that("viridis theme is registered", {
  expect_true("viridis" %in% list_themes())
})

test_that("nature theme is registered", {
  expect_true("nature" %in% list_themes())
})

# ============================================
# Theme Application Tests
# ============================================

test_that("classic theme works in soplot", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_png(soplot(mat, theme = "classic", layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("colorblind theme works in soplot", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_png(soplot(mat, theme = "colorblind", layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("dark theme works in soplot", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_png(soplot(mat, theme = "dark", layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("gray theme works in soplot", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_png(soplot(mat, theme = "gray", layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("grey alias works same as gray", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_png(soplot(mat, theme = "grey", layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("minimal theme works in soplot", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_png(soplot(mat, theme = "minimal", layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("viridis theme works in soplot", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_png(soplot(mat, theme = "viridis", layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("nature theme works in soplot", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_png(soplot(mat, theme = "nature", layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Theme Registry Functions Tests
# ============================================

test_that("get_theme returns theme for valid name", {
  theme <- get_theme("classic")
  expect_true(!is.null(theme))
})

test_that("get_theme returns NULL for invalid theme", {
  result <- get_theme("nonexistent_theme_xyz")
  expect_null(result)
})

test_that("register_theme works", {
  # Create a dummy theme
  dummy_theme <- CographTheme$new(
    node_fill = "purple",
    node_border = "black"
  )

  # Register
  register_theme("test_dummy_theme", dummy_theme)
  expect_true("test_dummy_theme" %in% list_themes())

  # Get it back
  retrieved <- get_theme("test_dummy_theme")
  expect_true(!is.null(retrieved))
})
