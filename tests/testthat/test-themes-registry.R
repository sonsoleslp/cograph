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

# ============================================
# Theme Properties Tests
# ============================================

test_that("classic theme has expected properties", {
  theme <- get_theme("classic")

  expect_true(!is.null(theme))
  expect_true(inherits(theme, "CographTheme"))
})

test_that("colorblind theme has expected properties", {
  theme <- get_theme("colorblind")

  expect_true(!is.null(theme))
  expect_true(inherits(theme, "CographTheme"))
})

test_that("dark theme has expected properties", {
  theme <- get_theme("dark")

  expect_true(!is.null(theme))
  expect_true(inherits(theme, "CographTheme"))
})

test_that("gray and grey are identical themes", {
  gray_theme <- get_theme("gray")
  grey_theme <- get_theme("grey")

  expect_true(!is.null(gray_theme))
  expect_true(!is.null(grey_theme))
  # They should be the same theme object
  expect_identical(gray_theme, grey_theme)
})

test_that("minimal theme has expected properties", {
  theme <- get_theme("minimal")

  expect_true(!is.null(theme))
  expect_true(inherits(theme, "CographTheme"))
})

test_that("viridis theme has expected properties", {
  theme <- get_theme("viridis")

  expect_true(!is.null(theme))
  expect_true(inherits(theme, "CographTheme"))
})

test_that("nature theme has expected properties", {
  theme <- get_theme("nature")

  expect_true(!is.null(theme))
  expect_true(inherits(theme, "CographTheme"))
})

# ============================================
# list_themes Tests
# ============================================

test_that("list_themes returns character vector", {
  themes <- list_themes()

  expect_true(is.character(themes))
  expect_true(length(themes) > 0)
})

test_that("list_themes contains expected minimum themes", {
  themes <- list_themes()

  expected <- c("classic", "colorblind", "gray", "grey",
                "dark", "minimal", "viridis", "nature")

  for (t in expected) {
    expect_true(t %in% themes,
                info = paste("Expected theme", t, "not found"))
  }
})

# ============================================
# Theme Application in splot Tests
# ============================================

test_that("all registered themes work in splot", {
  mat <- create_test_matrix(4)
  # Test only known built-in themes (not test registrations from other tests)
  builtin_themes <- c("classic", "colorblind", "dark", "gray", "minimal",
                      "viridis", "nature", "seaborn", "high_contrast")

  for (theme_name in builtin_themes) {
    theme <- get_theme(theme_name)
    if (is.null(theme)) next  # Skip if theme not registered

    result <- tryCatch({
      with_temp_png(splot(mat, theme = theme_name))
      TRUE
    }, error = function(e) FALSE)

    expect_true(result, info = paste("Theme", theme_name, "failed in splot"))
  }
})

# ============================================
# Theme with sn_theme Tests
# ============================================

test_that("sn_theme applies theme to cograph network", {
  mat <- create_test_matrix(4)
  net <- cograph(mat)

  result <- sn_theme(net, "dark")

  expect_cograph_network(result)
})

test_that("sn_theme with all registered themes", {
  mat <- create_test_matrix(4)
  themes <- list_themes()

  for (theme_name in themes) {
    net <- cograph(mat)
    result <- tryCatch({
      sn_theme(net, theme_name)
      TRUE
    }, error = function(e) FALSE)

    expect_true(result, info = paste("sn_theme failed for", theme_name))
  }
})

# ============================================
# Theme in Pipe Chain Tests
# ============================================

test_that("theme works in pipe chain with splot", {
  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_png({
      cograph(mat) |>
        sn_theme("colorblind") |>
        splot()
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("theme works in pipe chain with soplot", {
  skip_if_not_installed("grid")

  mat <- create_test_matrix(4)

  result <- tryCatch({
    with_temp_png({
      cograph(mat) |>
        sn_theme("nature") |>
        soplot()
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# Direct Registration Function Tests
# ============================================

test_that("register_builtin_themes registers all themes", {
  # Call the registration function directly to ensure it's covered
  cograph:::register_builtin_themes()

  # Check all expected themes are registered
  themes <- list_themes()

  expect_true("classic" %in% themes)
  expect_true("colorblind" %in% themes)
  expect_true("gray" %in% themes)
  expect_true("grey" %in% themes)  # Alias for gray

  expect_true("dark" %in% themes)
  expect_true("minimal" %in% themes)
  expect_true("viridis" %in% themes)
  expect_true("nature" %in% themes)
})

test_that("register_builtin_themes creates valid CographTheme objects", {
  cograph:::register_builtin_themes()

  # All registered themes should be CographTheme objects
  for (theme_name in c("classic", "colorblind", "gray", "dark", "minimal", "viridis", "nature")) {
    theme <- get_theme(theme_name)
    expect_true(inherits(theme, "CographTheme"),
                info = paste("Theme", theme_name, "is not a CographTheme"))
  }
})
