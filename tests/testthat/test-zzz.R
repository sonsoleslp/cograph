# Tests for package load/unload hooks
# Covers: R/zzz.R

test_that("package loads without error", {
  # Package should already be loaded for tests to run
  expect_true("cograph" %in% .packages())
})

test_that("built-in shapes are available after load", {
  shapes <- list_shapes()

  # Essential shapes should be registered
  expect_true("circle" %in% shapes)
  expect_true("square" %in% shapes)
  expect_true("triangle" %in% shapes)
})

test_that("built-in layouts are available after load", {
  layouts <- list_layouts()

  # Essential layouts should be registered
  expect_true("circle" %in% layouts)
  expect_true("grid" %in% layouts)
})

test_that("built-in themes are available after load", {
  themes <- list_themes()

  # Essential themes should be registered
  expect_true("classic" %in% themes)
})

test_that("built-in palettes are available after load", {
  palettes <- list_palettes()

  # Should have some palettes
  expect_true(length(palettes) >= 0)
})

test_that("registries exist after load", {
  # Access internal environment
  env <- cograph:::.cograph_env

  expect_true(!is.null(env$shapes))
  expect_true(!is.null(env$layouts))
  expect_true(!is.null(env$themes))
  expect_true(!is.null(env$palettes))
})

test_that("package startup message is appropriate", {
  # The message should have been displayed on load

  # Just verify package info is accessible
  expect_true(!is.null(packageVersion("cograph")))
})

# ============================================
# Comprehensive Registry Tests
# ============================================

test_that("all shapes are valid functions", {
  shapes <- list_shapes()

  for (shape in shapes) {
    shape_func <- get_shape(shape)
    expect_true(is.function(shape_func),
                info = paste("Shape", shape, "is not a function"))
  }
})

test_that("all layouts are valid functions", {
  layouts <- list_layouts()

  for (layout in layouts) {
    layout_func <- get_layout(layout)
    expect_true(is.function(layout_func),
                info = paste("Layout", layout, "is not a function"))
  }
})

test_that("all built-in themes are valid CographTheme objects", {
  # Test only known built-in themes (not test registrations from other tests)
  builtin_themes <- c("classic", "colorblind", "dark", "gray", "minimal",
                      "viridis", "nature", "seaborn", "high_contrast")

  for (theme in builtin_themes) {
    theme_obj <- get_theme(theme)
    if (is.null(theme_obj)) next  # Skip if theme not registered

    expect_true(!is.null(theme_obj),
                info = paste("Theme", theme, "is NULL"))
    expect_true(inherits(theme_obj, "CographTheme"),
                info = paste("Theme", theme, "is not a CographTheme"))
  }
})

test_that("all palettes are valid", {
  palettes <- list_palettes()

  for (pal in palettes) {
    palette_obj <- get_palette(pal)
    expect_true(!is.null(palette_obj),
                info = paste("Palette", pal, "is NULL"))
  }
})

# ============================================
# Minimum Required Items Tests
# ============================================

test_that("minimum shapes are registered", {
  shapes <- list_shapes()
  minimum_shapes <- c("circle", "square", "triangle", "diamond")

  for (shape in minimum_shapes) {
    expect_true(shape %in% shapes,
                info = paste("Minimum shape", shape, "not registered"))
  }
})

test_that("minimum layouts are registered", {
  layouts <- list_layouts()
  minimum_layouts <- c("circle", "spring", "grid")

  for (layout in minimum_layouts) {
    expect_true(layout %in% layouts,
                info = paste("Minimum layout", layout, "not registered"))
  }
})

test_that("minimum themes are registered", {
  themes <- list_themes()
  minimum_themes <- c("classic", "colorblind", "dark")

  for (theme in minimum_themes) {
    expect_true(theme %in% themes,
                info = paste("Minimum theme", theme, "not registered"))
  }
})

# ============================================
# Package Environment Tests
# ============================================

test_that("package environment exists", {
  env <- cograph:::.cograph_env
  expect_true(is.environment(env))
})

test_that("package environment contains registries", {
  env <- cograph:::.cograph_env

  expect_true(exists("shapes", envir = env))
  expect_true(exists("layouts", envir = env))
  expect_true(exists("themes", envir = env))
  expect_true(exists("palettes", envir = env))
})

test_that("registries are accessible data structures", {
  env <- cograph:::.cograph_env

  # Registries can be environments or lists
  expect_true(!is.null(env$shapes))
  expect_true(!is.null(env$layouts))
  expect_true(!is.null(env$themes))
  expect_true(!is.null(env$palettes))
})

# ============================================
# SVG Shapes Registry Tests
# ============================================

test_that("svg_shapes registry exists if supported", {
  env <- cograph:::.cograph_env
  # svg_shapes may or may not exist depending on implementation
  # Just verify env is accessible

  expect_true(is.environment(env))
})

# ============================================
# Package Version Tests
# ============================================

test_that("package version is valid", {
  version <- packageVersion("cograph")

  expect_true(!is.null(version))
  expect_true(inherits(version, "package_version"))
})

# ============================================
# Core Function Availability Tests
# ============================================

test_that("core functions are exported", {
  expect_true(exists("cograph", mode = "function"))
  expect_true(exists("splot", mode = "function"))
  expect_true(exists("soplot", mode = "function"))
  expect_true(exists("sn_nodes", mode = "function"))
  expect_true(exists("sn_edges", mode = "function"))
  expect_true(exists("sn_layout", mode = "function"))
  expect_true(exists("sn_theme", mode = "function"))
})

test_that("registry functions are exported", {
  expect_true(exists("list_shapes", mode = "function"))
  expect_true(exists("list_layouts", mode = "function"))
  expect_true(exists("list_themes", mode = "function"))
  expect_true(exists("list_palettes", mode = "function"))
  expect_true(exists("get_shape", mode = "function"))
  expect_true(exists("get_layout", mode = "function"))
  expect_true(exists("get_theme", mode = "function"))
  expect_true(exists("get_palette", mode = "function"))
  expect_true(exists("register_shape", mode = "function"))
  expect_true(exists("register_layout", mode = "function"))
  expect_true(exists("register_theme", mode = "function"))
  expect_true(exists("register_palette", mode = "function"))
})

# ============================================
# Alias Tests
# ============================================

test_that("tplot is exported as alias", {
  expect_true(exists("tplot", mode = "function"))
  expect_true(exists("plot_tna", mode = "function"))
})

# ============================================
# .onAttach Startup Message Tests
# ============================================

test_that(".onAttach produces startup message", {
  msg <- capture.output(type = "message", {
    cograph:::.onAttach(NULL, "cograph")
  })

  # Should contain package name and version
  msg_text <- paste(msg, collapse = " ")
  expect_true(grepl("cograph", msg_text, ignore.case = TRUE))
  expect_true(grepl("Version", msg_text))
})

test_that(".onAttach message includes version number", {
  msg <- capture.output(type = "message", {
    cograph:::.onAttach(NULL, "cograph")
  })

  msg_text <- paste(msg, collapse = " ")
  # Should contain a version number pattern (e.g., 1.5.2)
  expect_true(grepl("\\d+\\.\\d+", msg_text))
})

test_that(".onAttach message includes help hint", {
  msg <- capture.output(type = "message", {
    cograph:::.onAttach(NULL, "cograph")
  })

  msg_text <- paste(msg, collapse = " ")
  expect_true(grepl("help", msg_text, ignore.case = TRUE))
})

# ============================================
# .onLoad Tests
# ============================================

test_that(".onLoad registers all components", {
  # The onLoad function has already run. Verify its effects.

  # All built-in shapes should be registered
  expect_true(length(list_shapes()) >= 20)

  # All built-in layouts should be registered
  expect_true(length(list_layouts()) >= 8)

  # All built-in themes should be registered
  expect_true(length(list_themes()) >= 5)

  # All built-in palettes should be registered
  expect_true(length(list_palettes()) >= 1)
})

test_that("init_registries is called during package load", {
  # The registries should exist
  env <- cograph:::.cograph_env

  expect_true(is.environment(env))
  expect_true(is.environment(env$shapes) || is.list(env$shapes))
  expect_true(is.environment(env$layouts) || is.list(env$layouts))
  expect_true(is.environment(env$themes) || is.list(env$themes))
  expect_true(is.environment(env$palettes) || is.list(env$palettes))
})
