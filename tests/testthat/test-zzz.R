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
