# test-class-theme.R - Tests for CographTheme R6 class
# Covers: R/class-theme.R

# ============================================
# Constructor Tests
# ============================================

test_that("CographTheme can be created with defaults", {
  theme <- CographTheme$new()
  expect_true(inherits(theme, "CographTheme"))
  expect_equal(theme$name, "custom")
})

test_that("CographTheme can be created with custom name", {
  theme <- CographTheme$new(name = "my_theme")
  expect_equal(theme$name, "my_theme")
})

test_that("CographTheme can be created with custom background", {
  theme <- CographTheme$new(background = "darkgray")
  expect_equal(theme$get("background"), "darkgray")
})

test_that("CographTheme can be created with all custom parameters", {
  theme <- CographTheme$new(
    name = "test",
    background = "black",
    node_fill = "red",
    node_border = "white",
    node_border_width = 2,
    edge_color = "green",
    edge_positive_color = "blue",
    edge_negative_color = "yellow",
    edge_width = 3,
    label_color = "orange",
    label_size = 12,
    title_color = "purple",
    title_size = 16,
    legend_background = "pink"
  )

  expect_equal(theme$name, "test")
  expect_equal(theme$get("background"), "black")
  expect_equal(theme$get("node_fill"), "red")
  expect_equal(theme$get("node_border"), "white")
  expect_equal(theme$get("node_border_width"), 2)
  expect_equal(theme$get("edge_color"), "green")
  expect_equal(theme$get("edge_positive_color"), "blue")
  expect_equal(theme$get("edge_negative_color"), "yellow")
  expect_equal(theme$get("edge_width"), 3)
  expect_equal(theme$get("label_color"), "orange")
  expect_equal(theme$get("label_size"), 12)
  expect_equal(theme$get("title_color"), "purple")
  expect_equal(theme$get("title_size"), 16)
  expect_equal(theme$get("legend_background"), "pink")
})

# ============================================
# get() Method Tests
# ============================================

test_that("get() returns correct values", {
  theme <- CographTheme$new(node_fill = "steelblue")
  expect_equal(theme$get("node_fill"), "steelblue")
})

test_that("get() returns NULL for unknown parameters", {
  theme <- CographTheme$new()
  expect_null(theme$get("nonexistent_param"))
})

test_that("get() returns all default values", {
  theme <- CographTheme$new()

  expect_equal(theme$get("background"), "white")
  expect_equal(theme$get("node_fill"), "#4A90D9")
  expect_equal(theme$get("node_border"), "#2C5AA0")
  expect_equal(theme$get("node_border_width"), 1)
  expect_equal(theme$get("edge_color"), "gray50")
})

# ============================================
# set() Method Tests
# ============================================

test_that("set() modifies parameter", {
  theme <- CographTheme$new()
  theme$set("node_fill", "purple")

  expect_equal(theme$get("node_fill"), "purple")
})

test_that("set() returns invisible self for chaining", {
  theme <- CographTheme$new()
  result <- theme$set("node_fill", "red")

  expect_identical(result, theme)
})

test_that("set() allows chaining", {
  theme <- CographTheme$new()

  theme$set("node_fill", "red")$set("edge_color", "blue")

  expect_equal(theme$get("node_fill"), "red")
  expect_equal(theme$get("edge_color"), "blue")
})

test_that("set() can add new parameters", {
  theme <- CographTheme$new()
  theme$set("custom_param", "custom_value")

  expect_equal(theme$get("custom_param"), "custom_value")
})

# ============================================
# get_all() Method Tests
# ============================================

test_that("get_all() returns list", {
  theme <- CographTheme$new()
  params <- theme$get_all()

  expect_true(is.list(params))
})

test_that("get_all() contains all default parameters", {
  theme <- CographTheme$new()
  params <- theme$get_all()

  expect_true("background" %in% names(params))
  expect_true("node_fill" %in% names(params))
  expect_true("node_border" %in% names(params))
  expect_true("edge_color" %in% names(params))
  expect_true("label_color" %in% names(params))
})

test_that("get_all() reflects set() changes", {
  theme <- CographTheme$new()
  theme$set("node_fill", "custom_color")

  params <- theme$get_all()
  expect_equal(params$node_fill, "custom_color")
})

# ============================================
# merge() Method Tests
# ============================================

test_that("merge() with another theme", {
  theme1 <- CographTheme$new(node_fill = "red")
  theme2 <- CographTheme$new(node_fill = "green", edge_color = "blue")

  merged <- theme1$merge(theme2)

  expect_true(inherits(merged, "CographTheme"))
  expect_equal(merged$get("node_fill"), "green")  # Overridden by theme2
  expect_equal(merged$get("edge_color"), "blue")  # From theme2
})

test_that("merge() with list of parameters", {
  theme <- CographTheme$new(node_fill = "red")
  merged <- theme$merge(list(node_fill = "blue", edge_color = "green"))

  expect_equal(merged$get("node_fill"), "blue")
  expect_equal(merged$get("edge_color"), "green")
})

test_that("merge() returns new theme instance", {
  theme1 <- CographTheme$new(node_fill = "red")
  theme2 <- CographTheme$new(node_fill = "blue")

  merged <- theme1$merge(theme2)

  # Original themes should be unchanged
  expect_equal(theme1$get("node_fill"), "red")
  expect_equal(theme2$get("node_fill"), "blue")

  # Merged should be different
  expect_false(identical(merged, theme1))
  expect_false(identical(merged, theme2))
})

test_that("merge() sets name to 'merged'", {
  theme1 <- CographTheme$new(name = "first")
  theme2 <- CographTheme$new(name = "second")

  merged <- theme1$merge(theme2)
  expect_equal(merged$name, "merged")
})

# ============================================
# clone_theme() Method Tests
# ============================================

test_that("clone_theme() creates independent copy", {
  theme <- CographTheme$new(node_fill = "red")
  cloned <- theme$clone_theme()

  expect_true(inherits(cloned, "CographTheme"))
  expect_equal(cloned$get("node_fill"), "red")

  # Modifying clone shouldn't affect original
  cloned$set("node_fill", "blue")
  expect_equal(theme$get("node_fill"), "red")
  expect_equal(cloned$get("node_fill"), "blue")
})

test_that("clone_theme() preserves name", {
  theme <- CographTheme$new(name = "original")
  cloned <- theme$clone_theme()

  expect_equal(cloned$name, "original")
})

test_that("clone_theme() preserves all parameters", {
  theme <- CographTheme$new(
    node_fill = "red",
    edge_color = "blue",
    background = "gray"
  )
  cloned <- theme$clone_theme()

  original_params <- theme$get_all()
  cloned_params <- cloned$get_all()

  expect_equal(original_params, cloned_params)
})

# ============================================
# print() Method Tests
# ============================================

test_that("print() shows theme name", {
  theme <- CographTheme$new(name = "my_theme")
  output <- capture.output(theme$print())

  expect_true(any(grepl("CographTheme: my_theme", output)))
})

test_that("print() shows background", {
  theme <- CographTheme$new(background = "lightblue")
  output <- capture.output(theme$print())

  expect_true(any(grepl("Background: lightblue", output)))
})

test_that("print() shows node fill", {
  theme <- CographTheme$new(node_fill = "steelblue")
  output <- capture.output(theme$print())

  expect_true(any(grepl("Node fill: steelblue", output)))
})

test_that("print() returns invisible self", {
  theme <- CographTheme$new()
  result <- capture.output(print_result <- theme$print())

  expect_identical(print_result, theme)
})

# ============================================
# name Active Binding Tests
# ============================================

test_that("name active binding returns theme name", {
  theme <- CographTheme$new(name = "test_theme")
  expect_equal(theme$name, "test_theme")
})

test_that("name is read-only", {
  theme <- CographTheme$new(name = "original")

  # Attempting to set should give an error (active binding is read-only)
  expect_error(theme$name <- "new_name")
})

# ============================================
# is_cograph_theme() Function Tests
# ============================================

test_that("is_cograph_theme() returns TRUE for CographTheme", {
  theme <- CographTheme$new()
  expect_true(cograph:::is_cograph_theme(theme))
})

test_that("is_cograph_theme() returns FALSE for other objects", {
  expect_false(cograph:::is_cograph_theme(list(a = 1)))
  expect_false(cograph:::is_cograph_theme("string"))
  expect_false(cograph:::is_cograph_theme(NULL))
  expect_false(cograph:::is_cograph_theme(123))
})

# ============================================
# Integration Tests
# ============================================

test_that("CographTheme works with sn_theme", {
  mat <- create_test_matrix(3)
  net <- cograph(mat)

  custom_theme <- CographTheme$new(
    name = "custom",
    node_fill = "purple",
    edge_color = "orange"
  )

  result <- net |> sn_theme(custom_theme)

  theme <- result$network$get_theme()
  expect_equal(theme$get("node_fill"), "purple")
})

test_that("CographTheme works in soplot", {
  mat <- create_test_matrix(3)

  custom_theme <- CographTheme$new(
    node_fill = "darkgreen",
    edge_color = "brown"
  )

  result <- tryCatch({
    with_temp_png(soplot(mat, theme = custom_theme, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("built-in themes are CographTheme instances", {
  themes <- list_themes()

  for (theme_name in themes) {
    theme <- get_theme(theme_name)
    expect_true(inherits(theme, "CographTheme"),
                info = paste("Theme", theme_name, "is not CographTheme"))
  }
})
