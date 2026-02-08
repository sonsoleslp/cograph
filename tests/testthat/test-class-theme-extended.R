# Extended tests for CographTheme R6 class
# Covers: R/class-theme.R (merge, clone_theme, print, get_all, set, name, is_cograph_theme)

# ============================================
# Theme Constructor Tests
# ============================================

test_that("CographTheme creates with all defaults", {
  theme <- CographTheme$new()

  expect_true(inherits(theme, "CographTheme"))
  expect_equal(theme$name, "custom")
  expect_equal(theme$get("background"), "white")
  expect_equal(theme$get("node_fill"), "#4A90D9")
  expect_equal(theme$get("node_border"), "#2C5AA0")
  expect_equal(theme$get("edge_color"), "gray50")
})

test_that("CographTheme creates with custom parameters", {
  theme <- CographTheme$new(
    name = "test",
    background = "black",
    node_fill = "red",
    node_border = "darkred",
    edge_color = "white"
  )

  expect_equal(theme$name, "test")
  expect_equal(theme$get("background"), "black")
  expect_equal(theme$get("node_fill"), "red")
  expect_equal(theme$get("node_border"), "darkred")
  expect_equal(theme$get("edge_color"), "white")
})

# ============================================
# Get/Set Tests
# ============================================

test_that("get returns correct values for all parameters", {
  theme <- CographTheme$new(
    edge_positive_color = "#00FF00",
    edge_negative_color = "#FF0000",
    edge_width = 2,
    label_color = "navy",
    label_size = 12,
    title_color = "blue",
    title_size = 16,
    legend_background = "gray90"
  )

  expect_equal(theme$get("edge_positive_color"), "#00FF00")
  expect_equal(theme$get("edge_negative_color"), "#FF0000")
  expect_equal(theme$get("edge_width"), 2)
  expect_equal(theme$get("label_color"), "navy")
  expect_equal(theme$get("label_size"), 12)
  expect_equal(theme$get("title_color"), "blue")
  expect_equal(theme$get("title_size"), 16)
  expect_equal(theme$get("legend_background"), "gray90")
})

test_that("get returns NULL for nonexistent parameter", {
  theme <- CographTheme$new()

  expect_null(theme$get("nonexistent_param"))
})

test_that("set updates parameter value", {
  theme <- CographTheme$new()

  theme$set("background", "navy")
  expect_equal(theme$get("background"), "navy")
})

test_that("set returns self for chaining", {
  theme <- CographTheme$new()

  result <- theme$set("background", "red")
  expect_true(inherits(result, "CographTheme"))
})

test_that("set can add new parameters", {
  theme <- CographTheme$new()

  theme$set("custom_param", "custom_value")
  expect_equal(theme$get("custom_param"), "custom_value")
})

# ============================================
# get_all Tests
# ============================================

test_that("get_all returns list with all parameters", {
  theme <- CographTheme$new()

  params <- theme$get_all()
  expect_true(is.list(params))
  expect_true("background" %in% names(params))
  expect_true("node_fill" %in% names(params))
  expect_true("node_border" %in% names(params))
  expect_true("edge_color" %in% names(params))
  expect_true("edge_positive_color" %in% names(params))
  expect_true("edge_negative_color" %in% names(params))
  expect_true("label_color" %in% names(params))
  expect_true("legend_background" %in% names(params))
})

# ============================================
# Merge Tests
# ============================================

test_that("merge with CographTheme creates merged theme", {
  theme1 <- CographTheme$new(background = "white", node_fill = "blue")
  theme2 <- CographTheme$new(background = "black", edge_color = "red")

  merged <- theme1$merge(theme2)

  expect_true(inherits(merged, "CographTheme"))
  expect_equal(merged$name, "merged")
  expect_equal(merged$get("background"), "black")  # overwritten
  expect_equal(merged$get("edge_color"), "red")  # from theme2
})

test_that("merge with list creates merged theme", {
  theme <- CographTheme$new(background = "white")

  merged <- theme$merge(list(background = "gray", node_fill = "green"))

  expect_equal(merged$get("background"), "gray")
  expect_equal(merged$get("node_fill"), "green")
})

test_that("merge does not modify original theme", {
  theme <- CographTheme$new(background = "white")

  theme$merge(list(background = "black"))

  expect_equal(theme$get("background"), "white")  # unchanged
})

# ============================================
# clone_theme Tests
# ============================================

test_that("clone_theme creates independent copy", {
  theme <- CographTheme$new(name = "original", background = "white")

  cloned <- theme$clone_theme()

  expect_equal(cloned$name, "original")
  expect_equal(cloned$get("background"), "white")
})

test_that("clone_theme is independent of original", {
  theme <- CographTheme$new(name = "original", background = "white")

  cloned <- theme$clone_theme()
  cloned$set("background", "black")

  expect_equal(theme$get("background"), "white")  # original unchanged
  expect_equal(cloned$get("background"), "black")
})

# ============================================
# Print Tests
# ============================================

test_that("print produces expected output", {
  theme <- CographTheme$new(name = "test_theme")

  expect_output(print(theme), "CographTheme: test_theme")
  expect_output(print(theme), "Background:")
  expect_output(print(theme), "Node fill:")
  expect_output(print(theme), "Node border:")
  expect_output(print(theme), "Edge color:")
  expect_output(print(theme), "Edge positive:")
  expect_output(print(theme), "Edge negative:")
})

# ============================================
# Active Binding Tests
# ============================================

test_that("name active binding returns theme name", {
  theme <- CographTheme$new(name = "my_theme")

  expect_equal(theme$name, "my_theme")
})

# ============================================
# is_cograph_theme Tests
# ============================================

test_that("is_cograph_theme returns TRUE for CographTheme", {
  theme <- CographTheme$new()

  expect_true(cograph:::is_cograph_theme(theme))
})

test_that("is_cograph_theme returns FALSE for non-theme", {
  expect_false(cograph:::is_cograph_theme(list(a = 1)))
  expect_false(cograph:::is_cograph_theme("classic"))
  expect_false(cograph:::is_cograph_theme(NULL))
})

# ============================================
# Node Border Width Tests
# ============================================

test_that("node_border_width parameter works", {
  theme <- CographTheme$new(node_border_width = 3)

  expect_equal(theme$get("node_border_width"), 3)
})
