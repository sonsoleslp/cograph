# Tests for aesthetic scale functions
# Covers: R/aes-scales.R

test_that("scale_size works with linear transformation", {
  values <- c(1, 2, 3, 4, 5)
  result <- cograph:::scale_size(values, range = c(0.03, 0.1), trans = "linear")

  expect_equal(length(result), 5)
  expect_true(all(result >= 0.03))
  expect_true(all(result <= 0.1))
  expect_equal(result[1], 0.03)  # Min value maps to min range
  expect_equal(result[5], 0.1)   # Max value maps to max range
})

test_that("scale_size works with sqrt transformation", {
  values <- c(1, 4, 9, 16, 25)
  result <- cograph:::scale_size(values, range = c(0.01, 0.1), trans = "sqrt")

  expect_equal(length(result), 5)
  expect_true(all(result >= 0.01))
  expect_true(all(result <= 0.1))
})

test_that("scale_size works with log transformation", {
  values <- c(1, 10, 100, 1000)
  result <- cograph:::scale_size(values, range = c(0.02, 0.08), trans = "log")

  expect_equal(length(result), 4)
  expect_true(all(result >= 0.02))
  expect_true(all(result <= 0.08))
})

test_that("scale_size handles all NA values", {
  values <- c(NA, NA, NA)
  result <- cograph:::scale_size(values, range = c(0.03, 0.1))

  expect_equal(length(result), 3)
  expect_true(all(result == mean(c(0.03, 0.1))))
})

test_that("scale_size handles constant values", {
  values <- c(5, 5, 5, 5)
  result <- cograph:::scale_size(values, range = c(0.03, 0.1))

  expect_equal(length(result), 4)
  expect_true(all(result == mean(c(0.03, 0.1))))
})

test_that("scale_size handles negative values with sqrt", {
  values <- c(-1, 0, 1, 4)
  result <- cograph:::scale_size(values, range = c(0.01, 0.1), trans = "sqrt")

  expect_equal(length(result), 4)
  expect_true(all(is.finite(result)))
})

test_that("scale_color works with palette name", {
  values <- c(0, 0.5, 1)
  result <- cograph:::scale_color(values, palette = "viridis")

  expect_equal(length(result), 3)
  expect_true(all(grepl("^#", result)))
})

test_that("scale_color works with single color", {
  values <- c(0, 0.5, 1)
  result <- cograph:::scale_color(values, palette = "red")

  expect_equal(length(result), 3)
  expect_true(all(result == "red"))
})

test_that("scale_color works with color vector", {
  values <- c(0, 0.5, 1)
  result <- cograph:::scale_color(values, palette = c("red", "yellow", "green"))

  expect_equal(length(result), 3)
  expect_true(all(grepl("^#", result)))
})

test_that("scale_color works with palette function", {
  values <- c(0, 0.5, 1)
  pal_fn <- grDevices::colorRampPalette(c("blue", "red"))
  result <- cograph:::scale_color(values, palette = pal_fn)

  expect_equal(length(result), 3)
})

test_that("scale_color handles all NA values", {
  values <- c(NA, NA, NA)
  result <- cograph:::scale_color(values, palette = "viridis")

  expect_equal(length(result), 3)
  expect_true(all(result == "gray50"))
})

test_that("scale_color respects limits", {
  values <- c(0, 50, 100)
  result <- cograph:::scale_color(values, palette = c("white", "black"), limits = c(0, 100))

  expect_equal(length(result), 3)
})

test_that("scale_color_discrete maps categorical values", {
  values <- c("A", "B", "A", "C", "B")
  result <- cograph:::scale_color_discrete(values, palette = "colorblind")

  expect_equal(length(result), 5)
  # Same category should get same color
  expect_equal(result[1], result[3])  # Both "A"
  expect_equal(result[2], result[5])  # Both "B"
})

test_that("scale_color_discrete works with single color palette", {
  values <- c("X", "Y", "Z")
  result <- cograph:::scale_color_discrete(values, palette = "steelblue")

  expect_equal(length(result), 3)
  expect_true(all(result == "steelblue"))
})

test_that("scale_color_discrete works with color vector", {
  values <- c("A", "B", "C")
  result <- cograph:::scale_color_discrete(values, palette = c("red", "green", "blue"))

  expect_equal(length(result), 3)
})

test_that("scale_color_discrete works with palette function", {
  values <- c("A", "B", "C")
  pal_fn <- function(n) rainbow(n)
  result <- cograph:::scale_color_discrete(values, palette = pal_fn)

  expect_equal(length(result), 3)
})

test_that("scale_width maps to range", {
  values <- c(1, 5, 10)
  result <- cograph:::scale_width(values, range = c(0.5, 3))

  expect_equal(length(result), 3)
  expect_equal(result[1], 0.5)
  expect_equal(result[3], 3)
})

test_that("scale_alpha maps to range and clamps", {
  values <- c(0, 0.5, 1)
  result <- cograph:::scale_alpha(values, range = c(0.3, 1))

  expect_equal(length(result), 3)
  expect_true(all(result >= 0))
  expect_true(all(result <= 1))
})

test_that("scale_alpha clamps values outside 0-1", {
  values <- c(-10, 0, 0.5, 1, 10)
  result <- cograph:::scale_alpha(values, range = c(0.2, 0.8))

  expect_true(all(result >= 0))
  expect_true(all(result <= 1))
})

# ============================================
# map_node_colors Tests
# ============================================

test_that("map_node_colors maps groups to default palette", {
  groups <- c("A", "B", "A", "C")
  result <- cograph:::map_node_colors(groups)

  expect_equal(length(result), 4)
  expect_equal(result[1], result[3])  # Same group = same color
})

test_that("map_node_colors uses function palette", {
  groups <- c("A", "B", "C")
  pal_fn <- function(n) rep("red", n)
  result <- cograph:::map_node_colors(groups, palette = pal_fn)

  expect_true(all(result == "red"))
})

test_that("map_node_colors recycles vector palette", {
  groups <- c("A", "B", "C", "D")
  result <- cograph:::map_node_colors(groups, palette = c("red", "blue"))

  expect_equal(length(result), 4)
})

# ============================================
# scale_node_sizes Tests
# ============================================

test_that("scale_node_sizes handles all NA values", {
  values <- c(NA, NA, NA)
  result <- cograph:::scale_node_sizes(values, range = c(0.03, 0.1))

  expect_equal(length(result), 3)
  expect_true(all(result == mean(c(0.03, 0.1))))
})

test_that("scale_node_sizes handles constant values", {
  values <- c(5, 5, 5)
  result <- cograph:::scale_node_sizes(values, range = c(0.03, 0.1))

  expect_equal(length(result), 3)
  expect_true(all(result == mean(c(0.03, 0.1))))
})

test_that("scale_node_sizes maps to range correctly", {
  values <- c(1, 5, 10)
  result <- cograph:::scale_node_sizes(values, range = c(0.03, 0.1))

  expect_equal(result[1], 0.03)
  expect_equal(result[3], 0.1)
})
