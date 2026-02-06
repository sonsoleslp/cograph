# Tests for validation utilities
# Covers: R/utils-validation.R

# ============================================
# validate_network Tests
# ============================================

test_that("validate_network accepts CographNetwork objects", {
  mat <- create_test_matrix(4)
  net <- cograph(mat)

  result <- cograph:::validate_network(net)
  expect_true(inherits(result, "CographNetwork"))
})

test_that("validate_network rejects non-network objects", {
  expect_error(
    cograph:::validate_network("not a network"),
    "must be a CographNetwork"
  )

  expect_error(
    cograph:::validate_network(list(a = 1)),
    "must be a CographNetwork"
  )
})

# ============================================
# validate_color Tests
# ============================================

test_that("validate_color accepts valid colors", {
  expect_true(cograph:::validate_color("red"))
  expect_true(cograph:::validate_color("#FF0000"))
  expect_true(cograph:::validate_color("steelblue"))
})

test_that("validate_color accepts transparent", {
  expect_true(cograph:::validate_color("transparent"))
})

test_that("validate_color accepts NULL and NA", {
  expect_true(cograph:::validate_color(NULL))
  expect_true(cograph:::validate_color(NA))
})

test_that("validate_color rejects invalid colors", {
  expect_error(
    cograph:::validate_color("not_a_real_color_xyz"),
    "not a valid color"
  )
})

# ============================================
# validate_range Tests
# ============================================

test_that("validate_range accepts values in range", {
  expect_true(cograph:::validate_range(5, min = 0, max = 10))
  expect_true(cograph:::validate_range(0, min = 0, max = 10))
  expect_true(cograph:::validate_range(10, min = 0, max = 10))
})

test_that("validate_range accepts vector of values", {
  expect_true(cograph:::validate_range(c(1, 5, 9), min = 0, max = 10))
})

test_that("validate_range rejects values below min", {
  expect_error(
    cograph:::validate_range(-1, min = 0, max = 10),
    "must be >="
  )
})

test_that("validate_range rejects values above max", {
  expect_error(
    cograph:::validate_range(15, min = 0, max = 10),
    "must be <="
  )
})

test_that("validate_range rejects non-numeric", {
  expect_error(
    cograph:::validate_range("five", min = 0, max = 10),
    "must be numeric"
  )
})

# ============================================
# validate_choice Tests
# ============================================

test_that("validate_choice accepts valid choices", {
  expect_true(cograph:::validate_choice("circle", c("circle", "square", "triangle")))
  expect_true(cograph:::validate_choice("square", c("circle", "square", "triangle")))
})

test_that("validate_choice rejects invalid choices", {
  expect_error(
    cograph:::validate_choice("hexagon", c("circle", "square", "triangle")),
    "must be one of"
  )
})

# ============================================
# validate_length Tests
# ============================================

test_that("validate_length accepts correct length", {
  expect_true(cograph:::validate_length(c(1, 2, 3), 3))
  expect_true(cograph:::validate_length(1:5, 5))
})

test_that("validate_length accepts single value when allow_single is TRUE", {
  expect_true(cograph:::validate_length(1, 5, allow_single = TRUE))
})

test_that("validate_length rejects wrong length", {
  expect_error(
    cograph:::validate_length(c(1, 2, 3), 5, allow_single = FALSE),
    "must have length"
  )
})

# ============================================
# recycle_to_length Tests
# ============================================

test_that("recycle_to_length returns unchanged if correct length", {
  result <- cograph:::recycle_to_length(c(1, 2, 3, 4), 4)
  expect_equal(result, c(1, 2, 3, 4))
})

test_that("recycle_to_length expands single value", {
  result <- cograph:::recycle_to_length(5, 4)
  expect_equal(result, c(5, 5, 5, 5))
})

test_that("recycle_to_length recycles vector", {
  result <- cograph:::recycle_to_length(c(1, 2), 4)
  expect_equal(result, c(1, 2, 1, 2))
})

test_that("recycle_to_length handles partial recycling", {
  result <- cograph:::recycle_to_length(c(1, 2), 5)
  expect_equal(length(result), 5)
})

# ============================================
# expand_param Tests
# ============================================

test_that("expand_param returns unchanged if correct length", {
  result <- cograph:::expand_param(c(1, 2, 3), 3)
  expect_equal(result, c(1, 2, 3))
})

test_that("expand_param expands single value", {
  result <- cograph:::expand_param(5, 4)
  expect_equal(result, c(5, 5, 5, 5))
})

test_that("expand_param errors on wrong length", {
  expect_error(
    cograph:::expand_param(c(1, 2, 3), 5),
    "must be length"
  )
})

# ============================================
# resolve_aesthetic Tests
# ============================================

test_that("resolve_aesthetic returns default when value is NULL", {
  result <- cograph:::resolve_aesthetic(NULL, default = "red")
  expect_equal(result, "red")
})

test_that("resolve_aesthetic returns NULL when both value and default are NULL", {
  result <- cograph:::resolve_aesthetic(NULL)
  expect_null(result)
})

test_that("resolve_aesthetic looks up column names", {
  data <- data.frame(color = c("red", "blue", "green"))

  result <- cograph:::resolve_aesthetic("color", data = data)
  expect_equal(result, c("red", "blue", "green"))
})

test_that("resolve_aesthetic returns literal value if not column name", {
  data <- data.frame(x = 1:3)

  result <- cograph:::resolve_aesthetic("steelblue", data = data)
  expect_equal(result, "steelblue")
})

test_that("resolve_aesthetic recycles to length", {
  result <- cograph:::resolve_aesthetic("red", n = 4)
  expect_equal(result, rep("red", 4))
})

test_that("resolve_aesthetic handles vector values", {
  result <- cograph:::resolve_aesthetic(c(1, 2, 3), n = 3)
  expect_equal(result, c(1, 2, 3))
})
