# Exhaustive tests for splot-labels.R internal functions
# Covers: R/splot-labels.R

# ============================================
# get_significance_stars Tests
# ============================================

test_that("get_significance_stars returns correct stars for p-values", {
  # Thresholds are < (not <=)
  expect_equal(get_significance_stars(0.0001), "***")  # < 0.001
  expect_equal(get_significance_stars(0.001), "**")    # 0.001 is not < 0.001, but < 0.01
  expect_equal(get_significance_stars(0.005), "**")    # < 0.01
  expect_equal(get_significance_stars(0.01), "*")      # 0.01 is not < 0.01, but < 0.05
  expect_equal(get_significance_stars(0.03), "*")      # < 0.05
  expect_equal(get_significance_stars(0.05), "")       # 0.05 is not < 0.05
  expect_equal(get_significance_stars(0.1), "")        # >= 0.05
  expect_equal(get_significance_stars(0.5), "")        # >= 0.05
})

test_that("get_significance_stars handles NULL input", {
  expect_null(get_significance_stars(NULL))
})

test_that("get_significance_stars handles NA values", {
  result <- get_significance_stars(c(0.005, NA, 0.0001))
  expect_equal(result[1], "**")   # 0.005 < 0.01
  expect_equal(result[2], "")     # NA
  expect_equal(result[3], "***")  # 0.0001 < 0.001
})

test_that("get_significance_stars handles vector input", {
  # Using values that clearly meet thresholds
  p_values <- c(0.0001, 0.005, 0.03, 0.1)
  result <- get_significance_stars(p_values)

  expect_equal(length(result), 4)
  expect_equal(result[1], "***")  # < 0.001
  expect_equal(result[2], "**")   # < 0.01
  expect_equal(result[3], "*")    # < 0.05
  expect_equal(result[4], "")     # >= 0.05
})

# ============================================
# format_pvalue Tests
# ============================================

test_that("format_pvalue formats correctly", {
  result <- format_pvalue(0.05, digits = 3, prefix = "p=")
  expect_true(grepl("p=", result))
  expect_true(grepl("0.050", result))
})

test_that("format_pvalue handles very small p-values", {
  result <- format_pvalue(0.0001, digits = 3, prefix = "p=")
  expect_true(grepl("<", result))
})

test_that("format_pvalue handles NULL input", {
  expect_equal(format_pvalue(NULL), "")
})

test_that("format_pvalue handles NA input", {
  expect_equal(format_pvalue(NA), "")
})

test_that("format_pvalue respects digits parameter", {
  result_2 <- format_pvalue(0.12345, digits = 2, prefix = "")
  result_4 <- format_pvalue(0.12345, digits = 4, prefix = "")

  expect_true(grepl("0.12", result_2))
  expect_true(grepl("0.1235", result_4))
})

test_that("format_pvalue respects prefix parameter", {
  result_default <- format_pvalue(0.05, prefix = "p=")
  result_custom <- format_pvalue(0.05, prefix = "P-value: ")

  expect_true(grepl("p=", result_default))
  expect_true(grepl("P-value:", result_custom))
})

# ============================================
# format_ci_range Tests
# ============================================

test_that("format_ci_range formats with bracket style", {
  result <- format_ci_range(0.1, 0.5, digits = 2, format = "bracket")

  expect_true(grepl("\\[", result))
  expect_true(grepl("\\]", result))
  expect_true(grepl("0.10", result))
  expect_true(grepl("0.50", result))
})

test_that("format_ci_range formats with dash style", {
  result <- format_ci_range(0.1, 0.5, digits = 2, format = "dash")

  expect_true(grepl("-", result))
  expect_false(grepl("\\[", result))
  expect_true(grepl("0.10", result))
  expect_true(grepl("0.50", result))
})

test_that("format_ci_range handles NULL inputs", {
  expect_equal(format_ci_range(NULL, 0.5), "")
  expect_equal(format_ci_range(0.1, NULL), "")
  expect_equal(format_ci_range(NULL, NULL), "")
})

test_that("format_ci_range handles NA inputs", {
  expect_equal(format_ci_range(NA, 0.5), "")
  expect_equal(format_ci_range(0.1, NA), "")
  expect_equal(format_ci_range(NA, NA), "")
})

test_that("format_ci_range respects digits parameter", {
  result <- format_ci_range(0.12345, 0.98765, digits = 3)

  expect_true(grepl("0.123", result))
  expect_true(grepl("0.988", result))
})

# ============================================
# resolve_stars Tests
# ============================================

test_that("resolve_stars returns empty strings for NULL input", {
  result <- resolve_stars(NULL, p_values = NULL, n = 5)
  expect_equal(length(result), 5)
  expect_true(all(result == ""))
})

test_that("resolve_stars computes from p-values when TRUE", {
  # Note: thresholds are < (not <=)
  # 0.0001 < 0.001 -> "***"
  # 0.005 < 0.01 -> "**"
  # 0.03 < 0.05 -> "*"
  # 0.1 >= 0.05 -> ""
  p_values <- c(0.0001, 0.005, 0.03, 0.1)
  result <- resolve_stars(TRUE, p_values = p_values, n = 4)

  expect_equal(result[1], "***")  # < 0.001
  expect_equal(result[2], "**")   # < 0.01
  expect_equal(result[3], "*")    # < 0.05
  expect_equal(result[4], "")     # >= 0.05
})

test_that("resolve_stars returns empty for FALSE input", {
  result <- resolve_stars(FALSE, p_values = c(0.001, 0.01), n = 2)
  expect_equal(length(result), 2)
  expect_true(all(result == ""))
})

test_that("resolve_stars treats numeric input as p-values", {
  # 0.0001 < 0.001 -> "***"
  # 0.03 < 0.05 -> "*"
  result <- resolve_stars(c(0.0001, 0.03), p_values = NULL, n = 2)
  expect_equal(result[1], "***")
  expect_equal(result[2], "*")
})

test_that("resolve_stars uses character input directly", {
  result <- resolve_stars(c("*", "**", "***"), p_values = NULL, n = 3)
  expect_equal(result[1], "*")
  expect_equal(result[2], "**")
  expect_equal(result[3], "***")
})

test_that("resolve_stars recycles character input", {
  result <- resolve_stars(c("*", "**"), p_values = NULL, n = 4)
  expect_equal(length(result), 4)
})

test_that("resolve_stars returns empty for TRUE with NULL p-values", {
  result <- resolve_stars(TRUE, p_values = NULL, n = 3)
  expect_equal(length(result), 3)
  expect_true(all(result == ""))
})

# ============================================
# format_edge_label_template Tests
# ============================================

test_that("format_edge_label_template substitutes {est}", {
  result <- format_edge_label_template(
    template = "{est}",
    weight = 0.5
  )
  expect_true(grepl("0.50", result))
})

test_that("format_edge_label_template substitutes {range}", {
  result <- format_edge_label_template(
    template = "{range}",
    ci_lower = 0.3,
    ci_upper = 0.7
  )
  expect_true(grepl("0.30", result))
  expect_true(grepl("0.70", result))
})

test_that("format_edge_label_template substitutes {low} and {up}", {
  result <- format_edge_label_template(
    template = "{low} to {up}",
    ci_lower = 0.2,
    ci_upper = 0.8
  )
  expect_true(grepl("0.20", result))
  expect_true(grepl("0.80", result))
  expect_true(grepl("to", result))
})

test_that("format_edge_label_template substitutes {p}", {
  result <- format_edge_label_template(
    template = "{p}",
    p_value = 0.05
  )
  expect_true(grepl("p=", result))
  expect_true(grepl("0.050", result))
})

test_that("format_edge_label_template substitutes {stars}", {
  result <- format_edge_label_template(
    template = "{est}{stars}",
    weight = 0.5,
    stars = "***"
  )
  expect_true(grepl("0.50", result))
  expect_true(grepl("\\*\\*\\*", result))
})

test_that("format_edge_label_template handles empty template", {
  expect_equal(format_edge_label_template(NULL), "")
  expect_equal(format_edge_label_template(""), "")
})

test_that("format_edge_label_template handles missing values", {
  result <- format_edge_label_template(
    template = "{est} {range}",
    weight = NA,
    ci_lower = NA,
    ci_upper = NA
  )
  # Should return trimmed result without NAs
  expect_true(nchar(trimws(result)) == 0 || !grepl("NA", result))
})

test_that("format_edge_label_template respects ci_format", {
  result_bracket <- format_edge_label_template(
    template = "{range}",
    ci_lower = 0.3, ci_upper = 0.7,
    ci_format = "bracket"
  )
  result_dash <- format_edge_label_template(
    template = "{range}",
    ci_lower = 0.3, ci_upper = 0.7,
    ci_format = "dash"
  )

  expect_true(grepl("\\[", result_bracket))
  expect_true(grepl("-", result_dash))
  expect_false(grepl("\\[", result_dash))
})

test_that("format_edge_label_template respects p_prefix", {
  result <- format_edge_label_template(
    template = "{p}",
    p_value = 0.05,
    p_prefix = "P: "
  )
  expect_true(grepl("P:", result))
})

test_that("format_edge_label_template cleans up whitespace", {
  result <- format_edge_label_template(
    template = "{est}   {range}",
    weight = 0.5,
    ci_lower = 0.3, ci_upper = 0.7
  )
  # Multiple spaces should be reduced to single space
  expect_false(grepl("  ", result))
})

# ============================================
# get_template_from_style Tests
# ============================================

test_that("get_template_from_style returns NULL for 'none'", {
  expect_null(get_template_from_style("none"))
})

test_that("get_template_from_style returns correct templates", {
  expect_equal(get_template_from_style("estimate"), "{est}")
  expect_equal(get_template_from_style("full"), "{est} {range}")
  expect_equal(get_template_from_style("range"), "{range}")
  expect_equal(get_template_from_style("stars"), "{stars}")
})

test_that("get_template_from_style returns NULL for unknown style", {
  expect_null(get_template_from_style("unknown_style"))
})

# ============================================
# build_edge_labels_from_template Tests
# ============================================

test_that("build_edge_labels_from_template returns NULL for 'none' style", {
  result <- build_edge_labels_from_template(
    template = NULL,
    style = "none",
    n = 3
  )
  expect_null(result)
})

test_that("build_edge_labels_from_template builds labels from template", {
  result <- build_edge_labels_from_template(
    template = "{est}",
    weights = c(0.5, 0.7, 0.3),
    n = 3
  )

  expect_equal(length(result), 3)
  expect_true(grepl("0.50", result[1]))
  expect_true(grepl("0.70", result[2]))
  expect_true(grepl("0.30", result[3]))
})

test_that("build_edge_labels_from_template uses style preset", {
  result <- build_edge_labels_from_template(
    template = NULL,
    style = "estimate",
    weights = c(0.5, 0.7),
    n = 2
  )

  expect_equal(length(result), 2)
  expect_true(grepl("0.50", result[1]))
})

test_that("build_edge_labels_from_template handles CI ranges", {
  result <- build_edge_labels_from_template(
    template = "{est} {range}",
    weights = c(0.5, 0.7),
    ci_lower = c(0.3, 0.5),
    ci_upper = c(0.7, 0.9),
    n = 2
  )

  expect_equal(length(result), 2)
  expect_true(grepl("0.50", result[1]))
  expect_true(grepl("\\[", result[1]))
})

test_that("build_edge_labels_from_template handles p-values", {
  result <- build_edge_labels_from_template(
    template = "{est} {p}",
    weights = c(0.5),
    p_values = c(0.05),
    n = 1
  )

  expect_true(grepl("0.50", result))
  expect_true(grepl("p=", result))
})

test_that("build_edge_labels_from_template handles stars", {
  result <- build_edge_labels_from_template(
    template = "{est}{stars}",
    weights = c(0.5, 0.7, 0.3),
    stars = c("***", "**", "*"),
    n = 3
  )

  expect_true(grepl("\\*\\*\\*", result[1]))
  expect_true(grepl("\\*\\*", result[2]))
})

test_that("build_edge_labels_from_template recycles inputs", {
  result <- build_edge_labels_from_template(
    template = "{est}",
    weights = c(0.5),  # Only one weight, should recycle
    n = 3
  )

  expect_equal(length(result), 3)
  expect_equal(result[1], result[2])
})

# ============================================
# Integration Tests with splot
# ============================================

test_that("splot renders edge labels", {
  mat <- create_test_matrix(3, weighted = TRUE)

  result <- with_temp_png({
    splot(mat, edge_labels = TRUE, layout = "circle")
    TRUE
  })

  expect_true(result)
})

test_that("soplot renders edge labels with templates", {
  mat <- create_test_matrix(3, weighted = TRUE)

  result <- tryCatch({
    with_temp_png(soplot(mat, edge_labels = TRUE, layout = "circle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})
