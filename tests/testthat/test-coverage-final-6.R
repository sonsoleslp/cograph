# test-coverage-final-6.R
# Tests for "package not installed" fallback paths using mocked has_package()
# Covers: input-igraph.R:18-20,93-95,122-124, input-qgraph.R:18-20,
#         input-statnet.R:18-20, from-qgraph.R:21,27,33,39,
#         shapes-svg.R:95-98,156-164,227-233

# Helper: create a mock has_package that blocks specific packages
mock_has_pkg <- function(blocked_pkgs) {
  force(blocked_pkgs)
  function(pkg) {
    if (pkg %in% blocked_pkgs) return(FALSE)
    requireNamespace(pkg, quietly = TRUE)
  }
}

# ============================================
# input-igraph.R: has_package("igraph") fallbacks
# ============================================

test_that("parse_igraph errors when igraph not available", {
  local_mocked_bindings(
    has_package = mock_has_pkg("igraph"),
    .package = "cograph"
  )

  expect_error(
    cograph:::parse_igraph(structure(list(), class = "igraph")),
    "igraph.*required"
  )
})

test_that("apply_igraph_layout errors when igraph not available", {
  local_mocked_bindings(
    has_package = mock_has_pkg("igraph"),
    .package = "cograph"
  )

  mat <- matrix(c(0, 1, 1, 0), nrow = 2)
  net <- CographNetwork$new(mat)

  expect_error(
    cograph:::apply_igraph_layout(net, identity),
    "igraph.*required"
  )
})

test_that("apply_igraph_layout_by_name errors when igraph not available", {
  local_mocked_bindings(
    has_package = mock_has_pkg("igraph"),
    .package = "cograph"
  )

  mat <- matrix(c(0, 1, 1, 0), nrow = 2)
  net <- CographNetwork$new(mat)

  expect_error(
    cograph:::apply_igraph_layout_by_name(net, "fr"),
    "igraph.*required"
  )
})

# ============================================
# input-qgraph.R: has_package("qgraph") fallback
# ============================================

test_that("parse_qgraph errors when qgraph not available", {
  local_mocked_bindings(
    has_package = mock_has_pkg("qgraph"),
    .package = "cograph"
  )

  expect_error(
    cograph:::parse_qgraph(structure(list(), class = "qgraph")),
    "qgraph.*required"
  )
})

# ============================================
# input-statnet.R: has_package("network") fallback
# ============================================

test_that("parse_statnet errors when network not available", {
  local_mocked_bindings(
    has_package = mock_has_pkg("network"),
    .package = "cograph"
  )

  expect_error(
    cograph:::parse_statnet(structure(list(), class = "network")),
    "network.*required"
  )
})

# ============================================
# from-qgraph.R: RColorBrewer/colorspace fallbacks (lines 21, 27, 33, 39)
# ============================================

test_that("tna_color_palette uses hcl.colors fallback for 1-2 states", {
  local_mocked_bindings(
    has_package = mock_has_pkg(c("RColorBrewer", "colorspace")),
    .package = "cograph"
  )

  colors <- cograph:::tna_color_palette(2)
  expect_length(colors, 2)
  expect_true(all(nchar(colors) > 0))
})

test_that("tna_color_palette uses hcl.colors fallback for 3-8 states", {
  local_mocked_bindings(
    has_package = mock_has_pkg(c("RColorBrewer", "colorspace")),
    .package = "cograph"
  )

  colors <- cograph:::tna_color_palette(5)
  expect_length(colors, 5)
})

test_that("tna_color_palette uses hcl.colors fallback for 9-12 states", {
  local_mocked_bindings(
    has_package = mock_has_pkg(c("RColorBrewer", "colorspace")),
    .package = "cograph"
  )

  colors <- cograph:::tna_color_palette(10)
  expect_length(colors, 10)
})

test_that("tna_color_palette uses hcl.colors fallback for 13+ states", {
  local_mocked_bindings(
    has_package = mock_has_pkg(c("RColorBrewer", "colorspace")),
    .package = "cograph"
  )

  colors <- cograph:::tna_color_palette(15)
  expect_length(colors, 15)
})

# ============================================
# shapes-svg.R: grImport2/rsvg fallback paths
# ============================================

test_that("parse_svg warns when grImport2 not available", {
  local_mocked_bindings(
    has_package = mock_has_pkg("grImport2"),
    .package = "cograph"
  )

  svg_data <- list(
    source = "<svg></svg>",
    is_file = FALSE,
    parsed = NULL
  )

  expect_warning(
    result <- cograph:::parse_svg(svg_data),
    "grImport2.*required"
  )
  expect_null(result)
})

test_that("draw_svg_shape falls back to circle when parse_svg returns NULL", {
  skip_if_not_installed("grid")

  local_mocked_bindings(
    has_package = mock_has_pkg("grImport2"),
    .package = "cograph"
  )

  svg_data <- list(
    source = "<svg></svg>",
    is_file = FALSE,
    parsed = NULL
  )

  result <- with_temp_png({
    grid::grid.newpage()
    grob <- suppressWarnings(
      cograph:::draw_svg_shape(
        x = 0.5, y = 0.5, size = 0.1,
        svg_data = svg_data,
        fill = "red", border_color = "black",
        border_width = 1, alpha = 1
      )
    )
    grob
  })

  # Should return a circleGrob fallback
  expect_true(grid::is.grob(result))
})

test_that("draw_svg_shape_base falls back to circle when rsvg not available", {
  local_mocked_bindings(
    has_package = mock_has_pkg("rsvg"),
    .package = "cograph"
  )

  svg_data <- list(
    source = "<svg></svg>",
    is_file = FALSE,
    parsed = NULL
  )

  with_temp_png({
    plot.new()
    plot.window(xlim = c(0, 1), ylim = c(0, 1))
    cograph:::draw_svg_shape_base(
      x = 0.5, y = 0.5, size = 0.05,
      svg_data = svg_data,
      fill = "blue", border_color = "black",
      border_width = 1
    )
  })

  expect_true(TRUE)  # No error = success
})
