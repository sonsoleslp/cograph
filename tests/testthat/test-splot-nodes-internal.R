# test-splot-nodes-internal.R - Internal Node Rendering Function Tests
# Tests for low-level node drawing functions in R/splot-nodes.R

# ============================================
# Helper for splot testing
# ============================================

with_temp_plot <- function(expr) {
  png_file <- tempfile(fileext = ".png")
  png(png_file, width = 400, height = 400)
  on.exit({
    dev.off()
    unlink(png_file)
  })
  force(expr)
}

# Make internal functions available
draw_node_base <- cograph:::draw_node_base
draw_pie_node_base <- cograph:::draw_pie_node_base
draw_donut_node_base <- cograph:::draw_donut_node_base
draw_polygon_donut_node_base <- cograph:::draw_polygon_donut_node_base
draw_donut_pie_node_base <- cograph:::draw_donut_pie_node_base
draw_double_donut_pie_node_base <- cograph:::draw_double_donut_pie_node_base
draw_node_label_base <- cograph:::draw_node_label_base
draw_neural_node_base <- cograph:::draw_neural_node_base
draw_chip_node_base <- cograph:::draw_chip_node_base
draw_robot_node_base <- cograph:::draw_robot_node_base
draw_network_node_base <- cograph:::draw_network_node_base
draw_database_node_base <- cograph:::draw_database_node_base
render_nodes_base <- cograph:::render_nodes_base

# ============================================
# SPECIAL NODE SHAPES
# ============================================

test_that("draw_node_base renders neural shape", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_node_base(0, 0, 0.3, shape = "neural", col = "lightblue",
                     border.col = "darkblue", border.width = 1)
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_node_base renders chip shape", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_node_base(0, 0, 0.3, shape = "chip", col = "gray80",
                     border.col = "black", border.width = 1)
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_node_base renders robot shape", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_node_base(0, 0, 0.3, shape = "robot", col = "steelblue",
                     border.col = "darkblue", border.width = 1)
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_node_base renders network shape", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_node_base(0, 0, 0.3, shape = "network", col = "lightyellow",
                     border.col = "orange", border.width = 1)
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_node_base renders database shape", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_node_base(0, 0, 0.3, shape = "database", col = "lightgreen",
                     border.col = "darkgreen", border.width = 1)
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_node_base renders ellipse shape", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_node_base(0, 0, 0.3, size2 = 0.2, shape = "ellipse",
                     col = "pink", border.col = "red", border.width = 1)
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_node_base renders rectangle shape", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_node_base(0, 0, 0.3, size2 = 0.15, shape = "rectangle",
                     col = "lavender", border.col = "purple", border.width = 2)
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_node_base renders polygon-based shapes (gear, cloud, brain)", {
  shapes <- c("gear", "cloud", "brain", "star", "heart", "cross")

  for (shape in shapes) {
    result <- tryCatch({
      with_temp_plot({
        plot.new()
        plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
        draw_node_base(0, 0, 0.3, shape = shape, col = "coral",
                       border.col = "maroon", border.width = 1)
      })
      TRUE
    }, error = function(e) FALSE)

    expect_true(result, info = paste("Failed for shape:", shape))
  }
})

# ============================================
# SPECIAL SHAPE DIRECT TESTS
# ============================================

test_that("draw_neural_node_base with custom n_connections", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_neural_node_base(0, 0, 0.3, col = "lightblue",
                            border.col = "darkblue", border.width = 1.5,
                            n_connections = 8)
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_chip_node_base with custom pins_per_side", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_chip_node_base(0, 0, 0.3, col = "gray80",
                          border.col = "black", border.width = 1,
                          pins_per_side = 5)
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# PIE NODE EDGE CASES
# ============================================

test_that("draw_pie_node_base handles NULL values", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_pie_node_base(0, 0, 0.3, values = NULL)
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_pie_node_base handles empty values", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_pie_node_base(0, 0, 0.3, values = numeric(0))
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_pie_node_base handles single value with default_color", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_pie_node_base(0, 0, 0.3, values = c(1),
                         default_color = "steelblue")
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_pie_node_base handles zero proportions", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_pie_node_base(0, 0, 0.3, values = c(0, 1, 0, 2),
                         colors = c("red", "blue", "green", "yellow"))
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_pie_node_base handles custom pie_border.width", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_pie_node_base(0, 0, 0.3, values = c(1, 2, 3),
                         pie_border.width = 0.5)
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_pie_node_base with very thin border (< 0.1)", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_pie_node_base(0, 0, 0.3, values = c(1, 2, 3),
                         pie_border.width = 0.05)
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# DONUT NODE EDGE CASES
# ============================================

test_that("draw_donut_node_base handles single value (progress donut)", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_donut_node_base(0, 0, 0.3, values = 0.75,
                           colors = "steelblue")
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_donut_node_base handles multiple values (segmented donut)", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_donut_node_base(0, 0, 0.3, values = c(1, 2, 3),
                           colors = c("red", "green", "blue"))
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_donut_node_base handles zero proportion", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_donut_node_base(0, 0, 0.3, values = 0)
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_donut_node_base handles full ring (value = 1)", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_donut_node_base(0, 0, 0.3, values = 1, colors = "navy")
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_donut_node_base handles value > 1 (clamps to 1)", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_donut_node_base(0, 0, 0.3, values = 1.5)
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_donut_node_base handles negative value (clamps to 0)", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_donut_node_base(0, 0, 0.3, values = -0.5)
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_donut_node_base with outer border color (double border)", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_donut_node_base(0, 0, 0.3, values = 0.7,
                           outer_border.col = "red",
                           border.col = "black")
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_donut_node_base with dashed border (border.lty)", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_donut_node_base(0, 0, 0.3, values = 0.5, border.lty = 2)
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_donut_node_base with show_value and formatting", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_donut_node_base(0, 0, 0.3, values = 0.75,
                           show_value = TRUE,
                           value_cex = 1.2,
                           value_col = "navy",
                           value_fontface = "bold.italic",
                           value_fontfamily = "serif",
                           value_digits = 1,
                           value_prefix = "$",
                           value_suffix = "%")
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_donut_node_base with all fontface options", {
  fontfaces <- c("plain", "bold", "italic", "bold.italic")

  for (face in fontfaces) {
    result <- tryCatch({
      with_temp_plot({
        plot.new()
        plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
        draw_donut_node_base(0, 0, 0.3, values = 0.5,
                             show_value = TRUE,
                             value_fontface = face)
      })
      TRUE
    }, error = function(e) FALSE)

    expect_true(result, info = paste("Failed for fontface:", face))
  }
})

test_that("draw_donut_node_base with default_color fallback", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_donut_node_base(0, 0, 0.3, values = 0.5,
                           colors = NULL,
                           default_color = "coral")
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# POLYGON DONUT NODE TESTS
# ============================================

test_that("draw_polygon_donut_node_base with various shapes", {
  shapes <- c("square", "hexagon", "triangle", "pentagon", "octagon")

  for (shape in shapes) {
    result <- tryCatch({
      with_temp_plot({
        plot.new()
        plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
        draw_polygon_donut_node_base(0, 0, 0.3, values = 0.6,
                                     donut_shape = shape)
      })
      TRUE
    }, error = function(e) FALSE)

    expect_true(result, info = paste("Failed for shape:", shape))
  }
})

test_that("draw_polygon_donut_node_base with NULL values defaults to 1", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_polygon_donut_node_base(0, 0, 0.3, values = NULL)
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_polygon_donut_node_base with multi-segment values", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_polygon_donut_node_base(0, 0, 0.3, values = c(1, 2, 3),
                                   colors = c("red", "green", "blue"),
                                   donut_shape = "hexagon")
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_polygon_donut_node_base with outer border", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_polygon_donut_node_base(0, 0, 0.3, values = 0.7,
                                   outer_border.col = "darkred",
                                   donut_shape = "square")
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_polygon_donut_node_base with show_value formatting", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_polygon_donut_node_base(0, 0, 0.3, values = 0.85,
                                   show_value = TRUE,
                                   value_prefix = "",
                                   value_suffix = "%",
                                   value_digits = 0,
                                   donut_shape = "hexagon")
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# DONUT PIE NODE TESTS
# ============================================

test_that("draw_donut_pie_node_base renders correctly", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_donut_pie_node_base(0, 0, 0.3,
                               donut_value = 0.75,
                               donut_color = "steelblue",
                               pie_values = c(1, 2, 3),
                               pie_colors = c("red", "green", "blue"))
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_donut_pie_node_base with NULL pie values", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_donut_pie_node_base(0, 0, 0.3,
                               donut_value = 0.6,
                               pie_values = NULL)
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_donut_pie_node_base with empty pie values", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_donut_pie_node_base(0, 0, 0.3,
                               donut_value = 0.6,
                               pie_values = numeric(0))
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_donut_pie_node_base with pie_default_color", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_donut_pie_node_base(0, 0, 0.3,
                               donut_value = 0.8,
                               pie_values = c(1),
                               pie_default_color = "coral")
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_donut_pie_node_base with custom border widths", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_donut_pie_node_base(0, 0, 0.3,
                               donut_value = 0.7,
                               pie_values = c(1, 2),
                               pie_border.width = 0.3,
                               donut_border.width = 1.5)
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_donut_pie_node_base with zero donut value", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_donut_pie_node_base(0, 0, 0.3,
                               donut_value = 0,
                               pie_values = c(1, 1))
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# DOUBLE DONUT PIE NODE TESTS
# ============================================

test_that("draw_double_donut_pie_node_base renders all layers", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_double_donut_pie_node_base(0, 0, 0.4,
                                      donut_values = c(1, 2),
                                      donut_colors = c("red", "blue"),
                                      donut2_values = 0.7,
                                      donut2_colors = "green",
                                      pie_values = c(1, 1, 1),
                                      pie_colors = c("yellow", "orange", "pink"))
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_double_donut_pie_node_base with NULL outer donut", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_double_donut_pie_node_base(0, 0, 0.4,
                                      donut_values = NULL,
                                      donut2_values = 0.5,
                                      pie_values = c(1, 2))
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_double_donut_pie_node_base with NULL inner donut", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_double_donut_pie_node_base(0, 0, 0.4,
                                      donut_values = c(1, 1),
                                      donut2_values = NULL,
                                      pie_values = c(2, 1))
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_double_donut_pie_node_base with NULL pie", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_double_donut_pie_node_base(0, 0, 0.4,
                                      donut_values = 0.8,
                                      donut2_values = c(1, 2, 1),
                                      pie_values = NULL)
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_double_donut_pie_node_base with progress donuts", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_double_donut_pie_node_base(0, 0, 0.4,
                                      donut_values = 0.9,
                                      donut_colors = "navy",
                                      donut2_values = 0.6,
                                      donut2_colors = "coral")
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_double_donut_pie_node_base with custom ratios", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_double_donut_pie_node_base(0, 0, 0.4,
                                      donut_values = c(1, 1),
                                      donut2_values = c(1, 2),
                                      pie_values = c(1, 1),
                                      outer_inner_ratio = 0.8,
                                      inner_inner_ratio = 0.5)
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# LABEL RENDERING TESTS
# ============================================

test_that("draw_node_label_base handles NULL label", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_node_label_base(0, 0, label = NULL)
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_node_label_base handles NA label", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_node_label_base(0, 0, label = NA)
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_node_label_base handles empty string label", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_node_label_base(0, 0, label = "")
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_node_label_base renders with all parameters", {
  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      draw_node_label_base(0, 0, label = "Test",
                           cex = 1.5, col = "navy",
                           font = 2, family = "serif",
                           hjust = 0, vjust = 1, srt = 45)
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("draw_node_label_base with pos parameter", {
  positions <- c(1, 2, 3, 4)  # below, left, above, right

  for (pos in positions) {
    result <- tryCatch({
      with_temp_plot({
        plot.new()
        plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
        draw_node_label_base(0, 0, label = "Test",
                             pos = pos, offset = 0.3)
      })
      TRUE
    }, error = function(e) FALSE)

    expect_true(result, info = paste("Failed for pos:", pos))
  }
})

# ============================================
# RENDER_NODES_BASE TESTS
# ============================================

test_that("render_nodes_base handles empty network", {
  layout <- matrix(numeric(0), ncol = 2)

  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      render_nodes_base(layout, vsize = numeric(0))
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("render_nodes_base renders nodes with labels", {
  layout <- matrix(c(0, 0.5, -0.5, 0.5, 0, -0.5), ncol = 2, byrow = TRUE)

  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      render_nodes_base(layout,
                        vsize = c(0.1, 0.15, 0.12),
                        shape = "circle",
                        labels = c("A", "B", "C"),
                        label.cex = 1,
                        label.color = "black")
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("render_nodes_base renders mixed pie nodes", {
  layout <- matrix(c(0, 0.5, -0.5, 0.5, 0, -0.5), ncol = 2, byrow = TRUE)

  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      render_nodes_base(layout,
                        vsize = c(0.15, 0.15, 0.15),
                        pie = list(c(1, 2), c(3, 1), c(1, 1, 2)),
                        pieColor = list(c("red", "blue"), c("green", "yellow"), NULL))
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("render_nodes_base renders mixed donut nodes", {
  layout <- matrix(c(0, 0.5, -0.5, 0.5, 0, -0.5), ncol = 2, byrow = TRUE)

  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      render_nodes_base(layout,
                        vsize = c(0.15, 0.15, 0.15),
                        donut = list(0.5, 0.7, 0.9),
                        donutColor = list("steelblue", "coral", "forestgreen"))
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("render_nodes_base renders donut with pie combination", {
  layout <- matrix(c(0, 0.5, -0.5, 0.5), ncol = 2, byrow = TRUE)

  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      render_nodes_base(layout,
                        vsize = c(0.2, 0.2),
                        donut = list(0.7, 0.5),
                        donutColor = list("navy", "maroon"),
                        pie = list(c(1, 2), c(2, 1, 1)),
                        pieColor = list(c("red", "blue"), c("green", "yellow", "pink")))
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("render_nodes_base renders with different shapes per node", {
  layout <- matrix(c(-0.5, 0, 0.5, 0, 0, 0.5, 0, -0.5), ncol = 2, byrow = TRUE)

  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      render_nodes_base(layout,
                        vsize = rep(0.12, 4),
                        shape = c("circle", "square", "triangle", "diamond"),
                        color = c("red", "green", "blue", "yellow"),
                        border.color = "black")
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("render_nodes_base with partial pie/donut lists", {
  layout <- matrix(c(0, 0.5, -0.5, 0.5, 0, -0.5), ncol = 2, byrow = TRUE)

  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      # Only first two nodes have pie values
      render_nodes_base(layout,
                        vsize = c(0.15, 0.15, 0.15),
                        shape = "circle",
                        pie = list(c(1, 2), c(3, 1)))
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("render_nodes_base renders labels selectively (empty labels)", {
  layout <- matrix(c(0, 0.5, -0.5, 0.5, 0, -0.5), ncol = 2, byrow = TRUE)

  result <- tryCatch({
    with_temp_plot({
      plot.new()
      plot.window(xlim = c(-1, 1), ylim = c(-1, 1))
      render_nodes_base(layout,
                        vsize = rep(0.1, 3),
                        labels = c("A", "", NA))
    })
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# SPLOT INTEGRATION WITH SPECIAL SHAPES
# ============================================

test_that("splot renders neural shape nodes", {
  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_plot(splot(mat, node_shape = "neural"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders chip shape nodes", {
  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_plot(splot(mat, node_shape = "chip"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders robot shape nodes", {
  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_plot(splot(mat, node_shape = "robot"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders network shape nodes", {
  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_plot(splot(mat, node_shape = "network"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders database shape nodes", {
  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_plot(splot(mat, node_shape = "database"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders gear shape nodes", {
  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_plot(splot(mat, node_shape = "gear"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders cloud shape nodes", {
  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_plot(splot(mat, node_shape = "cloud"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders brain shape nodes", {
  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_plot(splot(mat, node_shape = "brain"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders ellipse shape nodes", {
  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_plot(splot(mat, node_shape = "ellipse"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders rectangle shape nodes", {
  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_plot(splot(mat, node_shape = "rectangle"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

# ============================================
# DONUT VALUE FORMATTING VIA SPLOT
# ============================================

test_that("splot renders donut with value prefix and suffix", {
  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_plot(splot(mat, node_shape = "donut",
                         donut_values = c(0.5, 0.7, 0.3),
                         donut_show_value = TRUE,
                         donut_value_prefix = "$",
                         donut_value_suffix = "k"))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders donut with custom digit formatting", {
  mat <- create_test_matrix(3)

  result <- tryCatch({
    with_temp_plot(splot(mat, node_shape = "donut",
                         donut_values = c(0.333, 0.666, 0.999),
                         donut_show_value = TRUE,
                         donut_value_digits = 1))
    TRUE
  }, error = function(e) FALSE)

  expect_true(result)
})

test_that("splot renders donut with fontface options", {
  fontfaces <- c("plain", "bold", "italic", "bold.italic")
  mat <- create_test_matrix(3)

  for (face in fontfaces) {
    result <- tryCatch({
      with_temp_plot(splot(mat, node_shape = "donut",
                           donut_values = c(0.5, 0.6, 0.7),
                           donut_show_value = TRUE,
                           donut_value_fontface = face))
      TRUE
    }, error = function(e) FALSE)

    expect_true(result, info = paste("Failed for fontface:", face))
  }
})

test_that("splot renders polygon donut with different shapes", {
  shapes <- c("square", "hexagon", "triangle", "pentagon", "octagon")
  mat <- create_test_matrix(3)

  for (shape in shapes) {
    result <- tryCatch({
      with_temp_plot(splot(mat, node_shape = "polygon_donut",
                           donut_values = c(0.4, 0.6, 0.8),
                           donut_shape = shape))
      TRUE
    }, error = function(e) FALSE)

    expect_true(result, info = paste("Failed for donut_shape:", shape))
  }
})

# ============================================
# sn_nodes() with special shapes
# ============================================

test_that("sn_nodes() accepts special shapes", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  special_shapes <- c("neural", "chip", "robot", "network", "database")

  for (shape in special_shapes) {
    result <- sn_nodes(net, shape = shape)
    aes <- result$network$get_node_aes()
    expect_true(all(aes$shape == shape), info = paste("Failed for shape:", shape))
  }
})

test_that("sn_nodes() sets donut_border_width", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  result <- sn_nodes(net, donut_border_width = 2)
  aes <- result$network$get_node_aes()

  expect_equal(aes$donut_border_width, 2)
})

test_that("sn_nodes() sets donut_value_fontfamily", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  result <- sn_nodes(net, donut_value_fontfamily = "mono")
  aes <- result$network$get_node_aes()

  expect_equal(aes$donut_value_fontfamily, "mono")
})
