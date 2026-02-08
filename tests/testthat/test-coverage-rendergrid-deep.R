# test-coverage-rendergrid-deep.R
# Deep coverage tests for render-edges.R, render-grid.R, render-nodes.R,
# and shapes-special.R to reach 100% coverage.

# ============================================
# Helpers for direct rendering calls
# ============================================

# Helper to call render_nodes_grid directly in a PNG device
render_nodes_directly <- function(net_obj) {
  with_temp_png({
    grid::grid.newpage()
    vp <- grid::viewport(width = 0.9, height = 0.9)
    grid::pushViewport(vp)
    grobs <- cograph:::render_nodes_grid(net_obj$network)
    grid::grid.draw(grobs)
    grid::popViewport()
  })
}

# Helper to call render_edges_grid directly in a PNG device
# This bypasses soplot which re-introduces default width/color via sn_edges cloning
render_edges_directly <- function(net_obj) {
  with_temp_png({
    grid::grid.newpage()
    vp <- grid::viewport(width = 0.9, height = 0.9)
    grid::pushViewport(vp)
    grobs <- cograph:::render_edges_grid(net_obj$network)
    grid::grid.draw(grobs)
    grid::popViewport()
  })
}

# Helper to call render_edge_labels_grid directly in a PNG device
render_edge_labels_directly <- function(net_obj) {
  with_temp_png({
    grid::grid.newpage()
    vp <- grid::viewport(width = 0.9, height = 0.9)
    grid::pushViewport(vp)
    grobs <- cograph:::render_edge_labels_grid(net_obj$network)
    grid::grid.draw(grobs)
    grid::popViewport()
  })
}

# Helper to set up a network with positioned nodes
setup_positioned_network <- function(n = 3) {
  mat <- matrix(0, n, n)
  if (n >= 2) { mat[1, 2] <- 1; mat[2, 1] <- 1 }
  if (n >= 3) { mat[2, 3] <- 1; mat[3, 2] <- 1 }
  if (n >= 4) { mat[3, 4] <- 1; mat[4, 3] <- 1 }
  net <- cograph(mat)
  nodes <- net$network$get_nodes()
  angles <- seq(0, 2 * pi * (1 - 1/n), length.out = n)
  nodes$x <- 0.5 + 0.3 * cos(angles)
  nodes$y <- 0.5 + 0.3 * sin(angles)
  net$network$set_nodes(nodes)
  net
}

# Helper to create a network with width/color removed from edge_aes
# by directly manipulating the internal CographNetwork object.
# This is needed because clone_network() re-introduces defaults.
setup_network_no_width_color <- function(mat) {
  net <- cograph(mat)
  nodes <- net$network$get_nodes()
  n <- nrow(nodes)
  angles <- seq(0, 2 * pi * (1 - 1/n), length.out = n)
  new_x <- 0.5 + 0.3 * cos(angles)
  new_y <- 0.5 + 0.3 * sin(angles)
  # Set layout coords first (this also updates nodes$x/y)
  layout_df <- data.frame(x = new_x, y = new_y)
  net$network$set_layout_coords(layout_df)
  # Remove width and color so render_edges_grid uses weight-based scaling and sign-based colors
  net$network$set_edge_aes(list(width = NULL, color = NULL))
  net
}

# ============================================
# render-edges.R: Weight-based scaling (lines 30-45)
# ============================================

test_that("render_edges_grid uses weight-based scaling when no explicit width (lines 30-42)", {
  # Lines 30-42: aes$width is NULL AND edges have weight column
  # Must call render_edges_grid directly because soplot/sn_edges clone re-introduces defaults
  mat <- matrix(0, 3, 3)
  mat[1, 2] <- 0.8
  mat[2, 3] <- 0.4
  mat[1, 3] <- 0.6
  mat <- mat + t(mat)
  net <- setup_network_no_width_color(mat)
  # Verify width is actually NULL
  aes <- net$network$get_edge_aes()
  expect_null(aes$width)
  # Direct call to render_edges_grid, bypassing soplot
  result <- tryCatch({ render_edges_directly(net); TRUE }, error = function(e) { message(e$message); FALSE })
  expect_true(result)
})

test_that("render_edges_grid weight-based scaling with edge_scale_mode (lines 37-41)", {
  # Lines 37-41: scale_edge_widths with explicit mode/range
  mat <- create_test_matrix(4, weighted = TRUE)
  net <- setup_network_no_width_color(mat)
  # Set scaling params directly on edge_aes
  net$network$set_edge_aes(list(
    edge_scale_mode = "sqrt",
    edge_width_range = c(1, 6),
    esize = 10
  ))
  result <- tryCatch({ render_edges_directly(net); TRUE }, error = function(e) { message(e$message); FALSE })
  expect_true(result)
})

test_that("render_edges_grid with no weights uses default width (lines 43-45)", {
  # Lines 43-45: aes$width is NULL AND no weight column -> use theme default
  mat <- create_test_matrix(4, weighted = FALSE)
  net <- setup_network_no_width_color(mat)
  # Remove weight column from edges
  edges_df <- net$network$get_edges()
  edges_df$weight <- NULL
  net$network$set_edges(edges_df)
  result <- tryCatch({ render_edges_directly(net); TRUE }, error = function(e) { message(e$message); FALSE })
  expect_true(result)
})

# ============================================
# render-edges.R: Color resolution (lines 58-65)
# ============================================

test_that("render_edges_grid resolves colors by weight sign (lines 58-62)", {
  # Lines 58-62: aes$color is NULL, resolve by weight sign (positive/negative/zero)
  # Must call render_edges_grid directly because soplot/sn_edges clone re-introduces defaults
  mat <- matrix(0, 3, 3)
  mat[1, 2] <- 0.8   # positive
  mat[2, 3] <- -0.5  # negative
  mat[1, 3] <- 0     # zero weight -> default color
  mat <- mat + t(mat)
  net <- setup_network_no_width_color(mat)
  # Verify color is actually NULL
  aes <- net$network$get_edge_aes()
  expect_null(aes$color)
  result <- tryCatch({ render_edges_directly(net); TRUE }, error = function(e) { message(e$message); FALSE })
  expect_true(result)
})

test_that("render_edges_grid colors with no weight column (lines 63-64)", {
  # Lines 63-64: aes$color is NULL AND no weight column -> rep(default_col, m)
  mat <- create_test_matrix(3, weighted = FALSE)
  net <- setup_network_no_width_color(mat)
  # Remove weight column
  edges_df <- net$network$get_edges()
  edges_df$weight <- NULL
  net$network$set_edges(edges_df)
  result <- tryCatch({ render_edges_directly(net); TRUE }, error = function(e) { message(e$message); FALSE })
  expect_true(result)
})

# ============================================
# render-edges.R: Cut threshold fading (lines 81-82)
# ============================================

test_that("render_edges_grid applies cut threshold fading (lines 81-82)", {
  # Lines 74-83: cut threshold making edges below cut fade
  # aes$cut must be > 0, and edges must have weight column with some below cut
  # Use direct rendering to ensure width is set (not NULL) while cut is active
  mat <- matrix(0, 4, 4)
  mat[1, 2] <- 0.8
  mat[2, 3] <- 0.1  # below cut of 0.5
  mat[3, 4] <- 0.05 # below cut of 0.5
  mat[1, 4] <- 0.9
  mat <- mat + t(mat)
  net <- cograph(mat)
  nodes <- net$network$get_nodes()
  n <- nrow(nodes)
  angles <- seq(0, 2 * pi * (1 - 1/n), length.out = n)
  nodes$x <- 0.5 + 0.3 * cos(angles)
  nodes$y <- 0.5 + 0.3 * sin(angles)
  net$network$set_nodes(nodes)
  # Set cut > 0 directly to trigger the fading path
  net$network$set_edge_aes(list(cut = 0.5, alpha = 0.9))
  result <- tryCatch({ render_edges_directly(net); TRUE }, error = function(e) { message(e$message); FALSE })
  expect_true(result)
})

# ============================================
# render-edges.R: Self-loop skip in reciprocal (line 108)
# ============================================

test_that("render_edges_grid skips self-loops in reciprocal detection (line 108)", {
  # Line 108: from_i == to_i -> next (skip self-loop in reciprocal scan)
  mat <- matrix(0, 3, 3)
  mat[1, 1] <- 0.5  # self-loop
  mat[1, 2] <- 0.8
  mat[2, 1] <- 0.6  # reciprocal
  mat[2, 3] <- 0.4
  expect_soplot_works(mat, show_arrows = TRUE)
})

# ============================================
# render-edges.R: CI color explicit (line 213)
# ============================================

test_that("render_edges_grid uses explicit ci_color (line 213)", {
  # Line 213: edge_ci_color is not NULL and not NA -> recycle_to_length
  mat <- create_test_matrix(3, weighted = TRUE)
  n_edges <- sum(mat != 0) / 2  # symmetric
  net <- cograph(mat) |>
    sn_edges(
      ci = rep(0.1, n_edges),
      ci_color = "blue"
    )
  expect_soplot_works(net)
})

# ============================================
# render-edges.R: Self-loop with CI underlay (lines 252-269)
# ============================================

test_that("render_edges_grid draws self-loop with CI underlay (lines 252-269)", {
  # Lines 252-269: self-loop CI underlay + main self-loop drawing
  mat <- matrix(0, 3, 3)
  mat[1, 1] <- 0.5  # self-loop
  mat[2, 2] <- 0.7  # another self-loop
  mat[1, 2] <- 0.3
  mat[2, 1] <- 0.3
  n_edges <- sum(mat != 0)
  net <- cograph(mat) |>
    sn_edges(ci = rep(0.1, n_edges))
  expect_soplot_works(net, show_arrows = TRUE)
})

test_that("render_edges_grid draws self-loop CI with custom color (line 254)", {
  # Line 254: CI underlay for self-loop with explicit ci_color
  mat <- matrix(0, 3, 3)
  mat[1, 1] <- 0.5
  mat[1, 2] <- 0.3
  mat[2, 1] <- 0.3
  n_edges <- sum(mat != 0)
  net <- cograph(mat) |>
    sn_edges(ci = rep(0.08, n_edges), ci_color = "red")
  expect_soplot_works(net)
})

# ============================================
# render-edges.R: CI underlay for curved edges (lines 286-292)
# ============================================

test_that("render_edges_grid draws CI underlay on curved edges (lines 286-292)", {
  # Lines 285-292: CI underlay with curvature != 0
  mat <- matrix(0, 3, 3)
  mat[1, 2] <- 0.8
  mat[2, 1] <- 0.5  # reciprocal -> auto-curves
  mat[1, 3] <- 0.6
  mat[3, 1] <- 0.4  # reciprocal -> auto-curves
  n_edges <- sum(mat != 0)
  net <- cograph(mat) |>
    sn_edges(ci = rep(0.1, n_edges))
  expect_soplot_works(net)
})

test_that("render_edges_grid CI underlay on forced curved edges", {
  # Lines 286-292: CI underlay on force-curved non-reciprocal edges
  mat <- create_test_matrix(3, weighted = TRUE)
  n_edges <- sum(mat != 0) / 2
  net <- cograph(mat) |>
    sn_edges(
      ci = rep(0.05, n_edges),
      curves = "force"
    )
  expect_soplot_works(net)
})

# ============================================
# render-edges.R: draw_self_loop (lines 441-506)
# ============================================

test_that("draw_self_loop_grid is exercised via soplot self-loops (lines 441-506)", {
  # Lines 441-506: the entire draw_self_loop function
  mat <- matrix(0, 4, 4)
  mat[1, 1] <- 1
  mat[2, 2] <- 0.5
  mat[3, 3] <- 0.8
  mat[1, 2] <- 0.3
  mat[2, 1] <- 0.3
  expect_soplot_works(mat, loop_rotation = pi / 3)
})

# ============================================
# render-edges.R: Template-based labels (lines 532-547)
# ============================================

test_that("render_edge_labels_grid uses template-based labels (lines 532-547)", {
  # Lines 530-547: has_template TRUE path with label_style
  mat <- create_test_matrix(3, weighted = TRUE)
  n_edges <- sum(mat != 0) / 2
  net <- cograph(mat) |>
    sn_edges(label_style = "estimate")
  expect_soplot_works(net)
})

test_that("render_edge_labels_grid uses label_template (lines 532-547)", {
  # Lines 527-547: label_template triggers template path
  mat <- create_test_matrix(3, weighted = TRUE)
  n_edges <- sum(mat != 0) / 2
  net <- cograph(mat) |>
    sn_edges(label_template = "{est}")
  expect_soplot_works(net)
})

test_that("render_edge_labels_grid null labels returns early (line 555)", {
  # Line 555: labels is NULL after template building -> early return
  # has_template=TRUE requires label_style != "none" (or label_template set)
  # build_edge_labels_from_template returns NULL when style maps to NULL in switch
  # An unrecognized style falls through switch default -> template = NULL -> return(NULL)
  # We bypass sn_edges validation by setting aes directly
  net <- setup_positioned_network(3)
  net$network$set_edge_aes(list(
    label_style = "custom_nonexistent_style"  # unknown style -> template=NULL -> labels=NULL
  ))
  result <- tryCatch({
    render_edge_labels_directly(net)
    TRUE
  }, error = function(e) { message(e$message); FALSE })
  expect_true(result)
})

# ============================================
# render-edges.R: Fontface bold.italic and numeric (lines 597-598, 601)
# ============================================

test_that("render_edge_labels_grid handles bold.italic fontface (line 597)", {
  # Line 597: "bold.italic" = 4
  mat <- create_test_matrix(3, weighted = TRUE)
  expect_soplot_works(mat, edge_labels = TRUE, edge_label_fontface = "bold.italic")
})

test_that("render_edge_labels_grid handles numeric fontface (line 601)", {
  # Line 601: ff is numeric, not character -> pass through
  # sn_edges validates fontface as string, so we set it directly via set_edge_aes
  net <- setup_positioned_network(3)
  net$network$set_edge_aes(list(
    labels = c("a", "b"),
    label_fontface = 2  # numeric, bypassing sn_edges validation
  ))
  result <- with_temp_png({
    grid::grid.newpage()
    vp <- grid::viewport(width = 0.9, height = 0.9)
    grid::pushViewport(vp)
    grobs <- cograph:::render_edge_labels_grid(net$network)
    grid::grid.draw(grobs)
    grid::popViewport()
  })
  expect_true(TRUE)
})

test_that("render_edge_labels_grid handles unknown fontface default (line 598)", {
  # Line 598: unknown fontface string -> default to 1
  net <- setup_positioned_network(3)
  net$network$set_edge_aes(list(
    labels = c("a", "b"),
    label_fontface = "unknown_face"  # unknown -> default 1
  ))
  result <- with_temp_png({
    grid::grid.newpage()
    vp <- grid::viewport(width = 0.9, height = 0.9)
    grid::pushViewport(vp)
    grobs <- cograph:::render_edge_labels_grid(net$network)
    grid::grid.draw(grobs)
    grid::popViewport()
  })
  expect_true(TRUE)
})

# ============================================
# render-edges.R: Self-loop skip in label rendering (lines 643, 720-721)
# ============================================

test_that("render_edge_labels_grid skips self-loops for labels (lines 643, 720-721)", {
  # Line 643: from_i == to_i -> next in reciprocal detection
  # Lines 720-721: from_idx == to_idx -> nullGrob + next for labels
  mat <- matrix(0, 3, 3)
  mat[1, 1] <- 0.5  # self-loop
  mat[1, 2] <- 0.8
  mat[2, 1] <- 0.6  # reciprocal
  mat[2, 3] <- 0.4
  expect_soplot_works(mat, edge_labels = TRUE, show_arrows = TRUE)
})

# ============================================
# render-edges.R: Force curve mode in labels (line 692)
# ============================================

test_that("render_edge_labels_grid force curve mode skips self-loops (line 692)", {
  # Line 692: is_reciprocal[i] || from_i == to_i -> next in force mode
  mat <- matrix(0, 3, 3)
  mat[1, 1] <- 0.5  # self-loop should be skipped
  mat[1, 2] <- 0.8
  mat[2, 3] <- 0.4
  expect_soplot_works(mat, edge_labels = TRUE, curves = "force")
})

# ============================================
# render-edges.R: Zero-length edge labels (lines 747-748)
# ============================================

test_that("render_edge_labels_grid handles zero-length edge (lines 747-748)", {
  # Lines 746-748: len == 0 -> nullGrob
  # Nodes at same position create zero-length edge
  net <- setup_positioned_network(3)
  # Put two nodes at same position so edge between them has len=0
  nodes <- net$network$get_nodes()
  nodes$x <- c(0.5, 0.5, 0.8)  # nodes 1 and 2 at same x
  nodes$y <- c(0.5, 0.5, 0.8)  # nodes 1 and 2 at same y
  net$network$set_nodes(nodes)
  net$network$set_edge_aes(list(labels = c("a", "b")))
  result <- with_temp_png({
    grid::grid.newpage()
    vp <- grid::viewport(width = 0.9, height = 0.9)
    grid::pushViewport(vp)
    grobs <- cograph:::render_edge_labels_grid(net$network)
    grid::grid.draw(grobs)
    grid::popViewport()
  })
  expect_true(TRUE)
})

# ============================================
# render-edges.R: Shadow rendering (lines 842-850)
# ============================================

test_that("render_edge_labels_grid renders shadows (lines 842-850)", {
  # Lines 840-850: label_shadows[i] == TRUE -> shadow text grob
  mat <- create_test_matrix(3, weighted = TRUE)
  net <- cograph(mat) |>
    sn_edges(
      labels = TRUE,
      label_shadow = TRUE,
      label_shadow_color = "gray30",
      label_shadow_offset = 0.8,
      label_shadow_alpha = 0.4
    )
  expect_soplot_works(net)
})

# ============================================
# render-grid.R: tna object handling (lines 281-299)
# ============================================

test_that("soplot tna object handling (lines 281-299)", {
  # Lines 280-299: tna object handling
  skip_if_not_installed("tna")
  # Create a tna object and pass it to soplot
  data <- data.frame(
    s1 = c("A", "B", "C", "A", "B"),
    s2 = c("B", "C", "A", "C", "A"),
    s3 = c("C", "A", "B", "B", "C")
  )
  model <- tna::build_model(data, type = "relative")
  # soplot with tna + user override (title) triggers lines 292-294
  result <- tryCatch({
    with_temp_png(soplot(model, title = "TNA Test"))
    TRUE
  }, error = function(e) { message(e$message); FALSE })
  expect_true(result)
})

# ============================================
# render-grid.R: Duplicate edge error (lines 353-381)
# ============================================

test_that("soplot duplicate edge error message (lines 353-368)", {
  # Lines 353-368: edge_duplicates is NULL with duplicate edges -> error
  # Must create an undirected network with duplicate edges.
  # Build a cograph network explicitly undirected and add duplicate edges.
  mat <- matrix(0, 3, 3)
  mat[1, 2] <- 0.5
  mat[2, 1] <- 0.5
  mat[2, 3] <- 0.3
  mat[3, 2] <- 0.3
  net <- cograph(mat)
  # Manually add a duplicate edge
  edges_df <- net$network$get_edges()
  dup_edge <- edges_df[1, , drop = FALSE]
  dup_edge$weight <- 0.2
  edges_df <- rbind(edges_df, dup_edge)
  net$network$set_edges(edges_df)
  expect_error(
    with_temp_png(soplot(net)),
    "duplicate edge"
  )
})

test_that("soplot duplicate edge aggregation (lines 370-371)", {
  # Lines 370-371: edge_duplicates specified -> aggregate
  mat <- matrix(0, 3, 3)
  mat[1, 2] <- 0.5
  mat[2, 1] <- 0.5
  mat[2, 3] <- 0.3
  mat[3, 2] <- 0.3
  net <- cograph(mat)
  # Manually add a duplicate edge
  edges_df <- net$network$get_edges()
  dup_edge <- edges_df[1, , drop = FALSE]
  dup_edge$weight <- 0.2
  edges_df <- rbind(edges_df, dup_edge)
  net$network$set_edges(edges_df)
  expect_soplot_works(net, edge_duplicates = "mean")
})

test_that("soplot labels length mismatch error (lines 379-381)", {
  # Lines 379-381: labels length mismatch -> error
  mat <- create_test_matrix(3)
  expect_error(
    with_temp_png(soplot(mat, labels = c("A", "B"))),
    "labels length"
  )
})

# ============================================
# render-grid.R: donut_fill as list (line 435)
# ============================================

test_that("soplot donut_fill as list passes through (line 435)", {
  # Line 435: donut_fill is already a list -> effective_donut_values = donut_fill
  mat <- create_test_matrix(3)
  expect_soplot_works(mat, donut_fill = list(0.3, 0.7, 0.5))
})

# ============================================
# render-grid.R: donut_color list with 2*n_nodes (line 447)
# ============================================

test_that("soplot donut_color list with 2*n_nodes entries (line 447)", {
  # Line 445-447: donut_color is list with length == 2 * n_nodes
  mat <- create_test_matrix(3)
  # 3 nodes * 2 = 6 entries: (fill1, bg1, fill2, bg2, fill3, bg3)
  expect_soplot_works(
    mat,
    donut_fill = c(0.5, 0.7, 0.3),
    donut_color = as.list(c("red", "gray90", "blue", "gray90", "green", "gray90"))
  )
})

# ============================================
# render-grid.R: Zero range layout scaling (lines 611-612)
# ============================================

test_that("soplot handles zero range layout (lines 611-612)", {
  # Lines 610-612: max_range <= 1e-10 -> all nodes at center
  # Create a network where all nodes have same position
  # Must set BOTH nodes AND layout_coords to identical positions,
  # because clone_network() calls set_layout_coords which overwrites node positions
  mat <- matrix(c(0, 1, 1, 0), 2, 2)
  net <- cograph(mat)
  # Set both layout coords and nodes to identical positions
  identical_layout <- data.frame(x = c(0.5, 0.5), y = c(0.5, 0.5))
  net$network$set_layout_coords(identical_layout)
  # Verify positions are identical
  nodes <- net$network$get_nodes()
  expect_equal(nodes$x[1], nodes$x[2])
  result <- tryCatch({
    with_temp_png(soplot(net))
    TRUE
  }, error = function(e) { message(e$message); FALSE })
  expect_true(result)
})

# ============================================
# render-grid.R: create_grid_grob error (line 736)
# ============================================

test_that("create_grid_grob errors on non-cograph_network (line 736)", {
  # Line 735-736: non-cograph_network -> error
  expect_error(
    cograph:::create_grid_grob("not_a_network"),
    "must be a cograph_network"
  )
})

# ============================================
# render-grid.R: render_legend_grid (lines 790, 800, 818)
# ============================================

test_that("render_legend_grid returns empty for no nodes (line 790)", {
  # Line 790: nrow(nodes) == 0 -> gList()
  # Call render_legend_grid directly with empty network
  net <- cograph:::CographNetwork$new()
  net$set_nodes(data.frame(id = integer(0), label = character(0),
                           x = numeric(0), y = numeric(0),
                           stringsAsFactors = FALSE))
  result <- with_temp_png({
    grid::grid.newpage()
    grobs <- cograph:::render_legend_grid(net)
    grid::grid.draw(grobs)
  })
  expect_true(TRUE)
})

test_that("render_legend_grid uses label as fallback name (line 800)", {
  # Line 800: nodes$name is NULL -> use nodes$label
  # Call directly with network where nodes have no name column
  net <- cograph:::CographNetwork$new()
  nodes_df <- data.frame(id = 1:3, label = c("A", "B", "C"),
                         x = c(0.3, 0.5, 0.7), y = c(0.3, 0.5, 0.7),
                         stringsAsFactors = FALSE)
  # Remove name column if it exists
  nodes_df$name <- NULL
  net$set_nodes(nodes_df)
  result <- with_temp_png({
    grid::grid.newpage()
    vp <- grid::viewport(width = 0.9, height = 0.9)
    grid::pushViewport(vp)
    grobs <- cograph:::render_legend_grid(net)
    grid::grid.draw(grobs)
    grid::popViewport()
  })
  expect_true(TRUE)
})

test_that("render_legend_grid handles n_items == 0 (line 818)", {
  # Line 818: after unique() if n_items == 0 -> gList()
  # Create a network with nodes but where unique() produces 0 rows
  # This is hard to trigger since nodes always produce at least 1 unique pair
  # But we test through soplot to at least exercise the legend code
  mat <- create_test_matrix(3)
  expect_soplot_works(mat, legend = TRUE)
})

# ============================================
# render-grid.R: render_legend_grid default position (lines 846-847)
# ============================================

test_that("render_legend_grid handles unknown position (lines 846-847)", {
  # Lines 844-847: else branch (unknown position) -> defaults to topright
  mat <- create_test_matrix(3)
  net <- cograph(mat) |> sn_nodes(size = 0.05)
  result <- tryCatch({
    with_temp_png({
      soplot(net, legend = TRUE, legend_position = "center")
    })
    TRUE
  }, error = function(e) FALSE)
  expect_true(result)
})

# ============================================
# render-nodes.R: Direct testing via render_nodes_grid
# ============================================

test_that("render_nodes_grid returns gList for 0 nodes (line 20)", {
  # Line 20: n == 0 -> gList()
  # Call render_nodes_grid directly with empty network
  net <- cograph:::CographNetwork$new()
  net$set_nodes(data.frame(id = integer(0), label = character(0),
                           x = numeric(0), y = numeric(0),
                           stringsAsFactors = FALSE))
  result <- with_temp_png({
    grid::grid.newpage()
    grobs <- cograph:::render_nodes_grid(net)
    grid::grid.draw(grobs)
  })
  expect_true(TRUE)
})

test_that("render_nodes_grid donut_shape NULL fallback to circle (line 101)", {
  # Line 98-101: donut_shape is NULL -> "circle"
  # Must call render_nodes_grid directly because soplot always sets donut_shape
  net <- setup_positioned_network(3)
  net$network$set_node_aes(list(
    donut_values = list(0.5, 0.7, 0.3),
    size = 0.05
  ))
  # donut_shape is NOT set -> NULL -> line 101 returns "circle"
  result <- tryCatch({ render_nodes_directly(net); TRUE }, error = function(e) FALSE)
  expect_true(result)
})

test_that("render_nodes_grid donut_shape length >= i (line 99)", {
  # Line 99: donut_shape length >= i -> use donut_shape[i]
  # Use 4 nodes with 2 donut_shapes: i=1,2 hit TRUE branch (length >= i), i=3,4 hit FALSE (fallback to [1])
  net <- setup_positioned_network(4)
  net$network$set_node_aes(list(
    donut_values = list(0.5, 0.7, 0.3, 0.8),
    donut_shape = c("hexagon", "square"),  # length 2: hits both branches for 4 nodes
    size = 0.05
  ))
  result <- tryCatch({ render_nodes_directly(net); TRUE }, error = function(e) FALSE)
  expect_true(result)
})

test_that("render_nodes_grid non-list donut_colors in overlay (lines 119-120)", {
  # Lines 117-120: donut_colors is not a list -> use [1]
  net <- setup_positioned_network(3)
  net$network$set_node_aes(list(
    donut_values = list(0.5, 0.7, 0.3),
    donut_colors = "steelblue",  # not a list
    size = 0.05
  ))
  result <- tryCatch({ render_nodes_directly(net); TRUE }, error = function(e) FALSE)
  expect_true(result)
})

test_that("render_nodes_grid list donut_colors in overlay (lines 117-118)", {
  # Lines 117-118: donut_colors is a list with enough elements
  net <- setup_positioned_network(3)
  net$network$set_node_aes(list(
    donut_values = list(0.5, 0.7, 0.3),
    donut_colors = list("red", "blue", "green"),
    size = 0.05
  ))
  result <- tryCatch({ render_nodes_directly(net); TRUE }, error = function(e) FALSE)
  expect_true(result)
})

test_that("render_nodes_grid explicit donut without values defaults to 1.0 (line 163)", {
  # Line 162-163: shape is "donut" but no donut_values -> values = 1.0
  net <- setup_positioned_network(3)
  net$network$set_node_aes(list(
    shape = "donut",
    size = 0.05
    # No donut_values -> defaults to 1.0
  ))
  result <- tryCatch({ render_nodes_directly(net); TRUE }, error = function(e) FALSE)
  expect_true(result)
})

test_that("render_nodes_grid explicit donut with list donut_colors (lines 167-168)", {
  # Lines 167-168: shape is "donut", donut_colors is list with enough elements
  net <- setup_positioned_network(3)
  net$network$set_node_aes(list(
    shape = "donut",
    donut_values = list(0.5, 0.7, 0.3),
    donut_colors = list("red", "blue", "green"),
    size = 0.05
  ))
  result <- tryCatch({ render_nodes_directly(net); TRUE }, error = function(e) FALSE)
  expect_true(result)
})

test_that("render_nodes_grid explicit donut non-list donut_colors (lines 169-170)", {
  # Lines 169-170: shape is "donut", donut_colors is not a list
  net <- setup_positioned_network(3)
  net$network$set_node_aes(list(
    shape = "donut",
    donut_values = list(0.5, 0.7, 0.3),
    donut_colors = "maroon",  # not a list
    size = 0.05
  ))
  result <- tryCatch({ render_nodes_directly(net); TRUE }, error = function(e) FALSE)
  expect_true(result)
})

test_that("render_nodes_grid donut_shape[1] fallback for explicit donut (line 181)", {
  # Line 178-181: donut_shape length < i -> use [1]
  net <- setup_positioned_network(4)
  net$network$set_node_aes(list(
    shape = "donut",
    donut_values = list(0.3, 0.5, 0.7, 0.9),
    donut_shape = "square",  # length 1, should be recycled
    size = 0.05
  ))
  result <- tryCatch({ render_nodes_directly(net); TRUE }, error = function(e) FALSE)
  expect_true(result)
})

test_that("render_nodes_grid passes value_format parameter (line 199)", {
  # Line 198-199: aes$donut_value_format is set
  net <- setup_positioned_network(3)
  net$network$set_node_aes(list(
    shape = "donut",
    donut_values = list(0.5, 0.7, 0.3),
    donut_show_value = TRUE,
    donut_value_format = function(x) paste0(round(x * 100), "%"),
    size = 0.05
  ))
  result <- tryCatch({ render_nodes_directly(net); TRUE }, error = function(e) FALSE)
  expect_true(result)
})

# ============================================
# render-nodes.R: double_donut_pie non-list values (lines 244-245, 249-252, 259-260, 267)
# These need direct render_nodes_grid calls to bypass soplot's preprocessing
# ============================================

test_that("render_nodes_grid double_donut_pie non-list donut_values (lines 244-245)", {
  # Lines 244-245: donut_values is NOT a list -> use [i] indexing
  net <- setup_positioned_network(3)
  net$network$set_node_aes(list(
    shape = "double_donut_pie",
    donut_values = c(0.5, 0.7, 0.3),  # numeric vector, not list
    donut2_values = list(0.6, 0.4, 0.2),
    pie_values = list(c(1, 2), c(3, 1), c(2, 2)),
    pie_colors = c("red", "blue"),
    donut_inner_ratio = 0.7,
    donut2_inner_ratio = 0.4,
    size = 0.05
  ))
  result <- tryCatch({ render_nodes_directly(net); TRUE }, error = function(e) FALSE)
  expect_true(result)
})

test_that("render_nodes_grid double_donut_pie non-list donut_colors (line 252)", {
  # Line 249-252: donut_colors is not a list -> pass as is
  net <- setup_positioned_network(3)
  net$network$set_node_aes(list(
    shape = "double_donut_pie",
    donut_values = list(0.5, 0.7, 0.3),
    donut_colors = "steelblue",  # not a list
    donut2_values = list(0.6, 0.4, 0.2),
    pie_values = list(c(1, 2), c(3, 1), c(2, 2)),
    pie_colors = c("red", "blue"),
    size = 0.05
  ))
  result <- tryCatch({ render_nodes_directly(net); TRUE }, error = function(e) FALSE)
  expect_true(result)
})

test_that("render_nodes_grid double_donut_pie list donut_colors (line 250)", {
  # Line 249-250: donut_colors IS a list -> use [[i]]
  net <- setup_positioned_network(3)
  net$network$set_node_aes(list(
    shape = "double_donut_pie",
    donut_values = list(0.5, 0.7, 0.3),
    donut_colors = list("steelblue", "tomato", "forestgreen"),  # list -> hits [[i]]
    donut2_values = list(0.6, 0.4, 0.2),
    pie_values = list(c(1, 2), c(3, 1), c(2, 2)),
    pie_colors = c("red", "blue"),
    size = 0.05
  ))
  result <- tryCatch({ render_nodes_directly(net); TRUE }, error = function(e) FALSE)
  expect_true(result)
})

test_that("render_nodes_grid double_donut_pie non-list donut2_values (lines 259-260)", {
  # Lines 259-260: donut2_values is NOT a list -> use [i] indexing
  net <- setup_positioned_network(3)
  net$network$set_node_aes(list(
    shape = "double_donut_pie",
    donut_values = list(0.5, 0.7, 0.3),
    donut2_values = c(0.6, 0.4, 0.2),  # numeric vector, not list
    pie_values = list(c(1, 2), c(3, 1), c(2, 2)),
    pie_colors = c("red", "blue"),
    size = 0.05
  ))
  result <- tryCatch({ render_nodes_directly(net); TRUE }, error = function(e) FALSE)
  expect_true(result)
})

test_that("render_nodes_grid double_donut_pie non-list donut2_colors (line 267)", {
  # Line 267: donut2_colors is not a list -> pass as is
  net <- setup_positioned_network(3)
  net$network$set_node_aes(list(
    shape = "double_donut_pie",
    donut_values = list(0.5, 0.7, 0.3),
    donut2_values = list(0.6, 0.4, 0.2),
    donut2_colors = "purple",  # not a list
    pie_values = list(c(1, 2), c(3, 1), c(2, 2)),
    pie_colors = c("red", "blue"),
    size = 0.05
  ))
  result <- tryCatch({ render_nodes_directly(net); TRUE }, error = function(e) FALSE)
  expect_true(result)
})

# ============================================
# render-nodes.R: render_node_labels_grid n == 0 (line 326)
# ============================================

test_that("render_node_labels_grid returns gList for 0 nodes (line 326)", {
  # Line 326: n == 0 -> gList()
  # Call render_node_labels_grid directly with empty network
  net <- cograph:::CographNetwork$new()
  net$set_nodes(data.frame(id = integer(0), label = character(0),
                           x = numeric(0), y = numeric(0),
                           stringsAsFactors = FALSE))
  result <- with_temp_png({
    grid::grid.newpage()
    grobs <- cograph:::render_node_labels_grid(net)
    grid::grid.draw(grobs)
  })
  expect_true(TRUE)
})

# ============================================
# Comprehensive edge label tests
# ============================================

test_that("render_edge_labels_grid with label_bg = NA (transparent)", {
  # Exercises the NA label_bg path -> no background drawn
  mat <- create_test_matrix(3, weighted = TRUE)
  expect_soplot_works(mat, edge_labels = TRUE, edge_label_bg = NA)
})

test_that("render_edge_labels_grid with label_border circle", {
  mat <- create_test_matrix(3, weighted = TRUE)
  expect_soplot_works(
    mat,
    edge_labels = TRUE,
    edge_label_border = "circle",
    edge_label_border_color = "red"
  )
})

test_that("render_edge_labels_grid with label_border rounded", {
  mat <- create_test_matrix(3, weighted = TRUE)
  expect_soplot_works(
    mat,
    edge_labels = TRUE,
    edge_label_border = "rounded"
  )
})

test_that("render_edge_labels_grid with label_underline", {
  mat <- create_test_matrix(3, weighted = TRUE)
  expect_soplot_works(
    mat,
    edge_labels = TRUE,
    edge_label_underline = TRUE
  )
})

# ============================================
# Combined complex scenarios
# ============================================

test_that("soplot handles reciprocal edges with CI + labels + shadows", {
  # This exercises many uncovered lines in render-edges.R:
  # - reciprocal detection
  # - CI underlay for curved edges
  # - shadow rendering
  # - self-loop skip in labels
  mat <- matrix(0, 4, 4)
  mat[1, 1] <- 0.5  # self-loop
  mat[1, 2] <- 0.8
  mat[2, 1] <- 0.6  # reciprocal
  mat[2, 3] <- 0.4
  mat[3, 4] <- 0.7
  mat[4, 3] <- 0.3  # reciprocal
  n_edges <- sum(mat != 0)
  net <- cograph(mat) |>
    sn_edges(
      ci = rep(0.05, n_edges),
      ci_color = "gray50",
      ci_scale = 3,
      ci_alpha = 0.2,
      labels = TRUE,
      label_shadow = TRUE,
      label_shadow_color = "black",
      label_shadow_offset = 1.0,
      label_shadow_alpha = 0.3,
      label_fontface = "bold.italic"
    )
  expect_soplot_works(net, show_arrows = TRUE)
})

test_that("soplot with curves=force, self-loops, CI, and labels", {
  # Exercises force mode with self-loops and CI underlays
  mat <- matrix(0, 3, 3)
  mat[1, 1] <- 0.5
  mat[1, 2] <- 0.8
  mat[2, 3] <- 0.4
  n_edges <- sum(mat != 0)
  net <- cograph(mat) |>
    sn_edges(
      ci = rep(0.1, n_edges),
      curves = "force",
      labels = TRUE
    )
  expect_soplot_works(net)
})

test_that("soplot with all edge style variations on weighted network", {
  mat <- create_test_matrix(4, weighted = TRUE, symmetric = FALSE)
  for (style in c("solid", "dashed", "dotted", "longdash", "twodash")) {
    expect_soplot_works(mat, edge_style = style, show_arrows = TRUE)
  }
})

test_that("soplot bidirectional curved edges with arrows", {
  # Exercises draw_curved_edge bidirectional branch
  mat <- create_test_matrix(3, symmetric = FALSE)
  expect_soplot_works(mat, bidirectional = TRUE, show_arrows = TRUE, curvature = 0.3)
})

test_that("soplot bidirectional straight edges with arrows", {
  # Exercises draw_straight_edge bidirectional branch
  mat <- create_test_matrix(3, symmetric = FALSE)
  expect_soplot_works(mat, bidirectional = TRUE, show_arrows = TRUE, curves = FALSE)
})

# ============================================
# render-edges.R: Additional label styling paths
# ============================================

test_that("render_edge_labels_grid with label_style 'full'", {
  mat <- create_test_matrix(3, weighted = TRUE)
  n_edges <- sum(mat != 0) / 2
  net <- cograph(mat) |>
    sn_edges(
      label_style = "full",
      label_p = rep(0.05, n_edges)
    )
  expect_soplot_works(net)
})

test_that("render_edge_labels_grid with custom label_template and CI", {
  mat <- create_test_matrix(3, weighted = TRUE)
  n_edges <- sum(mat != 0) / 2
  net <- cograph(mat) |>
    sn_edges(
      label_template = "{est} [{low}, {up}]",
      ci_lower = rep(0.1, n_edges),
      ci_upper = rep(0.9, n_edges),
      label_digits = 3,
      label_ci_format = "bracket"
    )
  expect_soplot_works(net)
})

test_that("render_edge_labels_grid with label_style 'stars'", {
  mat <- create_test_matrix(3, weighted = TRUE)
  n_edges <- sum(mat != 0) / 2
  net <- cograph(mat) |>
    sn_edges(
      label_style = "stars",
      label_p = rep(0.01, n_edges)
    )
  expect_soplot_works(net)
})

# ============================================
# render-edges.R: edge label position on curved edges with reciprocals
# ============================================

test_that("render_edge_labels_grid positions labels on auto-curved reciprocals", {
  # Exercises the curve != 0 label positioning path in render_edge_labels_grid
  mat <- matrix(0, 3, 3)
  mat[1, 2] <- 0.8
  mat[2, 1] <- 0.5  # reciprocal
  mat[2, 3] <- 0.6
  expect_soplot_works(mat, edge_labels = TRUE, show_arrows = TRUE)
})

# ============================================
# render-grid.R: soplot with edge_start_style (not applicable to soplot,
# these are splot-only parameters)
# ============================================

# The edge_start_style parameters are splot-only, not soplot.
# Test splot edge_start_style for coverage of splot.R

test_that("splot edge_start_style dashed covers rendering path", {
  mat <- create_test_matrix(3, symmetric = FALSE)
  expect_splot_works(
    mat,
    show_arrows = TRUE,
    edge_start_style = "dashed",
    edge_start_length = 0.2
  )
})

test_that("splot edge_start_style dotted covers rendering path", {
  mat <- create_test_matrix(3, symmetric = FALSE)
  expect_splot_works(
    mat,
    show_arrows = TRUE,
    edge_start_style = "dotted",
    edge_start_length = 0.15
  )
})

# ============================================
# Donut shapes with non-circle base
# ============================================

test_that("soplot polygon_donut via donut_fill + donut_shape hexagon", {
  mat <- create_test_matrix(3)
  expect_soplot_works(
    mat,
    donut_fill = c(0.3, 0.6, 0.9),
    donut_shape = "hexagon",
    donut_show_value = TRUE,
    donut_value_digits = 1
  )
})

test_that("soplot polygon_donut via donut_fill + donut_shape triangle", {
  mat <- create_test_matrix(3)
  expect_soplot_works(
    mat,
    donut_fill = c(0.5, 0.7, 0.3),
    donut_shape = "triangle"
  )
})

test_that("soplot polygon_donut via donut_fill + donut_shape diamond", {
  mat <- create_test_matrix(3)
  expect_soplot_works(
    mat,
    donut_fill = c(0.4, 0.6, 0.8),
    donut_shape = "diamond"
  )
})

test_that("soplot polygon_donut via donut_fill + donut_shape pentagon", {
  mat <- create_test_matrix(3)
  expect_soplot_works(
    mat,
    donut_fill = c(0.2, 0.5, 1.0),
    donut_shape = "pentagon"
  )
})

# ============================================
# shapes-special.R coverage
# ============================================

test_that("soplot renders brain shape", {
  mat <- create_test_matrix(3)
  expect_soplot_works(mat, node_shape = "brain")
})

test_that("soplot renders cloud shape", {
  mat <- create_test_matrix(3)
  expect_soplot_works(mat, node_shape = "cloud")
})

test_that("soplot renders gear shape", {
  mat <- create_test_matrix(3)
  expect_soplot_works(mat, node_shape = "gear")
})

test_that("soplot renders database shape", {
  mat <- create_test_matrix(3)
  expect_soplot_works(mat, node_shape = "database")
})

test_that("soplot renders network shape", {
  mat <- create_test_matrix(3)
  expect_soplot_works(mat, node_shape = "network")
})

# ============================================
# Edge rendering with max/min/cut parameters
# ============================================

test_that("render_edges_grid with maximum and edge_size", {
  mat <- create_test_matrix(4, weighted = TRUE)
  expect_soplot_works(mat, maximum = 0.5, edge_size = 8)
})

test_that("render_edges_grid with edge_cutoff for fade effect", {
  mat <- create_test_matrix(4, weighted = TRUE)
  expect_soplot_works(mat, edge_cutoff = 0.4, edge_alpha = 0.7)
})

# ============================================
# render-edges.R: CI underlay with ci_arrows TRUE
# ============================================

test_that("render_edges_grid CI underlay with ci_arrows (line 289)", {
  mat <- create_test_matrix(3, weighted = TRUE)
  n_edges <- sum(mat != 0) / 2
  net <- cograph(mat) |>
    sn_edges(
      ci = rep(0.1, n_edges),
      ci_arrows = TRUE
    )
  expect_soplot_works(net)
})

test_that("render_edges_grid CI underlay with ci_style dotted", {
  mat <- create_test_matrix(3, weighted = TRUE)
  n_edges <- sum(mat != 0) / 2
  net <- cograph(mat) |>
    sn_edges(
      ci = rep(0.1, n_edges),
      ci_style = 3
    )
  expect_soplot_works(net)
})

# ============================================
# render-edges.R: various loop_rotation values
# ============================================

test_that("draw_self_loop with various rotation angles", {
  mat <- matrix(0, 3, 3)
  mat[1, 1] <- 1
  mat[2, 2] <- 1
  mat[1, 2] <- 0.5
  mat[2, 1] <- 0.5

  for (rot in c(0, pi/4, pi/2, pi, 3*pi/2)) {
    expect_soplot_works(mat, loop_rotation = rot)
  }
})

# ============================================
# render-edges.R: curve_shape and curve_pivot
# ============================================

test_that("render_edges_grid with curve_shape and curve_pivot", {
  mat <- create_test_matrix(4, symmetric = FALSE)
  expect_soplot_works(
    mat,
    curvature = 0.4,
    curve_shape = 0.5,
    curve_pivot = 0.3,
    show_arrows = TRUE
  )
})

# ============================================
# render-grid.R: soplot with multiple edge_duplicates methods
# ============================================

test_that("soplot edge_duplicates with 'first' method", {
  df <- data.frame(
    from = c(1, 2, 1),
    to = c(2, 3, 2),
    weight = c(0.5, 0.3, 0.2)
  )
  expect_soplot_works(df, edge_duplicates = "first")
})

test_that("soplot edge_duplicates with 'max' method", {
  df <- data.frame(
    from = c(1, 2, 1),
    to = c(2, 3, 2),
    weight = c(0.5, 0.3, 0.8)
  )
  expect_soplot_works(df, edge_duplicates = "max")
})

test_that("soplot edge_duplicates with 'min' method", {
  df <- data.frame(
    from = c(1, 2, 1),
    to = c(2, 3, 2),
    weight = c(0.5, 0.3, 0.2)
  )
  expect_soplot_works(df, edge_duplicates = "min")
})

# ============================================
# shapes-special.R: polygon_donut with NULL values (lines 237-238)
# ============================================

test_that("polygon_donut with NULL values defaults to 1 (lines 237-238)", {
  # Lines 236-238: values is NULL -> values = 1, colors = fill_col
  # Call draw_polygon_donut directly with NULL values
  result <- with_temp_png({
    grid::grid.newpage()
    vp <- grid::viewport(width = 0.9, height = 0.9)
    grid::pushViewport(vp)
    grob <- cograph:::draw_polygon_donut(
      x = 0.5, y = 0.5, size = 0.1,
      fill = "blue", border_color = "black", border_width = 1,
      alpha = 1, values = NULL, colors = NULL,
      donut_shape = "square", show_value = FALSE
    )
    grid::grid.draw(grob)
    grid::popViewport()
  })
  expect_true(TRUE)
})

# ============================================
# shapes-special.R: polygon_donut multi-segment with NULL colors (line 268)
# and vert_idx > n_verts break (line 278)
# ============================================

test_that("polygon_donut multi-segment with NULL colors (lines 268, 278)", {
  # Line 268: multi-segment donut with colors = NULL -> rainbow colors
  # Line 278: vert_idx > n_verts boundary break
  result <- with_temp_png({
    grid::grid.newpage()
    vp <- grid::viewport(width = 0.9, height = 0.9)
    grid::pushViewport(vp)
    grob <- cograph:::draw_polygon_donut(
      x = 0.5, y = 0.5, size = 0.1,
      fill = "blue", border_color = "black", border_width = 1,
      alpha = 1,
      values = c(3, 2, 1),  # multi-segment
      colors = NULL,         # NULL -> rainbow
      donut_shape = "square",
      show_value = FALSE
    )
    grid::grid.draw(grob)
    grid::popViewport()
  })
  expect_true(TRUE)
})

# ============================================
# shapes-special.R: polygon_donut with value_format function (line 303)
# ============================================

test_that("polygon_donut with value_format function (line 303)", {
  # Line 302-303: value_format is function -> formatted_value = value_format(center_value)
  result <- with_temp_png({
    grid::grid.newpage()
    vp <- grid::viewport(width = 0.9, height = 0.9)
    grid::pushViewport(vp)
    grob <- cograph:::draw_polygon_donut(
      x = 0.5, y = 0.5, size = 0.1,
      fill = "blue", border_color = "black", border_width = 1,
      alpha = 1,
      values = 0.75,     # single value -> progress donut
      colors = "red",
      donut_shape = "hexagon",
      show_value = TRUE,
      value_format = function(v) paste0(round(v * 100), "%")
    )
    grid::grid.draw(grob)
    grid::popViewport()
  })
  expect_true(TRUE)
})

# ============================================
# shapes-special.R: donut multi-segment with explicit colors (line 458)
# ============================================

test_that("donut multi-segment with explicit colors (line 458)", {
  # Line 457-458: donut with multiple values and colors explicitly set
  # colors is not NULL -> sapply(colors, adjust_alpha, ...)
  result <- with_temp_png({
    grid::grid.newpage()
    vp <- grid::viewport(width = 0.9, height = 0.9)
    grid::pushViewport(vp)
    grob <- cograph:::draw_donut(
      x = 0.5, y = 0.5, size = 0.1,
      fill = "blue", border_color = "black", border_width = 1,
      alpha = 1,
      values = c(3, 2, 1),       # multi-segment
      colors = c("red", "green", "blue")  # explicit colors
    )
    grid::grid.draw(grob)
    grid::popViewport()
  })
  expect_true(TRUE)
})

# ============================================
# shapes-special.R: double_donut_pie multi-segment outer ring with NULL colors (line 770)
# ============================================

test_that("double_donut_pie multi-segment outer donut with NULL colors (line 770)", {
  # Line 769-770: outer ring has length(values) > 1 and colors is NULL -> rainbow
  result <- with_temp_png({
    grid::grid.newpage()
    vp <- grid::viewport(width = 0.9, height = 0.9)
    grid::pushViewport(vp)
    grob <- cograph:::draw_double_donut_pie(
      x = 0.5, y = 0.5, size = 0.1,
      fill = "blue", border_color = "black", border_width = 1,
      alpha = 1,
      donut_values = c(3, 2, 1),   # multi-segment outer ring
      donut_colors = NULL,          # NULL -> rainbow (line 770)
      donut2_values = 0.6,          # single value inner ring
      donut2_colors = "orange",
      pie_values = c(1, 2),
      pie_colors = c("red", "green"),
      outer_inner_ratio = 0.7,
      inner_inner_ratio = 0.4
    )
    grid::grid.draw(grob)
    grid::popViewport()
  })
  expect_true(TRUE)
})

# ============================================
# shapes-special.R: multi-segment donut via render_nodes_grid
# ============================================

test_that("render_nodes_grid donut with single-value donut (line 458 via node)", {
  # Node with donut shape and scalar donut values
  mat <- create_test_matrix(3, weighted = TRUE)
  net <- cograph(mat)
  result <- tryCatch({
    with_temp_png(soplot(net, donut_values = list(0.5, 0.7, 0.3),
                         donut_colors = list("red", "green", "blue")))
    TRUE
  }, error = function(e) { message(e$message); FALSE })
  expect_true(result)
})

# ============================================
# double_donut_pie via render_nodes_grid with multi-segment outer ring, NULL colors
# ============================================

test_that("render_nodes_grid double_donut_pie with scalar donut values (line 770)", {
  # double_donut_pie with scalar donut values
  mat <- create_test_matrix(3, weighted = TRUE)
  net <- cograph(mat)
  result <- tryCatch({
    with_temp_png(soplot(net, node_shape = "double_donut_pie",
                         donut_values = list(0.7, 0.5, 0.3),
                         donut2_values = list(0.6, 0.4, 0.2),
                         pie_values = list(c(1, 2), c(3, 1), c(2, 2)),
                         pie_colors = c("red", "blue")))
    TRUE
  }, error = function(e) { message(e$message); FALSE })
  expect_true(result)
})
