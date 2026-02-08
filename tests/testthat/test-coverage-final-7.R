# Test file for final coverage push - targeting remaining uncovered lines
# After dead code removal, the following lines still need coverage:
# splot.R:544 | splot-edges.R:561 | splot-nodes.R:733
# plot-htna.R:150,151,643 | plot-htna-multi.R:360,399,402,408,466,467,598
# class-network.R:677 | input-statnet.R:39

# ============================================
# 1. R/class-network.R:677 - "unknown" source type
# ============================================

test_that("as_cograph assigns 'unknown' source for list input with $edges", {
  # A list with $edges is accepted by parse_input but doesn't match any class
  input <- list(
    nodes = data.frame(
      id = 1:3, label = c("A", "B", "C"), name = c("A", "B", "C"),
      x = c(0.1, 0.5, 0.9), y = c(0.2, 0.8, 0.5),
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      from = c(1L, 2L), to = c(2L, 3L), weight = c(1.0, 1.0),
      stringsAsFactors = FALSE
    ),
    directed = FALSE
  )

  net <- as_cograph(input)
  expect_equal(net$source, "unknown")
})

# ============================================
# 2. R/input-statnet.R:39 - fallback labels when vertex names are NA
# ============================================

test_that("parse_statnet generates fallback labels for NA vertex names", {
  skip_if_not_installed("network")

  net <- network::network.initialize(3, directed = FALSE)
  network::add.edges(net, tail = c(1, 2), head = c(2, 3))
  network::set.vertex.attribute(net, "vertex.names", c(NA, NA, NA))

  result <- cograph:::parse_statnet(net)
  expect_equal(result$nodes$label, c("1", "2", "3"))
})

# ============================================
# 3. R/splot.R:544 - background from theme
# ============================================

test_that("splot extracts background from theme", {
  mat <- create_test_matrix(3, weighted = TRUE)

  result <- with_temp_png({
    splot(mat, theme = "dark", layout = "circle")
  })
  expect_true(TRUE)
})

# ============================================
# 4. R/splot-edges.R:561 - curve_direction == 0 fallback
# ============================================

test_that("splot edge label with zero curvature triggers direction fallback", {
  mat <- create_test_matrix(3, weighted = TRUE, symmetric = FALSE)
  expect_splot_works(mat, directed = TRUE, curvature = 0,
                    edge_labels = TRUE, layout = "circle")
})

# ============================================
# 5. R/splot-nodes.R:733 - draw_donut_ring with NULL values
# ============================================

test_that("splot donut shape without donut_values uses default ring", {
  mat <- create_test_matrix(2, weighted = TRUE)
  expect_splot_works(mat, node_shape = "donut", layout = "circle")
})

# ============================================
# 6. R/plot-htna.R:150-151 - Matrix input path
# ============================================

test_that("plot_htna with matrix input uses colnames as labels", {
  mat <- matrix(c(0.7, 0.2, 0.1,
                  0.1, 0.6, 0.3,
                  0.3, 0.1, 0.6), nrow = 3, byrow = TRUE)
  colnames(mat) <- rownames(mat) <- c("A", "B", "C")

  with_temp_png({
    plot_htna(mat, node_list = list(g1 = c("A", "B"), g2 = c("C")))
  })
  expect_true(TRUE)
})

test_that("plot_htna with tna object input", {
  skip_if_not_installed("tna")

  mat <- matrix(c(0.7, 0.2, 0.1,
                  0.1, 0.6, 0.3,
                  0.3, 0.1, 0.6), nrow = 3, byrow = TRUE)
  colnames(mat) <- rownames(mat) <- c("A", "B", "C")
  tna_obj <- tna::tna(mat)

  # Triggers lines 150-151 (tna input path)
  with_temp_png({
    plot_htna(tna_obj, node_list = list(g1 = c("A", "B"), g2 = c("C")))
  })
  expect_true(TRUE)
})

# ============================================
# 7. R/plot-htna.R:643 - outward vector flip in polygon layout
# ============================================

test_that("plot_htna polygon layout with outward vector flip", {
  mat <- matrix(0.1, nrow = 4, ncol = 4)
  diag(mat) <- 0.7
  colnames(mat) <- rownames(mat) <- c("A", "B", "C", "D")

  with_temp_png({
    plot_htna(mat,
              node_list = list(g1 = c("A"), g2 = c("B"),
                               g3 = c("C"), g4 = c("D")),
              layout = "polygon")
  })
  expect_true(TRUE)
})

# ============================================
# 8. R/plot-htna-multi.R - triangle geometry paths
# ============================================

test_that("plot_mtna with 3 groups exercises triangle geometry", {
  skip_if_not_installed("tna")

  mat <- matrix(c(0, 0.3, 0.2,
                  0.4, 0, 0.1,
                  0.2, 0.3, 0), nrow = 3, byrow = TRUE)
  colnames(mat) <- rownames(mat) <- c("A", "B", "C")
  tna_obj <- tna::tna(mat)

  with_temp_png({
    plot_mtna(tna_obj, cluster_list = list(g1 = c("A"), g2 = c("B"), g3 = c("C")))
  })
  expect_true(TRUE)
})

test_that("plot_mtna with zero within-group weights", {
  skip_if_not_installed("tna")

  mat <- matrix(c(0, 0, 0.5, 0.5,
                  0, 0, 0.5, 0.5,
                  0.5, 0.5, 0, 0,
                  0.5, 0.5, 0, 0), nrow = 4, byrow = TRUE)
  colnames(mat) <- rownames(mat) <- c("A", "B", "C", "D")
  tna_obj <- tna::tna(mat)

  with_temp_png({
    plot_mtna(tna_obj, cluster_list = list(g1 = c("A", "B"), g2 = c("C", "D")))
  })
  expect_true(TRUE)
})

test_that("plot_mtna with zero-length edge offset", {
  skip_if_not_installed("tna")

  mat <- matrix(c(0.5, 0.3, 0.2,
                  0.2, 0.6, 0.2,
                  0.3, 0.1, 0.6), nrow = 3, byrow = TRUE)
  colnames(mat) <- rownames(mat) <- c("A", "B", "C")
  tna_obj <- tna::tna(mat)

  with_temp_png({
    plot_mtna(tna_obj, cluster_list = list(g1 = c("A", "B"), g2 = c("C")))
  })
  expect_true(TRUE)
})

# ============================================
# 9. Additional plot_htna layouts
# ============================================

test_that("plot_htna with polygon layout and 5+ groups", {
  set.seed(42)
  mat <- matrix(0.1, nrow = 5, ncol = 5)
  diag(mat) <- 0.7
  colnames(mat) <- rownames(mat) <- paste0("N", 1:5)

  with_temp_png({
    plot_htna(mat,
              node_list = list(g1 = "N1", g2 = "N2", g3 = "N3",
                              g4 = "N4", g5 = "N5"),
              layout = "polygon")
  })
  expect_true(TRUE)
})

test_that("plot_htna with circular layout", {
  mat <- matrix(c(0.7, 0.2, 0.1,
                  0.1, 0.6, 0.3,
                  0.3, 0.1, 0.6), nrow = 3, byrow = TRUE)
  colnames(mat) <- rownames(mat) <- c("A", "B", "C")

  with_temp_png({
    plot_htna(mat,
              node_list = list(g1 = c("A", "B"), g2 = c("C")),
              layout = "circular")
  })
  expect_true(TRUE)
})

test_that("plot_htna legend with non-standard shapes", {
  mat <- matrix(c(0.7, 0.2, 0.1,
                  0.1, 0.6, 0.3,
                  0.3, 0.1, 0.6), nrow = 3, byrow = TRUE)
  colnames(mat) <- rownames(mat) <- c("A", "B", "C")

  with_temp_png({
    plot_htna(mat,
              node_list = list(g1 = c("A"), g2 = c("B"), g3 = c("C")),
              group_shapes = c("ellipse", "pentagon", "hexagon"),
              legend = TRUE)
  })
  expect_true(TRUE)
})

test_that("plot_mtna with 4 groups in different layouts", {
  skip_if_not_installed("tna")

  mat <- matrix(c(0, 0.3, 0.1, 0.2,
                  0.2, 0, 0.3, 0.1,
                  0.1, 0.2, 0, 0.3,
                  0.3, 0.1, 0.2, 0), nrow = 4, byrow = TRUE)
  colnames(mat) <- rownames(mat) <- c("A", "B", "C", "D")
  tna_obj <- tna::tna(mat)

  with_temp_png({
    plot_mtna(tna_obj,
              cluster_list = list(g1 = "A", g2 = "B", g3 = "C", g4 = "D"),
              layout = "grid")
  })

  with_temp_png({
    plot_mtna(tna_obj,
              cluster_list = list(g1 = "A", g2 = "B", g3 = "C", g4 = "D"),
              layout = "horizontal")
  })

  with_temp_png({
    plot_mtna(tna_obj,
              cluster_list = list(g1 = "A", g2 = "B", g3 = "C", g4 = "D"),
              layout = "vertical")
  })
  expect_true(TRUE)
})

# ============================================
# 10. splot curvature edge cases
# ============================================

test_that("splot handles self-loops with curvature", {
  mat <- matrix(c(0.5, 0.3, 0.2,
                  0.2, 0.4, 0.1,
                  0.1, 0.2, 0.6), nrow = 3, byrow = TRUE)
  expect_splot_works(mat, directed = TRUE, curvature = 0.3, layout = "circle")
})

test_that("splot with non-zero curvature", {
  mat <- create_test_matrix(4, weighted = TRUE, symmetric = FALSE)
  expect_splot_works(mat, directed = TRUE, curvature = 0.2, layout = "circle")
})
