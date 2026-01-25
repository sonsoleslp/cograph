#' @title Scaling Constants
#' @description Central scaling constants for parameter alignment between splot/soplot.
#' @name scale-constants
#' @keywords internal
NULL

#' Sonnet Scaling Constants
#'
#' Central location for all scaling factors used in splot() and soplot().
#' These constants are calibrated to produce similar visual output to qgraph
#' when using equivalent parameter values.
#'
#' @details
#' The default scaling mode uses values calibrated to match qgraph visual appearance:
#' - `node_size = 6` in Sonnet should look similar to `vsize = 6` in qgraph
#' - `label_size = 1` uses cex-style multiplier (independent of node size)
#' - `arrow_size = 1` produces consistent arrows between splot and soplot
#'
#' Legacy mode preserves the original Sonnet v1.x behavior where:
#' - Node sizes used a 0.04 scale factor
#' - Label sizes were coupled to node size (vsize * 8)
#' - Arrow sizes differed between splot (0.03) and soplot (0.015)
#'
#' @format A list with the following elements:
#' \describe{
#'   \item{node_factor}{Scale factor applied to node_size parameter}
#'   \item{node_default}{Default node size when not specified}
#'   \item{label_default}{Default label size (cex multiplier)}
#'   \item{label_coupled}{Whether label size is coupled to node size}
#'   \item{edge_base}{Base edge width}
#'   \item{edge_scale}{Edge width scale factor}
#'   \item{edge_default}{Default edge width}
#'   \item{arrow_factor}{Scale factor for arrow sizes}
#'   \item{arrow_default}{Default arrow size}
#' }
#'
#' @keywords internal
SONNET_SCALE <- list(
  # Node sizing: node_size=6 should look like qgraph vsize=6
  # Calibrated: 6 * 0.015 = 0.09 user coords (similar visual size to qgraph)
  node_factor = 0.015,
  node_default = 6,

  # Label sizing: independent of node, cex-style
  # label_size=1 is the baseline (like cex=1 in base R)

  label_default = 1,
  label_coupled = FALSE,

  # Edge sizing (legacy simple parameters)
  edge_base = 0.5,
  edge_scale = 3,
  edge_default = 1,

  # Edge width scaling (qgraph-matched + extensions)
  # Output range [min_width, max_width] for scaled edges
  edge_width_range = c(0.5, 4),
  # Scaling mode: "linear", "log", "sqrt", "rank"
  edge_scale_mode = "linear",
  # Default cut = 75th percentile when NULL
  edge_cut_quantile = 0.75,
  # Default width when no weights present
  edge_width_default = 1,

  # Arrow sizing - unified between splot and soplot
  # Calibrated to produce visually balanced arrows
  arrow_factor = 0.02,
  arrow_default = 1,

  # soplot-specific: NPC coordinates
  # When converting node_size for soplot (NPC coords), use this factor
  soplot_node_factor = 0.008
)

#' Legacy Scaling Constants (Pre-v2.0 Behavior)
#'
#' Scaling constants that preserve the original Sonnet v1.x behavior.
#' Use `scaling = "legacy"` to enable these values.
#'
#' @format A list with the same structure as \code{SONNET_SCALE}
#' @keywords internal
SONNET_SCALE_LEGACY <- list(
  # Original splot values
  node_factor = 0.04,
  node_default = 3,

  # Label size coupled to node size (vsize * 8)
  label_default = NULL,
  label_coupled = TRUE,

  # Edge sizing (unchanged)
  edge_base = 0.5,
  edge_scale = 3,
  edge_default = NULL,

  # Edge width scaling (legacy uses simpler linear scaling)
  edge_width_range = c(0.5, 4),
  edge_scale_mode = "linear",
  edge_cut_quantile = 0.75,
  edge_width_default = 1,

  # Original arrow factors
  # splot used 0.03, soplot used 0.015
  arrow_factor = 0.03,
  arrow_factor_soplot = 0.015,
  arrow_default = 1,

  # soplot-specific (original behavior)
  soplot_node_factor = 0.01
)

#' Get Scaling Constants
#'
#' Returns the appropriate scaling constants based on the scaling mode.
#'
#' @param scaling Character: "default" for qgraph-matched scaling,
#'   "legacy" for pre-v2.0 behavior.
#' @return A list of scaling constants.
#' @keywords internal
get_scale_constants <- function(scaling = "default") {
  if (identical(scaling, "legacy")) {
    SONNET_SCALE_LEGACY
  } else {
    SONNET_SCALE
  }
}

#' Compute Adaptive Base Edge Size (qgraph-style)
#'
#' Calculates the base edge size that decreases with more nodes,
#' matching qgraph's adaptive esize formula.
#'
#' @param n_nodes Number of nodes in the network.
#' @param directed Whether the network is directed (directed networks use thinner edges).
#' @return Numeric base edge size.
#'
#' @details
#' The formula is: `esize = 15 * exp(-n_nodes / 90) + 1`
#'
#' This produces approximately:
#' - 10 nodes: ~14
#' - 50 nodes: ~9
#' - 100 nodes: ~6
#' - 200 nodes: ~3
#'
#' For directed networks, the esize is halved (with a minimum of 1).
#'
#' @keywords internal
compute_adaptive_esize <- function(n_nodes, directed = FALSE) {
  # qgraph formula: decreases with more nodes
  esize <- 15 * exp(-n_nodes / 90) + 1

  if (directed) {
    esize <- max(esize / 2, 1)
  }

  esize
}

#' Scale Edge Widths Based on Weights
#'
#' Unified edge width scaling function that supports multiple scaling modes,
#' two-tier cutoff system (like qgraph), and output range specification.
#'
#' @param weights Numeric vector of edge weights.
#' @param esize Base edge size. NULL uses adaptive sizing based on n_nodes.
#' @param n_nodes Number of nodes (for adaptive esize calculation).
#' @param directed Whether network is directed (affects adaptive esize).
#' @param mode Scaling mode: "linear", "log", "sqrt", or "rank".
#' @param maximum Max weight for normalization. NULL for auto-detect.
#' @param minimum Min weight threshold. Edges below this get minimum width.
#' @param cut Two-tier cutoff threshold. NULL = auto (75th percentile),
#'   0 = disabled (continuous scaling), positive number = manual threshold.
#' @param range Output width range as c(min_width, max_width).
#' @return Numeric vector of scaled edge widths.
#'
#' @details
#' ## Scaling Modes
#'
#' - **linear** (default): Direct proportional scaling, matches qgraph behavior.
#' - **log**: Logarithmic scaling for wide weight ranges. Uses log1p for stability.
#' - **sqrt**: Square root scaling for moderate compression.
#' - **rank**: Rank-based scaling for equal visual spacing regardless of weight distribution.
#'
#' ## Two-Tier System (cut parameter)
#'
#' When cut > 0, edges are divided into two tiers:
#' - Below cut: Minimal width variation (20% of range)
#' - Above cut: Full width scaling (80% of range)
#'
#' This matches qgraph's behavior where weak edges are visually de-emphasized.
#'
#' @examples
#' \dontrun{
#' weights <- c(0.1, 0.3, 0.5, 0.8, 1.0)
#'
#' # Linear scaling (default)
#' scale_edge_widths(weights, mode = "linear")
#'
#' # Log scaling for wide ranges
#' scale_edge_widths(c(0.01, 0.1, 1, 10, 100), mode = "log")
#'
#' # With two-tier cut
#' scale_edge_widths(weights, cut = 0.5)
#'
#' # Rank-based (equal visual spacing)
#' scale_edge_widths(weights, mode = "rank", cut = 0)
#' }
#'
#' @keywords internal
scale_edge_widths <- function(weights,
                               esize = NULL,
                               n_nodes = NULL,
                               directed = FALSE,
                               mode = "linear",
                               maximum = NULL,
                               minimum = 0,
                               cut = NULL,
                               range = c(0.5, 4)) {
  if (length(weights) == 0) return(numeric(0))

  # Use absolute values
  abs_weights <- abs(weights)

  # Compute adaptive esize if not provided
  if (is.null(esize)) {
    if (!is.null(n_nodes)) {
      esize <- compute_adaptive_esize(n_nodes, directed)
    } else {
      esize <- 4  # fallback default
    }
  }

  # Auto-detect maximum
  if (is.null(maximum)) {
    maximum <- max(abs_weights, na.rm = TRUE)
  }
  if (maximum == 0 || is.na(maximum)) maximum <- 1

  # Auto-compute cut (75th percentile) if NULL
  if (is.null(cut)) {
    valid_weights <- abs_weights[abs_weights > minimum & !is.na(abs_weights)]
    if (length(valid_weights) > 0) {
      cut <- stats::quantile(valid_weights, 0.75, na.rm = TRUE)
    } else {
      cut <- 0
    }
    if (is.na(cut)) cut <- 0
  }

  # Apply scaling mode to normalize weights
  normalized <- switch(mode,
    "linear" = abs_weights / maximum,
    "log" = log1p(abs_weights) / log1p(maximum),
    "sqrt" = sqrt(abs_weights) / sqrt(maximum),
    "rank" = {
      r <- rank(abs_weights, ties.method = "average", na.last = "keep")
      min_r <- min(r, na.rm = TRUE)
      max_r <- max(r, na.rm = TRUE)
      if (max_r > min_r) {
        (r - min_r) / (max_r - min_r)
      } else {
        rep(0.5, length(abs_weights))
      }
    },
    abs_weights / maximum  # fallback to linear
  )

  # Handle NA values
  normalized[is.na(normalized)] <- 0

  # Clamp to [0, 1]
  normalized <- pmin(pmax(normalized, 0), 1)

  # Apply two-tier system if cut > 0
  if (cut > 0 && cut < maximum) {
    cut_normalized <- cut / maximum

    # Below cut: map to [range[1], range[1] + small_range]
    # Above cut: map to [mid_range, range[2]]
    small_range <- (range[2] - range[1]) * 0.2
    mid_point <- range[1] + small_range

    widths <- numeric(length(weights))
    below_cut <- normalized < cut_normalized

    # Below cut: minimal width variation
    if (any(below_cut)) {
      widths[below_cut] <- range[1] + (normalized[below_cut] / cut_normalized) * small_range
    }

    # Above cut: scale to full range
    if (any(!below_cut)) {
      above_normalized <- (normalized[!below_cut] - cut_normalized) / (1 - cut_normalized)
      widths[!below_cut] <- mid_point + above_normalized * (range[2] - mid_point)
    }
  } else {
    # No cut: simple linear mapping to range
    widths <- range[1] + normalized * (range[2] - range[1])
  }

  # Apply minimum threshold (set to min width)
  widths[abs_weights < minimum | is.na(abs_weights)] <- range[1]

  # Scale by esize factor (normalize so esize=4 gives ~1x scaling)
  widths <- widths * (esize / 4)

  widths
}
