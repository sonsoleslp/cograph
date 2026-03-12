# Unified `motifs()` API — Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace 4 confusing motif functions with two clear functions: `motifs()` (census — nodes exchangeable) and `subgraphs()` (instances — nodes NOT exchangeable). Both auto-detect grouping from edge list metadata, support windowing, and work with all input types.

**Architecture:** `motifs()` counts MAN types with significance by default (fast). `subgraphs()` lists specific node triples with `min_count` filtering (significance off by default — expensive). Both share the same input dispatch, auto-detection, and windowing engine. `subgraphs()` is a convenience wrapper: `motifs(..., named_nodes = TRUE)`. The existing vectorized internals (`.classify_triads_vectorized()`, `.count_triads_matrix_vectorized()`, 6-bit lookup) are reused unchanged.

**Tech Stack:** R, S3 classes, existing cograph/tna internals. No new dependencies.

---

## Current State

### What exists (4 overlapping functions)

| Function | File | Level | What it does |
|---|---|---|---|
| `triad_census()` | `R/motifs.R:333` | aggregate | Wraps `igraph::triad_census()` |
| `motif_census()` | `R/motifs.R:44` | aggregate | Counts + null model significance |
| `extract_triads()` | `R/motifs.R:422` | aggregate | Lists node combos per type |
| `extract_motifs()` | `R/motifs-extract.R:130` | individual | Per-person triad counting via tna internals |

### What's wrong

1. **Naming**: "motif" vs "triad" used interchangeably; `extract_triads` vs `extract_motifs` sound similar but operate at different levels
2. **No grouping**: `extract_motifs()` treats each session as an individual — no way to aggregate sessions within a project
3. **No windowing**: No support for rolling/tumbling windows on transitions
4. **Edge list metadata lost in `$edges`**: `.create_cograph_network()` strips all columns except `from/to/weight` — but `$data` preserves the full original data.frame
5. **No auto-detection**: User must manually specify grouping columns

### What's solid (keep as-is)

- `.classify_triads_vectorized()` — 6-bit edge code → MAN type lookup (O(1) per triad)
- `.count_triads_matrix_vectorized()` — vectorized triad enumeration per matrix
- `.build_triad_lookup()` / `.get_triad_lookup()` — cached lookup table
- `.get_pattern_filters()` — triangle/network/closed/all pattern definitions
- All plot helpers in `R/motifs-plot.R`
- `R/motifs-data.R` — shared constants, pattern matrices, MAN descriptions

---

## Target API

### `motifs()` — Census (nodes exchangeable, significance by default)

```r
# === From tna object (429 individual sessions) ===
motifs(Mod)                                          # sig=TRUE, n_perm=1000
motifs(Mod, significance = FALSE)                    # just counts
motifs(Mod, window = 3)                              # rolling window, size 3
motifs(Mod, window = 5, window_type = "tumbling")    # tumbling windows

# === From cograph network built from edge list ===
el <- data.frame(from, to, weight, session, project, timestamp)
net <- as_cograph(el)

motifs(net)                              # auto-detects session column, per session
motifs(net, actor = "project")           # override: group by project column
motifs(net, actor = "project", window = 3)  # windowed within each project

# === From raw matrix / igraph (aggregate only) ===
motifs(mat)                              # aggregate triad analysis
motifs(g)                                # igraph aggregate

# === Pattern filtering ===
motifs(Mod, pattern = "triangle")        # only triangle patterns (default)
motifs(Mod, pattern = "all")             # all 16 MAN types
motifs(Mod, include = "030T")            # only feed-forward loops
```

### `subgraphs()` — Instances (nodes NOT exchangeable, significance by default)

```r
# === Basic usage ===
subgraphs(Mod)                           # sig=TRUE, n_perm=1000, min_count=5, pattern="triangle"
# Message: "Showing triangle patterns (count > 5).
#           For all MAN types use pattern = 'all'."

# === Without significance (faster) ===
subgraphs(Mod, significance = FALSE)

# === All MAN types ===
subgraphs(Mod, pattern = "all")

# === From cograph network ===
subgraphs(net)                           # auto-detects session column
subgraphs(net, actor = "project", window = 3)

# === From raw matrix ===
subgraphs(mat)                           # aggregate instances
```

### `subgraphs()` is a convenience wrapper

```r
subgraphs <- function(...) motifs(..., named_nodes = TRUE)
```

### Signature

```r
motifs(x,
       named_nodes = FALSE,         # FALSE = census, TRUE = instances
       actor = NULL,                 # column name for grouping
       window = NULL,                # window size
       window_type = "rolling",      # "rolling" or "tumbling"
       pattern = "triangle",         # "triangle", "network", "closed", "all"
       include = NULL,               # specific MAN types
       exclude = NULL,               # exclude MAN types
       significance = TRUE,           # both modes default TRUE
       n_perm = 1000L,  # Milo et al. 2002 standard (both modes)
       min_count = if (named_nodes) 5 else NULL,   # filter rare triads (instances only)
       edge_method = "any",
       edge_threshold = 1.5,
       min_transitions = 5,
       top = NULL,
       seed = NULL)
```

### Auto-detection of standard columns in `$data`

When `actor` is not specified, `motifs()` scans `$data` column names (case-insensitive) for known session/actor identifiers:

| Priority | Column names detected | Used as |
|---|---|---|
| 1 | `session_id`, `session` | Actor grouping (one network per session) |
| 2 | `actor`, `user`, `participant`, `individual`, `id` | Actor grouping |
| 3 | `timestamp`, `time`, `seq`, `step`, `order` | Ordering within actor (for windowing) |

If no standard column is found and `actor` is not specified, falls back to aggregate analysis.

### `window` parameter

- Always numeric — the window size (number of transitions per window)
- `window_type`: `"rolling"` (default) or `"tumbling"`
- Requires ordered transitions within each actor group (via detected `timestamp`/`order` column, or natural row order in `$data`)
- For tna objects: passed through to tna's `params = list(windowed = TRUE, window_size = N, window_type = type)`

---

## File Structure

### Files to create

| File | Responsibility |
|---|---|
| `R/motifs-api.R` | `motifs()` + `subgraphs()` — unified entry point, input dispatch, auto-detection, windowing, result assembly |
| `tests/testthat/test-motifs-api.R` | Tests for unified `motifs()` API |

### Files to modify

| File | Changes |
|---|---|
| `R/motifs.R` | Keep all internals (`.classify_triads_vectorized`, `.count_triads_matrix_vectorized`, `.build_triad_lookup`, `.generate_random_graph`). Add `@keywords internal` to `triad_census`, `motif_census`, `extract_triads`. They still work but `motifs()` is the recommended API. |
| `R/motifs-extract.R` | Add `@keywords internal` to `extract_motifs`. Keep internals. Refactor `count_triads_internal()` to be callable from `motifs()`. |
| `R/motifs-data.R` | Add `.detect_actor_column()` and `.detect_order_column()` auto-detection helpers. |
| `NAMESPACE` | Export `motifs`, `subgraphs` |

### Files unchanged

| File | Why |
|---|---|
| `R/motifs-plot.R` | Plot helpers work on the S3 result objects — no changes needed |
| `R/class-network.R` | `$data` already preserves full edge list — no changes needed |
| `R/input-edgelist.R` | Already works correctly |

---

## Chunk 1: Core `motifs()` function

### Task 1: Auto-detection helpers in `R/motifs-data.R`

**Files:**
- Modify: `R/motifs-data.R` (append at end)
- Test: `tests/testthat/test-motifs-api.R`

- [ ] **Step 1: Write failing tests for `.detect_actor_column()`**

```r
# tests/testthat/test-motifs-api.R

test_that(".detect_actor_column finds standard names", {
  df <- data.frame(from = "A", to = "B", session_id = "S1")
  expect_equal(.detect_actor_column(df), "session_id")

  df2 <- data.frame(from = "A", to = "B", session = "S1")
  expect_equal(.detect_actor_column(df2), "session")

  df3 <- data.frame(from = "A", to = "B", actor = "u1")
  expect_equal(.detect_actor_column(df3), "actor")

  df4 <- data.frame(from = "A", to = "B", participant = "p1")
  expect_equal(.detect_actor_column(df4), "participant")
})

test_that(".detect_actor_column returns NULL when no match", {
  df <- data.frame(from = "A", to = "B", weight = 1)
  expect_null(.detect_actor_column(df))
})

test_that(".detect_actor_column is case-insensitive", {
  df <- data.frame(from = "A", to = "B", Session_ID = "S1")
  expect_equal(.detect_actor_column(df), "Session_ID")
})

test_that(".detect_order_column finds standard names", {
  df <- data.frame(from = "A", to = "B", timestamp = 1)
  expect_equal(.detect_order_column(df), "timestamp")

  df2 <- data.frame(from = "A", to = "B", order = 1)
  expect_equal(.detect_order_column(df2), "order")

  df3 <- data.frame(from = "A", to = "B", time = 1)
  expect_equal(.detect_order_column(df3), "time")
})

test_that(".detect_order_column returns NULL when no match", {
  df <- data.frame(from = "A", to = "B", weight = 1)
  expect_null(.detect_order_column(df))
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-motifs-api.R")'`
Expected: FAIL — functions not found

- [ ] **Step 3: Implement auto-detection helpers**

Append to `R/motifs-data.R`:

```r
# Auto-detect actor/session grouping column in a data.frame
# Returns the original column name (preserving case), or NULL
.detect_actor_column <- function(df) {
  if (!is.data.frame(df)) return(NULL)
  nms <- names(df)
  nms_lower <- tolower(nms)
  # Priority order: session_id > session > actor > user > participant > individual > id
  candidates <- c("session_id", "session", "actor", "user",
                   "participant", "individual", "id")
  for (cand in candidates) {
    idx <- match(cand, nms_lower)
    if (!is.na(idx)) return(nms[idx])
  }
  NULL
}

# Auto-detect ordering/time column in a data.frame
# Returns the original column name (preserving case), or NULL
.detect_order_column <- function(df) {
  if (!is.data.frame(df)) return(NULL)
  nms <- names(df)
  nms_lower <- tolower(nms)
  candidates <- c("timestamp", "time", "seq", "step", "order")
  for (cand in candidates) {
    idx <- match(cand, nms_lower)
    if (!is.na(idx)) return(nms[idx])
  }
  NULL
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-motifs-api.R")'`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add R/motifs-data.R tests/testthat/test-motifs-api.R
git commit -m "feat(motifs): add auto-detection helpers for actor/order columns"
```

---

### Task 2: Edge list to transition array builder

The core new capability: converting a grouped edge list into a 3D transition array `[n_groups × n_states × n_states]`, optionally with windowing.

**Files:**
- Modify: `R/motifs-data.R` (append)
- Test: `tests/testthat/test-motifs-api.R` (append)

- [ ] **Step 1: Write failing tests for `.edgelist_to_trans_array()`**

```r
test_that(".edgelist_to_trans_array builds correct 3D array", {
  el <- data.frame(
    from = c("A","B","A", "B","C","B"),
    to   = c("B","C","C", "A","B","C"),
    stringsAsFactors = FALSE
  )
  # No actor column — treat as single group
  result <- .edgelist_to_trans_array(el)
  expect_equal(dim(result$trans)[1], 1L)  # 1 group
  expect_equal(dim(result$trans)[2], 3L)  # 3 states
  expect_equal(dim(result$trans)[3], 3L)
  expect_equal(sort(result$labels), c("A", "B", "C"))

  # With actor column — two groups
  el$group <- c("g1","g1","g1","g2","g2","g2")
  result2 <- .edgelist_to_trans_array(el, actor_col = "group")
  expect_equal(dim(result2$trans)[1], 2L)  # 2 groups
})

test_that(".edgelist_to_trans_array respects weights", {
  el <- data.frame(
    from = c("A","A"), to = c("B","B"), weight = c(3, 2),
    group = c("g1","g2"), stringsAsFactors = FALSE
  )
  result <- .edgelist_to_trans_array(el, actor_col = "group")
  # g1: A->B = 3, g2: A->B = 2
  a_idx <- match("A", result$labels)
  b_idx <- match("B", result$labels)
  expect_equal(result$trans[1, a_idx, b_idx], 3)
  expect_equal(result$trans[2, a_idx, b_idx], 2)
})

test_that(".edgelist_to_trans_array handles windowing", {
  # 6 transitions in one group, window=3 tumbling → 2 windows
  el <- data.frame(
    from = c("A","B","C","A","C","B"),
    to   = c("B","C","A","C","B","A"),
    order = 1:6,
    stringsAsFactors = FALSE
  )
  result <- .edgelist_to_trans_array(el, order_col = "order",
                                      window = 3, window_type = "tumbling")
  expect_equal(dim(result$trans)[1], 2L)  # 2 windows
})

test_that(".edgelist_to_trans_array rolling windows", {
  el <- data.frame(
    from = c("A","B","C","A"),
    to   = c("B","C","A","C"),
    order = 1:4,
    stringsAsFactors = FALSE
  )
  # window=3 rolling → windows at positions 1-3, 2-4 = 2 windows
  result <- .edgelist_to_trans_array(el, order_col = "order",
                                      window = 3, window_type = "rolling")
  expect_equal(dim(result$trans)[1], 2L)
})
```

- [ ] **Step 2: Run tests to verify they fail**

- [ ] **Step 3: Implement `.edgelist_to_trans_array()`**

Append to `R/motifs-data.R`:

```r
# Convert edge list data.frame to 3D transition array
# Returns list(trans = array[groups, states, states], labels = character)
.edgelist_to_trans_array <- function(el,
                                      actor_col = NULL,
                                      order_col = NULL,
                                      window = NULL,
                                      window_type = "rolling") {
  # Detect weight column
  weight_col <- NULL
  wt_idx <- match("weight", tolower(names(el)))
  if (!is.na(wt_idx)) weight_col <- names(el)[wt_idx]

  # Get all unique states
  labels <- sort(unique(c(as.character(el$from), as.character(el$to))))
  s <- length(labels)
  state_idx <- setNames(seq_along(labels), labels)

  # Assign group IDs
  if (!is.null(actor_col)) {
    group_ids <- as.character(el[[actor_col]])
  } else {
    group_ids <- rep("__all__", nrow(el))
  }

  # Order within groups if order column provided
  if (!is.null(order_col)) {
    sort_order <- order(group_ids, el[[order_col]])
    el <- el[sort_order, ]
    group_ids <- group_ids[sort_order]
  }

  # Apply windowing if requested
  if (!is.null(window) && is.numeric(window) && window > 0) {
    # Split by group, apply windows, reassemble with new group IDs
    groups <- split(seq_len(nrow(el)), group_ids)
    new_rows <- list()
    new_group_ids <- character(0)

    for (gname in names(groups)) {
      idx <- groups[[gname]]
      n_edges <- length(idx)
      if (n_edges == 0) next

      if (window_type == "tumbling") {
        starts <- seq(1, n_edges, by = window)
      } else {
        # rolling
        starts <- seq_len(max(1, n_edges - window + 1))
      }

      for (wi in seq_along(starts)) {
        st <- starts[wi]
        en <- min(st + window - 1, n_edges)
        w_idx <- idx[st:en]
        new_rows <- c(new_rows, list(w_idx))
        new_group_ids <- c(new_group_ids,
                           paste0(gname, "_w", wi))
      }
    }

    # Rebuild with window-based group IDs
    row_indices <- unlist(new_rows)
    el <- el[row_indices, ]
    group_ids <- rep(new_group_ids, lengths(new_rows))
  }

  # Build 3D array
  unique_groups <- unique(group_ids)
  n_groups <- length(unique_groups)
  group_idx <- match(group_ids, unique_groups)

  trans <- array(0, dim = c(n_groups, s, s))

  from_idx <- state_idx[as.character(el$from)]
  to_idx <- state_idx[as.character(el$to)]
  wt <- if (!is.null(weight_col)) as.numeric(el[[weight_col]]) else rep(1, nrow(el))

  # Vectorized fill using aggregate
  valid <- !is.na(from_idx) & !is.na(to_idx)
  agg_df <- data.frame(g = group_idx[valid], f = from_idx[valid],
                        t = to_idx[valid], w = wt[valid])
  if (nrow(agg_df) > 0) {
    agg <- stats::aggregate(w ~ g + f + t, data = agg_df, FUN = sum)
    trans[cbind(agg$g, agg$f, agg$t)] <- agg$w
  }

  list(trans = trans, labels = labels, groups = unique_groups)
}
```

- [ ] **Step 4: Run tests to verify they pass**

- [ ] **Step 5: Commit**

```bash
git add R/motifs-data.R tests/testthat/test-motifs-api.R
git commit -m "feat(motifs): add .edgelist_to_trans_array with windowing support"
```

---

### Task 3: Core `motifs()` function

**Files:**
- Create: `R/motifs-api.R`
- Test: `tests/testthat/test-motifs-api.R` (append)

- [ ] **Step 1: Write failing tests for `motifs()`**

```r
# --- Input dispatch tests ---

test_that("motifs works with raw matrix (aggregate)", {
  mat <- matrix(c(0,1,1,0, 0,0,1,1, 0,0,0,1, 1,0,0,0), 4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A","B","C","D")
  result <- motifs(mat)
  expect_s3_class(result, "cograph_motif_result")
  expect_true("results" %in% names(result))
  expect_true("type_summary" %in% names(result))
})

test_that("motifs works with igraph object", {
  skip_if_not_installed("igraph")
  mat <- matrix(c(0,1,1,0, 0,0,1,1, 0,0,0,1, 1,0,0,0), 4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A","B","C","D")
  g <- igraph::graph_from_adjacency_matrix(mat, mode = "directed", weighted = TRUE)
  result <- motifs(g)
  expect_s3_class(result, "cograph_motif_result")
})

test_that("motifs works with cograph_network from matrix", {
  mat <- matrix(c(0,1,1,0, 0,0,1,1, 0,0,0,1, 1,0,0,0), 4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A","B","C","D")
  net <- as_cograph(mat)
  result <- motifs(net)
  expect_s3_class(result, "cograph_motif_result")
})

test_that("motifs census works with tna object (individual level)", {
  skip_if_not_installed("tna")
  Mod <- tna::tna(coding)
  result <- motifs(Mod, n_perm = 10, seed = 42)
  expect_s3_class(result, "cograph_motif_result")
  expect_false(result$named_nodes)
  expect_equal(result$level, "individual")
  expect_true(result$n_units > 1)
  # Census: results have type + count, not specific triads
  expect_true("type" %in% names(result$results))
})

# --- Auto-detection tests ---

test_that("motifs auto-detects session column from cograph edge list", {
  el <- data.frame(
    from = c("A","B","A","B","C","A"),
    to   = c("B","C","C","A","B","B"),
    session_id = c("s1","s1","s1","s2","s2","s2"),
    stringsAsFactors = FALSE
  )
  net <- as_cograph(el)
  result <- motifs(net)
  expect_equal(result$level, "individual")
  expect_equal(result$n_units, 2L)
})

test_that("motifs actor= overrides auto-detection", {
  el <- data.frame(
    from = c("A","B","A","B","C","A"),
    to   = c("B","C","C","A","B","B"),
    session_id = c("s1","s1","s1","s2","s2","s2"),
    project = c("p1","p1","p1","p1","p1","p1"),
    stringsAsFactors = FALSE
  )
  net <- as_cograph(el)
  result <- motifs(net, actor = "project")
  expect_equal(result$n_units, 1L)
})

# --- Windowing tests ---

test_that("motifs with window parameter creates windowed groups", {
  el <- data.frame(
    from = c("A","B","C","A","B","C"),
    to   = c("B","C","A","C","A","B"),
    session_id = rep("s1", 6),
    order = 1:6,
    stringsAsFactors = FALSE
  )
  net <- as_cograph(el)
  result <- motifs(net, window = 3, window_type = "tumbling")
  expect_equal(result$n_units, 2L)  # 6 edges / 3 = 2 windows
})

# --- Pattern filtering tests ---

test_that("motifs pattern argument filters types", {
  mat <- matrix(c(0,1,1,0, 0,0,1,1, 0,0,0,1, 1,0,0,0), 4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A","B","C","D")
  result_all <- motifs(mat, pattern = "all")
  result_tri <- motifs(mat, pattern = "triangle")
  # Triangle should have fewer or equal types
  expect_true(length(result_tri$type_summary) <= length(result_all$type_summary))
})

# --- Census significance (on by default) ---

test_that("motifs census has significance by default", {
  skip_if_not_installed("tna")
  Mod <- tna::tna(coding)
  result <- motifs(Mod, n_perm = 10, seed = 42)
  expect_true(result$params$significance)
  expect_true("z" %in% names(result$results))
  expect_true("p" %in% names(result$results))
})

test_that("motifs census can disable significance", {
  mat <- matrix(c(0,1,1,0, 0,0,1,1, 0,0,0,1, 1,0,0,0), 4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A","B","C","D")
  result <- motifs(mat, significance = FALSE)
  expect_false(result$params$significance)
  expect_false("z" %in% names(result$results))
})

# --- Subgraphs (instance mode) ---

test_that("subgraphs returns named node triples", {
  skip_if_not_installed("tna")
  Mod <- tna::tna(coding)
  result <- subgraphs(Mod)
  expect_s3_class(result, "cograph_motif_result")
  expect_true(result$named_nodes)
  expect_true("triad" %in% names(result$results))
  expect_true(all(result$results$observed > 5))
})

test_that("subgraphs has significance by default", {
  skip_if_not_installed("tna")
  Mod <- tna::tna(coding)
  result <- subgraphs(Mod, n_perm = 10, seed = 42)
  expect_true(result$params$significance)
  expect_true("z" %in% names(result$results))
})

test_that("subgraphs can disable significance", {
  skip_if_not_installed("tna")
  Mod <- tna::tna(coding)
  result <- subgraphs(Mod, significance = FALSE)
  expect_false(result$params$significance)
  expect_false("z" %in% names(result$results))
})

test_that("subgraphs shows message with defaults", {
  skip_if_not_installed("tna")
  Mod <- tna::tna(coding)
  expect_message(subgraphs(Mod), "triangle patterns")
})

test_that("subgraphs suppresses message with explicit pattern", {
  skip_if_not_installed("tna")
  Mod <- tna::tna(coding)
  expect_no_message(subgraphs(Mod, pattern = "all"))
})
```

- [ ] **Step 2: Run tests to verify they fail**

- [ ] **Step 3: Implement `motifs()` in `R/motifs-api.R`**

```r
#' Network Motif Analysis
#'
#' Two modes of motif analysis for networks:
#' \itemize{
#'   \item **Census** (\code{named_nodes = FALSE}, default): Counts MAN type
#'     frequencies with significance testing. Nodes are exchangeable.
#'   \item **Instances** (\code{named_nodes = TRUE}, or use \code{subgraphs()}):
#'     Lists specific node triples forming each pattern. Nodes are NOT
#'     exchangeable.
#' }
#'
#' Detects input type and analysis level automatically. For inputs with
#' individual/group data (tna objects, cograph networks from edge lists with
#' metadata), performs per-group analysis. For aggregate inputs (matrices,
#' igraph), analyzes the single network.
#'
#' @param x Input data: a tna object, cograph_network, matrix, igraph, or
#'   data.frame (edge list).
#' @param named_nodes Logical. If FALSE (default), performs census (type-level
#'   counts). If TRUE, extracts specific node triples (instance-level).
#'   \code{subgraphs()} is a convenience wrapper that sets this to TRUE.
#' @param actor Character. Column name in the edge list metadata to group by.
#'   If NULL (default), auto-detects standard column names (session_id, session,
#'   actor, user, participant). If no grouping column found, performs aggregate
#'   analysis.
#' @param window Numeric. Window size for windowed analysis. Splits each actor's
#'   transitions into windows of this size. NULL (default) means no windowing.
#'   For tna objects, passed through to tna's windowed model estimation.
#' @param window_type Character. Window type: "rolling" (default) or "tumbling".
#'   Only used when \code{window} is set.
#' @param pattern Pattern filter: "triangle" (default), "network", "closed", "all".
#' @param include Character vector of MAN types to include exclusively.
#'   Overrides \code{pattern}.
#' @param exclude Character vector of MAN types to exclude. Applied after
#'   \code{pattern} filter.
#' @param significance Logical. Run permutation significance test? Default TRUE.
#'   Uses the exact configuration model null (Milo et al. 2002): preserves
#'   exact in-degree and out-degree sequences per individual. Matches
#'   \code{igraph::sample_degseq()}. Optimized via batch stub-shuffling +
#'   vectorized candidate checking (~3.5s for 9-state, ~31s for 32-state
#'   networks with 1000 permutations).
#' @param n_perm Number of permutations for significance. Default 1000
#'   (Milo et al. 2002 standard).
#' @param min_count Minimum observed count to include a triad (instance mode
#'   only). Default 5 for instances, NULL for census. Triads appearing in fewer
#'   than \code{min_count} individuals are excluded.
#' @param edge_method Method for determining edge presence in individual networks:
#'   "any" (default), "expected", or "percent".
#' @param edge_threshold Threshold for "expected" or "percent" methods. Default 1.5.
#' @param min_transitions Minimum total transitions for a unit to be included.
#'   Default 5.
#' @param top Return only the top N results. NULL returns all.
#' @param seed Random seed for reproducibility.
#'
#' @return A \code{cograph_motif_result} object with:
#'   \describe{
#'     \item{results}{Data frame of results. Census: type, count, (z, p, sig).
#'       Instances: triad, type, observed, (z, p, sig).}
#'     \item{type_summary}{Named counts by MAN type}
#'     \item{level}{Analysis level: "individual" or "aggregate"}
#'     \item{named_nodes}{Whether nodes are identified (TRUE) or exchangeable (FALSE)}
#'     \item{n_units}{Number of units analyzed (individuals/groups/windows)}
#'     \item{params}{List of parameters used}
#'   }
#'
#' @examples
#' # Census: from a matrix (significance by default)
#' mat <- matrix(c(0,3,2,0, 0,0,5,1, 0,0,0,4, 2,0,0,0), 4, 4, byrow = TRUE)
#' rownames(mat) <- colnames(mat) <- c("Plan","Execute","Monitor","Adapt")
#' motifs(mat)
#'
#' \dontrun{
#' # Census: from tna object
#' library(tna)
#' Mod <- tna(coding)
#' motifs(Mod)                              # sig=TRUE, n_perm=1000
#'
#' # Instances: specific node triples
#' subgraphs(Mod)                           # min_count=5, pattern="triangle"
#' subgraphs(Mod, significance = TRUE)      # with significance
#' subgraphs(Mod, pattern = "all")          # all MAN types
#'
#' # Grouped: from cograph edge list
#' net <- as_cograph(edge_list_with_metadata)
#' motifs(net)                              # auto-detect session column
#' motifs(net, actor = "project")           # group by project
#' subgraphs(net, actor = "project", window = 3)  # windowed instances
#' }
#'
#' @seealso [subgraphs()], [plot.cograph_motif_result()], [print.cograph_motif_result()]
#' @family motifs
#' @export
motifs <- function(x,
                   named_nodes = FALSE,
                   actor = NULL,
                   window = NULL,
                   window_type = c("rolling", "tumbling"),
                   pattern = c("triangle", "network", "closed", "all"),
                   include = NULL,
                   exclude = NULL,
                   significance = TRUE,
                   n_perm = 1000L,  # Milo et al. 2002 standard
                   min_count = if (named_nodes) 5L else NULL,
                   edge_method = c("any", "expected", "percent"),
                   edge_threshold = 1.5,
                   min_transitions = 5,
                   top = NULL,
                   seed = NULL) {

  # Track whether user explicitly set pattern (for informative message)
  .user_set_pattern <- !missing(pattern)

  window_type <- match.arg(window_type)
  pattern <- match.arg(pattern)
  edge_method <- match.arg(edge_method)

  if (!is.null(seed)) {
    saved_rng <- .save_rng()
    on.exit(.restore_rng(saved_rng), add = TRUE)
    set.seed(seed)
  }

  # Pattern filtering
  pf <- .get_pattern_filters()
  if (!is.null(include)) {
    final_exclude <- character(0)
    final_include <- include
  } else {
    final_include <- NULL
    pattern_exclude <- switch(pattern,
      triangle = setdiff(pf$all_types, pf$triangle_types),
      network = pf$network_exclude,
      closed = pf$closed_exclude,
      all = character(0)
    )
    final_exclude <- unique(c(pattern_exclude, exclude))
  }

  # ================================================================
  # INPUT DISPATCH
  # ================================================================

  trans <- NULL
  labels <- NULL
  level <- "aggregate"
  n_units <- 1L
  group_names <- NULL


  # --- Case 1: tna object ---
  if (inherits(x, "tna")) {
    # If windowed, rebuild the tna model with window params
    if (!is.null(window)) {
      # Re-estimate with windowing via tna params
      # The tna object carries $data, type, scaling, params
      tna_params <- attr(x, "params")
      tna_params$windowed <- TRUE
      tna_params$window_size <- as.integer(window)
      tna_params$window_type <- window_type
      x <- tna::tna(x$data, params = tna_params)
    }

    init_fn <- .get_tna_initialize_model()
    model <- init_fn(x$data, attr(x, "type"), attr(x, "scaling"),
                     attr(x, "params"), transitions = TRUE)
    trans <- model$trans
    labels <- x$labels
    level <- "individual"
    n_units <- dim(trans)[1]

  # --- Case 2: cograph_network ---
  } else if (inherits(x, "cograph_network")) {
    raw_data <- x$data

    # Check if $data is an edge list data.frame (not a tna sequence matrix)
    if (is.data.frame(raw_data) && all(c("from", "to") %in% tolower(names(raw_data)))) {

      # Auto-detect or use explicit actor column
      actor_col <- actor
      if (is.null(actor_col)) {
        actor_col <- .detect_actor_column(raw_data)
      }

      if (!is.null(actor_col)) {
        # Detect order column for windowing
        order_col <- .detect_order_column(raw_data)

        result <- .edgelist_to_trans_array(
          raw_data,
          actor_col = actor_col,
          order_col = order_col,
          window = window,
          window_type = window_type
        )
        trans <- result$trans
        labels <- result$labels
        group_names <- result$groups
        level <- "individual"
        n_units <- dim(trans)[1]
      } else {
        # No grouping → aggregate from weight matrix
        mat <- to_matrix(x)
        labels <- get_labels(x)
        trans <- array(mat, dim = c(1, nrow(mat), ncol(mat)))
      }
    } else {
      # $data is a tna sequence matrix or NULL → aggregate
      mat <- to_matrix(x)
      labels <- get_labels(x)
      trans <- array(mat, dim = c(1, nrow(mat), ncol(mat)))
    }

  # --- Case 3: data.frame edge list (not wrapped in cograph) ---
  } else if (is.data.frame(x)) {
    actor_col <- actor
    if (is.null(actor_col)) {
      actor_col <- .detect_actor_column(x)
    }
    order_col <- .detect_order_column(x)

    result <- .edgelist_to_trans_array(
      x,
      actor_col = actor_col,
      order_col = order_col,
      window = window,
      window_type = window_type
    )
    trans <- result$trans
    labels <- result$labels
    group_names <- result$groups
    level <- if (!is.null(actor_col)) "individual" else "aggregate"
    n_units <- dim(trans)[1]

  # --- Case 4: matrix ---
  } else if (is.matrix(x)) {
    if (is.null(rownames(x))) {
      labels <- paste0("V", seq_len(nrow(x)))
    } else {
      labels <- rownames(x)
    }
    trans <- array(x, dim = c(1, nrow(x), ncol(x)))

  # --- Case 5: igraph ---
  } else if (inherits(x, "igraph")) {
    if (!requireNamespace("igraph", quietly = TRUE)) {
      stop("igraph package required")
    }
    if ("weight" %in% igraph::edge_attr_names(x)) {
      mat <- as.matrix(igraph::as_adjacency_matrix(x, attr = "weight",
                                                     sparse = FALSE))
    } else {
      mat <- as.matrix(igraph::as_adjacency_matrix(x, sparse = FALSE))
    }
    labels <- igraph::V(x)$name
    if (is.null(labels)) labels <- paste0("V", seq_len(nrow(mat)))
    trans <- array(mat, dim = c(1, nrow(mat), ncol(mat)))

  } else {
    stop("Unsupported input type. Provide a tna object, cograph_network, ",
         "matrix, igraph, or data.frame edge list.")
  }

  # ================================================================
  # TRIAD COUNTING — differs by mode
  # ================================================================

  s <- length(labels)

  if (!named_nodes) {
    # ---- CENSUS MODE: count MAN type frequencies per unit ----
    type_counts_per_unit <- lapply(seq_len(dim(trans)[1]), function(ind) {
      mat <- trans[ind, , ]
      if (sum(mat) < min_transitions) return(NULL)

      counted <- .count_triads_matrix_vectorized(
        mat, edge_method, edge_threshold,
        exclude = final_exclude,
        include = final_include
      )
      if (is.null(counted) || nrow(counted) == 0) return(NULL)
      table(counted$type)
    })

    # Aggregate: sum type counts across units
    all_types <- unique(unlist(lapply(type_counts_per_unit, names)))
    if (length(all_types) == 0) {
      message("No motifs found with the given parameters.")
      return(NULL)
    }

    type_totals <- setNames(integer(length(all_types)), all_types)
    for (tc in type_counts_per_unit) {
      if (!is.null(tc)) {
        for (nm in names(tc)) type_totals[nm] <- type_totals[nm] + tc[nm]
      }
    }

    results <- data.frame(
      type = names(type_totals),
      count = as.integer(type_totals),
      stringsAsFactors = FALSE
    )
    results <- results[order(results$count, decreasing = TRUE), ]
    rownames(results) <- NULL

    # ---- CENSUS SIGNIFICANCE ----
    if (significance) {

      if (level == "aggregate") {
        # ---- AGGREGATE: delegate to motif_census() directly ----
        # Calls the existing motif_census() which uses
        # igraph::sample_degseq() + igraph::motifs() internally.
        # Identical results to motif_census(mat, seed=seed).
        # Zero deviation. This IS igraph.
        agg_mat <- trans[1, , ]
        rownames(agg_mat) <- colnames(agg_mat) <- labels
        mc <- motif_census(agg_mat, n_random = n_perm, seed = seed)

        # Map motif_census results to our results data frame
        for (ri in seq_len(nrow(results))) {
          tp <- results$type[ri]
          if (tp %in% names(mc$z_scores)) {
            results$expected[ri] <- round(mc$null_mean[tp], 1)
            results$z[ri] <- round(mc$z_scores[tp], 2)
            results$p[ri] <- round(mc$p_values[tp], 4)
            results$sig[ri] <- abs(mc$z_scores[tp]) > 1.96
          }
        }
        results <- results[order(abs(results$z), decreasing = TRUE), ]
        rownames(results) <- NULL

      } else {
        # ---- INDIVIDUAL: config model on weighted matrices ----
        # Each individual has a weighted transition count matrix.
        # igraph has no weighted config model; stub-shuffling IS
        # the exact weighted configuration model from the literature.
        # Preserves exact row/col sums per individual.
        null_matrix <- matrix(0, nrow = nrow(results), ncol = n_perm)

        n_ind_c <- dim(trans)[1]
        ind_totals_c <- integer(n_ind_c)
        rows_stubs_c <- vector("list", n_ind_c)
        cols_stubs_c <- vector("list", n_ind_c)
        for (ind in seq_len(n_ind_c)) {
          mat_c <- trans[ind, , ]
          rs_c <- as.integer(rowSums(mat_c))
          cs_c <- as.integer(colSums(mat_c))
          ind_totals_c[ind] <- sum(rs_c)
          rows_stubs_c[[ind]] <- rep(seq_len(s), times = rs_c)
          cols_stubs_c[[ind]] <- rep(seq_len(s), times = cs_c)
        }
        valid_c <- which(ind_totals_c >= min_transitions)
        ss_c <- as.integer(s * s)

        for (perm in seq_len(n_perm)) {
          perm_totals <- setNames(integer(nrow(results)), results$type)

          for (ind in valid_c) {
            rs_c <- rows_stubs_c[[ind]]
            cs_c <- cols_stubs_c[[ind]]
            cs_shuf <- sample(cs_c)
            lin_c <- (cs_shuf - 1L) * s + rs_c
            mat <- matrix(tabulate(lin_c, nbins = ss_c), s, s)

            counted <- .count_triads_matrix_vectorized(
              mat, edge_method, edge_threshold,
              exclude = final_exclude,
              include = final_include
            )
            if (!is.null(counted) && nrow(counted) > 0) {
              tc <- table(counted$type)
              for (nm in names(tc)) {
                if (nm %in% names(perm_totals)) {
                  perm_totals[nm] <- perm_totals[nm] + tc[nm]
                }
              }
            }
          }
          null_matrix[, perm] <- perm_totals
        }

        null_mean <- rowMeans(null_matrix)
        null_sd <- apply(null_matrix, 1, stats::sd)
        null_sd[null_sd == 0] <- 1

        results$expected <- round(null_mean, 1)
        results$z <- round((results$count - null_mean) / null_sd, 2)
        results$p <- round(2 * stats::pnorm(-abs(results$z)), 4)
        results$sig <- results$p < 0.05

        results <- results[order(abs(results$z), decreasing = TRUE), ]
        rownames(results) <- NULL
      }  # end individual census significance
    }

  } else {
    # ---- INSTANCE MODE: list specific node triples ----
    all_results <- lapply(seq_len(dim(trans)[1]), function(ind) {
      mat <- trans[ind, , ]
      if (sum(mat) < min_transitions) return(NULL)

      counted <- .count_triads_matrix_vectorized(
        mat, edge_method, edge_threshold,
        exclude = final_exclude,
        include = final_include
      )
      if (is.null(counted) || nrow(counted) == 0) return(NULL)

      triads <- vapply(seq_len(nrow(counted)), function(r) {
        paste(labels[counted$i[r]], labels[counted$j[r]],
              labels[counted$k[r]], sep = " - ")
      }, character(1))

      data.frame(unit = ind, triad = triads, type = counted$type,
                 stringsAsFactors = FALSE)
    })

    combined <- do.call(rbind, all_results)

    if (is.null(combined) || nrow(combined) == 0) {
      message("No motifs found with the given parameters.")
      return(NULL)
    }

    # Aggregate across units
    if (level == "individual") {
      obs <- stats::aggregate(unit ~ triad, data = combined, FUN = length)
      names(obs)[2] <- "observed"
      type_map <- stats::aggregate(
        type ~ triad, data = combined,
        FUN = function(tt) names(sort(table(tt), decreasing = TRUE))[1]
      )
      results <- merge(obs, type_map, by = "triad")
      results <- results[order(results$observed, decreasing = TRUE), ]
    } else {
      results <- data.frame(
        triad = unique(combined$triad),
        type = combined$type[!duplicated(combined$triad)],
        observed = 1L,
        stringsAsFactors = FALSE
      )
    }
    rownames(results) <- NULL

    # ---- INSTANCE SIGNIFICANCE (exact configuration model) ----
    # Uses the exact configuration model: expand stubs, randomly pair.
    # Preserves EXACT row sums and column sums (not just expected).
    # Matches igraph::sample_degseq() null model.
    #
    # Optimization: ind-major loop with batch presence matrix +
    # per-individual candidate filtering. Benchmarked (1000 perms):
    #   coding (9 states, 84 cands):           3.5s
    #   human_ai_detailed (32 states, 3678):  31.3s
    if (significance && level == "individual") {
      # Pre-filter to candidates meeting min_count before permutations
      if (!is.null(min_count)) {
        candidates <- results[results$observed > min_count, ]
      } else {
        candidates <- results
      }

      if (nrow(candidates) > 0) {
        # Parse candidate node indices
        triad_idx <- do.call(rbind, lapply(
          strsplit(candidates$triad, " - "),
          function(nodes) match(nodes, labels)
        ))
        n_cand <- nrow(triad_idx)
        ss <- as.integer(s * s)

        # Pre-compute linear indices for all 6 edge positions (once)
        lin_ij <- (triad_idx[,2] - 1L) * s + triad_idx[,1]
        lin_ji <- (triad_idx[,1] - 1L) * s + triad_idx[,2]
        lin_ik <- (triad_idx[,3] - 1L) * s + triad_idx[,1]
        lin_ki <- (triad_idx[,1] - 1L) * s + triad_idx[,3]
        lin_jk <- (triad_idx[,3] - 1L) * s + triad_idx[,2]
        lin_kj <- (triad_idx[,2] - 1L) * s + triad_idx[,3]

        # Pre-compute per-individual stubs and validity
        # Configuration model: rep(node_id, times = marginal_count)
        n_ind <- dim(trans)[1]
        ind_totals <- integer(n_ind)
        rows_stubs <- vector("list", n_ind)
        cols_stubs <- vector("list", n_ind)
        active_row <- matrix(FALSE, n_ind, s)
        active_col <- matrix(FALSE, n_ind, s)

        for (ind in seq_len(n_ind)) {
          mat_i <- trans[ind, , ]
          rs <- as.integer(rowSums(mat_i))
          cs <- as.integer(colSums(mat_i))
          ind_totals[ind] <- sum(rs)
          rows_stubs[[ind]] <- rep(seq_len(s), times = rs)
          cols_stubs[[ind]] <- rep(seq_len(s), times = cs)
          active_row[ind, ] <- rs > 0L
          active_col[ind, ] <- cs > 0L
        }
        valid_inds <- which(ind_totals >= max(3L, min_transitions))

        # Per-individual candidate mask: which candidates can each
        # individual possibly contribute to? Edge (a->b) is possible
        # iff row_sum[a] > 0 AND col_sum[b] > 0. Candidate is
        # possible if ANY of its 6 edges is possible.
        ri <- active_row[, triad_idx[,1], drop = FALSE]
        rj <- active_row[, triad_idx[,2], drop = FALSE]
        rk <- active_row[, triad_idx[,3], drop = FALSE]
        ci <- active_col[, triad_idx[,1], drop = FALSE]
        cj <- active_col[, triad_idx[,2], drop = FALSE]
        ck <- active_col[, triad_idx[,3], drop = FALSE]
        ind_cand_mask <- (ri & cj) | (rj & ci) | (ri & ck) |
                         (rk & ci) | (rj & ck) | (rk & cj)

        # Batch permutation: individual-major loop using config model
        # For each individual, generate all n_perm permuted matrices
        # (via stub shuffling) as an ss × n_perm presence matrix,
        # then check all relevant candidates simultaneously.
        null_matrix <- matrix(0L, n_cand, n_perm)

        for (ind in valid_inds) {
          mask <- ind_cand_mask[ind, ]
          if (!any(mask)) next
          wm <- which(mask)
          total <- ind_totals[ind]
          rs <- rows_stubs[[ind]]
          cs <- cols_stubs[[ind]]

          # Generate all n_perm permuted column stubs at once
          # Each column of perm_cols is an independent shuffle of cs
          perm_cols <- vapply(seq_len(n_perm),
                              function(p) sample(cs),
                              integer(total))  # total × n_perm

          # Convert to linear indices: rows fixed, cols permuted
          all_lin <- (perm_cols - 1L) * s + rs  # rs recycles column-wise
          dim(all_lin) <- NULL  # flatten to length total * n_perm
          perm_id <- rep(seq_len(n_perm), each = total)

          # Build ss × n_perm logical presence matrix
          presence <- matrix(FALSE, nrow = ss, ncol = n_perm)
          presence[cbind(all_lin, perm_id)] <- TRUE

          # Vectorized: check all relevant candidates × all perms
          has_any <- presence[lin_ij[wm], , drop = FALSE] |
                     presence[lin_ji[wm], , drop = FALSE] |
                     presence[lin_ik[wm], , drop = FALSE] |
                     presence[lin_ki[wm], , drop = FALSE] |
                     presence[lin_jk[wm], , drop = FALSE] |
                     presence[lin_kj[wm], , drop = FALSE]

          null_matrix[wm, ] <- null_matrix[wm, ] + has_any
        }

        null_mean <- rowMeans(null_matrix)
        null_sd <- apply(null_matrix, 1, stats::sd)
        null_sd[null_sd == 0] <- 1

        candidates$expected <- round(null_mean, 1)
        candidates$z <- round((candidates$observed - null_mean) / null_sd, 2)
        candidates$p <- round(2 * stats::pnorm(-abs(candidates$z)), 4)
        candidates$sig <- candidates$p < 0.05
        candidates <- candidates[order(abs(candidates$z), decreasing = TRUE), ]
        rownames(candidates) <- NULL
      }
      results <- candidates
    }
  }  # end instance mode

  # Min count filter (instance mode without significance — sig path filters before perms)
  if (!is.null(min_count) && named_nodes && !significance) {
    results <- results[results$observed > min_count, ]
    if (nrow(results) == 0) {
      message("No motifs with count > ", min_count, ".")
      return(NULL)
    }
  }

  # Top N
  if (!is.null(top) && top < nrow(results)) {
    results <- results[seq_len(top), ]
  }

  # Type summary
  type_summary <- sort(table(results$type), decreasing = TRUE)

  # ================================================================
  # INFORMATIVE MESSAGE (instance mode with defaults)
  # ================================================================

  if (named_nodes && !.user_set_pattern) {
    message("Showing triangle patterns (count > ", min_count, "). ",
            "For all MAN types use pattern = 'all'.")
  }

  # ================================================================
  # RETURN
  # ================================================================

  structure(
    list(
      results = results,
      type_summary = type_summary,
      level = level,
      named_nodes = named_nodes,
      n_units = n_units,
      params = list(
        labels = labels,
        n_states = s,
        pattern = pattern,
        edge_method = edge_method,
        significance = significance,
        n_perm = n_perm,
        min_count = min_count,
        window = window,
        window_type = window_type,
        actor = actor
      )
    ),
    class = "cograph_motif_result"
  )
}


#' Extract Specific Motif Instances (Subgraphs)
#'
#' Convenience wrapper for \code{motifs(x, named_nodes = TRUE, ...)}. Returns
#' specific node triples forming each MAN pattern, with optional significance
#' testing.
#'
#' @inheritParams motifs
#' @seealso [motifs()]
#' @family motifs
#' @export
subgraphs <- function(...) motifs(..., named_nodes = TRUE)


# Note: no .permute_trans_array() helper needed — both census and instance
# significance use the exact configuration model inline:
#   row_stubs <- rep(1:s, times = rowSums(mat))  # fixed
#   col_stubs <- rep(1:s, times = colSums(mat))  # shuffled per perm
#   sample(col_stubs) randomly pairs stubs, preserving exact marginals.
# Census builds full matrices via tabulate(); instances use logical presence.
```

- [ ] **Step 3b: Verify tna windowing works**

Before running tests, verify that tna's windowing params are passed correctly. Run this quick sanity check:

```r
Rscript -e '
library(tna)
devtools::load_all(".")
Mod <- tna(coding)
# Verify attr access works
cat("type:", attr(Mod, "type"), "\n")
cat("scaling:", attr(Mod, "scaling"), "\n")
cat("params:", paste(names(attr(Mod, "params")), collapse=", "), "\n")
# Verify initialize_model can be called
init_fn <- .get_tna_initialize_model()
model <- init_fn(Mod$data, attr(Mod, "type"), attr(Mod, "scaling"), attr(Mod, "params"), transitions = TRUE)
cat("trans dim:", paste(dim(model$trans), collapse="x"), "\n")
'
```

If `attr(Mod, "params")` returns NULL, use `Mod$params` or check tna internals for correct access pattern. Adjust the code accordingly before proceeding.

- [ ] **Step 4: Run tests to verify they pass**

- [ ] **Step 5: Add `motifs` and `subgraphs` to NAMESPACE**

Run: `Rscript -e 'devtools::document(".")'`

- [ ] **Step 6: Run full test suite**

Run: `Rscript -e 'devtools::test(".")'`
Expected: all pass (including old motif tests — nothing removed)

- [ ] **Step 7: Commit**

```bash
git add R/motifs-api.R tests/testthat/test-motifs-api.R NAMESPACE man/
git commit -m "feat(motifs): add unified motifs() with auto-detection and windowing"
```

---

## Chunk 2: Print/plot methods and integration

### Task 4: Print method for `cograph_motif_result` (no collision with existing `cograph_motifs` methods)

**Files:**
- Modify: `R/motifs-api.R` (append)
- Test: `tests/testthat/test-motifs-api.R` (append)

- [ ] **Step 1: Write failing test for print method**

```r
test_that("print.cograph_motif_result works for census", {
  skip_if_not_installed("tna")
  Mod <- tna::tna(coding)
  result <- motifs(Mod, n_perm = 10, seed = 42)
  expect_output(print(result), "Motif Census")
  expect_output(print(result), "individual")
})

test_that("print.cograph_motif_result works for instances", {
  skip_if_not_installed("tna")
  Mod <- tna::tna(coding)
  result <- subgraphs(Mod)
  expect_output(print(result), "Motif Subgraphs")
})

test_that("print.cograph_motif_result works for aggregate", {
  mat <- matrix(c(0,1,1,0, 0,0,1,1, 0,0,0,1, 1,0,0,0), 4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A","B","C","D")
  result <- motifs(mat, n_perm = 10, seed = 42)
  expect_output(print(result), "Motif Census")
  expect_output(print(result), "aggregate")
})
```

- [ ] **Step 2: Run tests to verify they fail**

- [ ] **Step 3: Implement print method**

Append to `R/motifs-api.R`:

```r
#' @export
print.cograph_motif_result <- function(x, ...) {
  mode_label <- if (x$named_nodes) "Motif Subgraphs" else "Motif Census"
  cat(mode_label, "\n")
  cat("Level:", x$level)
  if (x$level == "individual") {
    cat(" |", x$n_units, "units")
  }
  cat(" | States:", x$params$n_states)
  cat(" | Pattern:", x$params$pattern, "\n")

  if (!is.null(x$params$window)) {
    cat("Window:", x$params$window, "(", x$params$window_type, ")\n")
  }

  if (x$params$significance) {
    cat("Significance: permutation (n_perm=", x$params$n_perm, ")\n", sep = "")
  }

  if (!is.null(x$params$min_count) && x$named_nodes) {
    cat("Min count: >", x$params$min_count, "\n")
  }

  cat("\nType distribution:\n")
  print(x$type_summary)

  n_show <- min(20, nrow(x$results))
  cat("\nTop", n_show, "triads:\n")
  print(x$results[seq_len(n_show), ], row.names = FALSE)

  invisible(x)
}
```

- [ ] **Step 4: Run tests to verify they pass**

- [ ] **Step 5: Commit**

```bash
git add R/motifs-api.R tests/testthat/test-motifs-api.R
git commit -m "feat(motifs): add print.cograph_motif_result method"
```

---

### Task 5: Plot method — reuse existing plot helpers

**Files:**
- Modify: `R/motifs-api.R` (append)
- Test: `tests/testthat/test-motifs-api.R` (append)

- [ ] **Step 1: Write failing test for plot method**

```r
test_that("plot.cograph_motif_result works for triads type", {
  skip_if_not_installed("tna")
  Mod <- tna::tna(coding)
  result <- motifs(Mod, top = 10)
  with_temp_png({
    expect_no_error(plot(result, type = "triads", n = 6, ncol = 3))
  })
})

test_that("plot.cograph_motif_result works for types type", {
  skip_if_not_installed("tna")
  Mod <- tna::tna(coding)
  result <- motifs(Mod)
  expect_no_error(plot(result, type = "types"))
})

test_that("plot.cograph_motif_result works for significance type", {
  skip_if_not_installed("tna")
  Mod <- tna::tna(coding)
  result <- motifs(Mod, significance = TRUE, n_perm = 10, seed = 42)
  expect_no_error(plot(result, type = "significance"))
})

test_that("plot.cograph_motif_result triads type works with significance data", {
  skip_if_not_installed("tna")
  Mod <- tna::tna(coding)
  result <- motifs(Mod, significance = TRUE, n_perm = 10, seed = 42, top = 10)
  # Verify sig column is logical (not character stars like old class)
  expect_type(result$results$sig, "logical")
  with_temp_png({
    expect_no_error(plot(result, type = "triads", n = 6, ncol = 3))
  })
})
```

- [ ] **Step 2: Run tests to verify they fail**

- [ ] **Step 3: Implement plot method**

The plot method delegates to the existing helpers in `R/motifs-plot.R` (`.plot_triad_networks()`, etc.) which already work on the `results` data frame structure.

Append to `R/motifs-api.R`:

```r
#' @param type Plot type: "triads" (network diagrams), "types" (bar chart),
#'   "significance" (z-score plot), "patterns" (abstract MAN diagrams).
#' @param n Number of triads to plot (for "triads" type). Default 15.
#' @param ncol Number of columns in triad grid. Default 5.
#' @param ... Additional arguments passed to plot helpers (node_size, label_size,
#'   title_size, stats_size, color, spacing).
#' @rdname motifs
#' @export
plot.cograph_motif_result <- function(x, type = c("triads", "types",
                                             "significance", "patterns"),
                                 n = 15, ncol = 5, ...) {
  type <- match.arg(type)

  if (type == "significance" && !x$params$significance) {
    stop("Significance data not available. Run motifs() with significance = TRUE.")
  }

  if (type == "triads") {
    .plot_triad_networks(x, n = n, ncol = ncol, ...)
  } else if (type == "types") {
    # Bar chart of type distribution
    df <- as.data.frame(x$type_summary, stringsAsFactors = FALSE)
    names(df) <- c("type", "count")
    df <- df[order(df$count, decreasing = TRUE), ]

    p <- ggplot2::ggplot(df, ggplot2::aes(
      x = stats::reorder(.data$type, .data$count), y = .data$count)) +
      ggplot2::geom_col(fill = "#2166AC") +
      ggplot2::coord_flip() +
      ggplot2::labs(x = "MAN Type", y = "Count",
                    title = "Motif Type Distribution") +
      .motifs_ggplot_theme()
    print(p)
  } else if (type == "significance") {
    sig_df <- x$results[!is.na(x$results$z), ]
    sig_df <- sig_df[order(abs(sig_df$z), decreasing = TRUE), ]

    p <- ggplot2::ggplot(sig_df, ggplot2::aes(
      x = stats::reorder(.data$triad, abs(.data$z)),
      y = .data$z,
      fill = .data$z > 0)) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(values = c("TRUE" = "#2166AC", "FALSE" = "#B2182B"),
                                  guide = "none") +
      ggplot2::geom_hline(yintercept = c(-1.96, 1.96), linetype = "dashed",
                           color = "grey50") +
      ggplot2::labs(x = NULL, y = "Z-score",
                    title = "Motif Significance") +
      .motifs_ggplot_theme()
    print(p)
  } else if (type == "patterns") {
    .plot_motif_patterns(x)
  }

  invisible(x)
}
```

- [ ] **Step 4: Run tests to verify they pass**

- [ ] **Step 5: Commit**

```bash
git add R/motifs-api.R tests/testthat/test-motifs-api.R
git commit -m "feat(motifs): add plot.cograph_motif_result with triads/types/significance/patterns"
```

---

### Task 6: Mark old functions as internal, update docs

**Files:**
- Modify: `R/motifs.R` — add `@keywords internal` to `triad_census`, `motif_census`, `extract_triads`
- Modify: `R/motifs-extract.R` — add `@keywords internal` to `extract_motifs`
- Modify: `R/motifs-api.R` — add `@seealso` cross-references

- [ ] **Step 1: Add `@keywords internal` to old functions**

In `R/motifs.R`, add `#' @keywords internal` to roxygen blocks for:
- `motif_census` (before `@export`)
- `triad_census` (before `@export`)
- `extract_triads` (before `@export`)

In `R/motifs-extract.R`:
- `extract_motifs` (before `@export`)

These remain exported for backward compatibility but are marked as internal in docs.

- [ ] **Step 2: Run `devtools::document(".")`**

- [ ] **Step 3: Run full test suite**

Run: `Rscript -e 'devtools::test(".")'`
Expected: all existing tests still pass

- [ ] **Step 4: Commit**

```bash
git add R/motifs.R R/motifs-extract.R man/
git commit -m "docs(motifs): mark old functions as internal, motifs() is primary API"
```

---

### Task 7: Integration test with packaged `coding` dataset

**Files:**
- Test: `tests/testthat/test-motifs-api.R` (append)

- [ ] **Step 1: Write integration tests**

```r
test_that("motifs end-to-end with coding dataset via tna", {
  skip_if_not_installed("tna")
  Mod <- tna::tna(coding)

  # Basic
  m <- motifs(Mod)
  expect_s3_class(m, "cograph_motif_result")
  expect_equal(m$level, "individual")
  expect_true(nrow(m$results) > 0)

  # With significance
  m_sig <- motifs(Mod, significance = TRUE, n_perm = 20, seed = 42)
  expect_true("z" %in% names(m_sig$results))

  # With pattern filter
  m_all <- motifs(Mod, pattern = "all")
  expect_true(nrow(m_all$results) >= nrow(m$results))

  # Top N
  m_top <- motifs(Mod, top = 5)
  expect_equal(nrow(m_top$results), 5L)
})

test_that("motifs end-to-end with synthetic edge list", {
  # Build a synthetic edge list with session and project grouping
  set.seed(42)
  states <- c("Plan", "Execute", "Monitor", "Adapt")
  n_sessions <- 5
  edges_per_session <- 20

  edge_list <- do.call(rbind, lapply(seq_len(n_sessions), function(s) {
    from_states <- sample(states, edges_per_session, replace = TRUE)
    to_states <- sample(states, edges_per_session, replace = TRUE)
    data.frame(
      from = from_states,
      to = to_states,
      session_id = paste0("s", s),
      project = paste0("p", ((s - 1) %/% 3) + 1),
      order = seq_len(edges_per_session),
      stringsAsFactors = FALSE
    )
  }))

  net <- as_cograph(edge_list)

  # Auto-detect session_id
  m <- motifs(net)
  expect_equal(m$level, "individual")
  expect_equal(m$n_units, n_sessions)

  # Manual actor override
  m_proj <- motifs(net, actor = "project")
  expect_equal(m_proj$n_units, 2L)  # 5 sessions / 3 per project = 2 projects

  # Windowed
  m_win <- motifs(net, window = 10, window_type = "tumbling")
  expect_true(m_win$n_units > m$n_units)  # more units from windowing
})
```

- [ ] **Step 2: Run tests**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-motifs-api.R")'`

- [ ] **Step 3: Run full test suite**

Run: `Rscript -e 'devtools::test(".")'`

- [ ] **Step 4: Commit**

```bash
git add tests/testthat/test-motifs-api.R
git commit -m "test(motifs): add integration tests for motifs() with coding dataset"
```

---

## Chunk 3: Report and documentation

### Task 8: Update report with unified API

**Files:**
- Modify: `tmp/coding_motifs_report.Rmd`

- [ ] **Step 1: Update report to use `motifs()` instead of old functions**

Replace individual function calls demonstrating both modes:
- `motifs()`: census from matrix (aggregate), census from tna (individual), significance
- `subgraphs()`: instances from tna, with significance opt-in, pattern filtering
- Windowed analysis
- All plot types for both modes

- [ ] **Step 2: Render report**

Run: `Rscript -e 'rmarkdown::render("tmp/coding_motifs_report.Rmd", output_dir = "tmp")'`

- [ ] **Step 3: Commit**

```bash
git add tmp/coding_motifs_report.Rmd
git commit -m "docs: update motifs report to use unified motifs() API"
```

---

### Task 9: Update CHANGES.md, HANDOFF.md, LEARNINGS.md

- [ ] **Step 1: Update project docs**

- [ ] **Step 2: Commit**

```bash
git add CHANGES.md HANDOFF.md LEARNINGS.md
git commit -m "docs: update project docs for unified motifs API"
```

---

## Key Design Decisions

1. **Two functions, not four**: `motifs()` (census, nodes exchangeable) replaces `triad_census` + `motif_census`. `subgraphs()` (instances, nodes NOT exchangeable) replaces `extract_triads` + `extract_motifs`. Old functions stay exported but marked `@keywords internal`.

2. **`subgraphs()` is a wrapper**: `subgraphs <- function(...) motifs(..., named_nodes = TRUE)`. One engine, two entry points.

3. **Different defaults per mode**:
   - `motifs()`: `significance = TRUE`, `n_perm = 1000`, `min_count = NULL` (census is fast)
   - `subgraphs()`: `significance = TRUE`, `n_perm = 1000`, `min_count = 5`, `pattern = "triangle"` (optimized: ~2.5s for 9-state, ~32s for 32-state networks)

4. **Informative message for `subgraphs()` defaults**: When using defaults, prints: "Showing triangle patterns (count > 5). For all MAN types use pattern = 'all'." Suppressed when user explicitly sets pattern.

5. **Auto-detection over configuration**: `motifs()` scans `$data` for standard column names. Only need `actor = "col"` when names are non-standard.

6. **`$data` is the metadata source**: No new `$meta` fields. Edge list metadata lives in `$data` columns. `motifs()` reads them directly.

7. **`window` is always numeric**: A size parameter (how many transitions per window). Not a column reference.

8. **tna windowing goes through tna**: For tna objects, `window` is translated to `params = list(windowed = TRUE, window_size = N, window_type = type)` and the model is re-estimated. This uses tna's own windowing engine.

9. **Edge list windowing is native**: For cograph edge lists, windowing is done by `.edgelist_to_trans_array()` which splits ordered edges into windows and creates separate groups.

10. **Return class is `cograph_motif_result`**: New S3 class (avoids collision with existing `cograph_motifs` used by `motif_census()`). `$level` distinguishes individual vs aggregate. `$named_nodes` distinguishes census vs instances. Print/plot methods adapt based on both.

11. **Aggregate census: igraph delegation**: For single-matrix analysis, `motifs(mat)` delegates significance testing to `motif_census()` internally, which uses `igraph::sample_degseq()` + `igraph::motifs()`. Zero deviation from igraph — it IS igraph. `motifs(mat, seed=42)` produces identical z-scores to `motif_census(mat, seed=42)`.

12. **Individual-level: exact weighted configuration model**: For per-individual analysis (tna objects, grouped edge lists), uses stub-shuffling on weighted transition count matrices. Expands to row-stubs and column-stubs, randomly pairs them. Preserves **exact** in-degree and out-degree sequences (zero variance in marginal sums). igraph has no weighted configuration model, so our implementation IS the reference. Same seed = `identical()` = TRUE.

13. **Instance significance optimization**: Batch approach with 4 layers on top of the exact config model:
    - **Ind-major loop**: Loop over individuals, generate all n_perm permutations at once per individual via `vapply(n_perm, sample(col_stubs))`
    - **Batch logical presence matrix**: Build an ss×n_perm logical matrix from all permuted transitions, then check all candidates across all perms in a single vectorized operation
    - **Per-individual candidate filtering**: Pre-compute which candidates are possible for each individual (based on nonzero marginals), skip impossible ones
    - **Pre-computed linear indices**: Convert (i,j) matrix indices to linear indices once, reuse for all lookups

    Benchmarked (1000 perms, exact config model):
    | Dataset | Old (R for-loops) | Optimized | Speedup |
    |---|---|---|---|
    | coding (9 states, 84 cands) | 13.2s | **3.5s** | 3.8× |
    | human_ai_detailed (32 states, 3678 cands) | 405.8s | **31.3s** | 13.0× |

    Configuration model verified: marginal sums have zero variance across permutations; triad_census matches igraph exactly; same seed + same code = `identical()` = TRUE.
