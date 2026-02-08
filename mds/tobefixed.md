# Issues to Fix Before CRAN Submission

## 1. Duplicate Example Number in qgraph-to-splot.Rmd

**File**: `vignettes/qgraph-to-splot.Rmd`
**Lines**: 189 and 198
**Severity**: Must fix

"Example 9" appears twice:

- Line 189: `# Example 9: Extracting and tweaking qgraph parameters`
- Line 198: `# Example 9: tplot for qgraph-style parameter names`

**Fix**: Change line 198 to `# Example 10: tplot for qgraph-style parameter names`

---

## 2. Wrong File Extension in NEWS.md

**File**: `NEWS.md`
**Line**: 17
**Severity**: Must fix

References `vignettes/qgraph-to-splot.md` but the actual file is `vignettes/qgraph-to-splot.Rmd`.

**Current**:
```
- Added qgraph to splot migration guide (`vignettes/qgraph-to-splot.md`)
```

**Fix**: Change `.md` to `.Rmd`:
```
- Added qgraph to splot migration guide (`vignettes/qgraph-to-splot.Rmd`)
```

---

## 3. Sentence Fragment in DESCRIPTION

**File**: `DESCRIPTION`
**Lines**: 10-14
**Severity**: Must fix (CRAN reviewers check Description grammar)

The Description field contains a sentence fragment. "As a modern, extensible network visualization package." is not a complete sentence.

**Current**:
```
Description: Provides high-quality static and interactive network plots. As a
    modern, extensible network visualization package. It accepts adjacency
    matrices, edge lists, or igraph objects and offers customizable layouts,
    node shapes, edge styles, and themes. Designed for publication-ready
    visualizations with a pipe-friendly API.
```

**Fix**: Merge the fragment into one sentence:
```
Description: Provides high-quality static and interactive network plots
    as a modern, extensible network visualization package. It accepts
    adjacency matrices, edge lists, or igraph objects and offers customizable
    layouts, node shapes, edge styles, and themes. Designed for
    publication-ready visualizations with a pipe-friendly API.
```

---

## 4. .Rbuildignore Regex Pattern Missing Anchors

**File**: `.Rbuildignore`
**Line**: 25
**Severity**: Should fix

The pattern `mds` has no regex anchors. This means it would also match any file or folder containing "mds" anywhere in the name (e.g., a hypothetical `commands/` folder).

**Current**:
```
mds
```

**Fix**:
```
^mds$
```

---

## 5. README Understates Node Shapes Count

**File**: `README.Rmd` (line 37) and `README.md` (line 29)
**Severity**: Should fix

Claims "12+ node shapes" but the actual count is 25+ shapes: circle, square, triangle, diamond, pentagon, hexagon, star, heart, ellipse, cross, rectangle, neural, chip, robot, brain, network, database, cloud, gear, none, pie, donut, polygon_donut, donut_pie, double_donut_pie, plus custom SVG.

**Current**:
```markdown
- **12+ node shapes** including pie charts and donut rings
```

**Fix**:
```markdown
- **25+ node shapes** including pie charts and donut rings
```

**Note**: After editing README.Rmd, re-knit to regenerate README.md.

---

## 6. Thick Edges in plot_mlna(), plot_mtna(), and plot_htna()

**Files**: `R/mlna.R`, `R/plot-htna-multi.R`, `R/plot-htna.R`
**Severity**: Bug (visual)

Edge line widths (`lwd`) are hardcoded with no scaling factor, producing overly thick edges especially at higher DPI or larger figure sizes. The border `lwd` values for cluster shapes are also too thick.

**Fix** (already implemented in https://github.com/mohsaqr/Sonnet/tree/cograph):

Add a `scale` parameter to all three functions and compute an `edge_scale` factor that reduces `lwd` proportionally. Apply `edge_scale` to every `lwd` value in the function.

### R/mlna.R

Add parameter `scale = 1` to function signature. At the top of the function body, add:

```r
size_scale <- sqrt(scale)
node_size <- node_size / size_scale
edge_scale <- 1 / size_scale
```

Then multiply all `lwd` values by `edge_scale`:

```r
# Before (current):
lwd <- 0.5 + 2.5 * (abs(weight) / max_w)
lwd = 2.5
lwd <- 0.8 + 1.5 * (abs(weight) / max_w)
lwd = 1.5

# After (fixed):
lwd <- (0.5 + 2.5 * (abs(weight) / max_w)) * edge_scale
lwd = 2.5 * edge_scale
lwd <- (0.8 + 1.5 * (abs(weight) / max_w)) * edge_scale
lwd = 1.5 * edge_scale
```

Also scale text sizes by `/ size_scale` (legend `cex`, label `cex`, `pt.cex`).

### R/plot-htna-multi.R

Same `scale` parameter and `edge_scale` computation. Additionally, read an `edge.lwd` multiplier from `...`:

```r
edge_lwd_mult <- if (!is.null(dots$edge.lwd)) dots$edge.lwd else 1
```

Then apply both `edge_scale` and `edge_lwd_mult` to all `lwd` values:

```r
# Before (current):
lwd <- 1 + 5 * (weight / max_weight)
lwd <- 0.5 + 2 * (weight / max_within)
lwd = 3

# After (fixed):
lwd <- (1 + 5 * (weight / max_weight)) * edge_scale * edge_lwd_mult
lwd <- (0.5 + 2 * (weight / max_within)) * edge_scale * edge_lwd_mult
lwd = 3 * edge_scale
```

### R/plot-htna.R

Same `scale` parameter. Scale the legend text, extension line widths, and polygon/circular layout radius by `scale`:

```r
size_scale <- sqrt(scale)
# Legend:
pt.cex = 2.5 / size_scale
cex = 1.4 / size_scale
# Extension lines:
lwd = 1 / size_scale
# Polygon/circular layout radius:
radius <- 2 * scale
```

### Reference

Full implementation: https://github.com/mohsaqr/Sonnet/tree/cograph
- `R/mlna.R` lines 108-115
- `R/plot-htna-multi.R` lines 93-103
- `R/plot-htna.R` lines 132-136
