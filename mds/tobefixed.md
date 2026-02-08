# Issues to Fix Before CRAN Submission

## 1. ~~Duplicate Example Number in qgraph-to-splot.Rmd~~ DONE

**File**: `vignettes/qgraph-to-splot.Rmd`

Line 198 changed from `# Example 9` to `# Example 10`.

---

## 2. ~~Wrong File Extension in NEWS.md~~ DONE

**File**: `NEWS.md`

Changed `.md` to `.Rmd`.

---

## 3. ~~Sentence Fragment in DESCRIPTION~~ DONE

**File**: `DESCRIPTION`

Merged "As a" fragment into the first sentence.

---

## 4. ~~.Rbuildignore Regex Pattern Missing Anchors~~ DONE

**File**: `.Rbuildignore`

Changed `mds` to `^mds$`.

---

## 5. ~~README Understates Node Shapes Count~~ DONE

**File**: `README.Rmd` and `README.md`

Changed "12+ node shapes" to "25+ node shapes".

---

## 6. ~~Thick Edges in plot_mlna(), plot_mtna(), and plot_htna()~~ DONE

**Files**: `R/mlna.R`, `R/plot-htna-multi.R`, `R/plot-htna.R`

Added `scale` parameter with `edge_scale = 1 / sqrt(scale)` to all three functions. All `lwd` values multiplied by `edge_scale`, all `cex`/`pt.cex` values divided by `size_scale`. Also reduced base `lwd` multipliers to prevent thick edges at default scale:

- `mlna.R`: between-layer `0.3 + 1.2x`, shell `1.5`, within-layer `0.3 + 1.0x`, node border `0.8`, padding reduced
- `plot-htna-multi.R`: summary `0.5 + 2.5x`, shell `1.5`, within-cluster `0.3 + 1.0x`
- `plot-htna.R`: legend and extension line `lwd`/`cex` scaled by `size_scale`
