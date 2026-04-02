# Network Analysis with cograph: Import, Utilities, Centralities & Communities

## 1 Introduction

Most network analysis in R involves a frustrating ritual: convert your
data into the right format, learn a package-specific API, compute one
metric at a time, then wrangle outputs back into something usable. If
your data comes from a different source — a transition matrix here, an
igraph object there, a statnet network elsewhere — you spend more time
on format conversion than on actual analysis.

**cograph** eliminates this friction. Every function accepts every major
network format directly: adjacency matrices, edge-list data frames,
igraph, statnet network, qgraph, and tna objects. You never convert
manually. You call `centrality_degree(mat)` or `communities(g)` and it
works, regardless of what `mat` or `g` happens to be.

This tutorial covers the analysis side of cograph:

1.  **Importing networks** — six formats, zero conversion code
2.  **Inspecting and converting** — getters, summaries, format
    conversion
3.  **Filtering and selecting** — dplyr-style expressions,
    centrality-aware selection
4.  **Centrality measures** — 25 node-level measures, edge centrality,
    directional variants
5.  **Network properties** — 37 summary metrics plus specialized
    diagnostics
6.  **Community detection** — 11 algorithms, consensus, comparison

For **visualization and plotting**, see the companion tutorial:
[Plotting with
cograph](http://sonsoles.me/cograph/articles/1_cograph-tutorial-plotting.md).
For **TNA-specific workflows**, see the [TNA
tutorial](http://sonsoles.me/cograph/articles/3_plotting-tna-models.md).
For a tutorial centered around **communities**, see [Visualization of
communities and hyper order
networks](http://sonsoles.me/cograph/articles/cograph-tutorial-communities.md).

> **GitHub Development Version**
>
> This tutorial covers the full feature set available in the **GitHub
> development version** of cograph. The CRAN release provides the core
> plotting engine; the development version adds the full analysis
> toolkit (centralities, communities, network properties, filtering, and
> selection).
>
> ``` r
> # Development version with full features
> devtools::install_github("sonsoleslp/cograph")
> ```

## 2 Sample Data

Throughout this tutorial we use a 5-state directed transition matrix
representing regulatory learning behaviors. This is the kind of weighted
directed network that appears in learning analytics, process mining, and
behavioral research.

``` r
# A 5-state transition matrix (directed, weighted)
states <- c("Explore", "Plan", "Monitor", "Adapt", "Reflect")
mat <- matrix(
  c(0.00, 0.35, 0.20, 0.15, 0.30,
    0.25, 0.00, 0.30, 0.20, 0.25,
    0.15, 0.25, 0.00, 0.35, 0.25,
    0.10, 0.20, 0.35, 0.00, 0.35,
    0.20, 0.15, 0.25, 0.40, 0.00),
  nrow = 5, byrow = TRUE,
  dimnames = list(states, states)
)
round(mat, 2)
```

            Explore Plan Monitor Adapt Reflect
    Explore    0.00 0.35    0.20  0.15    0.30
    Plan       0.25 0.00    0.30  0.20    0.25
    Monitor    0.15 0.25    0.00  0.35    0.25
    Adapt      0.10 0.20    0.35  0.00    0.35
    Reflect    0.20 0.15    0.25  0.40    0.00

We also create a larger 8-node undirected network for demonstrations
that benefit from more structure:

``` r
# 8-node network for community detection examples
set.seed(42)
nodes8 <- c("A", "B", "C", "D", "E", "F", "G", "H")
mat8 <- matrix(0, 8, 8, dimnames = list(nodes8, nodes8))

# Create two clusters with strong internal connections
# Cluster 1: A, B, C, D
mat8["A", "B"] <- mat8["B", "A"] <- 0.8
mat8["A", "C"] <- mat8["C", "A"] <- 0.6
mat8["B", "C"] <- mat8["C", "B"] <- 0.7
mat8["B", "D"] <- mat8["D", "B"] <- 0.5
mat8["C", "D"] <- mat8["D", "C"] <- 0.6

# Cluster 2: E, F, G, H
mat8["E", "F"] <- mat8["F", "E"] <- 0.9
mat8["E", "G"] <- mat8["G", "E"] <- 0.7
mat8["F", "G"] <- mat8["G", "F"] <- 0.8
mat8["G", "H"] <- mat8["H", "G"] <- 0.6
mat8["F", "H"] <- mat8["H", "F"] <- 0.5

# Weak between-cluster bridges
mat8["D", "E"] <- mat8["E", "D"] <- 0.15
mat8["C", "G"] <- mat8["G", "C"] <- 0.10
```

## 3 Importing Networks

Every cograph function accepts six network formats directly. You never
need a separate conversion step — just pass your data and cograph
figures out the rest.

### 3.1 From a Matrix

The most common input: a square adjacency or weight matrix. Row and
column names become node labels. Directedness is auto-detected from
matrix symmetry (symmetric = undirected, asymmetric = directed).

``` r
# Asymmetric matrix → auto-detected as directed
net <- as_cograph(mat)
net
```

    Cograph network: 5 nodes, 20 edges ( directed )
    Source: matrix
      Nodes (5): Explore, Plan, Monitor, Adapt, Reflect
      Edges: 20 / 20 (density: 100.0%)
      Weights: [0.100, 0.400]  |  mean: 0.250
      Strongest edges:
        Reflect -> Adapt  0.400
        Explore -> Plan  0.350
        Adapt -> Monitor  0.350
        Monitor -> Adapt  0.350
        Adapt -> Reflect  0.350
    Layout: none 

``` r
# Symmetric matrix → auto-detected as undirected
net_u <- as_cograph(mat8)
net_u
```

    Cograph network: 8 nodes, 12 edges ( undirected )
    Source: matrix
      Nodes (8): A, B, C, D, E, F, G, H
      Edges: 12 / 28 (density: 42.9%)
      Weights: [0.100, 0.900]  |  mean: 0.579
      Strongest edges:
        E -- F  0.900
        A -- B  0.800
        F -- G  0.800
        B -- C  0.700
        E -- G  0.700
    Layout: none 

You can override auto-detection with `directed = TRUE` or
`directed = FALSE`.

### 3.2 From a Data Frame (Edge List)

An edge-list data frame with source, target, and optionally weight
columns. Column names are auto-detected from common conventions
(`from`/`to`, `source`/`target`, `v1`/`v2`, etc.).

``` r
# Create an edge list data frame
edges_df <- data.frame(
  from   = c("A", "A", "B", "B", "C"),
  to     = c("B", "C", "C", "D", "D"),
  weight = c(0.8, 0.6, 0.7, 0.5, 0.6)
)

net_df <- as_cograph(edges_df)
net_df
```

    Cograph network: 4 nodes, 5 edges ( undirected )
    Source: edgelist
    Data: data.frame (5 x 3)
      Nodes (4): A, B, C, D
    Weights: 0.5 to 0.8
    Layout: none 

### 3.3 From an igraph Object

If you already have an igraph object from another analysis, pass it
directly. All vertex and edge attributes are preserved.

``` r
library(igraph)

# Resolve namespace conflicts (igraph masks some cograph functions)
communities    <- cograph::communities
membership     <- cograph::membership.cograph_communities
modularity     <- cograph::modularity.cograph_communities
edge_betweenness <- cograph::edge_betweenness
degree_distribution <- cograph::degree_distribution

# Create an igraph object
g <- graph_from_adjacency_matrix(mat, mode = "directed", weighted = TRUE)

# Pass directly --- no conversion needed
net_ig <- as_cograph(g)
net_ig
```

    Cograph network: 5 nodes, 20 edges ( directed )
    Source: igraph
      Nodes (5): Explore, Plan, Monitor, Adapt, Reflect
    Weights: 0.1 to 0.4
    Layout: none 

> **From a statnet network Object**
>
> If you work with the `network` / `sna` ecosystem, cograph accepts
> statnet network objects directly:
>
> ``` r
> library(network)
> n <- network(mat8, directed = FALSE)
> net_sn <- as_cograph(n)
> ```

> **From a qgraph Object**
>
> For users of the `qgraph` package (common in psychological network
> analysis):
>
> ``` r
> library(qgraph)
> q <- qgraph(mat)
> net_qg <- as_cograph(q)
> ```

> **From a TNA Object**
>
> cograph has first-class integration with the `tna` package. TNA
> objects carry additional metadata (initial state probabilities, state
> labels, color schemes) that cograph preserves automatically:
>
> ``` r
> library(tna)
> model <- tna(engagement)
> net_tna <- as_cograph(model)
> ```
>
> When a TNA object is detected, cograph automatically adjusts weight
> inversion for path-based centrality measures (because TNA transition
> probabilities are similarities, not distances).

> **Format Auto-Detection Summary**
>
> | Input           | Detection Method                                               | Directed?                                                                                |
> |-----------------|----------------------------------------------------------------|------------------------------------------------------------------------------------------|
> | Square matrix   | [`is.matrix()`](https://rdrr.io/r/base/matrix.html)            | Auto from symmetry                                                                       |
> | Data frame      | [`is.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) | Auto from bidirectional edges                                                            |
> | igraph          | `inherits(x, "igraph")`                                        | From igraph’s [`is_directed()`](http://sonsoles.me/cograph/reference/is_directed.md)     |
> | statnet network | `inherits(x, "network")`                                       | From [`network::is.directed()`](https://rdrr.io/pkg/network/man/network.indicators.html) |
> | qgraph          | `inherits(x, "qgraph")`                                        | From qgraph attributes                                                                   |
> | tna             | `inherits(x, "tna")`                                           | Auto from matrix symmetry                                                                |
>
> You can always override with `directed = TRUE` or `directed = FALSE`.

## 4 Inspecting Networks

Once you have a cograph_network object, a set of getter functions gives
you clean access to every component.

``` r
# Basic properties
# Nodes
n_nodes(net) 
```

    [1] 5

``` r
#Edges
n_edges(net)
```

    [1] 20

``` r
# Directed?
cograph::is_directed(net) 
```

    [1] TRUE

``` r
# Labels
get_labels(net)
```

    [1] "Explore" "Plan"    "Monitor" "Adapt"   "Reflect"

``` r
# Node table
get_nodes(net)
```

      id   label    name  x  y
    1  1 Explore Explore NA NA
    2  2    Plan    Plan NA NA
    3  3 Monitor Monitor NA NA
    4  4   Adapt   Adapt NA NA
    5  5 Reflect Reflect NA NA

``` r
# Edge table (from, to, weight)
get_edges(net)
```

       from to weight
    1     2  1   0.25
    2     3  1   0.15
    3     4  1   0.10
    4     5  1   0.20
    5     1  2   0.35
    6     3  2   0.25
    7     4  2   0.20
    8     5  2   0.15
    9     1  3   0.20
    10    2  3   0.30
    11    4  3   0.35
    12    5  3   0.25
    13    1  4   0.15
    14    2  4   0.20
    15    3  4   0.35
    16    5  4   0.40
    17    1  5   0.30
    18    2  5   0.25
    19    3  5   0.25
    20    4  5   0.35

The [`summary()`](https://rdrr.io/r/base/summary.html) method provides a
one-line overview of the network structure:

``` r
summary(net)
```

    Cograph Network Summary
    ======================

    Structure:
      Nodes: 5
      Edges: 20
      Type: Directed

    Edge Statistics:
      Min weight: 0.1
      Max weight: 0.4
      Mean weight: 0.25

    Node Labels:
       Explore, Plan, Monitor, Adapt, Reflect

    Layout: not computed 

> **All Getter Functions**
>
> | Function         | Returns                            |
> |------------------|------------------------------------|
> | `get_nodes(x)`   | Node data frame (id, label, name)  |
> | `get_edges(x)`   | Edge data frame (from, to, weight) |
> | `get_labels(x)`  | Character vector of node labels    |
> | `get_source(x)`  | Input format name (e.g., “matrix”) |
> | `get_data(x)`    | Original raw input data            |
> | `get_meta(x)`    | Metadata list                      |
> | `n_nodes(x)`     | Integer node count                 |
> | `n_edges(x)`     | Integer edge count                 |
> | `is_directed(x)` | Logical                            |
> | `get_groups(x)`  | Group assignments (if set)         |

## 5 Conversion

Sometimes you need to move between formats — for example, to use an
igraph-specific algorithm or to export an edge list to a file. cograph
provides four conversion functions that accept **any** input format:

``` r
# Matrix → igraph (for access to igraph's full toolkit)
g <- to_igraph(mat)
g
```

    IGRAPH e2886c6 DNW- 5 20 --
    + attr: name (v/c), weight (e/n)
    + edges from e2886c6 (vertex names):
     [1] Explore->Plan    Explore->Monitor Explore->Adapt   Explore->Reflect
     [5] Plan   ->Explore Plan   ->Monitor Plan   ->Adapt   Plan   ->Reflect
     [9] Monitor->Explore Monitor->Plan    Monitor->Adapt   Monitor->Reflect
    [13] Adapt  ->Explore Adapt  ->Plan    Adapt  ->Monitor Adapt  ->Reflect
    [17] Reflect->Explore Reflect->Plan    Reflect->Monitor Reflect->Adapt  

``` r
# igraph → matrix (for matrix algebra or heatmaps)
m <- to_matrix(g)
round(m, 2)
```

            Explore Plan Monitor Adapt Reflect
    Explore    0.00 0.35    0.20  0.15    0.30
    Plan       0.25 0.00    0.30  0.20    0.25
    Monitor    0.15 0.25    0.00  0.35    0.25
    Adapt      0.10 0.20    0.35  0.00    0.35
    Reflect    0.20 0.15    0.25  0.40    0.00

``` r
# Any format → edge list data frame (for CSV export or dplyr pipelines)
df <- to_data_frame(mat)
df
```

          from      to weight
    1  Explore    Plan   0.35
    2  Explore Monitor   0.20
    3  Explore   Adapt   0.15
    4  Explore Reflect   0.30
    5     Plan Explore   0.25
    6     Plan Monitor   0.30
    7     Plan   Adapt   0.20
    8     Plan Reflect   0.25
    9  Monitor Explore   0.15
    10 Monitor    Plan   0.25
    11 Monitor   Adapt   0.35
    12 Monitor Reflect   0.25
    13   Adapt Explore   0.10
    14   Adapt    Plan   0.20
    15   Adapt Monitor   0.35
    16   Adapt Reflect   0.35
    17 Reflect Explore   0.20
    18 Reflect    Plan   0.15
    19 Reflect Monitor   0.25
    20 Reflect   Adapt   0.40

> **Conversion Functions**
>
> | Function           | Returns                | Use Case                             |
> |--------------------|------------------------|--------------------------------------|
> | `to_igraph(x)`     | igraph object          | Access igraph’s full algorithm suite |
> | `to_matrix(x)`     | Adjacency matrix       | Matrix algebra, heatmap input        |
> | `to_data_frame(x)` | Edge list data frame   | CSV export, dplyr pipelines          |
> | `to_network(x)`    | statnet network object | Use with sna/ergm packages           |
>
> All accept matrix, data.frame, igraph, network, cograph_network, or
> tna objects. The `directed` parameter can override auto-detection.

## 6 Filtering and Selection

Filtering lets you extract subsets of your network using expressions
evaluated against node or edge properties — including centrality
measures computed lazily on demand. This is one of the most powerful
features in cograph: you can filter by structural properties without
computing them first.

### 6.1 Filtering Edges

[`filter_edges()`](http://sonsoles.me/cograph/reference/filter_edges.md)
uses dplyr-style expressions evaluated against the edge data frame. Keep
only the strong transitions:

``` r
# Keep only edges with weight > 0.25
strong_net <- filter_edges(mat, weight > 0.25)
get_edges(strong_net)
```

      from to weight
    1    1  2   0.35
    2    2  3   0.30
    3    4  3   0.35
    4    3  4   0.35
    5    5  4   0.40
    6    1  5   0.30
    7    4  5   0.35

Multiple conditions combine with AND logic:

``` r
# Edges from "Adapt" with weight > 0.30
adapt_strong <- filter_edges(mat, from == "Adapt", weight > 0.30)
get_edges(adapt_strong)
```

    [1] from   to     weight
    <0 rows> (or 0-length row.names)

### 6.2 Filtering Nodes

[`filter_nodes()`](http://sonsoles.me/cograph/reference/filter_nodes.md)
evaluates expressions against node properties. What makes this powerful
is that **centrality measures are available as lazy-computed columns** —
you don’t need to compute them first:

``` r
# Keep only nodes with degree centrality > 4
# "degree" is computed on the fly inside the filter
high_degree <- filter_nodes(mat, degree > 4)
get_labels(high_degree)
```

    [1] "Explore" "Plan"    "Monitor" "Adapt"   "Reflect"

> **Available Lazy Centrality Columns in filter_nodes()**
>
> These column names are computed automatically when referenced in a
> filter expression:
>
> | Column         | Measure                      |
> |----------------|------------------------------|
> | `degree`       | Degree centrality            |
> | `strength`     | Weighted degree              |
> | `betweenness`  | Betweenness centrality       |
> | `closeness`    | Closeness centrality         |
> | `eigenvector`  | Eigenvector centrality       |
> | `pagerank`     | PageRank                     |
> | `hub`          | Hub score                    |
> | `authority`    | Authority score              |
> | `coreness`     | K-core decomposition         |
> | `constraint`   | Burt’s structural constraint |
> | `eccentricity` | Eccentricity                 |
> | `transitivity` | Local clustering coefficient |
>
> These are not pre-computed — they are evaluated only when you
> reference them, so there is no performance cost when filtering by
> other attributes.

### 6.3 Selecting Nodes

[`select_nodes()`](http://sonsoles.me/cograph/reference/select_nodes.md)
provides six selection modes that combine with AND logic:

``` r
# Select specific nodes by name
sub <- select_nodes(mat, name = c("Plan", "Monitor", "Adapt"))
get_labels(sub)
```

    [1] "Plan"    "Monitor" "Adapt"  

``` r
# Top 3 nodes by betweenness centrality
top3 <- select_nodes(mat, top = 3, by = "betweenness")
get_labels(top3)
```

    [1] "Explore" "Plan"    "Adapt"  

``` r
# Select "Monitor" and its direct neighbors
neighborhood <- select_neighbors(mat, of = "Monitor", order = 1)
get_labels(neighborhood)
```

    [1] "Explore" "Plan"    "Monitor" "Adapt"   "Reflect"

> **All Selection Modes**
>
> | Mode                | Parameter                       | Example                                                                                       |
> |---------------------|---------------------------------|-----------------------------------------------------------------------------------------------|
> | By name             | `name = c("A", "B")`            | Exact node names                                                                              |
> | By index            | `index = c(1, 3, 5)`            | Positional indices                                                                            |
> | Top-N by centrality | `top = 5, by = "degree"`        | Highest centrality                                                                            |
> | Neighbors           | `neighbors_of = "A", order = 2` | Ego network to depth 2                                                                        |
> | Component           | `component = "largest"`         | Largest connected component                                                                   |
> | NSE expression      | `... (e.g., degree > 5)`        | Any expression, like [`filter_nodes()`](http://sonsoles.me/cograph/reference/filter_nodes.md) |
>
> Combine modes freely — they are intersected with AND logic.

### 6.4 Selecting Edges

``` r
# Top 5 strongest edges
top5 <- select_top_edges(mat, n = 5)
get_edges(top5)
```

      from to weight
    1    1  2   0.35
    2    4  3   0.35
    3    3  4   0.35
    4    5  4   0.40
    5    4  5   0.35

``` r
# Bridge edges (whose removal disconnects components)
bridges <- select_bridges(mat8)
get_edges(bridges)
```

    [1] from   to     weight
    <0 rows> (or 0-length row.names)

> **Edge Selection Modes**
>
> | Mode            | Parameter                    | Description                            |
> |-----------------|------------------------------|----------------------------------------|
> | Top-N           | `top = 10, by = "weight"`    | Strongest/highest edges                |
> | Involving nodes | `involving = c("A", "B")`    | Edges touching specific nodes          |
> | Between sets    | `between = list(set1, set2)` | Edges between two node groups          |
> | Bridges only    | `bridges_only = TRUE`        | Edges whose removal disconnects        |
> | Mutual only     | `mutual_only = TRUE`         | Reciprocated edges (directed networks) |
>
> **Lazy edge metrics** are available in expressions: `abs_weight`,
> `from_degree`, `to_degree`, `edge_betweenness`, `is_bridge`,
> `is_mutual`, `same_community`.

### 6.5 Convenience Wrappers

For the most common operations, cograph provides one-call wrappers:

| Function                           | Equivalent                                          |
|------------------------------------|-----------------------------------------------------|
| `select_top(x, n, by)`             | `select_nodes(x, top = n, by = by)`                 |
| `select_neighbors(x, of, order)`   | `select_nodes(x, neighbors_of = of, order = order)` |
| `select_component(x, "largest")`   | `select_nodes(x, component = "largest")`            |
| `select_bridges(x)`                | `select_edges(x, bridges_only = TRUE)`              |
| `select_top_edges(x, n)`           | `select_edges(x, top = n)`                          |
| `select_edges_involving(x, nodes)` | `select_edges(x, involving = nodes)`                |
| `select_edges_between(x, s1, s2)`  | `select_edges(x, between = list(s1, s2))`           |

## 7 Centrality Measures

Centrality reduces each node to a single number reflecting its
structural importance. Different measures answer different questions,
and the right measure depends on what “important” means for your
research question.

### 7.1 The centrality() Function

[`centrality()`](http://sonsoles.me/cograph/reference/centrality.md)
computes multiple measures at once and returns a tidy data frame:

``` r
# Compute the most common measures at once
cent <- centrality(mat, measures = c(
  "degree", "strength", "betweenness", "closeness",
  "eigenvector", "pagerank", "harmonic", "hub", "authority"
))
cent
```

         node degree_all strength_all betweenness closeness_all eigenvector
    1 Explore          8         1.70         2.5      1.428571   0.6499134
    2    Plan          8         1.95         0.5      1.176471   0.8201157
    3 Monitor          8         2.10         0.0      1.111111   0.9744193
    4   Adapt          8         2.10         0.5      1.176471   1.0000000
    5 Reflect          8         2.15         0.0      1.111111   0.9936078
       pagerank harmonic_all       hub authority
    1 0.1538221     25.66667 1.0000000 0.5925176
    2 0.1875499     19.66667 0.9462459 0.8253838
    3 0.2166792     18.66667 0.9487956 0.9482319
    4 0.2210600     22.33333 0.9868906 0.9314441
    5 0.2208887     19.00000 0.9277966 1.0000000

Request specific measures:

``` r
# Just degree and betweenness
cent_subset <- centrality(mat, measures = c("degree", "betweenness", "pagerank"))
cent_subset
```

         node degree_all betweenness  pagerank
    1 Explore          8         2.5 0.1538221
    2    Plan          8         0.5 0.1875499
    3 Monitor          8         0.0 0.2166792
    4   Adapt          8         0.5 0.2210600
    5 Reflect          8         0.0 0.2208887

> **Key Parameters**
>
> | Parameter    | Default | What It Controls                                            |
> |--------------|---------|-------------------------------------------------------------|
> | `measures`   | `"all"` | Which measures to compute (character vector or “all”)       |
> | `mode`       | `"all"` | Direction for mode-aware measures: `"all"`, `"in"`, `"out"` |
> | `normalized` | `FALSE` | Normalize to \[0, 1\]?                                      |
> | `weighted`   | `TRUE`  | Use edge weights?                                           |
> | `directed`   | `NULL`  | Override directedness (NULL = auto-detect)                  |
> | `damping`    | `0.85`  | PageRank damping factor                                     |
> | `alpha`      | `1`     | Alpha centrality exponent                                   |
> | `k`          | `3`     | k-reach neighborhood depth                                  |
> | `digits`     | `NULL`  | Round output (NULL = no rounding)                           |
> | `sort_by`    | `NULL`  | Sort rows by a measure name                                 |

### 7.2 Individual Wrappers

Every measure has a dedicated function that returns a named numeric
vector. These are simpler when you need just one measure:

``` r
# Each returns a named numeric vector
centrality_degree(mat)
```

    Explore    Plan Monitor   Adapt Reflect
          8       8       8       8       8 

``` r
centrality_betweenness(mat)
```

    Explore    Plan Monitor   Adapt Reflect
        2.5     0.5     0.0     0.5     0.0 

``` r
centrality_pagerank(mat)
```

      Explore      Plan   Monitor     Adapt   Reflect
    0.1538221 0.1875499 0.2166792 0.2210600 0.2208887 

### 7.3 Directional Measures

For directed networks, ten measures support `mode = "in"` and
`mode = "out"`. cograph provides shorthand wrappers so you don’t need
the `mode` argument:

``` r
# InStrength: total incoming weight (which states are "attractors"?)
centrality_instrength(mat)
```

    Explore    Plan Monitor   Adapt Reflect
       0.70    0.95    1.10    1.10    1.15 

``` r
# OutStrength: total outgoing weight (which states are frequent origins?)
centrality_outstrength(mat)
```

    Explore    Plan Monitor   Adapt Reflect
          1       1       1       1       1 

> **All 25 Centrality Measures**
>
> | Measure                      | Directional? | Question It Answers                                        |
> |------------------------------|--------------|------------------------------------------------------------|
> | **degree**                   | in/out/all   | How many connections does this node have?                  |
> | **strength**                 | in/out/all   | How much total weight flows through this node?             |
> | **betweenness**              | —            | Does this node sit on paths between other nodes?           |
> | **closeness**                | in/out/all   | How quickly can this node reach (or be reached by) others? |
> | **eigenvector**              | —            | Is this node connected to other well-connected nodes?      |
> | **pagerank**                 | —            | How important is this node in a random-walk model?         |
> | **authority**                | —            | Is this node a destination for important hubs?             |
> | **hub**                      | —            | Does this node point to important authorities?             |
> | **eccentricity**             | in/out/all   | How far is the farthest reachable node?                    |
> | **coreness**                 | in/out/all   | What is the densest core this node belongs to?             |
> | **constraint**               | —            | How constrained is this node by its local neighborhood?    |
> | **transitivity**             | —            | Do this node’s neighbors connect to each other?            |
> | **harmonic**                 | in/out/all   | Closeness variant robust to disconnected networks          |
> | **diffusion**                | —            | How well does this node spread activation?                 |
> | **leverage**                 | —            | Does this node have more connections than its neighbors?   |
> | **kreach**                   | —            | How many nodes are reachable within k steps?               |
> | **alpha**                    | —            | Bonacich alpha (exogenous influence) centrality            |
> | **power**                    | —            | Bonacich power centrality                                  |
> | **subgraph**                 | —            | Importance based on all closed walks through this node     |
> | **laplacian**                | —            | Importance in the graph’s Laplacian spectrum               |
> | **load**                     | —            | Load centrality (traffic-based betweenness variant)        |
> | **current_flow_closeness**   | —            | Information centrality via electrical current analogy      |
> | **current_flow_betweenness** | —            | Betweenness via current flow (not just shortest paths)     |
> | **voterank**                 | —            | Iterative influence maximization score                     |
> | **percolation**              | —            | Importance for network connectivity under failure          |
>
> **Directional wrappers**:
> [`centrality_indegree()`](http://sonsoles.me/cograph/reference/centrality_degree.md),
> [`centrality_outdegree()`](http://sonsoles.me/cograph/reference/centrality_degree.md),
> [`centrality_instrength()`](http://sonsoles.me/cograph/reference/centrality_strength.md),
> [`centrality_outstrength()`](http://sonsoles.me/cograph/reference/centrality_strength.md),
> [`centrality_incloseness()`](http://sonsoles.me/cograph/reference/centrality_closeness.md),
> [`centrality_outcloseness()`](http://sonsoles.me/cograph/reference/centrality_closeness.md),
> [`centrality_inharmonic()`](http://sonsoles.me/cograph/reference/centrality_harmonic.md),
> [`centrality_outharmonic()`](http://sonsoles.me/cograph/reference/centrality_harmonic.md),
> [`centrality_ineccentricity()`](http://sonsoles.me/cograph/reference/centrality_eccentricity.md),
> [`centrality_outeccentricity()`](http://sonsoles.me/cograph/reference/centrality_eccentricity.md).

### 7.4 Edge Centrality

Node centrality tells you which nodes are important. Edge centrality
tells you which **transitions** are important — which connections serve
as structural bridges.

``` r
# Edge betweenness: which edges carry the most shortest-path traffic?
eb <- edge_betweenness(mat)
eb
```

       Explore->Plan Explore->Monitor   Explore->Adapt Explore->Reflect
                 0.5              2.0              3.0              1.0
       Plan->Explore    Plan->Monitor      Plan->Adapt    Plan->Reflect
                 1.0              1.0              1.5              1.0
    Monitor->Explore    Monitor->Plan   Monitor->Adapt Monitor->Reflect
                 2.0              1.0              0.0              1.0
      Adapt->Explore      Adapt->Plan   Adapt->Monitor   Adapt->Reflect
                 2.0              1.5              0.0              1.0
    Reflect->Explore    Reflect->Plan Reflect->Monitor   Reflect->Adapt
                 1.5              1.5              1.0              0.0 

``` r
# Full edge centrality data frame
edf <- edge_centrality(mat)
edf
```

          from      to weight betweenness
    1  Explore    Plan   0.35         0.5
    2  Explore Monitor   0.20         2.0
    3  Explore   Adapt   0.15         3.0
    4  Explore Reflect   0.30         1.0
    5     Plan Explore   0.25         1.0
    6     Plan Monitor   0.30         1.0
    7     Plan   Adapt   0.20         1.5
    8     Plan Reflect   0.25         1.0
    9  Monitor Explore   0.15         2.0
    10 Monitor    Plan   0.25         1.0
    11 Monitor   Adapt   0.35         0.0
    12 Monitor Reflect   0.25         1.0
    13   Adapt Explore   0.10         2.0
    14   Adapt    Plan   0.20         1.5
    15   Adapt Monitor   0.35         0.0
    16   Adapt Reflect   0.35         1.0
    17 Reflect Explore   0.20         1.5
    18 Reflect    Plan   0.15         1.5
    19 Reflect Monitor   0.25         1.0
    20 Reflect   Adapt   0.40         0.0

### 7.5 TNA Weight Inversion

When the input is a TNA object, cograph automatically inverts weights
for path-based measures (betweenness, closeness, harmonic, eccentricity,
and others). This is because TNA transition probabilities are
**similarities** (higher = closer) while shortest-path algorithms expect
**distances** (lower = closer). You don’t need to do anything — it just
works.

## 8 Network Properties

While centrality characterizes individual nodes, network properties
characterize the **network as a whole**: how dense it is, how clustered,
how efficiently information flows through it.

### 8.1 network_summary()

The
[`network_summary()`](http://sonsoles.me/cograph/reference/network_summary.md)
function computes up to 37 metrics in a single call:

``` r
# Basic summary (18 metrics)
network_summary(mat)
```

      node_count edge_count density component_count diameter mean_distance min_cut
    1          5         20       1               1     0.35         0.242       4
      centralization_degree centralization_in_degree centralization_out_degree
    1                     0                        0                         0
      centralization_betweenness centralization_closeness centralization_eigen
    1                          0                        0                    0
      transitivity reciprocity assortativity_degree hub_score authority_score
    1        0.857           1                  NaN        NA              NA

``` r
# Extended summary (adds girth, radius, efficiency, etc.)
network_summary(mat, extended = TRUE)
```

      node_count edge_count density component_count diameter mean_distance min_cut
    1          5         20       1               1     0.35         0.242       4
      centralization_degree centralization_in_degree centralization_out_degree
    1                     0                        0                         0
      centralization_betweenness centralization_closeness centralization_eigen
    1                          0                        0                    0
      transitivity reciprocity assortativity_degree hub_score authority_score girth
    1        0.857           1                  NaN        NA              NA     2
      radius vertex_connectivity largest_clique_size cut_vertex_count bridge_count
    1    0.3                   4                   5                0            0
      global_efficiency local_efficiency
    1             4.595            2.226

``` r
# Detailed summary (adds centrality distribution statistics)
network_summary(mat, detailed = TRUE)
```

      node_count edge_count density component_count diameter mean_distance min_cut
    1          5         20       1               1     0.35         0.242       4
      centralization_degree centralization_in_degree centralization_out_degree
    1                     0                        0                         0
      centralization_betweenness centralization_closeness centralization_eigen
    1                          0                        0                    0
      transitivity reciprocity assortativity_degree hub_score authority_score
    1        0.857           1                  NaN        NA              NA
      mean_degree sd_degree median_degree mean_strength sd_strength
    1           8         0             8             2       0.184
      mean_betweenness mean_closeness mean_eigenvector mean_pagerank
    1              0.7          1.201            0.888           0.2
      mean_constraint mean_local_transitivity
    1           0.761                   0.214

> **What Each Metric Tells You**
>
> **Always included (18 metrics):**
>
> | Metric                         | Interpretation                                           |
> |--------------------------------|----------------------------------------------------------|
> | `node_count`, `edge_count`     | Network size                                             |
> | `density`                      | Proportion of possible edges that exist                  |
> | `component_count`              | Number of disconnected components                        |
> | `diameter`                     | Longest shortest path in the network                     |
> | `mean_distance`                | Average shortest path length                             |
> | `min_cut`                      | Minimum edges to disconnect the network                  |
> | `centralization_degree`        | How unequal is the degree distribution?                  |
> | `centralization_closeness`     | How centralized is reachability?                         |
> | `centralization_betweenness`   | How much does one node dominate bridging?                |
> | `centralization_eigenvector`   | How concentrated is influence?                           |
> | `transitivity`                 | Global clustering coefficient                            |
> | `reciprocity`                  | Proportion of mutual edges (directed)                    |
> | `assortativity_degree`         | Do high-degree nodes connect to other high-degree nodes? |
> | `hub_score`, `authority_score` | Max hub/authority centrality                             |
> | `is_connected`, `is_bipartite` | Structural properties                                    |
>
> **Extended (`extended = TRUE`, adds 8):**
>
> | Metric                | Interpretation                        |
> |-----------------------|---------------------------------------|
> | `girth`               | Shortest cycle length                 |
> | `radius`              | Minimum eccentricity                  |
> | `vertex_connectivity` | Min nodes to remove to disconnect     |
> | `largest_clique_size` | Size of the largest complete subgraph |
> | `cut_vertex_count`    | Articulation points (bridges)         |
> | `bridge_count`        | Bridge edges                          |
> | `global_efficiency`   | Average inverse shortest path         |
> | `local_efficiency`    | Mean ego-network efficiency           |
>
> **Detailed (`detailed = TRUE`, adds 11):**
>
> Descriptive statistics (mean, SD, median) for degree, strength,
> betweenness, closeness, eigenvector, PageRank, constraint, and local
> transitivity.

### 8.2 Individual Property Functions

For specific analyses, dedicated functions provide richer output than
the summary table:

``` r
# Small-world coefficient (compares clustering and path length to random graphs)
sw <- network_small_world(mat8, n_random = 100)
round(sw, 3)
```

    [1] 2.422

``` r
# Rich-club coefficient (do high-degree nodes preferentially connect?)
rc <- network_rich_club(mat8, normalized = TRUE, n_random = 100)
round(rc, 3)
```

    [1] 1.389

> **All Individual Property Functions**
>
> | Function                         | Returns             | Use Case                            |
> |----------------------------------|---------------------|-------------------------------------|
> | `degree_distribution(x)`         | Numeric vector      | Degree histogram, power-law testing |
> | `network_girth(x)`               | Integer             | Shortest cycle length               |
> | `network_radius(x)`              | Integer             | Network compactness                 |
> | `network_vertex_connectivity(x)` | Integer             | Robustness to node failure          |
> | `network_clique_size(x)`         | Integer             | Largest complete subgraph           |
> | `network_cut_vertices(x)`        | Character vector    | Articulation points                 |
> | `network_bridges(x)`             | Data frame          | Bridge edges                        |
> | `network_global_efficiency(x)`   | Numeric \[0, 1\]    | Information flow efficiency         |
> | `network_local_efficiency(x)`    | Numeric \[0, 1\]    | Local fault tolerance               |
> | `network_small_world(x)`         | List (sigma, omega) | Small-world characterization        |
> | `network_rich_club(x, k)`        | Data frame          | Rich-club phenomenon                |

## 9 Community Detection

Communities are groups of nodes that are more densely connected to each
other than to the rest of the network. Detecting them reveals the
**modular structure** of a system: which behaviors cluster together,
which brain regions form functional modules, which actors form cohesive
subgroups.

### 9.1 Basic Usage

The
[`communities()`](http://sonsoles.me/cograph/reference/communities.md)
function applies a community detection algorithm and returns a
`cograph_communities` object:

``` r
# Detect communities using the default algorithm (Louvain)
comms <- communities(mat8)
comms
```

    Community structure (louvain)
      Number of communities: 2
      Modularity: 0.4631
      Community sizes: 4, 4
      Nodes: 8 

``` r
# Which community does each node belong to?
membership(comms)
```

    A B C D E F G H
    1 1 1 1 2 2 2 2 

``` r
# How large is each community?
community_sizes(comms)
```

    [1] 4 4

``` r
# Modularity score (how well-separated are the communities?)
modularity(comms)
```

    [1] 0.4630971

### 9.2 Choosing an Algorithm

cograph provides 11 community detection algorithms. Each has different
strengths:

``` r
# Fast greedy: modularity optimization (fast, undirected)
comms_fg <- community_fast_greedy(mat8)

# Walktrap: random-walk based (captures flow structure)
comms_wt <- community_walktrap(mat8)

# Leiden: improved Louvain with resolution parameter
comms_ld <- community_leiden(mat8)
```

> **All 11 Algorithms**
>
> | Function                                                                                                   | Short Alias                                                                         | Algorithm                 | Best For                        |
> |------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------|---------------------------|---------------------------------|
> | [`community_louvain()`](http://sonsoles.me/cograph/reference/community_louvain.md)                         | [`com_lv()`](http://sonsoles.me/cograph/reference/community_louvain.md)             | Louvain modularity        | General purpose, large networks |
> | [`community_leiden()`](http://sonsoles.me/cograph/reference/community_leiden.md)                           | [`com_ld()`](http://sonsoles.me/cograph/reference/community_leiden.md)              | Leiden (improved Louvain) | Better-defined communities      |
> | [`community_fast_greedy()`](http://sonsoles.me/cograph/reference/community_fast_greedy.md)                 | [`com_fg()`](http://sonsoles.me/cograph/reference/community_fast_greedy.md)         | Fast greedy               | Quick modularity optimization   |
> | [`community_walktrap()`](http://sonsoles.me/cograph/reference/community_walktrap.md)                       | [`com_wt()`](http://sonsoles.me/cograph/reference/community_walktrap.md)            | Random walk               | Flow-based structure            |
> | [`community_infomap()`](http://sonsoles.me/cograph/reference/community_infomap.md)                         | [`com_im()`](http://sonsoles.me/cograph/reference/community_infomap.md)             | Information flow          | Directed flow networks          |
> | [`community_label_propagation()`](http://sonsoles.me/cograph/reference/community_label_propagation.md)     | [`com_lp()`](http://sonsoles.me/cograph/reference/community_label_propagation.md)   | Label propagation         | Very large networks             |
> | [`community_edge_betweenness()`](http://sonsoles.me/cograph/reference/community_edge_betweenness.md)       | [`com_eb()`](http://sonsoles.me/cograph/reference/community_edge_betweenness.md)    | Edge betweenness          | Small networks, clear bridges   |
> | [`community_leading_eigenvector()`](http://sonsoles.me/cograph/reference/community_leading_eigenvector.md) | [`com_le()`](http://sonsoles.me/cograph/reference/community_leading_eigenvector.md) | Leading eigenvector       | Spectral decomposition          |
> | [`community_spinglass()`](http://sonsoles.me/cograph/reference/community_spinglass.md)                     | [`com_sg()`](http://sonsoles.me/cograph/reference/community_spinglass.md)           | Spin glass                | Small, fine-grained structure   |
> | [`community_optimal()`](http://sonsoles.me/cograph/reference/community_optimal.md)                         | [`com_op()`](http://sonsoles.me/cograph/reference/community_optimal.md)             | Exact optimization        | Tiny networks (NP-hard)         |
> | [`community_fluid()`](http://sonsoles.me/cograph/reference/community_fluid.md)                             | [`com_fl()`](http://sonsoles.me/cograph/reference/community_fluid.md)               | Fluid communities         | When k is known a priori        |
>
> Every function also has a **two-letter alias** (e.g.,
> [`com_lv()`](http://sonsoles.me/cograph/reference/community_louvain.md),
> [`com_fg()`](http://sonsoles.me/cograph/reference/community_fast_greedy.md))
> for quick interactive use.

### 9.3 Consensus Communities

A single algorithm run can be sensitive to initialization. Consensus
communities run the algorithm many times and keep only the assignments
that appear consistently:

``` r
# Run Louvain 100 times, keep consistent assignments
consensus <- community_consensus(mat8, method = "louvain", n_runs = 100, seed = 42)
consensus
```

    Community structure (consensus_louvain)
      Number of communities: 2
      Modularity: 0.5
      Community sizes: 4, 4
      Nodes: 8 

### 9.4 Comparing Community Structures

When you run two different algorithms, how similar are their results?
[`compare_communities()`](http://sonsoles.me/cograph/reference/compare_communities.md)
quantifies this with information-theoretic metrics:

``` r
# Compare Louvain vs. Walktrap
nmi <- compare_communities(comms, comms_wt, method = "nmi")
# Normalized Mutual Information
round(nmi, 3)
```

    [1] 1

``` r
# Adjusted Rand Index
ari <- compare_communities(comms, comms_wt, method = "adjusted.rand")
round(ari, 3)
```

    [1] 1

> **Comparison Methods**
>
> | Method            | Range         | Interpretation                                  |
> |-------------------|---------------|-------------------------------------------------|
> | `"nmi"`           | \[0, 1\]      | 1 = identical, 0 = unrelated                    |
> | `"vi"`            | \[0, log(n)\] | Lower = more similar (Variation of Information) |
> | `"rand"`          | \[0, 1\]      | Proportion of concordant pairs                  |
> | `"adjusted.rand"` | \[-1, 1\]     | Chance-corrected Rand (0 = random)              |
> | `"split.join"`    | \[0, 2n\]     | Lower = more similar                            |

## 10 Putting It All Together

A complete analysis pipeline, from import to communities, in a few
lines:

``` r
library(cograph)

# 1. Import (works with any format)
net <- as_cograph(my_matrix)

# 2. Inspect
summary(net)
n_nodes(net)
n_edges(net)

# 3. Filter
core <- filter_nodes(net, degree > 3)
strong <- filter_edges(net, weight > 0.2)

# 4. Select subnetwork
top5 <- select_top(net, n = 5, by = "betweenness")
ego <- select_neighbors(net, of = "Monitor", order = 2)

# 5. Centrality
centrality(net, measures = c("degree", "betweenness", "pagerank"))
centrality_instrength(net)
centrality_outstrength(net)

# 6. Network properties
network_summary(net, extended = TRUE, detailed = TRUE)
network_small_world(net)

# 7. Communities
comms <- communities(net)
membership(comms)
community_sizes(comms)
plot(comms, net)

# 8. Export
to_igraph(net)                # for igraph analyses
to_data_frame(net)            # for CSV export
write.csv(centrality(net), "centralities.csv")
```

## References

- cograph: Modern R Package for Network Visualization.
  <https://github.com/sonsoleslp/cograph>

- Saqr, M., López-Pernas, S., Törmänen, T., Kaliisa, R., Misiejuk, K., &
  Tikka, S. (2025). Transition Network Analysis: A Novel Framework for
  Modeling, Visualizing, and Identifying the Temporal Patterns of
  Learners and Learning Processes. In *Proceedings of the 15th
  International Learning Analytics and Knowledge Conference (LAK ’25)*
  (pp. 351–361). ACM. <https://doi.org/10.1145/3706468.3706513>

- Tikka, S., López-Pernas, S., & Saqr, M. (2025). tna: An R Package for
  Transition Network Analysis. *Applied Psychological Measurement*.
  <https://doi.org/10.1177/01466216251348840>
