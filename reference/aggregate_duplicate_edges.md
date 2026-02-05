# Aggregate Duplicate Edges

Combines duplicate edges by aggregating their weights using a specified
function (sum, mean, max, min, or first).

## Usage

``` r
aggregate_duplicate_edges(edges, method = "mean")
```

## Arguments

- edges:

  Data frame with `from`, `to`, and `weight` columns.

- method:

  Aggregation method: `"sum"` (default), `"mean"`, `"max"`, `"min"`,
  `"first"`, or a custom function that takes a numeric vector and
  returns a single value.

## Value

A deduplicated data frame with the same columns as the input, where each
node pair appears only once with its aggregated weight.

## Details

### Aggregation Methods

- **sum**:

  Total weight of all duplicate edges. Useful for frequency counts or
  when edges represent additive quantities (e.g., number of emails).

- **mean**:

  Average weight. Useful for averaging multiple measurements or when
  duplicates represent repeated observations.

- **max**:

  Maximum weight. Useful for finding the strongest connection or most
  recent value.

- **min**:

  Minimum weight. Useful for the most conservative estimate or earliest
  value.

- **first**:

  Keep first occurrence. Useful for preserving original order or when
  duplicates are erroneous.

The output edge list uses canonical node ordering (lower index first for
undirected networks), ensuring consistent from/to assignment.

## See also

[`detect_duplicate_edges`](http://sonsoles.me/cograph/reference/detect_duplicate_edges.md)
for identifying duplicates before aggregation

## Examples

``` r
if (FALSE) { # \dontrun{
# Create edges with duplicates
edges <- data.frame(
  from = c(1, 1, 2),
  to = c(2, 2, 3),
  weight = c(0.5, 0.3, 0.4)
)

# Aggregate by sum (0.5 + 0.3 = 0.8)
aggregate_duplicate_edges(edges, method = "sum")
#   from to weight
# 1    1  2    0.8
# 2    2  3    0.4

# Aggregate by mean (average: 0.4)
aggregate_duplicate_edges(edges, method = "mean")
#   from to weight
# 1    1  2    0.4
# 2    2  3    0.4

# Use custom aggregation function
aggregate_duplicate_edges(edges, method = function(x) sqrt(sum(x^2)))
} # }
```
