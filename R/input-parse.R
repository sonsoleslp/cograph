#' @title Input Parsing Functions
#' @description Functions for parsing network input into internal format.
#' @name input-parse
NULL

#' Parse Network Input
#'
#' Automatically detects input type and converts to internal format.
#'
#' @param input Network input: matrix, data.frame (edge list), or igraph object.
#' @param directed Logical. Force directed interpretation. NULL for auto-detect.
#' @return List with nodes, edges, directed, and weights components.
#' @keywords internal
parse_input <- function(input, directed = NULL) {
  # Detect input type
  if (is.matrix(input)) {
    parse_matrix(input, directed = directed)
  } else if (is.data.frame(input)) {
    parse_edgelist(input, directed = directed)
  } else if (inherits(input, "igraph")) {
    parse_igraph(input, directed = directed)
  } else if (is.list(input) && !is.null(input$edges)) {
    # Already parsed format
    input
  } else {
    stop("Unsupported input type. Expected matrix, data.frame, or igraph object.",
         call. = FALSE)
  }
}

#' Detect if Matrix is Symmetric
#'
#' @param m A matrix.
#' @param tol Tolerance for comparison.
#' @return Logical.
#' @keywords internal
is_symmetric_matrix <- function(m, tol = .Machine$double.eps^0.5) {
  if (!is.matrix(m)) return(FALSE)
  if (nrow(m) != ncol(m)) return(FALSE)
  isTRUE(all.equal(m, t(m), tolerance = tol, check.attributes = FALSE))
}

#' Create Node Data Frame
#'
#' @param n Number of nodes.
#' @param labels Optional node labels.
#' @param names Optional node names for legend (defaults to labels).
#' @return Data frame with node information.
#' @keywords internal
create_nodes_df <- function(n, labels = NULL, names = NULL) {
  if (is.null(labels)) {
    labels <- as.character(seq_len(n))
  }

  if (is.null(names)) {
    names <- labels
  }

  data.frame(
    id = seq_len(n),
    label = labels,
    name = names,
    x = NA_real_,
    y = NA_real_,
    stringsAsFactors = FALSE
  )
}

#' Create Edge Data Frame
#'
#' @param from Vector of source node indices.
#' @param to Vector of target node indices.
#' @param weight Vector of edge weights.
#' @param directed Logical. Is the network directed?
#' @return Data frame with edge information.
#' @keywords internal
create_edges_df <- function(from, to, weight = NULL, directed = FALSE) {
  if (is.null(weight)) {
    weight <- rep(1, length(from))
  }

  data.frame(
    from = from,
    to = to,
    weight = weight,
    stringsAsFactors = FALSE
  )
}
