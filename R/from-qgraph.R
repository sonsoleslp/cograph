#' Convert a qgraph object to Sonnet parameters
#'
#' Extracts the network, layout, node labels, node colors, and pie chart data
#' from a qgraph object and passes them to a Sonnet plotting engine.
#'
#' @param qgraph_object Return value of \code{qgraph::qgraph()}
#' @param engine Which Sonnet renderer to use: \code{"splot"}, \code{"soplot"}, or \code{"sonplot"}
#' @param plot If TRUE, immediately plot using the chosen engine
#' @param ... Override any extracted parameter
#' @return Invisibly, a named list of Sonnet parameters
#' @export
from_qgraph <- function(qgraph_object, engine = c("splot", "soplot", "sonplot"), plot = TRUE, ...) {
  engine <- match.arg(engine)

  if (!inherits(qgraph_object, "qgraph") && is.null(qgraph_object$Arguments)) {
    stop("Input does not appear to be a qgraph object (missing 'Arguments' field)")
  }

  args <- qgraph_object$Arguments
  overrides <- list(...)

  # --- Input matrix ---
  x <- args$input
  if (is.null(x)) {
    el <- qgraph_object$Edgelist
    n <- length(qgraph_object$graphAttributes$Nodes$names)
    if (is.null(n) || n == 0) n <- max(c(el$from, el$to))
    x <- matrix(0, n, n)
    for (i in seq_along(el$from)) {
      x[el$from[i], el$to[i]] <- el$weight[i]
    }
  }

  # --- Build params ---
  params <- list(x = x)

  # Layout
  if (!is.null(qgraph_object$layout)) {
    params$layout <- qgraph_object$layout
  }

  # Labels
  if (!is.null(args$labels)) {
    params$labels <- args$labels
  }

  # Groups
  if (!is.null(args$groups)) {
    params$groups <- args$groups
  }

  # Node colors
  if (!is.null(args$color)) {
    params$node_fill <- args$color
  }

  # Node shape
  if (!is.null(args$shape)) {
    params$node_shape <- map_qgraph_shape(args$shape)
  }

  # Pie charts
  if (!is.null(args$pie)) {
    params$pie_values <- args$pie
  }
  if (!is.null(args$pieColor)) {
    params$pie_colors <- args$pieColor
  }

  # Title
  if (!is.null(args$title)) {
    params$title <- args$title
  }

  # Apply overrides
  for (nm in names(overrides)) {
    params[[nm]] <- overrides[[nm]]
  }

  # Plot
  if (plot) {
    plot_params <- params
    if (engine == "soplot") {
      plot_params$network <- plot_params$x
      plot_params$x <- NULL
    }
    plot_fn <- switch(engine, splot = splot, soplot = soplot, sonplot = sonplot)
    do.call(plot_fn, plot_params)
  }

  invisible(params)
}

#' Map qgraph shape names to Sonnet equivalents
#' @param shapes Character vector of qgraph shape names
#' @return Character vector of Sonnet shape names
#' @keywords internal
map_qgraph_shape <- function(shapes) {
  mapping <- c(
    "rectangle" = "square",
    "square"    = "square",
    "circle"    = "circle",
    "ellipse"   = "circle",
    "triangle"  = "triangle",
    "diamond"   = "diamond"
  )
  result <- mapping[shapes]
  unknown <- is.na(result)
  result[unknown] <- shapes[unknown]
  unname(result)
}
