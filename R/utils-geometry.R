#' @title Geometry Utilities
#' @description Utility functions for geometric calculations.
#' @name utils-geometry
#' @keywords internal
NULL

#' Calculate Distance Between Two Points
#'
#' @param x1,y1 First point coordinates.
#' @param x2,y2 Second point coordinates.
#' @return Euclidean distance.
#' @keywords internal
point_distance <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

#' Calculate Angle Between Two Points
#'
#' @param x1,y1 Start point coordinates.
#' @param x2,y2 End point coordinates.
#' @return Angle in radians.
#' @keywords internal
point_angle <- function(x1, y1, x2, y2) {
  atan2(y2 - y1, x2 - x1)
}

#' Calculate Point on Circle
#'
#' @param cx,cy Center coordinates.
#' @param r Radius.
#' @param angle Angle in radians.
#' @return List with x, y coordinates.
#' @keywords internal
point_on_circle <- function(cx, cy, r, angle) {
  list(
    x = cx + r * cos(angle),
    y = cy + r * sin(angle)
  )
}

#' Calculate Bezier Curve Points
#'
#' Calculate points along a quadratic Bezier curve.
#'
#' @param x0,y0 Start point.
#' @param x1,y1 Control point.
#' @param x2,y2 End point.
#' @param n Number of points to generate.
#' @return Data frame with x, y coordinates.
#' @keywords internal
bezier_points <- function(x0, y0, x1, y1, x2, y2, n = 50) {
  t <- seq(0, 1, length.out = n)

  # Quadratic Bezier formula
  x <- (1 - t)^2 * x0 + 2 * (1 - t) * t * x1 + t^2 * x2
  y <- (1 - t)^2 * y0 + 2 * (1 - t) * t * y1 + t^2 * y2

  data.frame(x = x, y = y)
}

#' Calculate Control Point for Curved Edge
#'
#' @param x1,y1 Start point.
#' @param x2,y2 End point.
#' @param curvature Curvature amount (0 = straight line).
#' @param pivot Position along edge (0-1) where control point sits. 0 = near source,
#'   0.5 = middle (default), 1 = near target.
#' @param shape Spline tension affecting curvature intensity (-1 to 1).
#'   Negative = sharper curve, Positive = gentler curve. Default 0.
#' @return List with x, y coordinates of control point.
#' @keywords internal
curve_control_point <- function(x1, y1, x2, y2, curvature, pivot = 0.5, shape = 0) {
  # Point along the edge based on pivot (0 = source, 0.5 = midpoint, 1 = target)
  pivot <- max(0, min(1, pivot))  # Clamp to [0, 1]
  mx <- x1 + pivot * (x2 - x1)
  my <- y1 + pivot * (y2 - y1)

  # Perpendicular offset
  dx <- x2 - x1
  dy <- y2 - y1
  len <- sqrt(dx^2 + dy^2)

  if (len == 0) {
    return(list(x = mx, y = my))
  }

  # Perpendicular unit vector
  px <- -dy / len
  py <- dx / len

  # Adjust curvature based on shape parameter
  # shape = 0: no adjustment
  # shape < 0: sharper curve (increase curvature magnitude)
  # shape > 0: gentler curve (decrease curvature magnitude)
  shape <- max(-1, min(1, shape))  # Clamp to [-1, 1]
  adjusted_curvature <- curvature * (1 - shape * 0.5)

  # Control point
  list(
    x = mx + adjusted_curvature * len * px,
    y = my + adjusted_curvature * len * py
  )
}

#' Calculate Arrow Head Points
#'
#' @param x,y Arrow tip position.
#' @param angle Angle of incoming edge (radians).
#' @param size Arrow size.
#' @param width Arrow width ratio (default 0.5).
#' @return Data frame with x, y coordinates of arrow vertices.
#' @keywords internal
arrow_points <- function(x, y, angle, size, width = 0.5) {
  # Arrow points relative to tip
  left_angle <- angle + pi - atan(width)
  right_angle <- angle + pi + atan(width)
  back_len <- size / cos(atan(width))

  data.frame(
    x = c(x,
          x + back_len * cos(left_angle),
          x + back_len * cos(right_angle)),
    y = c(y,
          y + back_len * sin(left_angle),
          y + back_len * sin(right_angle))
  )
}

#' Offset Point from Center
#'
#' Calculate a point offset from another point by a given distance.
#'
#' @param x,y Original point.
#' @param toward_x,toward_y Point to offset toward.
#' @param offset Distance to offset.
#' @return List with x, y coordinates.
#' @keywords internal
offset_point <- function(x, y, toward_x, toward_y, offset) {
  angle <- point_angle(x, y, toward_x, toward_y)
  list(
    x = x + offset * cos(angle),
    y = y + offset * sin(angle)
  )
}

#' Calculate Edge Endpoint on Node Border
#'
#' @param node_x,node_y Node center.
#' @param other_x,other_y Other endpoint.
#' @param node_size Node radius.
#' @param shape Node shape.
#' @return List with x, y coordinates.
#' @keywords internal
edge_endpoint <- function(node_x, node_y, other_x, other_y, node_size,
                          shape = "circle") {
  angle <- point_angle(node_x, node_y, other_x, other_y)

  # For most shapes, approximate with circle
  # Could be refined for specific shapes
  list(
    x = node_x + node_size * cos(angle),
    y = node_y + node_size * sin(angle)
  )
}
