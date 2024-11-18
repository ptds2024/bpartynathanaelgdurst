#' Calculate the Radius of an Ice Cream Cone (Fully Vectorized)
#'
#' Computes the radius of an ice cream cone for a given vector of heights using a fully vectorized approach.
#' This implementation is optimized for performance by applying logical indexing and avoiding iterative computations.
#'
#' @param x A numeric vector representing the heights of the cone.
#' @return A numeric vector representing the radius of the cone at the specified heights.
#'         Returns 0 for heights outside the valid range.
#' @examples
#' # Example usage
#' cone_radius_vectorized_direct(c(-1, 5, 8.5, 9.5, 12))  # Returns radii for a vector of heights
#' @export


cone_radius_vectorized_direct <- function(x) {
  result <- numeric(length(x))

  result[x >= 0 & x < 8] <- x[x >= 0 & x < 8] / 8
  result[x >= 8 & x < 8 + pi / 2] <- 1 + 1.5 * sin(x[x >= 8 & x < 8 + pi / 2] - 8)
  result[x >= 8 + pi / 2 & x < 10] <- 2.5 - 2 * cos(x[x >= 8 + pi / 2 & x < 10] - 8)

  return(result)
}
