#' Calculate the Radius of an Ice Cream Cone (For Loop)
#'
#' Computes the radius of an ice cream cone for a given vector of heights using a for loop.
#' This implementation processes each element sequentially, making it less efficient than vectorized alternatives.
#'
#' @param x A numeric vector representing the heights of the cone.
#' @return A numeric vector representing the radius of the cone at the specified heights.
#'         Returns 0 for heights outside the valid range.
#' @examples
#' # Example usage
#' cone_radius_for(c(-1, 5, 8.5, 9.5, 12))  # Returns radii for a vector of heights
#' @export


cone_radius_for <- function(x) {
  result <- numeric(length(x))
  for (i in seq_along(x)) {
    if (x[i] < 0) {
      result[i] <- 0
    } else if (x[i] < 8) {
      result[i] <- x[i] / 8
    } else if (x[i] < 8 + pi / 2) {
      result[i] <- 1 + 1.5 * sin(x[i] - 8)
    } else if (x[i] < 10) {
      result[i] <- 2.5 - 2 * cos(x[i] - 8)
    } else {
      result[i] <- 0
    }
  }
  return(result)
}
