#' Calculate the Radius of an Ice Cream Cone (Non-Vectorized)
#'
#' Computes the radius of an ice cream cone for a given height using a non-vectorized approach.
#' This implementation evaluates one input value at a time, making it less efficient for large datasets.
#'
#' @param x A numeric value representing the height of the cone.
#' @return A numeric value representing the radius of the cone at the specified height.
#'         Returns 0 for invalid height values.
#' @examples
#' # Example usage
#' cone_radius(5)  # Within the range 0-8
#' cone_radius(9)  # Within the range 8-10
#' cone_radius(-2) # Outside valid range, returns 0
#' @export


cone_radius <- function(x) {
  if (x < 0) {
    return(0)
  } else if (x < 8) {
    return(x / 8)
  } else if (x < 8 + pi / 2) {
    return(1 + 1.5 * sin(x - 8))
  } else if (x < 10) {
    return(2.5 - 2 * cos(x - 8))
  } else {
    return(0)
  }
}
