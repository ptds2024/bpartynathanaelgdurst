#' Calculate the Radius of an Ice Cream Cone (purrr::map)
#'
#' Computes the radius of an ice cream cone for a given vector of heights using the `purrr::map_dbl` function.
#' This implementation applies a function to each element in the vector, leveraging `purrr` for cleaner syntax.
#'
#' @param x A numeric vector representing the heights of the cone.
#' @return A numeric vector representing the radius of the cone at the specified heights.
#'         Returns 0 for heights outside the valid range.
#' @examples
#' # Example usage
#' cone_radius_map(c(-1, 5, 8.5, 9.5, 12))  # Returns radii for a vector of heights
#' @export


cone_radius_map <- function(x) {
  purrr::map_dbl(x, function(xi) {
    if (xi < 0) {
      0
    } else if (xi < 8) {
      xi / 8
    } else if (xi < 8 + pi / 2) {
      1 + 1.5 * sin(xi - 8)
    } else if (xi < 10) {
      2.5 - 2 * cos(xi - 8)
    } else {
      0
    }
  })
}
