#' Calculate the Radius of an Ice Cream Cone (Rcpp)
#'
#' Computes the radius of an ice cream cone for a given vector of heights using an optimized C++ implementation.
#' @param x A numeric vector representing the heights of the cone.
#' @return A numeric vector representing the radius of the cone at the specified heights.
#'         Returns 0 for heights outside the valid range.
#' @examples
#' cone_radius_cpp(c(-1, 5, 8.5, 9.5, 12))
#' @export
#' @useDynLib bpartynathanaelgdurst, .registration = TRUE
#' @importFrom Rcpp sourceCpp


cone_radius_cpp <- function(x) {
  .Call('_bpartynathanaelgdurst_cone_radius_cpp', x)
}
