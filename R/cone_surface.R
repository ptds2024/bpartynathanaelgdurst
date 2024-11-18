#' Calculate the Surface Area of an Ice Cream Cone
#'
#' Computes the surface area of an ice cream cone using the radius function and numerical differentiation.
#'
#' @param lower The lower bound of the integration range (default: 0).
#' @param upper The upper bound of the integration range (default: 10).
#' @param epsilon A small value for numerical differentiation (default: 1e-3).
#' @return The calculated surface area of the cone.
#' @examples
#' cone_surface()
#' cone_surface(lower = 2, upper = 8)
#' @importFrom stats integrate
#' @export


cone_surface <- function(lower = 0, upper = 10, epsilon = 1e-3) {
  # Input validation
  if (!is.numeric(lower) || !is.numeric(upper) || !is.numeric(epsilon)) {
    stop("All inputs (lower, upper, epsilon) must be numeric.")
  }

  # Function for h(x) * sqrt(1 + (dh/dx)^2) for integration
  surface_area_integrand <- function(x) {
    h_x <- cone_radius_cpp(x)
    h_x_derivative <- (cone_radius_cpp(x + epsilon) - h_x) / epsilon
    h_x * sqrt(1 + h_x_derivative^2)
  }

  # Calculate the surface area using integration
  surface_area_result <- 2 * pi * integrate(surface_area_integrand, lower = lower, upper = upper)$value

  return(surface_area_result)
}
