#' Calculate the Volume of an Ice Cream Cone
#'
#' Computes the volume of an ice cream cone using the radius function.
#'
#' @param lower The lower bound of the integration range (default: 0).
#' @param upper The upper bound of the integration range (default: 10).
#' @return The calculated volume of the cone.
#' @examples
#' cone_volume()
#' cone_volume(lower = 2, upper = 8)
#' @importFrom stats integrate
#' @export


cone_volume <- function(lower = 0, upper = 10) {
  # Function to compute [h(x)]^2
  cone_radius_squared <- function(x) {
    cone_radius_cpp(x)^2
  }

  # Calculate volume using integration
  volume_result <- pi * integrate(cone_radius_squared, lower = lower, upper = upper)$value

  return(volume_result)
}
