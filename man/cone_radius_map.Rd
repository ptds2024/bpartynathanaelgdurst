% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cone_radius_purrr_map.R
\name{cone_radius_map}
\alias{cone_radius_map}
\title{Calculate the Radius of an Ice Cream Cone (purrr::map)}
\usage{
cone_radius_map(x)
}
\arguments{
\item{x}{A numeric vector representing the heights of the cone.}
}
\value{
A numeric vector representing the radius of the cone at the specified heights.
Returns 0 for heights outside the valid range.
}
\description{
Computes the radius of an ice cream cone for a given vector of heights using the \code{purrr::map_dbl} function.
This implementation applies a function to each element in the vector, leveraging \code{purrr} for cleaner syntax.
}
\examples{
# Example usage
cone_radius_map(c(-1, 5, 8.5, 9.5, 12))  # Returns radii for a vector of heights
}
