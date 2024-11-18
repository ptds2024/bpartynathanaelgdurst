#' Benchmark Different Implementations of cone_radius
#'
#' Benchmarks various implementations of the `cone_radius` function
#' for a predefined sequence of values and number of iterations. By default,
#' benchmarks all implementations on a sequence of 1000 values over 100 iterations.
#'
#' @name benchmark_cone_radius
#' @param implementations A named list of functions to benchmark. Defaults to:
#'   \itemize{
#'     \item \code{For_Loop}: `cone_radius_for`
#'     \item \code{Purrr_Map}: `cone_radius_map`
#'     \item \code{Apply}: `cone_radius_apply`
#'     \item \code{Direct_Vectorized}: `cone_radius_vectorized_direct`
#'     \item \code{Rcpp}: `cone_radius_cpp`
#'   }
#' @param x_values A numeric vector representing the heights to test
#'   (default: seq(0, 10, length.out = 1000)).
#' @param times An integer specifying the number of iterations for the benchmark
#'   (default: 100).
#' @param plot A logical value indicating whether to generate a plot of the results
#'   (default: FALSE).
#' @return A microbenchmark object containing the benchmark results.
#' @examples
#' # Default benchmark
#' benchmark_cone_radius()
#'
#' # Custom benchmark
#' benchmark_cone_radius(
#'   implementations = list(
#'     For_Loop = cone_radius_for,
#'     Rcpp = cone_radius_cpp
#'   ),
#'   x_values = seq(0, 10, length.out = 500),
#'   times = 50,
#'   plot = TRUE
#' )
#' @importFrom stats setNames
#' @export


utils::globalVariables(c("expr", "time"))

benchmark_cone_radius <- function(
    implementations = list(
      For_Loop = cone_radius_for,
      Purrr_Map = cone_radius_map,
      Apply = cone_radius_apply,
      Direct_Vectorized = cone_radius_vectorized_direct,
      Rcpp = cone_radius_cpp
    ),
    x_values = seq(0, 10, length.out = 1000),
    times = 100,
    plot = FALSE
) {
  # Ensure implementations is a named list
  if (!is.list(implementations) || is.null(names(implementations))) {
    stop("`implementations` must be a named list of functions.")
  }

  # Dynamically create expressions for benchmarking
  benchmark_expressions <- setNames(
    lapply(seq_along(implementations), function(i) {
      substitute(implementations[[i]](x_values), list(i = i))
    }),
    names(implementations)
  )

  # Run the benchmark
  results <- do.call(microbenchmark::microbenchmark, c(benchmark_expressions, list(times = times)))

  # Generate plot if requested
  if (plot) {
    p <- ggplot2::ggplot(results, ggplot2::aes(x = expr, y = time / 1000, fill = expr)) +  # Convert to microseconds
      ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.7) +  # Set transparency for boxplots
      ggplot2::geom_jitter(width = 0.2, size = 1, alpha = 0.5) +  # Add jittered points for data spread
      ggplot2::scale_y_log10(breaks = c(10, 30, 100, 300, 1000, 3000, 10000)) +  # Set custom log scale breaks
      ggplot2::scale_fill_brewer(palette = "Set2") +  # Use a color palette for distinct colors
      ggplot2::labs(
        title = "Benchmark Results for Vectorized Functions",
        x = "Method",
        y = "Execution Time (\u00b5s, Log Scale)"
      ) +
      ggplot2::theme_minimal(base_size = 14) +  # Increase base font size for readability
      ggplot2::theme(legend.position = "none")  # Remove legend as colors indicate methods

    print(p)  # Explicitly print the ggplot object
  }

  # Always print the benchmark results
  print(results)

  # Return the benchmark results invisibly to allow chaining or further usage
  invisible(results)
}
