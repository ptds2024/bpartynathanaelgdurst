#' Simulate Ice Cream Parties Based on Weather Data
#'
#' Simulates a specified number of ice cream parties based on weather data, estimating the total volume
#' and surface area of cones required for each party. Weather data is fetched from the OpenWeatherMap API,
#' and simulations are run in parallel for efficiency.
#'
#' @name simulate_parties
#' @param location A character string specifying the city name for weather data (e.g., "Lausanne").
#' @param api_key A character string representing the OpenWeatherMap API key.
#' @param n_parties An integer specifying the number of parties to simulate (default: 10000).
#' @param epsilon A numeric value for the small interval used in surface area calculations (default: 1e-3).
#' @param lower A numeric value specifying the lower bound of integration for cone volume and surface area (default: 0).
#' @param upper A numeric value specifying the upper bound of integration for cone volume and surface area (default: 10).
#' @param seed An integer seed for random number generation to ensure reproducibility (default: 123).
#' @return A data frame containing statistics (mean and standard deviation) for the total volume
#'         and surface area of cones.
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @importFrom parallel makeCluster stopCluster clusterEvalQ clusterExport parLapply
#' @importFrom stats rpois sd
#' @importFrom Rcpp cppFunction
#' @examplesIf Sys.getenv("OWM_API_KEY") != ""
#' # Example usage:
#' api_key <- Sys.getenv("OWM_API_KEY")
#'
#' # Simulate 100 parties based on weather data for Lausanne
#' results <- simulate_parties(
#'   location = "Lausanne",
#'   api_key = api_key,
#'   n_parties = 100,
#'   epsilon = 1e-3,
#'   lower = 0,
#'   upper = 10,
#'   seed = 42
#' )
#' print(results)
#'
#' @export


utils::globalVariables(c("cone_radius_with_variation_cpp"))

# Function to calculate cone volume and surface area
calculate_cone_volume_surface <- function(lower, upper, epsilon) {
  volume <- pi * integrate(function(x) cone_radius_with_variation_cpp(x)^2, lower = lower, upper = upper)$value
  surface_area <- 2 * pi * integrate(function(x) {
    h_x <- cone_radius_with_variation_cpp(x)
    h_x_derivative <- (cone_radius_with_variation_cpp(x + epsilon) - h_x) / epsilon
    h_x * sqrt(1 + h_x_derivative^2)
  }, lower = lower, upper = upper - epsilon)$value
  return(c(volume, surface_area))
}

# Function to simulate a single party
simulate_party <- function(lambda_guests, lower, upper, epsilon) {
  n_guests <- rpois(1, lambda_guests)
  n_cones <- sum(sample(c(1, 2), n_guests, replace = TRUE, prob = c(0.67, 0.33)))
  cone_metrics <- calculate_cone_volume_surface(lower, upper, epsilon)
  total_volume <- n_cones * cone_metrics[1]
  total_surface_area <- n_cones * cone_metrics[2]
  return(c(volume = total_volume, surface_area = total_surface_area))
}

# Main function
simulate_parties <- function(location = "Lausanne",
                             api_key,
                             n_parties = 10000,
                             epsilon = 1e-3,
                             lower = 0,
                             upper = 10,
                             seed = 123) {
  # Fetch forecast data directly with httr
  response <- httr::GET(
    url = "http://api.openweathermap.org/data/2.5/forecast",
    query = list(q = location, appid = api_key, units = "metric")
  )

  if (httr::status_code(response) != 200) {
    stop("Failed to fetch weather data. Check your API key and location.")
  }

  forecast <- jsonlite::fromJSON(httr::content(response, as = "text"), flatten = TRUE)
  forecast_data <- forecast$list

  # Extract relevant data
  dates <- as.POSIXct(forecast_data$dt_txt, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  temp <- as.double(forecast_data$main.temp)
  humidity <- as.integer(forecast_data$main.humidity)
  pressure <- as.integer(forecast_data$main.pressure)

  # Create DataFrame
  forecast_df <- data.frame(
    Date = dates,
    Temperature = temp,
    Humidity = humidity,
    Pressure = pressure
  )

  # Weather parameters
  Temperature <- mean(forecast_df$Temperature)
  Humidity <- mean(forecast_df$Humidity / 100)
  Pressure <- mean(forecast_df$Pressure)

  # Calculate lambda for guest estimation
  lambda_guests <- exp(0.5 + (0.5 * Temperature) - (3 * Humidity) + (0.001 * Pressure))

  # Set up cluster for parallel processing
  num_cores <- min(16, parallel::detectCores() - 1)
  cl <- parallel::makeCluster(num_cores)

  # Load and define necessary functions on workers
  parallel::clusterEvalQ(cl, {
    library(Rcpp)
    cppFunction('
    NumericVector cone_radius_with_variation_cpp(NumericVector x) {
      int n = x.size();
      NumericVector result(n);
      double variation = R::rnorm(0, 0.1);

      for (int i = 0; i < n; ++i) {
        if (x[i] < 0) result[i] = 0;
        else if (x[i] < 8) result[i] = x[i] / 8 + variation;
        else if (x[i] < 8 + M_PI / 2) result[i] = 1 + 1.5 * sin(x[i] - 8) + variation;
        else if (x[i] < 10) result[i] = 2.5 - 2 * cos(x[i] - 8) + variation;
        else result[i] = 0;
      }
      return result;
    }
    ')
  })

  # Export necessary data and functions to workers
  parallel::clusterExport(cl, varlist = c("simulate_party", "calculate_cone_volume_surface", "lambda_guests", "epsilon", "lower", "upper"), envir = environment())

  # Run the simulation in parallel
  set.seed(seed)
  results <- parallel::parLapply(cl, 1:n_parties, function(i) simulate_party(lambda_guests, lower, upper, epsilon))

  # Stop the cluster
  parallel::stopCluster(cl)

  # Extract volumes and surface areas
  volumes <- sapply(results, `[[`, "volume")
  surface_areas <- sapply(results, `[[`, "surface_area")

  # Calculate and return statistics
  stats <- data.frame(
    Metric = c(
      "Mean Volume (cm\u00b3)",
      "SD Volume (cm\u00b3)",
      "Mean Surface Area (cm\u00b2)",
      "SD Surface Area (cm\u00b2)"
    ),
    Value = c(
      mean(volumes), sd(volumes),
      mean(surface_areas), sd(surface_areas)
    )
  )

  return(stats)
}
