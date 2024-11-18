#' Launch the Shiny App for Weather and Simulation Insights
#'
#' Launches the Shiny application included in the package, located in the `inst/app` directory.
#' This application provides weather insights for three locations and simulates
#' resource requirements for ice cream parties based on weather conditions.
#'
#' Features of the app include:
#' - **Weather Insights**: View current weather and 5-day forecasts (temperature, humidity, pressure) for up to three cities using the OpenWeatherMap API.
#' - **Simulations**: Estimate total ice cream volume and chocolate coating requirements for 99% of events based on weather conditions.
#' - **Custom Visualizations**: Interactive plots for weather trends and simulation results.
#'
#' To run this app, you will need a valid OpenWeatherMap API key.
#'
#' @return This function does not return a value; it launches the Shiny app.
#' @examples
#' # Launch the app
#' if (interactive()) {
#'   run_app()
#' }
#' @export


run_app <- function() {
  app_dir <- system.file("app", package = "bpartynathanaelgdurst")
  if (app_dir == "") {
    stop("Could not find Shiny app directory. Please reinstall the package.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
