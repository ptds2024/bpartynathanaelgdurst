#
# This is a Shiny web application.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

######## [START] Packages requirements ########

##### Add here the packages needed #############################################
packagesNeeded <- c("dplyr", "knitr", "tidyr", "shiny",
                    "owmr", "httr", "ggplot2", "parallel", "Rcpp")
################################################################################

installedPackages <- installed.packages()

for (packageName in packagesNeeded) {
  packageExists <- is.element(packageName, installedPackages)
  if (packageExists != TRUE) {
    install.packages(packageName)
    library(packageName, character.only = TRUE)
    print(paste(packageName, "has been installed and the library is loaded!"))
  } else {
    library(packageName, character.only = TRUE)
    print(paste(packageName, "is installed and the library is loaded!"))
  }
}

rm(installedPackages, packageName, packagesNeeded, packageExists)

######## [END] Packages requirements ########

######## [START] Set working directory ########

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

######## [END] Set working directory ########

######## [START] Custom Functions ########

### Check API Key
check_api_key <- function() {
  renviron_path <- file.path(getwd(), ".Renviron")
  if (file.exists(renviron_path)) {
    readRenviron(renviron_path)
    Sys.sleep(0.2)
    return(Sys.getenv("OWM_API_KEY") != "")
  }
  return(FALSE)
}

### Save API Key
write_api_key_to_renviron <- function(api_key) {
  renviron_path <- file.path(getwd(), ".Renviron")
  if (!file.exists(renviron_path)) {
    file.create(renviron_path)
  }
  write(paste0("OWM_API_KEY=", api_key), renviron_path, append = TRUE)
  readRenviron(renviron_path)
}

### Validate city using API
check_city_validity <- function(city_name, api_key) {
  response <- httr::GET("http://api.openweathermap.org/data/2.5/weather",
                        query = list(q = city_name, appid = api_key))
  httr::status_code(response) == 200
}

### Function to prepare forecast data for plotting
create_forecast_df <- function(forecast) {
  forecast_data <- forecast$list
  dates <- as.POSIXct(forecast_data$dt_txt, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  temp <- as.double(forecast_data$main.temp)
  humidity <- as.integer(forecast_data$main.humidity)
  pressure <- as.integer(forecast_data$main.pressure)

  forecast_df <- data.frame(
    Date = dates,
    Temperature = temp,
    Humidity = humidity,
    Pressure = pressure
  )

  return(forecast_df)
}

### Helper function to plot weather factors
plot_factors <- function(forecast_df, factor_name, label, city) {
  ggplot(forecast_df, aes(x = Date, y = !!sym(factor_name))) +
    geom_line() +
    scale_x_datetime(date_labels = "%b %d", date_breaks = "1 day") +
    labs(title = paste(label, "Forecast for", city), x = "Date", y = label) +
    theme_minimal()
}

### Simulate a party (volume and surface area calculation)
simulate_party <- function(lambda_guests, n_cones_func, cone_metrics_func) {
  n_guests <- rpois(1, lambda_guests)
  n_cones <- n_cones_func(n_guests)
  cone_metrics <- cone_metrics_func()
  total_volume <- n_cones * cone_metrics[1]
  total_surface_area <- n_cones * cone_metrics[2]
  c(volume = total_volume, surface_area = total_surface_area)
}

### Calculate the volume and surface area of a cone with random variation
calculate_cone_volume_surface <- function(epsilon = 1e-5) {
  variation <- rnorm(1, mean = 0, sd = 0.1)
  # Volume calculation
  volume_integrand <- function(x) {
    cone_radius_with_variation_cpp(x, variation)^2
  }
  volume <- pi * integrate(volume_integrand, lower = 0, upper = 10)$value

  # Surface area calculation
  surface_integrand <- function(x) {
    h_x <- cone_radius_with_variation_cpp(x, variation)
    h_x_plus_epsilon <- cone_radius_with_variation_cpp(x + epsilon, variation)
    h_x_derivative <- (h_x_plus_epsilon - h_x) / epsilon
    h_x * sqrt(1 + h_x_derivative^2)
  }
  surface_area <- 2 * pi * integrate(surface_integrand, lower = 0, upper = 10 - epsilon)$value
  return(c(volume, surface_area))
}

######## [END] Custom Functions ########

############## [START] Shiny UI ##############

locationModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3(textOutput(ns("current_weather"))),
    fluidRow(
      column(4, plotOutput(ns("temp_plot"))),
      column(4, plotOutput(ns("humidity_plot"))),
      column(4, plotOutput(ns("pressure_plot")))
    ),
    fluidRow(
      column(6, plotOutput(ns("sim_volume_plot"))),
      column(6, plotOutput(ns("sim_surface_plot")))
    ),
    fluidRow(
      column(12, uiOutput(ns("simulation_summary")))
    )
  )
}

# Main UI
ui <- fluidPage(
  titlePanel("Enhanced Weather and Simulation App"),
  sidebarLayout(
    sidebarPanel(
      textInput("city1", "Enter City Name for Location 1:", value = ""),
      textInput("city2", "Enter City Name for Location 2:", value = ""),
      textInput("city3", "Enter City Name for Location 3:", value = ""),
      numericInput("n_sim", "Number of Simulations:", value = 10000, min = 1),
      actionButton("run_sim", "Run Simulations")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Location 1", locationModuleUI("loc1")),
        tabPanel("Location 2", locationModuleUI("loc2")),
        tabPanel("Location 3", locationModuleUI("loc3"))
      )
    )
  )
)

############## [END] Shiny UI ##############

############## [START] Shiny Server ##############

locationModuleServer <- function(id, city_input, n_sim_input, run_simulation) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive value to store weather data and simulation results
    weather_data <- reactiveValues(
      data = NULL,
      forecast_df = NULL,
      sim_results = NULL
    )

    # Fetch weather data when city changesr
    observeEvent(city_input(), {
      city <- city_input()
      if (!check_city_validity(city, Sys.getenv("OWM_API_KEY"))) {
        weather_data$data <- NULL
        weather_data$forecast_df <- NULL
        weather_data$sim_results <- NULL
        return()
      }
      data <- list(
        current = owmr::get_current(city, api_key = Sys.getenv("OWM_API_KEY"), units = "metric"),
        forecast = owmr::get_forecast(city, api_key = Sys.getenv("OWM_API_KEY"), units = "metric")
      )
      weather_data$data <- data
      weather_data$forecast_df <- create_forecast_df(data$forecast)
      weather_data$sim_results <- NULL  # Invalidate previous simulations
    })

    simulation_done <- reactiveVal(FALSE)

    # Run simulations when the "Run Simulations" button is clicked
    observeEvent(run_simulation(), {
      req(weather_data$data)
      simulation_done(FALSE)
      forecast_df <- weather_data$forecast_df
      # Use average forecasted values
      Temperature <- mean(forecast_df$Temperature)
      Humidity <- mean(forecast_df$Humidity) / 100  # Convert to 0-1 range
      Pressure <- mean(forecast_df$Pressure)

      # Calculate lambda_guests using the provided formula
      lambda_guests <- exp(0.5 + 0.5 * Temperature - 3 * Humidity + 0.001 * Pressure)

      n_sim <- n_sim_input()
      num_cores <- min(16, detectCores() - 1)
      cl <- makeCluster(num_cores)

      # Load necessary libraries on workers
      clusterEvalQ(cl, {
        library(Rcpp)
      })

      # Define the Rcpp function on the workers
      clusterEvalQ(cl, {
        Rcpp::cppFunction('
    NumericVector cone_radius_with_variation_cpp(NumericVector x, double variation) {
      int n = x.size();
      NumericVector h(n);
      for (int i = 0; i < n; ++i) {
        double xi = x[i];
        if (xi < 0) {
          h[i] = 0;
        } else if (xi < 8) {
          h[i] = xi / 8 + variation;
        } else if (xi < 8 + M_PI / 2) {
          h[i] = 1 + 1.5 * sin(xi - 8) + variation;
        } else if (xi < 10) {
          h[i] = 2.5 - 2 * cos(xi - 8) + variation;
        } else {
          h[i] = 0;
        }
      }
      return h;
    }
    ')
      })

      # Define n_cones_func before exporting
      n_cones_func <- function(n_guests) {
        sum(sample(c(1, 2), n_guests, replace = TRUE, prob = c(0.67, 0.33)))
      }

      # Export necessary functions and variables to the workers
      clusterExport(cl, varlist = c("lambda_guests", "calculate_cone_volume_surface", "simulate_party", "n_cones_func"), envir = environment())

      # Run the simulation in parallel
      results <- parLapply(cl, 1:n_sim, function(i) {
        simulate_party(lambda_guests, n_cones_func, calculate_cone_volume_surface)
      })

      stopCluster(cl)

      weather_data$sim_results <- results
      simulation_done(TRUE)
    })

    # Render outputs
    output$current_weather <- renderText({
      req(weather_data$data)
      data <- weather_data$data
      paste("Current Weather in", city_input(), ":",
            "Temperature:", data$current$main$temp, "°C",
            "Humidity:", data$current$main$humidity, "%",
            "Pressure:", data$current$main$pressure, "hPa")
    })

    output$temp_plot <- renderPlot({
      req(weather_data$forecast_df)
      plot_factors(weather_data$forecast_df, "Temperature", "Temperature (°C)", city_input())
    })

    output$humidity_plot <- renderPlot({
      req(weather_data$forecast_df)
      plot_factors(weather_data$forecast_df, "Humidity", "Humidity (%)", city_input())
    })

    output$pressure_plot <- renderPlot({
      req(weather_data$forecast_df)
      plot_factors(weather_data$forecast_df, "Pressure", "Pressure (hPa)", city_input())
    })

    output$sim_volume_plot <- renderPlot({
      req(weather_data$data)
      req(weather_data$sim_results)
      volumes <- sapply(weather_data$sim_results, function(x) x["volume"])
      hist(volumes, main = paste("Total Volume for", city_input()), xlab = "Volume (cm³)", breaks = 50)
    })

    output$sim_surface_plot <- renderPlot({
      req(weather_data$data)
      req(weather_data$sim_results)
      surface_areas <- sapply(weather_data$sim_results, function(x) x["surface_area"])
      hist(surface_areas, main = paste("Total Surface Area for", city_input()), xlab = "Surface Area (cm²)", breaks = 50)
    })

    output$simulation_summary <- renderUI({
      req(weather_data$data)
      req(weather_data$sim_results)
      volumes <- sapply(weather_data$sim_results, function(x) x["volume"])
      surface_areas <- sapply(weather_data$sim_results, function(x) x["surface_area"])

      volume_99 <- quantile(volumes, 0.99)
      surface_area_99 <- quantile(surface_areas, 0.99)

      HTML(paste0("<h4>Simulation Summary for ", city_input(), "</h4>",
                  "<p>Based on the simulations, to satisfy 99% of the events in ", city_input(), ", you should prepare for up to <strong>", round(volume_99, 2), " cm³</strong> of ice cream, ",
                  "and up to <strong>", round(surface_area_99, 2), " cm²</strong> of chocolate coating.</p>"))
    })

    # Return simulation_done reactiveVal
    return(list(simulation_done = simulation_done))

  })
}

# Main Server
server <- function(input, output, session) {
  # Reactive expressions for inputs
  city1 <- reactive(input$city1)
  city2 <- reactive(input$city2)
  city3 <- reactive(input$city3)
  n_sim <- reactive(input$n_sim)
  run_sim <- reactive(input$run_sim)

  # Check and handle API key
  api_key_ready <- reactiveVal(FALSE)

  observe({
    if (check_api_key()) {
      if (check_city_validity("Lausanne", Sys.getenv("OWM_API_KEY"))) {
        api_key_ready(TRUE)
      } else {
        api_key_ready(FALSE)
        showModal(modalDialog(
          title = "Enter OpenWeatherMap API Key",
          textInput("api_key", "Please enter your OpenWeatherMap API key:"),
          easyClose = FALSE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton("save_api_key", "Save")
          )
        ))
      }
    } else {
      api_key_ready(FALSE)
      showModal(modalDialog(
        title = "Enter OpenWeatherMap API Key",
        textInput("api_key", "Please enter your OpenWeatherMap API key:"),
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("save_api_key", "Save")
        )
      ))
    }
  })

  observeEvent(input$save_api_key, {
    api_key <- input$api_key
    if (nzchar(api_key)) {
      if (check_city_validity("Lausanne", api_key)) {
        write_api_key_to_renviron(api_key)
        readRenviron(file.path(getwd(), ".Renviron"))
        api_key_ready(TRUE)
        removeModal()
        showNotification("API key saved successfully!", type = "message")
      } else {
        showNotification("Invalid API key. Please provide a correct key!", type = "error")
      }
    } else {
      showNotification("Please provide a valid API key!", type = "error")
    }
  })

  # Reactive value to track if simulations are running
  simulations_running <- reactiveVal(FALSE)

  # When Run Simulations button is clicked
  observeEvent(input$run_sim, {
    simulations_running(TRUE)
    showNotification("Simulations are running...", type = "message", duration = NULL, id = "sim_notice", closeButton = FALSE)

    # Reset simulation_done in modules
    loc1$simulation_done(FALSE)
    loc2$simulation_done(FALSE)
    loc3$simulation_done(FALSE)
  })

  # Observe when all simulations are done
  observe({
    req(simulations_running())

    # Track which modules have completed simulations
    completed_simulations <- c(
      loc1$simulation_done(),
      loc2$simulation_done(),
      loc3$simulation_done()
    )

    # Filter for active city inputs
    active_cities <- c(input$city1, input$city2, input$city3)
    active_cities <- active_cities[active_cities != ""] # Exclude empty city inputs

    num_active_cities <- length(active_cities)
    num_completed_cities <- sum(completed_simulations[seq_len(num_active_cities)]) # Match active modules

    if (num_completed_cities == num_active_cities) {
      removeNotification("sim_notice")
      showNotification("Simulations completed!", type = "message", duration = 5)
      simulations_running(FALSE)
    }
  })

# # Reactive value to track if simulations are running
# simulations_running <- reactiveVal(FALSE)
#
# # When Run Simulations button is clicked
# observeEvent(input$run_sim, {
#   simulations_running(TRUE)
#   showNotification("Simulations are running...", type = "message", duration = NULL, id = "sim_notice", closeButton = FALSE)
#
#   # Reset simulation_done in modules
#   loc1$simulation_done(FALSE)
#   loc2$simulation_done(FALSE)
#   loc3$simulation_done(FALSE)
# })
#
# # Observe when all simulations are done
# observe({
#   req(simulations_running())
#
#   # Track which modules have completed simulations
#   completed_simulations <- c(
#     loc1$simulation_done(),
#     loc2$simulation_done(),
#     loc3$simulation_done()
#   )
#
#   # Check if simulations are completed for all active cities
#   num_active_cities <- sum(!is.null(c(input$city1, input$city2, input$city3)))
#   num_completed_cities <- sum(completed_simulations)
#
#   if (num_completed_cities == num_active_cities) {
#     removeNotification("sim_notice")
#     showNotification("Simulations completed!", type = "message", duration = 5)
#     simulations_running(FALSE)
#   }
# })


  # Initialize modules
  loc1 <- locationModuleServer("loc1", city1, n_sim, run_sim)
  loc2 <- locationModuleServer("loc2", city2, n_sim, run_sim)
  loc3 <- locationModuleServer("loc3", city3, n_sim, run_sim)
}

############## [END] Shiny Server ##############

# Run the application
shinyApp(ui = ui, server = server)

######## [END] Shiny App ########
