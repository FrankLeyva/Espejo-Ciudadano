# urban_server.R - Updated with Enhanced Data Management

urbanServer <- function(input, output, session, current_theme = NULL) {
  # Get dependencies from userData
  selectedYear <- session$userData$selectedYear
  data_manager <- session$userData$data_manager
  geo_data <- session$userData$geoData
  
  active_theme <- reactive({
    if (is.function(current_theme)) {
      current_theme()
    } else if (!is.null(current_theme)) {
      current_theme
    } else {
      get_section_theme("movilidad")
    }
  })
  
  # Try to load pre-saved plots first, then create if needed
  plots <- reactive({
    req(selectedYear())
    
    # Try to load saved plots
    saved_plots <- data_manager$load_saved_plots("urban", selectedYear())
    
    if (!is.null(saved_plots)) {
      return(saved_plots)
    }
    
    # If no saved plots, create them
    survey_id <- paste0("PER_", selectedYear())
    
    # Create plots using data manager
    plot_list <- list()
    
    # Environmental quality plot
    plot_key <- paste0("env_quality_plot_", survey_id)
    plot_list$env_quality_plot <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        survey_data <- data_manager$get_survey_data(survey_id)
        
        create_env_quality_plot(
          survey_data$responses, 
          custom_theme = active_theme()
        ) %>% 
          apply_plotly_theme(custom_theme = active_theme())
      }
    )
    
    # Save plots for future use
    data_manager$save_plots(plot_list, "urban", selectedYear())
    
    return(plot_list)
  })
  
  # Value box calculations
  calculations <- reactive({
    req(selectedYear())
    
    calc_cache_key <- paste0("urban_calculations_", selectedYear())
    if (!is.null(data_manager$cache[[calc_cache_key]])) {
      return(data_manager$cache[[calc_cache_key]])
    }
    
    survey_id <- paste0("PER_", selectedYear())
    
    # Get survey data
    survey_data <- data_manager$get_survey_data(survey_id)
    
    calc_list <- list()
    
    # Public transport usage for work (Q72.9 - Binary)
    if (!is.null(survey_data)) {
      pt_values <- survey_data$responses$Q72.9
      pt_percentage <- 100 * sum(pt_values == "1", na.rm = TRUE) / length(pt_values)
      calc_list$public_transport_work <- sprintf("%.1f%%", pt_percentage)
      
      # Private vehicle usage (Q73.8 - Binary)
      veh_values <- survey_data$responses$Q73.8
      veh_percentage <- 100 * sum(veh_values == "1", na.rm = TRUE) / length(veh_values)
      calc_list$private_vehicle_usage <- sprintf("%.1f%%", veh_percentage)
      
      # Bus satisfaction (Q75 - Interval 1-10)
      bus_values <- as.numeric(survey_data$responses$Q75)
      bus_values <- bus_values[!is.na(bus_values)]
      if (length(bus_values) > 0) {
        bus_avg <- mean(bus_values)
        calc_list$bus_satisfaction <- sprintf("%.1f/10", bus_avg)
      } else {
        calc_list$bus_satisfaction <- "N/A"
      }
      
      # Juarez Bus satisfaction (Q78 - Interval 1-10)
      jbus_values <- as.numeric(survey_data$responses$Q78)
      jbus_values <- jbus_values[!is.na(jbus_values)]
      if (length(jbus_values) > 0) {
        jbus_avg <- mean(jbus_values)
        calc_list$juarez_bus_satisfaction <- sprintf("%.1f/10", jbus_avg)
      } else {
        calc_list$juarez_bus_satisfaction <- "N/A"
      }
    } else {
      calc_list$public_transport_work <- "N/A"
      calc_list$private_vehicle_usage <- "N/A"
      calc_list$bus_satisfaction <- "N/A"
      calc_list$juarez_bus_satisfaction <- "N/A"
    }
    
    # Cache calculations
    data_manager$cache[[calc_cache_key]] <- calc_list
    
    return(calc_list)
  })
  
  # Render outputs
  output$env_quality_plot <- renderPlotly({
    plots()$env_quality_plot
  })
  
  # Render value box text outputs
  output$public_transport_work <- renderText({
    calculations()$public_transport_work
  })
  
  output$private_vehicle_usage <- renderText({
    calculations()$private_vehicle_usage
  })
  
  output$bus_satisfaction <- renderText({
    calculations()$bus_satisfaction
  })
  
  output$juarez_bus_satisfaction <- renderText({
    calculations()$juarez_bus_satisfaction
  })
}