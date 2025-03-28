# urbanServer.R
urbanServer <- function(input, output, session) {
  # Load survey data
  survey_data <- reactive({
    load_survey_data("PER_2024")
  })
  
  # Use the current theme
  current_theme <- reactiveVal(theme_config)
  
  # Public transport usage for work (Q72.9 - Binary)
  output$public_transport_work <- renderText({
    req(survey_data())
    
    # Extract binary values for public transport usage
    pt_values <- survey_data()$responses$Q72.9
    pt_values <- pt_values[!is.na(pt_values)]
    
    # Calculate percentage
    pt_percentage <- 100 * sum(pt_values == "1") / length(pt_values)
    
    sprintf("%.1f%%", pt_percentage)
  })
  
  # Private vehicle usage (Q73.8 - Binary)
  output$private_vehicle_usage <- renderText({
    req(survey_data())
    
    # Extract binary values for private vehicle usage
    veh_values <- survey_data()$responses$Q73.8
    veh_values <- veh_values[!is.na(veh_values)]
    
    # Calculate percentage
    veh_percentage <- 100 * sum(veh_values == "1") / length(veh_values)
    
    sprintf("%.1f%%", veh_percentage)
  })
  
  # Bus satisfaction (Q75 - Interval 1-10)
  output$bus_satisfaction <- renderText({
    req(survey_data())
    
    # Extract satisfaction values
    bus_values <- as.numeric(survey_data()$responses$Q75)
    bus_values <- bus_values[!is.na(bus_values)]
    
    # Calculate average
    bus_avg <- mean(bus_values)
    
    sprintf("%.1f/10", bus_avg)
  })
  
  # Juarez Bus satisfaction (Q78 - Interval 1-10)
  output$juarez_bus_satisfaction <- renderText({
    req(survey_data())
    
    # Extract satisfaction values
    jbus_values <- as.numeric(survey_data()$responses$Q78)
    jbus_values <- jbus_values[!is.na(jbus_values)]
    
    # Calculate average
    jbus_avg <- mean(jbus_values)
    
    sprintf("%.1f/10", jbus_avg)
  })
  
  # Environmental quality plot
  output$env_quality_plot <- renderPlotly({
    req(survey_data())
    create_env_quality_plot(survey_data()$responses, custom_theme = current_theme())
  })



}