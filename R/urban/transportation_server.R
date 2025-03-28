transportationServer <- function(input, output, session) {
  # Load survey data
  survey_data <- reactive({
    load_survey_data("PER_2024")
  })
  
  # Load geographical data
  geo_data <- reactive({
    tryCatch({
      sf::st_read('data/geo/Jrz_Map.geojson', quiet = TRUE)
    }, error = function(e) {
      showNotification(paste("Error loading geo data:", e$message), type = "error")
      NULL
    })
  })
  
  # Use the current theme
  current_theme <- reactiveVal(theme_config)
  
  # Prepare data for bus satisfaction map (Q75)
  output$bus_satisfaction_map <- renderLeaflet({
    req(survey_data(), geo_data())
    
    # Prepare interval data using existing function
    bus_satisfaction <- prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q75",
      metadata = survey_data()$metadata
    )
    
    create_interval_district_map(
      data = bus_satisfaction,
      geo_data = geo_data(),
      use_gradient = F,
      color_scale = "Blues",
      custom_theme = current_theme()
    )
  })
  
  # Prepare data for Juarez Bus satisfaction map (Q79)
  output$juarez_bus_satisfaction_map <- renderLeaflet({
    req(survey_data(), geo_data())
    
    # Prepare interval data using existing function
    juarez_bus_satisfaction <- prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q78",
      metadata = survey_data()$metadata
    )
    
    create_interval_district_map(
      data = juarez_bus_satisfaction,
      geo_data = geo_data(),
      use_gradient = F,
      color_scale = "Blues",
      custom_theme = current_theme()
    )
  })
  
  # Create bus issues plot (Q76.1 to Q76.7)
  output$bus_issues_plot <- renderPlotly({
    req(survey_data())
    create_transport_issues_plot(
      survey_data()$responses, 
      issue_type = "bus", 
      custom_theme = current_theme()
    )
  })
  
  # Create Juarez Bus issues plot (Q79.1 to Q79.7)
  output$juarez_bus_issues_plot <- renderPlotly({
    req(survey_data())
    create_transport_issues_plot(
      survey_data()$responses, 
      issue_type = "juarez_bus", 
      custom_theme = current_theme()
    )
  })
}