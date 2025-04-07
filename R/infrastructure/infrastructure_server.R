infrastructureServer <- function(input, output, session,current_theme = NULL) {
  selectedYear <- session$userData$selectedYear
  
  # Load survey data with dynamic year
  survey_data <- reactive({
    survey_id <- paste0("PER_", selectedYear())
    load_survey_data(survey_id)
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
  active_theme <- reactive({
    if (is.function(current_theme)) {
      # If current_theme is a reactive function, call it to get the value
      current_theme()
    } else if (!is.null(current_theme)) {
      # If it's a direct value, use it
      current_theme
    } else {
      # Default to infraestructura theme if nothing provided
      get_section_theme("infraestructura")
    }
  })
  
  # Education Plot
  output$education_plot <- renderLeaflet({
    req(survey_data())
    create_education_overview(survey_data()$responses, active_theme())
  })
  
  # Healthcare Plot
  output$healthcare_plot <- renderPlotly({
    req(survey_data())
    create_healthcare_overview(survey_data()$responses, active_theme())
  })
  
  # Utilities Plot
  output$utilities_plot <- renderPlotly({
    req(survey_data())
    create_utilities_overview(survey_data()$responses, active_theme())
  })
  
  # Housing Map
  output$housing_map <- renderLeaflet({
    req(survey_data(), geo_data())
    create_housing_overview(survey_data()$responses, geo_data(), active_theme())
  })
 
}