infrastructureServer <- function(input, output, session) {
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
  
  # Education Plot
  output$education_plot <- renderLeaflet({
    req(survey_data())
    create_education_overview(survey_data()$responses, current_theme())
  })
  
  # Healthcare Plot
  output$healthcare_plot <- renderPlotly({
    req(survey_data())
    create_healthcare_overview(survey_data()$responses, current_theme())
  })
  
  # Utilities Plot
  output$utilities_plot <- renderPlotly({
    req(survey_data())
    create_utilities_overview(survey_data()$responses, current_theme())
  })
  
  # Housing Map
  output$housing_map <- renderLeaflet({
    req(survey_data(), geo_data())
    create_housing_overview(survey_data()$responses, geo_data(), current_theme())
  })
 
}