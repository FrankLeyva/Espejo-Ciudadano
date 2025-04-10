# environmentServer.R
environmentServer <- function(input, output, session,current_theme = NULL) {
  # Load survey data
  # Get the selected year from userData
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
  
  active_theme <- reactive({
    if (is.function(current_theme)) {
      # If current_theme is a reactive function, call it to get the value
      current_theme()
    } else if (!is.null(current_theme)) {
      # If it's a direct value, use it
      current_theme
    } else {
      # Default to bienestar theme if nothing provided
      get_section_theme("bienestar")
    }
  })
  
  # Air quality map
  output$air_quality_map <- renderLeaflet({
    req(survey_data(), geo_data())
    
    air_quality_data <- prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q89",
      metadata = survey_data()$metadata
    )
    
    create_interval_district_map(
      data = air_quality_data,
      geo_data = geo_data(),
      use_gradient = F,
      color_scale = "Greens",
      custom_theme = active_theme()
    )
  })
  
  # Urban trees map
  output$urban_trees_map <- renderLeaflet({
    req(survey_data(), geo_data())
    
    urban_trees_data <- prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q90",
      metadata = survey_data()$metadata
    )
    
    create_interval_district_map(
      data = urban_trees_data,
      geo_data = geo_data(),
      use_gradient = F,
      color_scale = "Greens",
      custom_theme = active_theme()
    )
  })
  
  # Street cleanliness map
  output$street_cleanliness_map <- renderLeaflet({
    req(survey_data(), geo_data())
    
    street_cleanliness_data <- prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q91",
      metadata = survey_data()$metadata
    )
    
    create_interval_district_map(
      data = street_cleanliness_data,
      geo_data = geo_data(),
      use_gradient = F,
      color_scale = "Greens",
      custom_theme = active_theme()
    )
  })
  
  # Water quality map
  output$water_quality_map <- renderLeaflet({
    req(survey_data(), geo_data())
    
    water_quality_data <- prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q92",
      metadata = survey_data()$metadata
    )
    
    create_interval_district_map(
      data = water_quality_data,
      geo_data = geo_data(),
      use_gradient = F,
      color_scale = "Blues",
      custom_theme = active_theme()
    )
  })
  
  # Environmental problems plot
  output$env_problems_plot <- renderPlotly({
    req(survey_data())
    create_env_problems_plot(
      survey_data()$responses,
      custom_theme = active_theme()
    ) %>% 
      apply_plotly_theme(
        title = ""
      )
  })

}