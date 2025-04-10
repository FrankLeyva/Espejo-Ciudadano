# economy_server.R

economyServer <- function(input, output, session,current_theme = NULL) {
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
  
  # Process Q4 data for economic improvement map
  economic_improvement_data <- reactive({
    req(survey_data())
    
   # Prepare interval data for Q4
   prepare_interval_data(
    data = survey_data()$responses,
    question_id = "Q4",
    metadata = survey_data()$metadata
  )
})

# Create Economic Improvement Map
output$economic_improvement_map <- renderLeaflet({
  req(economic_improvement_data(), geo_data())
  
  # Interval district map with selected options 4 and 5 (improved somewhat or much)
  create_interval_district_map(
    economic_improvement_data(), 
    geo_data(),
    selected_responses = c("4", "5"),  # Values for "Mejorado algo" and "Mejorado mucho"
    highlight_extremes = TRUE,
    use_gradient = F,
    color_scale = "Blues",
    custom_theme = active_theme()
  )
})
  
  # Process Q5 data for income situation pie chart
  income_situation_data <- reactive({
    req(survey_data())
    
    # Prepare categorical data for Q5
    prepare_categorical_data(
      data = survey_data()$responses,
      question_id = "Q5",
      metadata = survey_data()$metadata
    )
  })

  output$income_situation_pie <- renderPlotly({
    req(income_situation_data())

    # Use the categorical module's pie chart function with debug enabled
    create_category_pie(
      income_situation_data(),
      max_categories = 5,  
      custom_theme = active_theme(),
      highlight_max = FALSE,
      palette = 'sequential',
      inverse = TRUE,
      truncate_labels = T
    ) %>% 
      layout(title="")
  })
  
  # Calculate income sufficiency percentage (options 1 and 2)
  output$income_sufficiency_text <- renderText({
    req(income_situation_data())
    
    # Count responses with values 1 or 2
    sufficient_count <- sum(income_situation_data()$value %in% c("Les alcanza bien y pueden ahorrar", 
                                                               "Les alcanza justo, sin grandes dificultades"))
    total_count <- nrow(income_situation_data())
    
    # Calculate percentage
    percentage <- round(100 * sufficient_count / total_count, 1)
    
    # Return formatted text
    paste0(percentage, "%")
  })
  
  # Calculate savings capability percentage (option 1 only)
  output$savings_capability_text <- renderText({
    req(income_situation_data())
    
    # Count responses with value 1
    savings_count <- sum(income_situation_data()$value == "Les alcanza bien y pueden ahorrar")
    total_count <- nrow(income_situation_data())
    
    # Calculate percentage
    percentage <- round(100 * savings_count / total_count, 1)
    
    # Return formatted text
    paste0(percentage, "%")
  })
  
  

}
