# identity_server.R

identityServer <- function(input, output, session) {
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
  
  # Process Q65H data for monuments wordcloud
  monuments_data <- reactive({
    req(survey_data())
    
    # Prepare nominal data for Q65H
    prepare_nominal_data(
      data = survey_data()$responses,
      question_id = "Q65H",
      metadata = survey_data()$metadata
    )
  })
  
  # Create monuments wordcloud
  output$monuments_wordcloud <- renderUI({
    req(monuments_data())
    
    # Create word frequency table with filters
    word_freq <- create_word_freq_table(
      monuments_data(),
      max_words = 100,
      exclude_stopwords = TRUE,
      min_chars = 1
    )
    
    # Use the wordcloud2 function
    tryCatch({
      # Get theme colors if available
      wordcloud2(data = word_freq, size = 0.5, color = "random-dark")
    }, error = function(e) {
      return(p(paste("Error al generar la nube de palabras:", e$message)))
    })
  })
  
  # Process Q80 data for city pride pie chart
  city_pride_data <- reactive({
    req(survey_data())
    
    # Prepare categorical data for Q80
    prepare_categorical_data(
      data = survey_data()$responses,
      question_id = "Q80",
      metadata = survey_data()$metadata
    )
  })
  
  # Create city pride pie chart
  output$city_pride_pie <- renderPlotly({
    req(city_pride_data())
    
    # Use the categorical module's pie chart function, excluding NS/NC (5)
    pride_data <- city_pride_data()
    pride_data <- pride_data[!pride_data$value %in% c("Ns/Nc"), ]
    
    create_category_pie(
      pride_data,
      max_categories = 4,
      custom_theme = current_theme()
    )
  })
  
  # Process Q64.2 data for neighborhood connection map
  neighborhood_connection_data <- reactive({
    req(survey_data())
    
    # Prepare interval data for Q64.2
    prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q64.2",
      metadata = survey_data()$metadata
    )
  })
  
  # Process Q64.3 data for neighbors connection map
  neighbors_connection_data <- reactive({
    req(survey_data())
    
    # Prepare interval data for Q64.3
    prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q64.3",
      metadata = survey_data()$metadata
    )
  })
  
  # Create neighborhood connection map
  output$neighborhood_connection_map <- renderLeaflet({
    req(neighborhood_connection_data(), geo_data())
    
    # Use interval district map with selected options 4 and 5 (bastante or mucho)
    create_interval_district_map(
      neighborhood_connection_data(),
      geo_data(),
      selected_responses = c("4", "5"),
      highlight_extremes = TRUE,
      use_gradient = F,
      color_scale = "Blues",
      custom_theme = current_theme()
    )
  })
  
  # Create neighbors connection map
  output$neighbors_connection_map <- renderLeaflet({
    req(neighbors_connection_data(), geo_data())
    
    # Use interval district map with selected options 4 and 5 (bastante or mucho)
    create_interval_district_map(
      neighbors_connection_data(),
      geo_data(),
      selected_responses = c("4", "5"),
      highlight_extremes = TRUE,
      use_gradient = F,
      color_scale = "Blues",
      custom_theme = current_theme()
    )
  })
  
  # Calculate statistics for value boxes
  
  # Neighborhood connection percentage (Q64.2 values 4-5)
  output$neighborhood_connection_pct <- renderText({
    req(neighborhood_connection_data())
    
    # Filter and calculate percentage
    filtered_data <- neighborhood_connection_data()
    connected_count <- sum(filtered_data$value_num %in% c(4, 5), na.rm = TRUE)
    total_count <- nrow(filtered_data)
    
    # Calculate percentage
    percentage <- round(100 * connected_count / total_count, 1)
    
    # Return formatted text
    paste0(percentage, "%")
  })
  
  # Neighbors connection percentage (Q64.3 values 4-5)
  output$neighbors_connection_pct <- renderText({
    req(neighbors_connection_data())
    
    # Filter and calculate percentage
    filtered_data <- neighbors_connection_data()
    connected_count <- sum(filtered_data$value_num %in% c(4, 5), na.rm = TRUE)
    total_count <- nrow(filtered_data)
    
    # Calculate percentage
    percentage <- round(100 * connected_count / total_count, 1)
    
    # Return formatted text
    paste0(percentage, "%")
  })
  
  # City pride percentage (Q80 values 3-4)
  output$city_pride_pct <- renderText({
    req(city_pride_data())
    
    # Filter and calculate percentage
    filtered_data <- city_pride_data()
    proud_count <- sum(filtered_data$value %in% c("Algo", "Mucho"), na.rm = TRUE)
    total_count <- sum(!filtered_data$value %in% c("Ns/Nc"), na.rm = TRUE)
    
    # Calculate percentage
    percentage <- round(100 * proud_count / total_count, 1)
    
    # Return formatted text
    paste0(percentage, "%")
  })
}
