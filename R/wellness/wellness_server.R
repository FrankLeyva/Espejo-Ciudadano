# wellness_server.R

wellnessServer <- function(input, output, session, current_theme = NULL) {
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
  
  # Use the current theme
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
  
  # Process Q4 data for economic situation pie chart
  economic_situation_data <- reactive({
    req(survey_data())
    
    prepare_categorical_data(
      data = survey_data()$responses,
      question_id = "Q4",
      metadata = survey_data()$metadata
    )
  })
  
  # Create economic situation pie chart
  output$economic_situation_pie <- renderPlotly({
    req(economic_situation_data())
    
    create_category_pie(
      economic_situation_data(),
      max_categories = 5,
      custom_theme = active_theme()
    )
  })
  
  # Process Q63 data for migration intention map
  migration_intention_data <- reactive({
    req(survey_data())
    
    prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q63",
      metadata = survey_data()$metadata
    )
  })
  
  # Create migration intention map
  output$migration_intention_map <- renderLeaflet({
    req(migration_intention_data(), geo_data())
    
    create_interval_district_map(
      migration_intention_data(),
      geo_data(),
      selected_responses = c("1", "2"),  # "Muchas veces" and "Algunas veces"
      highlight_extremes = TRUE,
      use_gradient = F,
      color_scale = "Blues",
      custom_theme = active_theme()
    )
  })
  
  # Create activities chart (Q16.1-Q16.15)
  output$activities_chart <- renderPlotly({
    req(survey_data())
    
    # Activity questions Q16.1 to Q16.15
    activity_questions <- paste0("Q16.", 1:15)
    
    # Activity labels
    activity_labels <- c(
      "Actividades dentro del hogar",
      "Contacto con naturaleza/parques", 
      "Visita centro comercial", 
      "Visita segundas/bazares", 
      "Visita bar/cantina", 
      "Cine", 
      "Biblioteca", 
      "Concierto/espectáculo musical",
      "Feria/fiesta popular", 
      "Museo/galería", 
      "Teatro/danza", 
      "Zona histórica/monumento", 
      "Conferencia/coloquio", 
      "Evento deportivo",
      "Ejercicio/actividad física"
    )
    
    # Calculate percentages for each activity
    percentages <- sapply(activity_questions, function(qid) {
      binary_data <- prepare_binary_data(
        data = survey_data()$responses,
        question_id = qid,
        metadata = survey_data()$metadata
      )
      
      if(nrow(binary_data) > 0) {
        return(100 * mean(binary_data$binary_value, na.rm = TRUE))
      } else {
        return(0)
      }
    })
    
    # Create data frame for plotting
    plot_data <- data.frame(
      activity = factor(activity_labels, levels = activity_labels[order(percentages, decreasing = TRUE)]),
      percentage = percentages[order(percentages, decreasing = TRUE)]
    )
    
    # Get color from active theme
    bar_color <- active_theme()$colors$primary
    
    # Create horizontal bar chart
    plot_ly(
      data = plot_data,
      y = ~activity,
      x = ~percentage,
      type = "bar",
      orientation = 'h',
      marker = list(color = bar_color),
      text = ~paste0(round(percentage, 1), "%"),
      textposition = "auto",
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Porcentaje de participación en actividades",
        xaxis = list(
          title = "Porcentaje de participación (%)",
          range = c(0, max(percentages) * 1.1)
        ),
        yaxis = list(title = "",
        categoryorder = 'total ascending'

      )
      )
  })
  
}
