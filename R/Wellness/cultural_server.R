# cultural_server.R

culturalServer <- function(input, output, session) {
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
  
  # Function to calculate percentage for binary questions
  get_binary_percentage <- function(question_id) {
    req(survey_data())
    
    # Prepare binary data
    binary_data <- prepare_binary_data(
      data = survey_data()$responses,
      question_id = question_id,
      metadata = survey_data()$metadata
    )
    
    # Calculate percentage
    if (nrow(binary_data) > 0) {
      percentage <- round(100 * mean(binary_data$binary_value, na.rm = TRUE), 1)
      return(paste0(percentage, "%"))
    } else {
      return("N/A")
    }
  }
  
  # Process info box data
  output$home_activities_pct <- renderText({
    get_binary_percentage("Q16.1")
  })
  
  output$exercise_activities_pct <- renderText({
    get_binary_percentage("Q16.15")
  })
  
  output$nature_activities_pct <- renderText({
    get_binary_percentage("Q16.2")
  })
  
  # Create cultural activities bar plot
  output$cultural_activities_plot <- renderPlotly({
    req(survey_data())
    
    # Cultural activity questions
    cultural_questions <- c(
      "Q16.7", "Q16.9", "Q16.10", "Q16.11", "Q16.12", "Q16.13", "Q16.14"
    )
    
    # Activity labels (simplified versions)
    activity_labels <- c(
      "Biblioteca", 
      "Feria o fiesta popular", 
      "Museo o galería", 
      "Teatro o danza", 
      "Zona histórica o monumento", 
      "Conferencias o coloquios", 
      "Evento deportivo"
    )
    
    # Calculate percentages for each activity
    percentages <- sapply(cultural_questions, function(qid) {
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
    
    # Get color from theme
    bar_color <- if (!is.null(current_theme())) {
      current_theme()$colors$primary
    } else {
      "#1f77b4"
    }
    
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
        title = "Porcentaje de participación en actividades culturales",
        xaxis = list(
          title = "Porcentaje de participación (%)",
          range = c(0, max(percentages) * 1.1)
        ),
        yaxis = list(title = "")
      )
  })
  
  # Create entertainment activities pie chart
  output$entertainment_activities_plot <- renderPlotly({
    req(survey_data())
    
    # Entertainment activity questions
    entertainment_questions <- c(
      "Q16.3", "Q16.4", "Q16.5", "Q16.6", "Q16.8"
    )
    
    # Activity labels
    activity_labels <- c(
      "Centro/plaza comercial", 
      "Segundas/bazares", 
      "Cantina/bar/antro", 
      "Cine", 
      "Concierto/espectáculo musical"
    )
    
    # Calculate percentages for each activity
    percentages <- sapply(entertainment_questions, function(qid) {
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
      activity = activity_labels,
      percentage = percentages,
      count = round(percentages * nrow(survey_data()$responses) / 100)
    )
    
    # Get colors from theme
    pie_colors <- if (!is.null(current_theme())) {
      colorRampPalette(c(current_theme()$colors$primary, current_theme()$colors$highlight))(length(activity_labels))
    } else {
      colorRampPalette(c("#1f77b4", "#ff7f0e"))(length(activity_labels))
    }
    
    # Create pie chart
    plot_ly(
      labels = ~plot_data$activity,
      values = ~plot_data$percentage,
      type = "pie",
      textinfo = "label+percent",
      hoverinfo = "text",
      text = ~paste0(plot_data$activity, ": ", round(plot_data$percentage, 1), "% (", plot_data$count, " personas)"),
      marker = list(colors = pie_colors)
    ) %>%
      layout(
        title = "Participación en actividades de ocio y entretenimiento",
        showlegend = TRUE
      )
  })

}