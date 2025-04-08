# cultural_server.R

culturalServer <- function(input, output, session,current_theme = NULL) {
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
  
  # Calculate percentages, counts, and totals for each activity
  results <- lapply(cultural_questions, function(qid) {
    binary_data <- prepare_binary_data(
      data = survey_data()$responses,
      question_id = qid,
      metadata = survey_data()$metadata
    )
    
    if(nrow(binary_data) > 0) {
      positive_count <- sum(binary_data$binary_value, na.rm = TRUE)
      total_count <- nrow(binary_data)
      percentage <- 100 * positive_count / total_count
      
      return(list(
        percentage = percentage,
        positive_count = positive_count,
        total_count = total_count
      ))
    } else {
      return(list(
        percentage = 0,
        positive_count = 0,
        total_count = 0
      ))
    }
  })
  
  # Extract percentages for ordering
  percentages <- sapply(results, function(x) x$percentage)
  positive_counts <- sapply(results, function(x) x$positive_count)
  total_counts <- sapply(results, function(x) x$total_count)
  
  # Create data frame for plotting with ordered indices
  ordered_indices <- order(percentages, decreasing = TRUE)
  plot_data <- data.frame(
    activity = factor(activity_labels[ordered_indices], levels = activity_labels[ordered_indices]),
    percentage = percentages[ordered_indices],
    positive_count = positive_counts[ordered_indices],
    total_count = total_counts[ordered_indices],
    rank = 1:length(percentages)  # Add rank for coloring
  )
  
  # Get colors from the active theme
  primary_color <- active_theme()$colors$primary
  highlight_color <- active_theme()$colors$secondary
  
  # If highlight color is not defined, fall back to a secondary color
  if (is.null(highlight_color)) {
    # Try to get another distinctive color from the theme
    if (!is.null(active_theme()$colors$secondary)) {
      highlight_color <- active_theme()$colors$secondary
    } else if (!is.null(active_theme()$colors$success)) {
      highlight_color <- active_theme()$colors$success
    } else {
      # Fall back to a brighter version of primary if nothing else available
      highlight_color <- colorRampPalette(c(primary_color, "#FFFFFF"))(3)[2]
    }
  }
  
  # Create color vector - highlight top 3
  bar_colors <- ifelse(plot_data$rank <= 3, highlight_color, primary_color)
  
  # Create horizontal bar chart
  plot_ly(
    data = plot_data,
    y = ~activity,
    x = ~percentage,
    type = "bar",
    orientation = 'h',
    marker = list(
      color = bar_colors,
      line = list(
        color = active_theme()$colors$neutral,
        width = 1
      )
    ),
    text = ~paste0(round(percentage, 1), "%"),
    textposition = "auto",
    hoverinfo = "text",
    hovertext = ~paste0(positive_count, "/", total_count, " Respuestas")
  ) %>%
    apply_plotly_theme(
      title = "",
      xlab = "Porcentaje de participación (%)",
      ylab = "",
      custom_theme = active_theme()
    ) %>%
    layout(
      xaxis = list(range = c(0, max(percentages) * 1.1)),
      yaxis = list(categoryorder = 'total ascending')
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
  
  # Calculate percentages and counts for each activity
  results <- data.frame(
    value = activity_labels,
    count = numeric(length(activity_labels))
  )
  
  # Fill in the counts based on binary responses
  for (i in 1:length(entertainment_questions)) {
    binary_data <- prepare_binary_data(
      data = survey_data()$responses,
      question_id = entertainment_questions[i],
      metadata = survey_data()$metadata
    )
    
    if(nrow(binary_data) > 0) {
      positive_count <- sum(binary_data$binary_value, na.rm = TRUE)
      results$count[i] <- positive_count
    }
  }
  
  # Create a data frame for the categorical pie chart
  # We need to replicate each activity label by its count to create frequency data
  pie_data <- data.frame(
    value = rep(results$value, results$count)
  )
  
  # Use the create_category_pie function with the cultural theme
  create_category_pie(
    data = pie_data,
    max_categories = length(activity_labels), 
    custom_theme = active_theme(),
    highlight_max = F,
    palette = "categorical",
    hide_ns_nc = TRUE,
    inverse=F
  ) %>%
  layout(title = "") %>%  # Remove title if not needed
  hide_legend()
})

}
