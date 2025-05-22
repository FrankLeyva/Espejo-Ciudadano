# cultural_server.R - Updated with Enhanced Data Management

culturalServer <- function(input, output, session, current_theme = NULL) {
  # Get dependencies from userData
  selectedYear <- session$userData$selectedYear
  data_manager <- session$userData$data_manager
  geo_data <- session$userData$geoData
  
  active_theme <- reactive({
    if (is.function(current_theme)) {
      current_theme()
    } else if (!is.null(current_theme)) {
      current_theme
    } else {
      get_section_theme("bienestar")
    }
  })
  
  # Try to load pre-saved plots first, then create if needed
  plots <- reactive({
    req(selectedYear())
    
    # Try to load saved plots
    saved_plots <- data_manager$load_saved_plots("cultural", selectedYear())
    
    if (!is.null(saved_plots)) {
      return(saved_plots)
    }
    
    # If no saved plots, create them
    survey_id <- paste0("PER_", selectedYear())
    
    # Create plots using data manager
    plot_list <- list()
    
    # Cultural activities bar plot
    plot_key <- paste0("cultural_activities_plot_", survey_id)
    plot_list$cultural_activities_plot <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        survey_data <- data_manager$get_survey_data(survey_id)
        
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
        results <- lapply(cultural_questions, function(qid) {
          binary_data <- data_manager$get_processed_data(survey_id, qid, "binary")
          
          if(!is.null(binary_data) && nrow(binary_data) > 0) {
            positive_count <- sum(binary_data$binary_value, na.rm = TRUE)
            total_count <- nrow(binary_data)
            percentage <- 100 * positive_count / total_count
            
            return(list(
              percentage = percentage,
              positive_count = positive_count,
              total_count = total_count
            ))
          } else {
            return(list(percentage = 0, positive_count = 0, total_count = 0))
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
        highlight_color <- active_theme()$colors$accent
        
        # If highlight color is not defined, fall back to a secondary color
        if (is.null(highlight_color)) {
          if (!is.null(active_theme()$colors$secondary)) {
            highlight_color <- active_theme()$colors$secondary
          } else if (!is.null(active_theme()$colors$success)) {
            highlight_color <- active_theme()$colors$success
          } else {
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
      }
    )
    
    # Entertainment activities pie chart
    plot_key <- paste0("entertainment_activities_plot_", survey_id)
    plot_list$entertainment_activities_plot <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        survey_data <- data_manager$get_survey_data(survey_id)
        
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
          binary_data <- data_manager$get_processed_data(survey_id, entertainment_questions[i], "binary")
          
          if(!is.null(binary_data) && nrow(binary_data) > 0) {
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
          highlight_max = FALSE,
          palette = "categorical",
          hide_ns_nc = TRUE,
          inverse = FALSE
        ) %>%
        layout(title = "") %>%  
        hide_legend() %>% 
        apply_plotly_theme(custom_theme = active_theme())
      }
    )
    
    # Save plots for future use
    data_manager$save_plots(plot_list, "cultural", selectedYear())
    
    return(plot_list)
  })
  
  # Value box calculations
  calculations <- reactive({
    req(selectedYear())
    
    calc_cache_key <- paste0("cultural_calculations_", selectedYear())
    if (!is.null(data_manager$cache[[calc_cache_key]])) {
      return(data_manager$cache[[calc_cache_key]])
    }
    
    survey_id <- paste0("PER_", selectedYear())
    calc_list <- list()
    
    # Function to calculate percentage for binary questions
    get_binary_percentage <- function(question_id) {
      binary_data <- data_manager$get_processed_data(survey_id, question_id, "binary")
      
      if (!is.null(binary_data) && nrow(binary_data) > 0) {
        percentage <- round(100 * mean(binary_data$binary_value, na.rm = TRUE), 1)
        return(paste0(percentage, "%"))
      } else {
        return("N/A")
      }
    }
    
    # Calculate percentages for info boxes
    calc_list$home_activities_pct <- get_binary_percentage("Q16.1")
    calc_list$exercise_activities_pct <- get_binary_percentage("Q16.15")
    calc_list$nature_activities_pct <- get_binary_percentage("Q16.2")
    
    # Cache calculations
    data_manager$cache[[calc_cache_key]] <- calc_list
    
    return(calc_list)
  })
  
  # Render outputs
  output$cultural_activities_plot <- renderPlotly({
    plots()$cultural_activities_plot
  })
  
  output$entertainment_activities_plot <- renderPlotly({
    plots()$entertainment_activities_plot
  })
  
  # Render value box text
  output$home_activities_pct <- renderText({
    calculations()$home_activities_pct
  })
  
  output$exercise_activities_pct <- renderText({
    calculations()$exercise_activities_pct
  })
  
  output$nature_activities_pct <- renderText({
    calculations()$nature_activities_pct
  })
}