# wellness_server.R - Updated with Enhanced Data Management

wellnessServer <- function(input, output, session, current_theme = NULL) {
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
    saved_plots <- data_manager$load_saved_plots("wellness", selectedYear())
    
    if (!is.null(saved_plots)) {
      return(saved_plots)
    }
    
    # If no saved plots, create them
    survey_id <- paste0("PER_", selectedYear())
    plot_list <- list()
    
    # Economic situation pie chart
    plot_key <- paste0("economic_situation_pie_", survey_id)
    plot_list$economic_situation_pie <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        economic_data <- data_manager$get_processed_data(survey_id, "Q4", "categorical")
        
        create_category_pie(
          economic_data,
          max_categories = 6,
          custom_theme = active_theme(),
          highlight_max = FALSE,
          palette = "sequential"
        ) %>% 
          apply_plotly_theme(
            title = "",
            custom_theme = active_theme()
          )
      }
    )
    
    # Activities chart (Q16.1-Q16.15)
    plot_key <- paste0("activities_chart_", survey_id)
    plot_list$activities_chart <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        activity_questions <- paste0("Q16.", 1:15)
        
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
        results <- lapply(activity_questions, function(qid) {
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
        
        # Create data frame for plotting
        ordered_indices <- order(percentages, decreasing = TRUE)
        plot_data <- data.frame(
          activity = factor(activity_labels[ordered_indices], levels = activity_labels[ordered_indices]),
          percentage = percentages[ordered_indices],
          positive_count = positive_counts[ordered_indices],
          total_count = total_counts[ordered_indices],
          rank = 1:length(percentages)
        )
        
        # Get colors from the active theme
        primary_color <- active_theme()$colors$primary
        highlight_color <- active_theme()$colors$accent
        
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
          hovertext = ~paste0(positive_count, "/", total_count, " respuestas)")
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
    
    # Save plots for future use
    data_manager$save_plots(plot_list, "wellness", selectedYear())
    
    return(plot_list)
  })
  
  # Maps - create separately since they use geo data
  maps <- reactive({
    req(selectedYear(), geo_data())
    
    # Try to load cached maps
    map_cache_key <- paste0("wellness_maps_", selectedYear())
    if (!is.null(data_manager$cache[[map_cache_key]])) {
      return(data_manager$cache[[map_cache_key]])
    }
    
    survey_id <- paste0("PER_", selectedYear())
    map_list <- list()
    
    # Migration intention map
    migration_data <- data_manager$get_processed_data(survey_id, "Q63", "interval")
    map_list$migration_intention_map <- create_interval_district_map(
      migration_data,
      geo_data(),
      selected_responses = c("1", "2"),  # "Muchas veces" and "Algunas veces"
      highlight_extremes = TRUE,
      use_gradient = FALSE,
      custom_theme = active_theme()
    )
    
    # Cache maps
    data_manager$cache[[map_cache_key]] <- map_list
    
    return(map_list)
  })
  
  # Value box calculations
  calculations <- reactive({
    req(selectedYear())
    
    calc_cache_key <- paste0("wellness_calculations_", selectedYear())
    if (!is.null(data_manager$cache[[calc_cache_key]])) {
      return(data_manager$cache[[calc_cache_key]])
    }
    
    survey_id <- paste0("PER_", selectedYear())
    
    # Get processed data for migration intentions
    migration_data <- data_manager$get_processed_data(survey_id, "Q63", "interval")
    
    calc_list <- list()
    
    # Calculate migration intention percentage
    if (!is.null(migration_data)) {
      thinking_count <- sum(migration_data$value_num %in% c(1, 2), na.rm = TRUE)
      total_count <- nrow(migration_data)
      calc_list$migration_intention_pct <- paste0(round(100 * thinking_count / total_count, 1), "%")
    } else {
      calc_list$migration_intention_pct <- "N/A"
    }
    
    # Cache calculations
    data_manager$cache[[calc_cache_key]] <- calc_list
    
    return(calc_list)
  })
  
  # Render outputs
  output$economic_situation_pie <- renderPlotly({
    plots()$economic_situation_pie
  })
  
  output$migration_intention_map <- renderLeaflet({
    maps()$migration_intention_map
  })
  
  output$activities_chart <- renderPlotly({
    plots()$activities_chart
  })
  
  output$migration_intention_pct <- renderText({
    calculations()$migration_intention_pct
  })
  
  # Download handler for migration map
  output$download_migration_map <- downloadHandler(
    filename = function() {
      paste("mapa_migracion_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      tmp_html <- tempfile(fileext = ".html")
      
      # Get the map
      map <- maps()$migration_intention_map
      
      # Add title and footer to the map
      map <- map %>%
        addControl(
          html = paste("<div style='background-color:white; padding:10px; border-radius:5px; font-weight:bold;'>", 
                      "Frecuencia con que piensa en irse de la ciudad", 
                      "</div>"),
          position = "topright"
        ) %>%
        addControl(
          html = paste("<div style='background-color:white; padding:8px; border-radius:5px; font-size:12px;'>", 
                      paste("Resultados de la Encuesta de Percepción y Participación Ciudadana y Buen Gobierno", selectedYear()),
                      "</div>"),
          position = "bottomright"
        )
      
      # Save and convert
      htmlwidgets::saveWidget(map, tmp_html, selfcontained = TRUE)
      
      pagedown::chrome_print(
        input = tmp_html,
        output = file,
        options = list(printBackground = TRUE, scale = 2.0),
        format = "png",
        browser = "/usr/bin/google-chrome",
        extra_args = c("--no-sandbox", "--disable-dev-shm-usage")
      )
      
      # Clean up temporary files
      if (file.exists(tmp_html)) {
        file.remove(tmp_html)
      }
    }
  )
}