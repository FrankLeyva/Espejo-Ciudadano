# inequality_server.R - Updated with Enhanced Data Management

inequalityServer <- function(input, output, session, current_theme = NULL) {
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
      get_section_theme("gobierno")
    }
  })
  
  # Try to load pre-saved plots first, then create if needed
  plots <- reactive({
    req(selectedYear())
    
    # Try to load saved plots
    saved_plots <- data_manager$load_saved_plots("inequality", selectedYear())
    
    if (!is.null(saved_plots)) {
      return(saved_plots)
    }
    
    # If no saved plots, create them
    survey_id <- paste0("PER_", selectedYear())
    
    # Create plots using data manager
    plot_list <- list()
    
    # Bar chart for Q88 - Institutions contributing to reducing inequality
    plot_key <- paste0("inequality_reduction_plot_", survey_id)
    plot_list$inequality_reduction_plot <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        survey_data <- data_manager$get_survey_data(survey_id)
        
        # Get Q88 data
        q88_data <- survey_data$responses$Q88
        q88_data <- q88_data[!is.na(q88_data)]
        
        # Define institutions labels
        institutions <- c(
          "1" = "Gobierno Federal",
          "2" = "Gobierno Municipal",
          "3" = "Gobierno Estatal",
          "4" = "Organizaciones de la sociedad civil",
          "5" = "Empresas",
          "6" = "Iglesias",
          "7" = "Universidades",
          "8" = "Escuelas (primaria, secundaria, preparatoria)",
          "9" = "La misma ciudadanía",
          "10" = "Ninguna",
          "11" = "No sabe/ No contestó"
        )
        
        # Create frequency table
        inst_table <- table(q88_data)
        
        # Convert to data frame
        bar_data <- data.frame(
          Institution = sapply(names(inst_table), function(x) institutions[x]),
          Count = as.numeric(inst_table),
          stringsAsFactors = FALSE
        )
        
        # Check for NA or NULL values in Institution
        bar_data <- bar_data %>% 
          mutate(Institution = ifelse(is.na(Institution), paste("Categoría", names(inst_table)), Institution))
        
        # Calculate percentages
        bar_data$Percentage <- round(100 * bar_data$Count / sum(bar_data$Count), 1)
        
        # Sort by count in descending order
        bar_data <- bar_data[order(-bar_data$Count), ]
        
        # Get colors from theme
        primary_color <- active_theme()$colors$primary
        highlight_color <- active_theme()$colors$accent
        
        if (is.null(highlight_color)) {
          highlight_color <- colorRampPalette(c(primary_color, "#FFFFFF"))(3)[2]
        }
        
        # Create single color vector for all bars initially
        colors <- rep(primary_color, nrow(bar_data))
        
        # Handle ties for highlighting top N items
        # First, identify the top 3 unique values
        unique_top_values <- unique(bar_data$Percentage)[1:min(3, length(unique(bar_data$Percentage)))]
        
        # Find all rows that have those top values
        top_indices <- which(bar_data$Percentage %in% unique_top_values)
        
        # Highlight all those rows
        colors[top_indices] <- highlight_color
        
        # Create horizontal bar chart
        plot_ly(
          data = bar_data,
          y = ~reorder(Institution, Count),
          x = ~Count,
          type = "bar",
          orientation = 'h',
          marker = list(
            color = colors,
            line = list(color = active_theme()$colors$neutral, width = 1)
          ),
          text = ~paste0(Percentage, "%"),
          textposition = "auto",
          hoverinfo = "text",
          hovertext = ~paste0(Institution, ": ", Count, " (", Percentage, "%)")
        ) %>%
          apply_plotly_theme(
            title = "",
            xlab = "Frecuencia",
            ylab = "",
            custom_theme = active_theme()
          ) %>%
          layout(
            yaxis = list(categoryorder = "total ascending"),
            xaxis = list(range = c(0, max(bar_data$Count) * 1.1))
          )
      }
    )
    
    # Save plots for future use
    data_manager$save_plots(plot_list, "inequality", selectedYear())
    
    return(plot_list)
  })
  
  # Maps - create separately since they use geo data
  maps <- reactive({
    req(selectedYear(), geo_data())
    
    # Try to load cached maps
    map_cache_key <- paste0("inequality_maps_", selectedYear())
    if (!is.null(data_manager$cache[[map_cache_key]])) {
      return(data_manager$cache[[map_cache_key]])
    }
    
    survey_id <- paste0("PER_", selectedYear())
    map_list <- list()
    
    # Map for Q84 - Rights Violation
    rights_data <- data_manager$get_processed_data(survey_id, "Q84", "binary")
    map_list$rights_violation_map <- create_binary_district_map(
      rights_data,
      geo_data(),
      highlight_extremes = TRUE,
      focus_on_true = TRUE,
      custom_theme = active_theme()
    )
    
    # Map for Q87 - Inequality Perception
    inequality_data <- data_manager$get_processed_data(survey_id, "Q87", "interval")
    map_list$inequality_perception_map <- create_interval_district_map(
      inequality_data,
      geo_data(),
      selected_responses = NULL,  # Show mean values
      highlight_extremes = TRUE,
      use_gradient = FALSE,
      color_scale = "RdBu",
      custom_theme = active_theme()
    )
    
    # Cache maps
    data_manager$cache[[map_cache_key]] <- map_list
    
    return(map_list)
  })
  
  # Value box calculations
  calculations <- reactive({
    req(selectedYear())
    
    calc_cache_key <- paste0("inequality_calculations_", selectedYear())
    if (!is.null(data_manager$cache[[calc_cache_key]])) {
      return(data_manager$cache[[calc_cache_key]])
    }
    
    survey_id <- paste0("PER_", selectedYear())
    
    # Get processed data
    rights_data <- data_manager$get_processed_data(survey_id, "Q84", "binary")
    inequality_data <- data_manager$get_processed_data(survey_id, "Q87", "interval")
    
    calc_list <- list()
    
    # Calculate rights violation percentage
    if (!is.null(rights_data)) {
      violation_count <- sum(rights_data$binary_value, na.rm = TRUE)
      total_count <- nrow(rights_data)
      calc_list$rights_violation_pct <- paste0(round(100 * violation_count / total_count, 1), "%")
    } else {
      calc_list$rights_violation_pct <- "N/A"
    }
    
    # Calculate mean inequality perception
    if (!is.null(inequality_data)) {
      calc_list$inequality_perception_mean <- round(mean(inequality_data$value_num, na.rm = TRUE), 1)
    } else {
      calc_list$inequality_perception_mean <- "N/A"
    }
    
    # Cache calculations
    data_manager$cache[[calc_cache_key]] <- calc_list
    
    return(calc_list)
  })
  
  # Render outputs
  output$inequality_reduction_plot <- renderPlotly({
    plots()$inequality_reduction_plot
  })
  
  output$rights_violation_map <- renderLeaflet({
    maps()$rights_violation_map
  })
  
  output$inequality_perception_map <- renderLeaflet({
    maps()$inequality_perception_map
  })
  
  # Render outputs for calculations
  output$rights_violation_pct <- renderText({
    calculations()$rights_violation_pct
  })
  
  output$inequality_perception_mean <- renderText({
    calculations()$inequality_perception_mean
  })
  
  # Download handler for Rights Violation map
  output$download_rights_violation_map <- downloadHandler(
    filename = function() {
      paste("mapa_derechos_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      tmp_html <- tempfile(fileext = ".html")
      
      # Get the map
      map <- maps()$rights_violation_map
      
      # Add title and footer to the map
      map <- map %>%
        addControl(
          html = paste("<div style='background-color:white; padding:10px; border-radius:5px; font-weight:bold;'>", 
                      "Porcentaje de encuestados que reporta haber tenido sus derechos violentados por distrito", 
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
  
  # Download handler for Inequality Perception map
  output$download_inequality_map <- downloadHandler(
    filename = function() {
      paste("mapa_desigualdad_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      tmp_html <- tempfile(fileext = ".html")
      
      # Get the map
      map <- maps()$inequality_perception_map
      
      # Add title and footer to the map
      map <- map %>%
        addControl(
          html = paste("<div style='background-color:white; padding:10px; border-radius:5px; font-weight:bold;'>", 
                      "Percepcion de la Desigualdad por Distrito, Escala 1-4", 
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