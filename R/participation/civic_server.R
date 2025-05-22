# civic_server.R - Updated with Enhanced Data Management

civicServer <- function(input, output, session, current_theme = NULL) {
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
      get_section_theme("participacion")
    }
  })
  
  # Try to load pre-saved plots first, then create if needed
  plots <- reactive({
    req(selectedYear())
    
    # Try to load saved plots
    saved_plots <- data_manager$load_saved_plots("civic", selectedYear())
    
    if (!is.null(saved_plots)) {
      return(saved_plots)
    }
    
    # If no saved plots, create them
    survey_id <- paste0("PAR_", selectedYear())
    plot_list <- list()
    
    # Participation mechanisms knowledge plot
    plot_key <- paste0("mechanisms_plot_", survey_id)
    plot_list$mechanisms_plot <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        # List of mechanisms questions
        mechanism_questions <- paste0("Q140.", 1:13)
        
        # Mechanism labels
        mechanism_labels <- c(
          "El referendum", 
          "El plebiscito", 
          "La iniciativa ciudadana", 
          "La revocación de mandato",
          "Audiencias públicas",
          "Consulta pública",
          "Consejos consultivos",
          "Comités de participación",
          "Planeación participativa",
          "Presupuesto participativo",
          "Cabildo abierto",
          "Contralorías sociales",
          "Mecanismos para niñas, niños y adolescentes"
        )
        
        # Get the survey data
        survey_data <- data_manager$get_survey_data(survey_id)
        
        # Calculate knowledge percentages
        percentages <- numeric(length(mechanism_questions))
        counts <- numeric(length(mechanism_questions))
        
        for (i in 1:length(mechanism_questions)) {
          values <- survey_data$responses[[mechanism_questions[i]]]
          values <- values[!is.na(values)]
          
          if(length(values) > 0) {
            counts[i] <- sum(values == "1")
            percentages[i] <- 100 * counts[i] / length(values)
          } else {
            counts[i] <- 0
            percentages[i] <- 0
          }
        }
        
        # Create dataframe for plot
        plot_data <- data.frame(
          Mechanism = mechanism_labels,
          Count = counts,
          Percentage = percentages,
          stringsAsFactors = FALSE
        )
        
        # Sort by percentage descending
        plot_data <- plot_data[order(-plot_data$Percentage), ]
        
        # Get colors from theme
        primary_color <- active_theme()$colors$primary
        highlight_color <- active_theme()$colors$accent
        
        if (is.null(highlight_color)) {
          highlight_color <- colorRampPalette(c(primary_color, "#FFFFFF"))(3)[2]
        }
        
        # Create single color vector for all bars initially
        colors <- rep(primary_color, nrow(plot_data))
        
        # Handle ties for highlighting top 3 items
        unique_top_values <- unique(plot_data$Percentage)[1:min(3, length(unique(plot_data$Percentage)))]
        top_indices <- which(plot_data$Percentage %in% unique_top_values)
        colors[top_indices] <- highlight_color
        
        # Create horizontal bar chart
        plot_ly(
          data = plot_data,
          y = ~Mechanism,
          x = ~Percentage,
          type = "bar",
          orientation = 'h',
          marker = list(
            color = colors,
            line = list(color = active_theme()$colors$neutral, width = 1)
          ),
          hoverinfo = "text",
          text = ~paste0(round(Percentage, 1), "%")
        ) %>%
          apply_plotly_theme(
            title = "",
            xlab = "Porcentaje de conocimiento (%)",
            ylab = "",
            custom_theme = active_theme()
          ) %>%
          layout(
            yaxis = list(categoryorder = 'total ascending'),
            xaxis = list(range = c(0, 100), ticksuffix = "%")
          )
      }
    )
    
    # Participation requirements plot
    plot_key <- paste0("requirements_plot_", survey_id)
    plot_list$requirements_plot <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        # List of requirement questions
        requirement_questions <- paste0("Q132.", 1:5)
        
        # Requirement labels
        requirement_labels <- c(
          "Información",
          "Espacios para participar",
          "Tiempo",
          "Dinero",
          "Confianza en las instituciones"
        )
        
        # Get the survey data
        survey_data <- data_manager$get_survey_data(survey_id)
        
        # Calculate percentages
        percentages <- numeric(length(requirement_questions))
        counts <- numeric(length(requirement_questions))
        
        for (i in 1:length(requirement_questions)) {
          values <- survey_data$responses[[requirement_questions[i]]]
          values <- values[!is.na(values)]
          
          if(length(values) > 0) {
            counts[i] <- sum(values == "1")
            percentages[i] <- 100 * counts[i] / length(values)
          } else {
            counts[i] <- 0
            percentages[i] <- 0
          }
        }
        
        # Create dataframe for plot
        plot_data <- data.frame(
          Requirement = requirement_labels,
          Count = counts,
          Percentage = percentages,
          stringsAsFactors = FALSE
        )
        
        # Sort by percentage descending
        plot_data <- plot_data[order(-plot_data$Percentage), ]
        
        # Get colors from theme
        primary_color <- active_theme()$colors$primary
        highlight_color <- active_theme()$colors$accent
        
        if (is.null(highlight_color)) {
          highlight_color <- colorRampPalette(c(primary_color, "#FFFFFF"))(3)[2]
        }
        
        # Create single color vector for all bars initially
        colors <- rep(primary_color, nrow(plot_data))
        
        # Handle ties for highlighting top 3 items
        unique_top_values <- unique(plot_data$Percentage)[1:min(3, length(unique(plot_data$Percentage)))]
        top_indices <- which(plot_data$Percentage %in% unique_top_values)
        colors[top_indices] <- highlight_color
        
        # Create horizontal bar chart
        plot_ly(
          data = plot_data,
          y = ~Requirement,
          x = ~Percentage,
          type = "bar",
          orientation = 'h',
          marker = list(
            color = colors,
            line = list(color = active_theme()$colors$neutral, width = 1)
          ),
          hoverinfo = "text",
          text = ~paste0(round(Percentage, 1), "%")
        ) %>%
          apply_plotly_theme(
            title = "",
            xlab = "Porcentaje (%)",
            ylab = "",
            custom_theme = active_theme()
          ) %>%
          layout(
            yaxis = list(categoryorder = 'total ascending'),
            xaxis = list(range = c(0, 100), ticksuffix = "%")
          )
      }
    )
    
    # Save plots for future use
    data_manager$save_plots(plot_list, "civic", selectedYear())
    
    return(plot_list)
  })
  
  # Maps - create separately since they use geo data
  maps <- reactive({
    req(selectedYear(), geo_data())
    
    # Try to load cached maps
    map_cache_key <- paste0("civic_maps_", selectedYear())
    if (!is.null(data_manager$cache[[map_cache_key]])) {
      return(data_manager$cache[[map_cache_key]])
    }
    
    survey_id <- paste0("PAR_", selectedYear())
    map_list <- list()
    
    # Interest map
    interest_data <- data_manager$get_processed_data(survey_id, "Q131", "interval")
    if (!is.null(interest_data)) {
      map_list$interest_map <- create_interval_district_map(
        data = interest_data, 
        geo_data = geo_data(),
        # Select responses from "POCO" to "MUCHO" (2, 3, 4, 5)
        selected_responses = c("2", "3", "4", "5"),
        highlight_extremes = TRUE,
        use_gradient = FALSE,
        color_scale = "Blues",
        custom_theme = active_theme()
      )
    }
    
    # Cache maps
    data_manager$cache[[map_cache_key]] <- map_list
    
    return(map_list)
  })
  
  # Render outputs
  output$mechanisms_plot <- renderPlotly({
    plots()$mechanisms_plot
  })
  
  output$requirements_plot <- renderPlotly({
    plots()$requirements_plot
  })
  
  output$interest_map <- renderLeaflet({
    maps()$interest_map
  })
  
  # Download handler for interest map
  output$download_interest_map <- downloadHandler(
    filename = function() {
      paste("mapa_interes_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      tmp_html <- tempfile(fileext = ".html")
      
      # Get the map
      map <- maps()$interest_map
      
      # Add title and footer to the map
      map <- map %>%
        addControl(
          html = paste("<div style='background-color:white; padding:10px; border-radius:5px; font-weight:bold;'>", 
                      "Porcentaje que está Interesado en Participar en la Política Municipal", 
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