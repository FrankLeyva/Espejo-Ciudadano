# identity_server.R - Updated with Enhanced Data Management

identityServer <- function(input, output, session, current_theme = NULL) {
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
    saved_plots <- data_manager$load_saved_plots("identity", selectedYear())
    
    if (!is.null(saved_plots)) {
      return(saved_plots)
    }
    
    # If no saved plots, create them
    survey_id <- paste0("PER_", selectedYear())
    
    # Create plots using data manager
    plot_list <- list()
    
    # Monument bar chart
    plot_key <- paste0("monuments_bar_", survey_id)
    plot_list$monuments_bar <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        survey_data <- data_manager$get_survey_data(survey_id)
        
        # Count frequencies
        monument_counts <- table(survey_data$responses$Q65)
        monument_mapping <- c(
          "1" = "La X", "2" = "El Monumento A Benito Juárez", "3" = "La Catedral",
          "4" = "El Parque Central", "5" = "El Chamizal", "6" = "La Casa De Juan Gabriel",
          "7" = "El Centro", "8" = "La Presidencia", "9" = "Umbral Del Milenio",
          "10" = "El Museo De La Ex-Aduana", "11" = "El Parque Borunda",
          "12" = "El Monumento A Zapata", "13" = "La Rodadora", "14" = "Letras JRZ",
          "15" = "La Plaza De Toros", "16" = "Monumento A Los Indomables",
          "17" = "Gimnasios Públicos", "18" = "La Torre Centinela", "19" = "El Gardie",
          "20" = "El Cigarro", "21" = "Estadio Benito Juárez", "22" = "La Iglesia De San Lorenzo",
          "23" = "Mercado Juárez", "24" = "Monumento A Tin Tan", "25" = "Monumento Al Trabajo",
          "26" = "Otro", "27" = "Ninguno"
        )
        
        # Create data frame
        freq_df <- data.frame(
          code = names(monument_counts),
          monument = sapply(names(monument_counts), function(x) monument_mapping[x]),
          count = as.vector(monument_counts)
        ) %>% filter(!monument %in% c("Otro", "Ninguno"))
        
        # Calculate percentages
        total_responses <- sum(freq_df$count)
        freq_df$percentage <- round(100 * freq_df$count / total_responses, 1)
        
        # Sort and limit to top 15
        freq_df <- freq_df[order(-freq_df$count), ]
        freq_df <- head(freq_df, 15)
        freq_df$rank <- 1:15
        
        # Create plot
        primary_color <- active_theme()$colors$primary
        highlight_color <- active_theme()$colors$accent
        if (is.null(highlight_color)) {
          highlight_color <- colorRampPalette(c(primary_color, "#FFFFFF"))(3)[2]
        }
        
        bar_colors <- ifelse(freq_df$rank <= 3, highlight_color, primary_color)
        
        plot_ly(
          data = freq_df,
          y = ~reorder(monument, count),
          x = ~percentage,
          type = "bar",
          orientation = 'h',
          marker = list(
            color = bar_colors,
            line = list(color = active_theme()$colors$neutral, width = 1)
          ),
          text = ~paste0(percentage, "%"),
          textposition = "auto",
          hoverinfo = "text",
          hovertext = ~paste0(count, " menciones")
        ) %>%
          apply_plotly_theme(
            title = "",
            xlab = "Porcentaje de menciones (%)",
            ylab = "",
            custom_theme = active_theme()
          ) %>% 
          layout(
            yaxis = list(categoryorder = "total ascending"),
            xaxis = list(ticksuffix = "%")
          )
      }
    )
    
    # City pride pie chart
    plot_key <- paste0("city_pride_pie_", survey_id)
    plot_list$city_pride_pie <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        pride_data <- data_manager$get_processed_data(survey_id, "Q80", "categorical")
        pride_data <- pride_data[!pride_data$value %in% c("Ns/Nc"), ]
        
        create_category_pie(
          pride_data,
          max_categories = 4,
          custom_theme = active_theme(),
          highlight_max = FALSE,
          palette = 'sequential',
          inverse = TRUE
        ) %>% apply_plotly_theme(title = '', custom_theme = active_theme())
      }
    )
    
    # Save plots for future use
    data_manager$save_plots(plot_list, "identity", selectedYear())
    
    return(plot_list)
  })
  
  # Maps - create separately since they use geo data
  maps <- reactive({
    req(selectedYear(), geo_data())
    
    # Try to load saved maps first
    map_cache_key <- paste0("identity_maps_", selectedYear())
    if (!is.null(data_manager$cache[[map_cache_key]])) {
      return(data_manager$cache[[map_cache_key]])
    }
    
    survey_id <- paste0("PER_", selectedYear())
    map_list <- list()
    
    # Neighborhood connection map
    neighborhood_data <- data_manager$get_processed_data(survey_id, "Q64.2", "interval")
    map_list$neighborhood_connection_map <- create_interval_district_map(
      neighborhood_data,
      geo_data(),
      selected_responses = c("4", "5"),
      highlight_extremes = TRUE,
      use_gradient = FALSE,
      color_scale = "Blues",
      custom_theme = active_theme()
    )
    
    # Neighbors connection map
    neighbors_data <- data_manager$get_processed_data(survey_id, "Q64.3", "interval")
    map_list$neighbors_connection_map <- create_interval_district_map(
      neighbors_data,
      geo_data(),
      selected_responses = c("4", "5"),
      highlight_extremes = TRUE,
      use_gradient = FALSE,
      color_scale = "Blues",
      custom_theme = active_theme()
    )
    
    # Cache maps
    data_manager$cache[[map_cache_key]] <- map_list
    
    return(map_list)
  })
  
  # Value box calculations
  calculations <- reactive({
    req(selectedYear())
    
    calc_cache_key <- paste0("identity_calculations_", selectedYear())
    if (!is.null(data_manager$cache[[calc_cache_key]])) {
      return(data_manager$cache[[calc_cache_key]])
    }
    
    survey_id <- paste0("PER_", selectedYear())
    
    # Get processed data
    neighborhood_data <- data_manager$get_processed_data(survey_id, "Q64.2", "interval")
    neighbors_data <- data_manager$get_processed_data(survey_id, "Q64.3", "interval")
    pride_data <- data_manager$get_processed_data(survey_id, "Q80", "categorical")
    
    calc_list <- list()
    
    # Calculate percentages
    if (!is.null(neighborhood_data)) {
      connected_count <- sum(neighborhood_data$value_num %in% c(4, 5), na.rm = TRUE)
      total_count <- nrow(neighborhood_data)
      calc_list$neighborhood_connection_pct <- paste0(round(100 * connected_count / total_count, 1), "%")
    } else {
      calc_list$neighborhood_connection_pct <- "N/A"
    }
    
    if (!is.null(neighbors_data)) {
      connected_count <- sum(neighbors_data$value_num %in% c(4, 5), na.rm = TRUE)
      total_count <- nrow(neighbors_data)
      calc_list$neighbors_connection_pct <- paste0(round(100 * connected_count / total_count, 1), "%")
    } else {
      calc_list$neighbors_connection_pct <- "N/A"
    }
    
    if (!is.null(pride_data)) {
      filtered_data <- pride_data[!pride_data$value %in% c("Ns/Nc"), ]
      proud_count <- sum(filtered_data$value %in% c("Algo", "Mucho"), na.rm = TRUE)
      total_count <- nrow(filtered_data)
      calc_list$city_pride_pct <- paste0(round(100 * proud_count / total_count, 1), "%")
    } else {
      calc_list$city_pride_pct <- "N/A"
    }
    
    # Cache calculations
    data_manager$cache[[calc_cache_key]] <- calc_list
    
    return(calc_list)
  })
  
  # Update tooltip content based on selected tab
  observe({
    req(input$neighborhood_tabs)
    
    active_tab <- input$neighborhood_tabs
    
    tooltip_content <- switch(active_tab,
      "Vínculo con la colonia o fraccionamiento" = "<b>ID</b>: PER Q64.2 <br>
            <b>Pregunta</b>:	La colona o fraccionamiento en el que vive <br>
             <b>Escala</b>:  	1=Nada;2=Poco;3=Algo;4=Bastante;5=Mucho;",
      "Vínculo con los vecinos" = "<b>ID</b>: PER Q64.3 <br>
            <b>Pregunta</b>:	Los vecinos que tiene <br>
             <b>Escala</b>:  	1=Nada;2=Poco;3=Algo;4=Bastante;5=Mucho;",
      "<b>ID</b>: PER Q64.2 <br>
            <b>Pregunta</b>:	La colona o fraccionamiento en el que vive <br>
             <b>Escala</b>:  	1=Nada;2=Poco;3=Algo;4=Bastante;5=Mucho;"
    )
    
    update_tooltip_content(session, "connection_tooltip", tooltip_content)
  })
  
  # Set initial tooltip
  observeEvent(session$clientData$url_protocol, {
    initial_tooltip <- "<b>ID</b>: PER Q64.2 <br>
            <b>Pregunta</b>:	La colona o fraccionamiento en el que vive <br>
             <b>Escala</b>:  	1=Nada;2=Poco;3=Algo;4=Bastante;5=Mucho;"
    
    update_tooltip_content(session, "connection_tooltip", initial_tooltip)
  }, once = TRUE)
  
  # Render outputs
  output$monuments_bar <- renderPlotly({
    plots()$monuments_bar
  })
  
  output$city_pride_pie <- renderPlotly({
    plots()$city_pride_pie
  })
  
  output$neighborhood_connection_map <- renderLeaflet({
    maps()$neighborhood_connection_map
  })
  
  output$neighbors_connection_map <- renderLeaflet({
    maps()$neighbors_connection_map
  })
  
  output$neighborhood_connection_pct <- renderText({
    calculations()$neighborhood_connection_pct
  })
  
  output$neighbors_connection_pct <- renderText({
    calculations()$neighbors_connection_pct
  })
  
  output$city_pride_pct <- renderText({
    calculations()$city_pride_pct
  })
  
  # Download handler
  output$download_connection_map <- downloadHandler(
    filename = function() {
      map_type <- ifelse(input$neighborhood_tabs == "Vínculo con la colonia o fraccionamiento", 
                        "colonia", "vecinos")
      paste("mapa_vinculo_", map_type, "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      tmp_html <- tempfile(fileext = ".html")
      
      if(input$neighborhood_tabs == "Vínculo con la colonia o fraccionamiento") {
        map <- maps()$neighborhood_connection_map
        title_text <- "Vínculo con la colonia o fraccionamiento"
      } else {
        map <- maps()$neighbors_connection_map
        title_text <- "Vínculo con los vecinos"
      }
      
      map <- map %>%
        addControl(
          html = paste("<div style='background-color:white; padding:10px; border-radius:5px; font-weight:bold;'>", 
                      title_text, "</div>"),
          position = "topright"
        ) %>%
        addControl(
          html = paste("<div style='background-color:white; padding:8px; border-radius:5px; font-size:12px;'>", 
                      paste("Resultados de la Encuesta de Percepción y Participación Ciudadana y Buen Gobierno", selectedYear()),
                      "</div>"),
          position = "bottomright"
        )
      
      htmlwidgets::saveWidget(map, tmp_html, selfcontained = TRUE)
      
      pagedown::chrome_print(
        input = tmp_html,
        output = file,
        options = list(printBackground = TRUE, scale = 2.0),
        format = "png",
        browser = "/usr/bin/google-chrome",
        extra_args = c("--no-sandbox", "--disable-dev-shm-usage")
      )
      
      if (file.exists(tmp_html)) {
        file.remove(tmp_html)
      }
    }
  )
}