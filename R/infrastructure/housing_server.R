# housing_server.R - Updated with Enhanced Data Management

housingServer <- function(input, output, session, current_theme = NULL) {
  # Get dependencies from userData
  selectedYear <- session$userData$selectedYear
  data_manager <- session$userData$data_manager
  geo_data <- session$userData$geoData
  
  # Setup active theme
  active_theme <- reactive({
    if (is.function(current_theme)) {
      current_theme()
    } else if (!is.null(current_theme)) {
      current_theme
    } else {
      get_section_theme("infraestructura")
    }
  })
  
  # Mapping of housing questions
  housing_questions <- c(
    "materials" = "Q26",  # Calidad de materiales
    "spaces" = "Q27",     # Tamaño y espacios
    "location" = "Q28"    # Ubicación y accesibilidad
  )
  
  # Update tooltip content based on selected tab
  observe({
    req(input$housing_tabs)
    
    active_tab <- input$housing_tabs
    
    tooltip_content <- switch(active_tab,
      "Calidad de Materiales" = "<b>ID</b>: PER Q26 <br>
            <b>Pregunta</b>:	Que tan satisfecho esta con LA CALIDAD DE LOS MATERIALES DE LA VIVIENDA? <br>
             <b>Escala</b>:  1-10",
      "Tamaño y Espacios" = "<b>ID</b>: PER Q27 <br>
            <b>Pregunta</b>:	Qué tan satisfecho está con EL TAMAÑO Y ESPACIOS DE LA VIVIENDA? <br>
             <b>Escala</b>:  1-10",
      "Ubicación y Accesibilidad" = "<b>ID</b>: PER Q28 <br>
            <b>Pregunta</b>:	que tan satisfecho está con LA ACCESIBILIDAD DE LA UBICACIÓN DE SU VIVIENDA? (Cercanía con centros comerciales, trabajo, escuelas, avenidas principales, etc.) <br>
             <b>Escala</b>:  1-10",
      "Comparativa" = "<b>ID</b>: PER Q26 - Q28 <br>
            <b>Pregunta</b>:	Satisfacción en multiples rasgos <br>
             <b>Escala</b>:  1-10",
      "<b>ID</b>: PER Q26 <br>
            <b>Pregunta</b>:	Que tan satisfecho esta con LA CALIDAD DE LOS MATERIALES DE LA VIVIENDA? <br>
             <b>Escala</b>:  1-10"
    )
    
    update_tooltip_content(session, "housing_tooltip", tooltip_content)
  })
  
  # Set initial tooltip
  observeEvent(session$clientData$url_protocol, {
    initial_tooltip <- "<b>ID</b>: PER Q26 <br>
            <b>Pregunta</b>:	Que tan satisfecho esta con LA CALIDAD DE LOS MATERIALES DE LA VIVIENDA? <br>
             <b>Escala</b>:  1-10"
    
    update_tooltip_content(session, "housing_tooltip", initial_tooltip)
  }, once = TRUE)
  
  # Get survey data directly - we'll process it manually to avoid the error
  survey_data <- reactive({
    req(selectedYear())
    survey_id <- paste0("PER_", selectedYear())
    data_manager$get_survey_data(survey_id)
  })
  
  # Process housing data manually
  processed_housing_data <- reactive({
    req(survey_data())
    
    data_cache_key <- paste0("processed_housing_data_", selectedYear())
    if (!is.null(data_manager$cache[[data_cache_key]])) {
      return(data_manager$cache[[data_cache_key]])
    }
    
    processed_data <- list()
    
    # Process each housing question
    for (aspect in names(housing_questions)) {
      q_id <- housing_questions[aspect]
      
      # Manual processing of interval data
      data <- survey_data()$responses
      metadata <- survey_data()$metadata
      
      # Extract question data and convert to numeric
      values <- data[[q_id]]
      values_numeric <- as.numeric(values)
      
      # Create district column if missing
      if (!"DISTRICT" %in% colnames(data)) {
        district <- rep(NA, nrow(data))
      } else {
        district <- data$DISTRICT
      }
      
      # Create processed dataframe
      processed_data[[aspect]] <- data.frame(
        value = values,
        value_num = values_numeric,
        district = district,
        stringsAsFactors = FALSE
      )
    }
    
    # Cache the processed data
    data_manager$cache[[data_cache_key]] <- processed_data
    
    return(processed_data)
  })
  
  # Maps reactive function
  maps <- reactive({
    req(processed_housing_data(), geo_data())
    
    map_cache_key <- paste0("housing_maps_", selectedYear())
    if (!is.null(data_manager$cache[[map_cache_key]])) {
      return(data_manager$cache[[map_cache_key]])
    }
    
    map_list <- list()
    
    # Create maps for each aspect of housing
    for (aspect in names(housing_questions)) {
      aspect_data <- processed_housing_data()[[aspect]]
      
      # Create map
      map_list[[aspect]] <- create_interval_district_map(
        data = aspect_data,
        geo_data = geo_data(),
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
  
  # Calculations for value boxes and plots
  calculations <- reactive({
    req(processed_housing_data())
    
    calc_cache_key <- paste0("housing_calculations_", selectedYear())
    if (!is.null(data_manager$cache[[calc_cache_key]])) {
      return(data_manager$cache[[calc_cache_key]])
    }
    
    calc_list <- list()
    
    # Helper function to calculate mode
    find_mode <- function(x) {
      x <- x[!is.na(x)]
      if (length(x) == 0) return(NA)
      
      freq_table <- table(x)
      mode_val <- as.numeric(names(freq_table)[which.max(freq_table)])
      return(mode_val)
    }
    
    # Calculate averages and best districts for each aspect
    for (aspect in names(housing_questions)) {
      aspect_data <- processed_housing_data()[[aspect]]
      
      # Average satisfaction
      calc_list[[paste0(aspect, "_avg")]] <- sprintf("%.1f / 10", 
                                                    mean(aspect_data$value_num, na.rm = TRUE))
      
      # Best district
      district_stats <- aspect_data %>%
        group_by(district) %>%
        summarise(
          mean_value = mean(value_num, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        filter(!is.na(district)) # Filter out rows with NA district
      
      if (nrow(district_stats) > 0) {
        best_district <- district_stats %>%
          filter(mean_value == max(mean_value, na.rm = TRUE))
        
        if (nrow(best_district) > 0) {
          calc_list[[paste0(aspect, "_best_district")]] <- sprintf("Distrito %s (%.1f)", 
                                                                best_district$district[1], 
                                                                best_district$mean_value[1])
        } else {
          calc_list[[paste0(aspect, "_best_district")]] <- "No disponible"
        }
      } else {
        calc_list[[paste0(aspect, "_best_district")]] <- "No disponible"
      }
      
      # District summary for comparison plot
      aspect_by_district <- aspect_data %>%
        filter(!is.na(district)) %>% # Filter out rows with NA district
        group_by(district) %>%
        summarise(
          mean_value = mean(value_num, na.rm = TRUE),
          mode_value = find_mode(value_num),
          count = n(),
          .groups = 'drop'
        ) %>%
        mutate(aspect = switch(aspect,
                              "materials" = "Materiales",
                              "spaces" = "Espacios",
                              "location" = "Ubicación"))
      
      calc_list[[paste0(aspect, "_by_district")]] <- aspect_by_district
    }
    
    # Cache calculations
    data_manager$cache[[calc_cache_key]] <- calc_list
    
    return(calc_list)
  })
  
  # Comparison plot
  output$comparison_plot <- renderPlotly({
    req(calculations())
    
    # Combine district data for comparison plot
    all_data <- bind_rows(
      calculations()$materials_by_district,
      calculations()$spaces_by_district,
      calculations()$location_by_district
    )
    
    # If no data, return an empty plot
    if (nrow(all_data) == 0) {
      return(plot_ly() %>% 
             layout(title = "No hay datos disponibles"))
    }
    
    # Define colors from active theme
    aspect_colors <- active_theme()$palettes$categorical
    
    # Create comparison plot
    plot_ly(
      all_data, 
      x = ~district, 
      y = ~mean_value, 
      color = ~aspect,
      colors = aspect_colors, 
      type = "bar",
      hoverinfo = "text",
      hovertext = ~paste0(
        aspect, "<br>",
        "Distrito: ", district, "<br>",
        "Promedio: ", round(mean_value, 2), "<br>",
        "Valor más frecuente: ", mode_value, "<br>",
        "N: ", count
      ),
      text = ~round(mean_value, 1),
      textposition = "inside",
      insidetextanchor = "middle",
      textfont = list(
        color = "white",
        size = 11
      )
    ) %>%
      layout(
        title = "Comparación de Satisfacción por Distrito y Aspecto",
        xaxis = list(
          title = "Distrito",
          tickangle = 0
        ),
        yaxis = list(
          title = "Nivel de Satisfacción (1-10)", 
          range = c(0, 10)
        ),
        barmode = "group",
        legend = list(
          title = list(text = "Aspecto"),
          orientation = "h",
          xanchor = "center",
          x = 0.5,
          y = 1.1
        ),
        margin = list(t = 100)
      ) %>%
      apply_plotly_theme()
  })
  
  # Render maps for each aspect
  output$materials_map <- renderLeaflet({
    req(maps())
    maps()$materials
  })
  
  output$spaces_map <- renderLeaflet({
    req(maps())
    maps()$spaces
  })
  
  output$location_map <- renderLeaflet({
    req(maps())
    maps()$location
  })
  
  # Render average values
  output$materials_avg <- renderText({
    req(calculations())
    calculations()$materials_avg
  })
  
  output$spaces_avg <- renderText({
    req(calculations())
    calculations()$spaces_avg
  })
  
  output$location_avg <- renderText({
    req(calculations())
    calculations()$location_avg
  })
  
  # Render best district values
  output$materials_best_district <- renderText({
    req(calculations())
    calculations()$materials_best_district
  })
  
  output$spaces_best_district <- renderText({
    req(calculations())
    calculations()$spaces_best_district
  })
  
  output$location_best_district <- renderText({
    req(calculations())
    calculations()$location_best_district
  })
  
  # Show/hide download button based on tab
  observeEvent(input$housing_tabs, {
    if (input$housing_tabs == "Comparativa") {
      shinyjs::hide("download_house_satis_map")
    } else {
      shinyjs::show("download_house_satis_map")
    }
  }, ignoreInit = FALSE)
  
  # Download handler for maps
  output$download_house_satis_map <- downloadHandler(
    filename = function() {
      map_type <- if (input$housing_tabs == "Calidad de Materiales") { 
                      "Materiales"
                  } else if (input$housing_tabs == "Tamaño y Espacios") {
                      "Espacios" 
                  } else if (input$housing_tabs == "Ubicación y Accesibilidad") {
                      "Ubicación" 
                  } else {
                      ""
                  }
      paste("mapa_vivienda_", map_type, "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Temporary file for HTML content
      tmp_html <- tempfile(fileext = ".html")
      
      # Get the appropriate map based on active tab
      if (input$housing_tabs == "Calidad de Materiales") {
        map <- maps()$materials
        title_text <- "Satisfacción con la Calidad de Materiales"
      } else if (input$housing_tabs == "Tamaño y Espacios") {
        map <- maps()$spaces
        title_text <- "Satisfacción con el Tamaño y Espacios"
      } else if (input$housing_tabs == "Ubicación y Accesibilidad") {
        map <- maps()$location
        title_text <- "Satisfacción con la Ubicación y Accesibilidad"
      }
      
      # Add title and footer
      map <- map %>%
        addControl(
          html = paste("<div style='background-color:white; padding:10px; border-radius:5px; font-weight:bold;'>", 
                      title_text, 
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
        options = list(
          printBackground = TRUE,
          scale = 2.0
        ),
        format = "png",
        browser = "/usr/bin/google-chrome",
        extra_args = c("--no-sandbox", "--disable-dev-shm-usage")
      )
      
      # Clean up
      if (file.exists(tmp_html)) {
        file.remove(tmp_html)
      }
    }
  )
}