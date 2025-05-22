# education_server.R - Updated with Enhanced Data Management

educationServer <- function(input, output, session, current_theme = NULL) {
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
      get_section_theme("infraestructura")
    }
  })

  # Try to load pre-saved plots first, then create if needed
  plots <- reactive({
    req(selectedYear())
    
    # Try to load saved plots
    saved_plots <- data_manager$load_saved_plots("education", selectedYear())
    
    if (!is.null(saved_plots)) {
      return(saved_plots)
    }
    
    # If no saved plots, create them
    survey_id <- paste0("PER_", selectedYear())
    plot_list <- list()
    
    # Education satisfaction comparison chart
    plot_key <- paste0("education_comparison_plot_", survey_id)
    plot_list$education_comparison_plot <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        # Helper function to calculate mode
        find_mode <- function(x) {
          # Remove NA values
          x <- x[!is.na(x)]
          if(length(x) == 0) return(NA)
          
          # Calculate frequencies
          freq_table <- table(x)
          # Find the value with highest frequency
          mode_val <- as.numeric(names(freq_table)[which.max(freq_table)])
          return(mode_val)
        }
        
        # Get processed data for each education level
        basic_edu_data <- data_manager$get_processed_data(survey_id, "Q7", "interval")
        highschool_edu_data <- data_manager$get_processed_data(survey_id, "Q10", "interval")
        college_edu_data <- data_manager$get_processed_data(survey_id, "Q13", "interval")
        
        # Preparar datos para cada nivel educativo por distrito
        basic_by_district <- basic_edu_data %>%
          group_by(district) %>%
          summarise(
            mean_value = mean(value_num, na.rm = TRUE),
            mode_value = find_mode(value_num),
            count = n(),
            .groups = 'drop'
          ) %>%
          mutate(level = "Educación Básica")
        
        highschool_by_district <- highschool_edu_data %>%
          group_by(district) %>%
          summarise(
            mean_value = mean(value_num, na.rm = TRUE),
            mode_value = find_mode(value_num),
            count = n(),
            .groups = 'drop'
          ) %>%
          mutate(level = "Educación Media Superior")
        
        college_by_district <- college_edu_data %>%
          group_by(district) %>%
          summarise(
            mean_value = mean(value_num, na.rm = TRUE),
            mode_value = find_mode(value_num),
            count = n(),
            .groups = 'drop'
          ) %>%
          mutate(level = "Educación Superior")
        
        # Combinar datos
        all_data <- bind_rows(basic_by_district, highschool_by_district, college_by_district)
        
        # Colores para cada nivel educativo
        level_colors <- active_theme()$palettes$categorical
        
        # Crear gráfico
        plot_ly(
          all_data, 
          x = ~district, 
          y = ~mean_value, 
          color = ~level,
          colors = level_colors, 
          type = "bar",
          # Texto para mostrar en hover
          hoverinfo = "text",
          hovertext = ~paste0(
            level, "<br>",
            "Distrito: ", district, "<br>",
            "Promedio: ", round(mean_value, 2), "<br>",
            "Valor más frecuente: ", mode_value, "<br>",
            "N: ", count
          ),
          # Texto para mostrar en las barras
          text = ~round(mean_value, 1),
          textposition = "outside",
          insidetextanchor = "middle",
          textfont = list(
            color = "black",
            size = 11
          )
        ) %>%
          layout(
            title = "Satisfacción con niveles educativos por distrito",
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
              title = list(text = "Nivel Educativo"),
              orientation = "h",
              xanchor = "center",
              x = 0.5,
              y = 1.1
            ),
            margin = list(t = 100) # Espacio para la leyenda superior
          ) %>%
          apply_plotly_theme(custom_theme = active_theme()) # Apply theme
      }
    )
    
    # Save plots for future use
    data_manager$save_plots(plot_list, "education", selectedYear())
    
    return(plot_list)
  })
  
  # Maps - create separately since they use geo data
  maps <- reactive({
    req(selectedYear(), geo_data())
    
    # Try to load cached maps
    map_cache_key <- paste0("education_maps_", selectedYear())
    if (!is.null(data_manager$cache[[map_cache_key]])) {
      return(data_manager$cache[[map_cache_key]])
    }
    
    survey_id <- paste0("PER_", selectedYear())
    map_list <- list()
    
    # Process student data (Q6) - general
    student_data <- data_manager$get_processed_data(survey_id, "Q6", "binary")
    map_list$students_map <- create_binary_district_map(
      student_data, 
      geo_data(),
      highlight_extremes = TRUE,
      focus_on_true = TRUE,
      custom_theme = active_theme()
    )
    
    # Basic education data - satisfaction (Q7)
    basic_edu_data <- data_manager$get_processed_data(survey_id, "Q7", "interval")
    map_list$basic_education_map <- create_interval_district_map(
      basic_edu_data, 
      geo_data(),
      selected_responses = NULL,
      highlight_extremes = TRUE,
      use_gradient = FALSE,
      custom_theme = active_theme()
    )
    
    # High school education data - satisfaction (Q10)
    highschool_edu_data <- data_manager$get_processed_data(survey_id, "Q10", "interval")
    map_list$highschool_education_map <- create_interval_district_map(
      highschool_edu_data, 
      geo_data(),
      selected_responses = NULL,
      highlight_extremes = TRUE,
      use_gradient = FALSE,
      custom_theme = active_theme()
    )
    
    # College education data - satisfaction (Q13)
    college_edu_data <- data_manager$get_processed_data(survey_id, "Q13", "interval")
    map_list$college_education_map <- create_interval_district_map(
      college_edu_data, 
      geo_data(),
      selected_responses = NULL,
      highlight_extremes = TRUE,
      use_gradient = FALSE,
      custom_theme = active_theme()
    )
    
    # Binary maps for students at each education level
    # Basic education binary
    data <- data_manager$get_survey_data(survey_id)$responses
    data <- data %>% mutate(Q7 = ifelse(Q7 == "" | grepl("^No", Q7), 0, 1))
    basic_edu_binary <- prepare_binary_data(
      data = data,
      question_id = "Q7",
      metadata = data_manager$get_survey_data(survey_id)$metadata
    )
    map_list$basic_students_map <- create_binary_district_map(
      basic_edu_binary, 
      geo_data(),
      highlight_extremes = TRUE,
      focus_on_true = TRUE,
      custom_theme = active_theme()
    )
    
    # High school education binary
    data <- data_manager$get_survey_data(survey_id)$responses
    data <- data %>% mutate(Q10 = ifelse(Q10 == "" | grepl("^No", Q10), 0, 1))
    highschool_edu_binary <- prepare_binary_data(
      data = data,
      question_id = "Q10",
      metadata = data_manager$get_survey_data(survey_id)$metadata
    )
    map_list$highschool_students_map <- create_binary_district_map(
      highschool_edu_binary, 
      geo_data(),
      highlight_extremes = TRUE,
      focus_on_true = TRUE,
      custom_theme = active_theme()
    )
    
    # College education binary
    data <- data_manager$get_survey_data(survey_id)$responses
    data <- data %>% mutate(Q13 = ifelse(Q13 == "" | grepl("^No", Q13), 0, 1))
    college_edu_binary <- prepare_binary_data(
      data = data,
      question_id = "Q13",
      metadata = data_manager$get_survey_data(survey_id)$metadata
    )
    map_list$college_students_map <- create_binary_district_map(
      college_edu_binary, 
      geo_data(),
      highlight_extremes = TRUE,
      focus_on_true = TRUE,
      custom_theme = active_theme()
    )
    
    # Cache maps
    data_manager$cache[[map_cache_key]] <- map_list
    
    return(map_list)
  })
  
  # Value box calculations
  calculations <- reactive({
    req(selectedYear())
    
    calc_cache_key <- paste0("education_calculations_", selectedYear())
    if (!is.null(data_manager$cache[[calc_cache_key]])) {
      return(data_manager$cache[[calc_cache_key]])
    }
    
    survey_id <- paste0("PER_", selectedYear())
    
    # Get processed data
    student_data <- data_manager$get_processed_data(survey_id, "Q6", "binary")
    basic_edu_data <- data_manager$get_processed_data(survey_id, "Q7", "interval")
    highschool_edu_data <- data_manager$get_processed_data(survey_id, "Q10", "interval")
    college_edu_data <- data_manager$get_processed_data(survey_id, "Q13", "interval")
    
    # Create binary versions for each education level
    data <- data_manager$get_survey_data(survey_id)$responses
    data_basic <- data %>% mutate(Q7 = ifelse(Q7 == "" | grepl("^No", Q7), 0, 1))
    basic_edu_binary <- prepare_binary_data(
      data = data_basic,
      question_id = "Q7",
      metadata = data_manager$get_survey_data(survey_id)$metadata
    )
    
    data_highschool <- data %>% mutate(Q10 = ifelse(Q10 == "" | grepl("^No", Q10), 0, 1))
    highschool_edu_binary <- prepare_binary_data(
      data = data_highschool,
      question_id = "Q10",
      metadata = data_manager$get_survey_data(survey_id)$metadata
    )
    
    data_college <- data %>% mutate(Q13 = ifelse(Q13 == "" | grepl("^No", Q13), 0, 1))
    college_edu_binary <- prepare_binary_data(
      data = data_college,
      question_id = "Q13",
      metadata = data_manager$get_survey_data(survey_id)$metadata
    )
    
    calc_list <- list()
    
    # Calculate student percentages
    if (!is.null(student_data)) {
      student_pct <- sum(student_data$binary_value, na.rm = TRUE) / nrow(student_data) * 100
      calc_list$student_pct <- paste0(round(student_pct, 1), "%")
    } else {
      calc_list$student_pct <- "N/A"
    }
    
    # Calculate percentages for each education level
    if (!is.null(basic_edu_binary)) {
      basic_pct <- sum(basic_edu_binary$binary_value, na.rm = TRUE) / nrow(basic_edu_binary) * 100
      calc_list$basic_edu_pct <- paste0(round(basic_pct, 1), "%")
    } else {
      calc_list$basic_edu_pct <- "N/A"
    }
    
    if (!is.null(highschool_edu_binary)) {
      highschool_pct <- sum(highschool_edu_binary$binary_value, na.rm = TRUE) / nrow(highschool_edu_binary) * 100
      calc_list$highschool_edu_pct <- paste0(round(highschool_pct, 1), "%")
    } else {
      calc_list$highschool_edu_pct <- "N/A"
    }
    
    if (!is.null(college_edu_binary)) {
      college_pct <- sum(college_edu_binary$binary_value, na.rm = TRUE) / nrow(college_edu_binary) * 100
      calc_list$college_edu_pct <- paste0(round(college_pct, 1), "%")
    } else {
      calc_list$college_edu_pct <- "N/A"
    }
    
    # Calculate average satisfaction scores for each level
    if (!is.null(basic_edu_data)) {
      basic_avg <- mean(basic_edu_data$value_num, na.rm = TRUE)
      calc_list$basic_edu_avg <- round(basic_avg, 1)
    } else {
      calc_list$basic_edu_avg <- "N/A"
    }
    
    if (!is.null(highschool_edu_data)) {
      highschool_avg <- mean(highschool_edu_data$value_num, na.rm = TRUE)
      calc_list$highschool_edu_avg <- round(highschool_avg, 1)
    } else {
      calc_list$highschool_edu_avg <- "N/A"
    }
    
    if (!is.null(college_edu_data)) {
      college_avg <- mean(college_edu_data$value_num, na.rm = TRUE)
      calc_list$college_edu_avg <- round(college_avg, 1)
    } else {
      calc_list$college_edu_avg <- "N/A"
    }
    
    # Cache calculations
    data_manager$cache[[calc_cache_key]] <- calc_list
    
    return(calc_list)
  })
  
  # Update tooltip content based on selected tab for students
  observe({
    req(input$students_tabs)
    
    active_tab <- input$students_tabs
    
    tooltip_content <- switch(active_tab,
      "General" = "<b>ID</b>: PER Q6 <br>
            <b>Pregunta</b>:	En su familia, hay por lo menos 1 o más estudiantes de cualquier nivel educativo? <br>
             <b>Escala</b>:  1=Sí; 2=No",
      "Educación Básica" = "<b>ID</b>: PER Q7 <br>
            <b>Pregunta</b>:		En una escala del 1 al 10, que tan satisfecho esta con la educacion que recibe? EDUCACIÓN BASICA: PRIMARIA Y SECUNDARIA <br>
             <b>Escala</b>:  Conteo ",
      "Educación Media Superior" = "<b>ID</b>: PER Q10 <br>
            <b>Pregunta</b>:		En una escala del 1 al 10, Que tan satisfecho esta con la educacion que recibe? EDUCACION MEDIA <br>
             <b>Escala</b>:  Conteo",
      "Educación Superior" = "<b>ID</b>: PER Q13 <br>
            <b>Pregunta</b>:		En una escala del 1 al 10, Qué tan satisfecho esta con la educacion que recibe? EDUCACION SUPERIOR <br>
             <b>Escala</b>:  Conteo ",
      "<b>ID</b>: PER Q6 <br>
            <b>Pregunta</b>:	En su familia, hay por lo menos 1 o más estudiantes de cualquier nivel educativo? <br>
             <b>Escala</b>:  1=Sí; 2=No"
    )
    
    update_tooltip_content(session, "students_tooltip", tooltip_content)
  })

  # Set initial tooltip for students
  observeEvent(session$clientData$url_protocol, {
    initial_tooltip <- "<b>ID</b>: PER Q6 <br>
            <b>Pregunta</b>:	En su familia, hay por lo menos 1 o más estudiantes de cualquier nivel educativo? <br>
             <b>Escala</b>:  1=Sí; 2=No"
    update_tooltip_content(session, "students_tooltip", initial_tooltip)
  }, once = TRUE)

  # Update tooltip content based on selected tab for education satisfaction
  observe({
    req(input$education_tabs)
    
    active_tab <- input$education_tabs
    
    tooltip_content <- switch(active_tab,
      "Comparativa" = "<b>ID</b>: PER Q7 Q8 Q9 <br>
            <b>Pregunta</b>:	Satisfacción en los tres niveles educativos <br>
             <b>Escala</b>:  1-10",
      "Educación Básica" = "<b>ID</b>: PER Q7 <br>
            <b>Pregunta</b>:		En una escala del 1 al 10, que tan satisfecho esta con la educacion que recibe? EDUCACIÓN BASICA: PRIMARIA Y SECUNDARIA <br>
             <b>Escala</b>:  1-10",
      "Educación Media Superior" = "<b>ID</b>: PER Q10 <br>
            <b>Pregunta</b>:		En una escala del 1 al 10, Que tan satisfecho esta con la educacion que recibe? EDUCACION MEDIA <br>
             <b>Escala</b>:  1-10",
      "Educación Superior" = "<b>ID</b>: PER Q13 <br>
            <b>Pregunta</b>:		En una escala del 1 al 10, Qué tan satisfecho esta con la educacion que recibe? EDUCACION SUPERIOR <br>
             <b>Escala</b>:  1-10",
       "<b>ID</b>: PER Q7 <br>
            <b>Pregunta</b>:		En una escala del 1 al 10, que tan satisfecho esta con la educacion que recibe? EDUCACIÓN BASICA: PRIMARIA Y SECUNDARIA <br>
             <b>Escala</b>:  1-10"
    )
    
    update_tooltip_content(session, "edu_satis_tooltip", tooltip_content)
  })

  # Set initial tooltip for education satisfaction
  observeEvent(session$clientData$url_protocol, {
    initial_tooltip <- "<b>ID</b>: PER Q7 <br>
            <b>Pregunta</b>:		En una escala del 1 al 10, que tan satisfecho esta con la educacion que recibe? EDUCACIÓN BASICA: PRIMARIA Y SECUNDARIA <br>
             <b>Escala</b>:  1-10"
    update_tooltip_content(session, "edu_satis_tooltip", initial_tooltip)
  }, once = TRUE)

  # Render outputs
  output$education_comparison_plot <- renderPlotly({
    plots()$education_comparison_plot
  })
  
  # Render maps based on tabs
  output$students_map <- renderLeaflet({
    maps()$students_map
  })
  
  output$basic_students_map <- renderLeaflet({
    maps()$basic_students_map
  })
  
  output$highschool_students_map <- renderLeaflet({
    maps()$highschool_students_map
  })
  
  output$college_students_map <- renderLeaflet({
    maps()$college_students_map
  })
  
  output$basic_education_map <- renderLeaflet({
    maps()$basic_education_map
  })
  
  output$highschool_education_map <- renderLeaflet({
    maps()$highschool_education_map
  })
  
  output$college_education_map <- renderLeaflet({
    maps()$college_education_map
  })
  
  # Handle showing/hiding download button for education satisfaction maps
  observeEvent(input$education_tabs, {
    if(input$education_tabs == "Comparativa") {
      shinyjs::hide("download_edu_satis_map")
    } else {
      shinyjs::show("download_edu_satis_map")
    }
  }, ignoreInit = FALSE)
  
  # Download handler for education satisfaction maps
  output$download_edu_satis_map <- downloadHandler(
    filename = function() {
      # Get map type for filename based on active tab
      map_type <- if(input$education_tabs == "Educación Básica"){ 
                      "Basica"} else if (input$education_tabs == "Educación Media Superior"){
                        "Media_Superior" } else if (input$education_tabs == "Educación Superior"){
                        "Superior" }else {
                        ""
                      }
      paste("mapa_satisf_educacion_", map_type, "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Temporary file for the HTML content
      tmp_html <- tempfile(fileext = ".html")
      
      # Create the appropriate map based on active tab
      if(input$education_tabs == "Educación Básica") {
        map <- maps()$basic_education_map
        title_text <- "Satisfacción con la Educación Básica"
      } else if(input$education_tabs == "Educación Media Superior") {
        map <- maps()$highschool_education_map
        title_text <- "Satisfacción con la Educación Media Superior"
      } else if(input$education_tabs == "Educación Superior") {
        map <- maps()$college_education_map
        title_text <- "Satisfacción con la Educación Superior"
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
  
  # Download handler for student maps
  output$download_students_map <- downloadHandler(
    filename = function() {
      # Get education level for filename based on active tab
      edu_level <- switch(input$students_tabs,
                          "Educación Básica" = "basica",
                          "Educación Media Superior" = "media_superior",
                          "Educación Superior" = "superior",
                          "general")
      paste("mapa_estudiantes_", edu_level, "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Temporary file for the HTML content
      tmp_html <- tempfile(fileext = ".html")
      
      # Create the appropriate map based on active tab
      map <- NULL
      title_text <- ""
      
      if(input$students_tabs == "Educación Básica") {
        map <- maps()$basic_students_map
        title_text <- "Hogares con Estudiantes en Educación Básica"
      } else if(input$students_tabs == "Educación Media Superior") {
        map <- maps()$highschool_students_map
        title_text <- "Hogares con Estudiantes en Educación Media Superior"
      } else if(input$students_tabs == "Educación Superior") {
        map <- maps()$college_students_map
        title_text <- "Hogares con Estudiantes en Educación Superior"
      } else {
        # Default: General student map
        map <- maps()$students_map
        title_text <- "Hogares con Estudiantes (General)"
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