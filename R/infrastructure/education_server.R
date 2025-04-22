# Función del servidor para el Dashboard de Educación
educationServer <- function(input, output, session, current_theme = NULL) {
  selectedYear <- session$userData$selectedYear
  
  # Load survey data with dynamic year
  survey_data <- reactive({
    survey_id <- paste0("PER_", selectedYear())
    load_survey_data(survey_id)
  })
  
  # Cargar datos geográficos
  geo_data <- reactive({
    tryCatch({
      sf::st_read('data/geo/Jrz_Map.geojson', quiet = TRUE)
    }, error = function(e) {
      showNotification(paste("Error cargando datos geográficos:", e$message), type = "error")
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
      # Default to infraestructura theme if nothing provided
      get_section_theme("infraestructura")
    }
  })

  # Datos sobre hogares con estudiantes (Q6)
  student_data <- reactive({
    req(survey_data())
    
    # Preparar datos binarios para Q6
    prepare_binary_data(
      data = survey_data()$responses,
      question_id = "Q6",
      metadata = survey_data()$metadata
    )
  })
  
  # Datos de satisfacción con educación básica (Q7)
  basic_edu_data <- reactive({
    req(survey_data())
    
    prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q7",
      metadata = survey_data()$metadata
    )
  })
  
  # Datos de satisfacción con educación media superior (Q10)
  highschool_edu_data <- reactive({
    req(survey_data())
    
    prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q10",
      metadata = survey_data()$metadata
    )
    
  })
  
  # Datos de satisfacción con educación superior (Q13)
  college_edu_data <- reactive({
    req(survey_data())
    
    prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q13",
      metadata = survey_data()$metadata
    )
  })
  
  basic_edu_binary <- reactive({
    req(survey_data())
    
    # Add binary values while preserving needed attributes
    data <-survey_data()$responses
    data <- data %>% mutate(Q7 = ifelse(Q7 == "" | grepl("^No", Q7), 0, 1))
    prepare_binary_data(
      data = data,
      question_id = "Q7",
      metadata = survey_data()$metadata
    )
  })
  
  highschool_edu_binary <- reactive({
    req(survey_data())
    
   # Add binary values while preserving needed attributes
    data <-survey_data()$responses
    data <- data %>% mutate(Q10 = ifelse(Q10 == "" | grepl("^No", Q10), 0, 1))
    prepare_binary_data(
      data = data,
      question_id = "Q10",
      metadata = survey_data()$metadata
    )
  })

  
  college_edu_binary <- reactive({
    req(survey_data())
    
   # Add binary values while preserving needed attributes
    data <-survey_data()$responses
    data <- data %>% mutate(Q13 = ifelse(Q13 == "" | grepl("^No", Q13), 0, 1))
    prepare_binary_data(
      data = data,
      question_id = "Q13",
      metadata = survey_data()$metadata
    )
  })
  
 
  
  output$basic_students_map <- renderLeaflet({
    req(basic_edu_binary(), geo_data())
    tryCatch({
      create_binary_district_map(
        basic_edu_binary(), 
        geo_data(),
        highlight_extremes = TRUE,
        focus_on_true = TRUE,  # Para mostrar porcentaje de "Sí"
        custom_theme = active_theme()
      )
    }, error = function(e) {
      # Log the error for debugging
      message("Error in basic_students_map: ", e$message)
      
      showNotification(paste("Error en mapa de educación básica:", e$message), type = "error")
      # Return a basic map with error message
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addControl(html = paste("Error al generar el mapa:", e$message), position = "topright")
    })
  })
  
  output$highschool_students_map <- renderLeaflet({
    req(highschool_edu_binary(), geo_data())
    tryCatch({
      create_binary_district_map(
        highschool_edu_binary(), 
        geo_data(),
        highlight_extremes = TRUE,
        focus_on_true = TRUE,
        custom_theme = active_theme()
      )
    }, error = function(e) {
      # Log the error for debugging
      message("Error in highschool_students_map: ", e$message)
      
      showNotification(paste("Error en mapa de educación media superior:", e$message), type = "error")
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addControl(html = paste("Error al generar el mapa:", e$message), position = "topright")
    })
  })
  
  output$college_students_map <- renderLeaflet({
    req(college_edu_binary(), geo_data())
    tryCatch({
      create_binary_district_map(
        college_edu_binary(), 
        geo_data(),
        highlight_extremes = TRUE,
        focus_on_true = TRUE,
        custom_theme = active_theme()
      )
    }, error = function(e) {
      # Log the error for debugging
      message("Error in college_students_map: ", e$message)
      
      showNotification(paste("Error en mapa de educación superior:", e$message), type = "error")
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addControl(html = paste("Error al generar el mapa:", e$message), position = "topright")
    })
  })
  

  # Keep original students map for the "General" tab
  output$students_map <- renderLeaflet({
    req(student_data(), geo_data())
    tryCatch({
      create_binary_district_map(
        student_data(), 
        geo_data(),
        highlight_extremes = TRUE,
        focus_on_true = TRUE,
        custom_theme = active_theme()
      )
    }, error = function(e) {
      showNotification(paste("Error en mapa general de estudiantes:", e$message), type = "error")
      leaflet() %>%
        addTiles() %>%
        addControl(html = paste("Error al generar el mapa:", e$message), position = "topright")
    })
  })
  
  # Renderizar mapas de satisfacción con niveles educativos - USING INTERVAL MODULE FUNCTIONS
  output$basic_education_map <- renderLeaflet({
    req(basic_edu_data(), geo_data())
    create_interval_district_map(
      basic_edu_data(), 
      geo_data(),
      selected_responses = NULL,  # Para mostrar promedios en lugar de porcentajes específicos
      highlight_extremes = TRUE,
      use_gradient = FALSE,       # Usar colores de distrito en lugar de gradiente
      custom_theme = active_theme()
    )

  })
  
  output$highschool_education_map <- renderLeaflet({
    req(highschool_edu_data(), geo_data())
    create_interval_district_map(
      highschool_edu_data(), 
      geo_data(),
      selected_responses = NULL,
      highlight_extremes = TRUE,
      use_gradient = FALSE,
      custom_theme = active_theme()
    )
  })
  
  output$college_education_map <- renderLeaflet({
    req(college_edu_data(), geo_data())
    create_interval_district_map(
      college_edu_data(), 
      geo_data(),
      selected_responses = NULL,
      highlight_extremes = TRUE,
      use_gradient = FALSE,
      custom_theme = active_theme()
    )
  })
  
  # Gráfico comparativo de satisfacción con niveles educativos
  output$education_comparison_plot <- renderPlotly({
    req(basic_edu_data(), highschool_edu_data(), college_edu_data())
    
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
    
    # Preparar datos para cada nivel educativo por distrito
    basic_by_district <- basic_edu_data() %>%
      group_by(district) %>%
      summarise(
        mean_value = mean(value_num, na.rm = TRUE),
        mode_value = find_mode(value_num),
        count = n(),
        .groups = 'drop'
      ) %>%
      mutate(level = "Educación Básica")
    
    highschool_by_district <- highschool_edu_data() %>%
      group_by(district) %>%
      summarise(
        mean_value = mean(value_num, na.rm = TRUE),
        mode_value = find_mode(value_num),
        count = n(),
        .groups = 'drop'
      ) %>%
      mutate(level = "Educación Media Superior")
    
    college_by_district <- college_edu_data() %>%
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
      apply_plotly_theme() # Opcional: ocultar la barra de herramientas de plotly
  })

  # Track active tabs for download buttons
  observeEvent(input$education_tabs, {
    # Store the active tab in a reactive value for the download handler
    tab_value <- input$education_tabs
  })

  observeEvent(input$education_tabs, {
    if(input$education_tabs == "Comparativa") {
      shinyjs::hide("download_edu_satis_map")
    } else {
      shinyjs::show("download_edu_satis_map")
    }
  }, ignoreInit = FALSE) 
  
  # Track active tabs for student maps
  observeEvent(input$students_tabs, {
    # Used for the download handler
    tab_value <- input$students_tabs
  })
  
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
        
        map <- create_interval_district_map(
          basic_edu_data(), 
          geo_data(),
          selected_responses = NULL,  # Para mostrar promedios en lugar de porcentajes específicos
          highlight_extremes = TRUE,
          use_gradient = FALSE,       # Usar colores de distrito en lugar de gradiente
          custom_theme = active_theme()
        )
        title_text <- "Satisfacción con la Educación Básica"
      } else if(input$education_tabs == "Educación Media Superior") {

        map <- create_interval_district_map(
          highschool_edu_data(), 
          geo_data(),
          selected_responses = NULL,
          highlight_extremes = TRUE,
          use_gradient = FALSE,
          custom_theme = active_theme()
        )
        title_text <- "Satisfacción con la Educación Media Superior"
      } else if(input$education_tabs == "Educación Superior") {
        map <-  create_interval_district_map(
          college_edu_data(), 
          geo_data(),
          selected_responses = NULL,
          highlight_extremes = TRUE,
          use_gradient = FALSE,
          custom_theme = active_theme()
        )
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
        map <- create_binary_district_map(
          basic_edu_binary(), 
          geo_data(),
          highlight_extremes = TRUE,
          focus_on_true = TRUE,
          custom_theme = active_theme()
        )
        title_text <- "Hogares con Estudiantes en Educación Básica"
      } else if(input$students_tabs == "Educación Media Superior") {
        map <- create_binary_district_map(
          highschool_edu_binary(), 
          geo_data(),
          highlight_extremes = TRUE,
          focus_on_true = TRUE,
          custom_theme = active_theme()
        )
        title_text <- "Hogares con Estudiantes en Educación Media Superior"
      } else if(input$students_tabs == "Educación Superior") {
        map <- create_binary_district_map(
          college_edu_binary(), 
          geo_data(),
          highlight_extremes = TRUE,
          focus_on_true = TRUE,
          custom_theme = active_theme()
        )
        title_text <- "Hogares con Estudiantes en Educación Superior"
      } else {
        # Default: General student map
        map <- create_binary_district_map(
          student_data(), 
          geo_data(),
          highlight_extremes = TRUE,
          focus_on_true = TRUE,
          custom_theme = active_theme()
        )
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