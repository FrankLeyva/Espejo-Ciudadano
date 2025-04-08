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
  
  # Renderizar mapa de hogares con estudiantes - USING BINARY MODULE FUNCTION
  output$students_map <- renderLeaflet({
    req(student_data(), geo_data())
    create_binary_district_map(
      student_data(), 
      geo_data(),
      highlight_extremes = TRUE,
      focus_on_true = TRUE,  # Para mostrar porcentaje de "Sí"
      custom_theme = active_theme()
    )
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
    level_colors <- c(
      "Educación Básica" = "#2A9D8F",
      "Educación Media Superior" = "#E9C46A", 
      "Educación Superior" = "#E76F51"
    )
    
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
      config(displayModeBar = FALSE) # Opcional: ocultar la barra de herramientas de plotly
  })
}