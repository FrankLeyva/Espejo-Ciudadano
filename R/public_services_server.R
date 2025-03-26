# Función del servidor para el Dashboard de Servicios Públicos
publicServicesServer <- function(input, output, session) {
  # Cargar datos de encuesta de Percepción 2024
  survey_data <- reactive({
    load_survey_data("PER_2024_V2")
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
  
  # Almacenar tema actual
  current_theme <- reactiveVal(theme_config)
  
  # Mostrar texto de la pregunta basado en el servicio seleccionado
  output$question_text <- renderText({
    req(survey_data(), input$selected_service)
    
    # Obtener metadatos de la pregunta
    question_meta <- survey_data()$metadata %>%
      filter(variable == input$selected_service) %>%
      first()
    
    if (!is.null(question_meta) && !is.na(question_meta$label)) {
      return(question_meta$label)
    } else {
      return("Información no disponible")
    }
  })
  
  # Calcular y mostrar valor para Q31 (días con agua)
  output$water_days <- renderText({
    req(survey_data())
    
    # Calcular promedio de días con agua
    water_data <- as.numeric(survey_data()$responses$Q31)
    water_data <- water_data[!is.na(water_data)]
    
    if (length(water_data) > 0) {
      mean_days <- mean(water_data, na.rm = TRUE)
      # Formatear a 1 decimal
      sprintf("%.1f días", mean_days)
    } else {
      "Datos no disponibles"
    }
  })
  
  # Calcular y mostrar valor para Q36 (cortes de luz)
  output$power_outages <- renderText({
    req(survey_data())
    
    # Calcular promedio de cortes de luz
    outage_data <- as.numeric(survey_data()$responses$Q36)
    outage_data <- outage_data[!is.na(outage_data)]
    
    if (length(outage_data) > 0) {
      mean_outages <- mean(outage_data, na.rm = TRUE)
      # Formatear a 1 decimal
      sprintf("%.1f veces", mean_outages)
    } else {
      "Datos no disponibles"
    }
  })
  
  # Calcular y mostrar valor para Q41 (frecuencia de recolección de basura)
  output$trash_pickup <- renderText({
    req(survey_data())
    
    # Obtener valores de frecuencia
    freq_values <- survey_data()$responses$Q41
    freq_values <- freq_values[!is.na(freq_values)]
    
    if (length(freq_values) > 0) {
      # Crear mapeo de valor a descripción de texto
      freq_mapping <- c(
        "1" = "Diario",
        "2" = "Cada tercer día",
        "3" = "Una vez por semana",
        "4" = "Cada quince días",
        "5" = "Una vez al mes",
        "6" = "Nunca"
      )
      
      # Encontrar la frecuencia más común
      freq_table <- table(freq_values)
      most_common <- names(freq_table)[which.max(freq_table)]
      
      # Devolver la descripción de texto de la frecuencia más común
      if (most_common %in% names(freq_mapping)) {
        freq_mapping[most_common]
      } else {
        "Categoría más común"
      }
    } else {
      "Datos no disponibles"
    }
  })
  
  # Preparar datos para el mapa 
  prepared_data <- reactive({
    req(survey_data(), input$selected_service)
    
    # Utilizar la función del módulo de intervalo para preparar los datos
    prepare_interval_data(
      data = survey_data()$responses,
      question_id = input$selected_service,
      metadata = survey_data()$metadata
    )
  })
  
  # Renderizar directamente el mapa de distrito sin los controles del módulo
  output$service_map <- renderLeaflet({
    req(prepared_data(), geo_data())
    
    # Usar la función para crear el mapa de distritos del módulo de intervalo
    # pero llamándola directamente con nuestros datos
    create_interval_district_map(
      data = prepared_data(),
      geo_data = geo_data(),
      use_gradient = FALSE,  # Usamos gradiente para mejor visualización
      color_scale = "Blues",
      custom_theme = current_theme()
    )
  })
  
  # Manejador de descarga
  output$download_data <- downloadHandler(
    filename = function() {
      paste("servicios_publicos_datos_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      # Crear un libro de trabajo
      wb <- createWorkbook()
      
      # Añadir una hoja de trabajo para cada servicio
      service_questions <- c("Q29", "Q30", "Q35", "Q40", "Q45", "Q51", "Q55", 
                            "Q56", "Q58", "Q59", "Q60", "Q61", "Q62")
      
      service_names <- c(
        "Agua", "Drenaje y Alcantarillado", "CFE", "Recolección de Basura",
        "Alumbrado Público", "Calles y Pavimentación", "Áreas verdes",
        "Unidades deportivas", "Bibliotecas", "Centros comunitarios",
        "Espacios para discapacitados", "Parques", "Transporte público"
      )
      
      for (i in 1:length(service_questions)) {
        # Crear un dataframe para este servicio
        q_id <- service_questions[i]
        service_name <- service_names[i]
        
        # Extraer datos
        service_data <- data.frame(
          Distrito = survey_data()$responses$DISTRICT,
          Calificacion = survey_data()$responses[[q_id]]
        )
        
        # Añadir al libro de trabajo
        addWorksheet(wb, service_name)
        writeData(wb, service_name, service_data)
      }
      
      # Añadir estadísticas resumidas
      addWorksheet(wb, "Resumen")
      summary_data <- data.frame(
        Servicio = service_names,
        Promedio = sapply(service_questions, function(q) {
          mean(as.numeric(survey_data()$responses[[q]]), na.rm = TRUE)
        }),
        Mediana = sapply(service_questions, function(q) {
          median(as.numeric(survey_data()$responses[[q]]), na.rm = TRUE)
        })
      )
      writeData(wb, "Resumen", summary_data)
      
      # Guardar el libro de trabajo
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}