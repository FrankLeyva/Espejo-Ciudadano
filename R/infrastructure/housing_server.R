# Función del servidor para el Dashboard de Vivienda
housingServer <- function(input, output, session, current_theme = NULL) {
  selectedYear <- session$userData$selectedYear
  
  survey_data <- session$userData$perSurveyData
  
  geo_data <- session$userData$geoData
  
  # Setup active theme
  active_theme <- reactive({
    if (is.function(current_theme)) {
      # If current_theme is a reactive function, call it to get the value
      current_theme()
    } else if (!is.null(current_theme)) {
      # If it's a direct value, use it
      current_theme
    } else {
      # Default to infrastructure theme if nothing provided
      get_section_theme("infraestructura")
    }
  })

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

  observeEvent(session$clientData$url_protocol, {
    initial_tooltip <- "<b>ID</b>: PER Q26 <br>
            <b>Pregunta</b>:	Que tan satisfecho esta con LA CALIDAD DE LOS MATERIALES DE LA VIVIENDA? <br>
             <b>Escala</b>:  1-10"
    
    update_tooltip_content(session, "housing_tooltip", initial_tooltip)
  }, once = TRUE)  	 

  # Mapeo de preguntas de vivienda
  housing_questions <- c(
    "materials" = "Q26",  # Calidad de materiales
    "spaces" = "Q27",     # Tamaño y espacios
    "location" = "Q28"    # Ubicación y accesibilidad
  )
  
  # Función para preparar datos usando prepare_interval_data
  housing_data_list <- lapply(housing_questions, function(q_id) {
    reactive({
      req(survey_data(), q_id)
      prepare_interval_data(
        data = survey_data()$responses,
        question_id = q_id,
        metadata = survey_data()$metadata
      )
    })
  })
  
  # Renderizar mapas para cada aspecto de vivienda usando create_interval_district_map
  for (aspect in names(housing_questions)) {
    local({
      current_aspect <- aspect
      map_output_id <- paste0(current_aspect, "_map")
      
      output[[map_output_id]] <- renderLeaflet({
        req(housing_data_list[[current_aspect]](), geo_data())
        
        create_interval_district_map(
          data = housing_data_list[[current_aspect]](),
          geo_data = geo_data(),
          highlight_extremes = TRUE,
          use_gradient = F,  # Enable gradient coloring for better visualization
          color_scale = "Blues",  # Use blues color scale
          custom_theme = active_theme()
        )
      })
    })
  }
  
  # Calcular y mostrar promedios generales
  for (aspect in names(housing_questions)) {
    local({
      current_aspect <- aspect
      avg_output_id <- paste0(current_aspect, "_avg")
      
      output[[avg_output_id]] <- renderText({
        req(housing_data_list[[current_aspect]]())
        avg <- mean(housing_data_list[[current_aspect]]()$value_num, na.rm = TRUE)
        sprintf("%.1f / 10", avg)
      })
    })
  }
  
  # Mostrar distritos con mayor satisfacción
  for (aspect in names(housing_questions)) {
    local({
      current_aspect <- aspect
      district_output_id <- paste0(current_aspect, "_best_district")
      
      output[[district_output_id]] <- renderText({
        req(housing_data_list[[current_aspect]]())
        
        district_stats <- housing_data_list[[current_aspect]]() %>%
          group_by(district) %>%
          summarise(
            mean_value = mean(value_num, na.rm = TRUE),
            .groups = 'drop'
          )
        
        best_district <- district_stats %>%
          filter(mean_value == max(mean_value, na.rm = TRUE))
        
        sprintf("Distrito %s (%.1f)", best_district$district[1], best_district$mean_value[1])
      })
    })
  }
  
  # Gráfico comparativo
output$comparison_plot <- renderPlotly({
  req(
    housing_data_list[["materials"]](),
    housing_data_list[["spaces"]](),
    housing_data_list[["location"]]()
  )
  
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
  
  # Preparar datos para cada aspecto por distrito
  materials_by_district <- housing_data_list[["materials"]]() %>%
    group_by(district) %>%
    summarise(
      mean_value = mean(value_num, na.rm = TRUE),
      mode_value = find_mode(value_num),
      count = n(),
      .groups = 'drop'
    ) %>%
    mutate(aspect = "Materiales")
  
  spaces_by_district <- housing_data_list[["spaces"]]() %>%
    group_by(district) %>%
    summarise(
      mean_value = mean(value_num, na.rm = TRUE), 
      mode_value = find_mode(value_num),
      count = n(),
      .groups = 'drop'
    ) %>%
    mutate(aspect = "Espacios")
  
  location_by_district <- housing_data_list[["location"]]() %>%
    group_by(district) %>%
    summarise(
      mean_value = mean(value_num, na.rm = TRUE), 
      mode_value = find_mode(value_num),
      count = n(),
      .groups = 'drop'
    ) %>%
    mutate(aspect = "Ubicación")
  
  # Combinar datos
  all_data <- bind_rows(materials_by_district, spaces_by_district, location_by_district)
  
  # Definir colores manualmente para asegurar que coincidan con los nombres
  aspect_colors <- active_theme()$palettes$categorical
  
  
  # Crear gráfico
  plot_ly(
    all_data, 
    x = ~district, 
    y = ~mean_value, 
    color = ~aspect,
    colors = aspect_colors, 
    type = "bar",
    # Texto para mostrar en hover
    hoverinfo = "text",
    hovertext = ~paste0(
      aspect, "<br>",
      "Distrito: ", district, "<br>",
      "Promedio: ", round(mean_value, 2), "<br>",
      "Valor más frecuente: ", mode_value, "<br>",
      "N: ", count
    ),
    # Texto para mostrar en las barras
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
      margin = list(t = 100) # Espacio para la leyenda superior
    ) %>%
    apply_plotly_theme()
})



observeEvent(input$housing_tabs, {
  # Store the active tab in a reactive value for the download handler
  tab_value <- input$housing_tabs
})
observeEvent(input$housing_tabs, {
  if(input$housing_tabs == "Comparativa") {
    shinyjs::hide("download_house_satis_map")
  } else {
    shinyjs::show("download_house_satis_map")
  }
}, ignoreInit = FALSE) 



# Download handler that adapts based on active tab
output$download_house_satis_map <- downloadHandler(
  filename = function() {
    # Get map type for filename based on active tab
    map_type <- if(input$housing_tabs == "Calidad de Materiales"){ 
                      "Materiales"} else if (input$housing_tabs == "Tamaño y Espacios"){
                        "Espacios" } else if (input$housing_tabs == "Ubicación y Accesibilidad"){
                        "Ubicación" }else {
                        ""
                      }
    paste("mapa_vivienda_", map_type, "_", Sys.Date(), ".png", sep = "")
  },
  content = function(file) {
    # Temporary file for the HTML content
    tmp_html <- tempfile(fileext = ".html")
    
    # Create the appropriate map based on active tab
    if(input$housing_tabs == "Calidad de Materiales") {
      map <- create_interval_district_map(
        data = housing_data_list[["materials"]](),
        geo_data = geo_data(),
        highlight_extremes = TRUE,
        use_gradient = FALSE,
        color_scale = "Blues",
        custom_theme = active_theme()
      )
      title_text <- "Satisfacción con la Calidad de Materiales"
    } else if(input$housing_tabs == "Tamaño y Espacios") {
      map <- create_interval_district_map(
        data = housing_data_list[["spaces"]](),
        geo_data = geo_data(),
        highlight_extremes = TRUE,
        use_gradient = FALSE,
        color_scale = "Blues",
        custom_theme = active_theme()
      )
      title_text <- "Satisfacción con el Tamaño y Espacios"
    } else if(input$housing_tabs == "Ubicación y Accesibilidad") {
      map <- create_interval_district_map(
        data = housing_data_list[["location"]](),
        geo_data = geo_data(),
        highlight_extremes = TRUE,
        use_gradient = FALSE,
        color_scale = "Blues",
        custom_theme = active_theme()
      )
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