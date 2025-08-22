# education_server.R - Simplified with Data Manager Integration

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
  
  # Load pre-saved plots
  plots <- reactive({
    req(selectedYear())
    data_manager$get_plots("infrastructure", selectedYear())
  })
  
  # Load pre-saved maps
  maps <- reactive({
    req(selectedYear())
    data_manager$get_maps("infrastructure", selectedYear())
  })
  
  # Load pre-calculated percentages
  percentages <- reactive({
    req(selectedYear())
    data_manager$get_percentages("infrastructure", selectedYear())
  })
  
  # Update tooltip content based on selected tab
  observe({
    req(input$education_tabs)
    
    active_tab <- input$education_tabs
    
    tooltip_content <- switch(active_tab,
      "Estudiantes por Nivel" = "<b>ID</b>: PER Q21 - Q24 <br>
            <b>Pregunta</b>: ¿Hay alguien en su hogar que actualmente esté estudiando? <br>
             <b>Escala</b>: Sí/No por nivel educativo",
      "Satisfacción Educación Básica" = "<b>ID</b>: PER Q21.2 <br>
            <b>Pregunta</b>: ¿Qué tan satisfecho está con la calidad de la educación básica? <br>
             <b>Escala</b>: 1-10",
      "Satisfacción Educación Media Superior" = "<b>ID</b>: PER Q22.2 <br>
            <b>Pregunta</b>: ¿Qué tan satisfecho está con la calidad de la educación media superior? <br>
             <b>Escala</b>: 1-10",
      "Satisfacción Educación Superior" = "<b>ID</b>: PER Q23.2 <br>
            <b>Pregunta</b>: ¿Qué tan satisfecho está con la calidad de la educación superior? <br>
             <b>Escala</b>: 1-10",
      "General" = "<b>ID</b>: PER Q21 - Q24 <br>
            <b>Pregunta</b>: Indicadores generales de educación <br>
             <b>Escala</b>: Mixta",
      "<b>ID</b>: PER Q21 - Q24 <br>
            <b>Pregunta</b>: ¿Hay alguien en su hogar que actualmente esté estudiando? <br>
             <b>Escala</b>: Sí/No por nivel educativo"
    )
    
    update_tooltip_content(session, "education_tooltip", tooltip_content)
  })
  
  # Set initial tooltip
  observeEvent(session$clientData$url_protocol, {
    initial_tooltip <- "<b>ID</b>: PER Q21 - Q24 <br>
            <b>Pregunta</b>: ¿Hay alguien en su hogar que actualmente esté estudiando? <br>
             <b>Escala</b>: Sí/No por nivel educativo"
    
    update_tooltip_content(session, "education_tooltip", initial_tooltip)
  }, once = TRUE)
  
 
  
 
  
  # Render maps using pre-saved maps
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
  


  output$education_comparison_plot <- renderPlotly({
    plots()$education_comparison_plot
  })

  # Render percentage values for value boxes
  output$students_general_pct <- renderText({
    percentages()$students_general_pct
  })
  
  output$students_basica_pct <- renderText({
    percentages()$students_basica_pct
  })
  
  output$students_media_superior_pct <- renderText({
    percentages()$students_media_superior_pct
  })
  
  output$students_superior_pct <- renderText({
    percentages()$students_superior_pct
  })
  
  output$satisfaction_basica_avg <- renderText({
    percentages()$satisfaction_basica_avg
  })
  
  output$satisfaction_media_superior_avg <- renderText({
    percentages()$satisfaction_media_superior_avg
  })
  
  output$satisfaction_superior_avg <- renderText({
    percentages()$satisfaction_superior_avg
  })
  
  # Download handler using pre-saved PNG files
  output$download_education_map <- downloadHandler(
    filename = function() {
      map_type <- switch(input$education_tabs,
        "Estudiantes por Nivel" = "general",
        "Satisfacción Educación Básica" = "Basica",
        "Satisfacción Educación Media Superior" = "Media_Superior", 
        "Satisfacción Educación Superior" = "Superior",
        "General" = "general",
        "general"
      )
      
      # Determine if it's students or satisfaction map
      if (input$education_tabs == "Estudiantes por Nivel" || input$education_tabs == "General") {
        paste("mapa_estudiantes_", map_type, "_", selectedYear(), "_", Sys.Date(), ".png", sep = "")
      } else {
        paste("mapa_satisf_educacion_", map_type, "_", selectedYear(), "_", Sys.Date(), ".png", sep = "")
      }
    },
    content = function(file) {
      # Determine map name based on active tab
      map_name <- switch(input$education_tabs,
        "Estudiantes por Nivel" = "mapa_estudiantes_general",
        "Satisfacción Educación Básica" = "mapa_satisf_educacion_Basica",
        "Satisfacción Educación Media Superior" = "mapa_satisf_educacion_Media_Superior",
        "Satisfacción Educación Superior" = "mapa_satisf_educacion_Superior", 
        "General" = "mapa_estudiantes_general",
        "mapa_estudiantes_general"
      )
      
              analytics$track_interaction(
        session_id, 
        "download", 
        input$navbar, 
        map_name
      )
      map_path <- data_manager$get_map_path(map_name, selectedYear())
      
      if (file.exists(map_path)) {
        file.copy(map_path, file)
      } else {
        warning(paste("Map file not found:", map_path))
        file.create(file)
      }
    }
  )
}