# housing_server.R - Simplified with Data Manager Integration

housingServer <- function(input, output, session, current_theme = NULL) {
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
    req(input$housing_tabs)
    
    active_tab <- input$housing_tabs
    
    tooltip_content <- switch(active_tab,
      "Calidad de Materiales" = "<b>ID</b>: PER Q26 <br>
            <b>Pregunta</b>: Que tan satisfecho esta con LA CALIDAD DE LOS MATERIALES DE LA VIVIENDA? <br>
             <b>Escala</b>: 1-10",
      "Tamaño y Espacios" = "<b>ID</b>: PER Q27 <br>
            <b>Pregunta</b>: Qué tan satisfecho está con EL TAMAÑO Y ESPACIOS DE LA VIVIENDA? <br>
             <b>Escala</b>: 1-10",
      "Ubicación y Accesibilidad" = "<b>ID</b>: PER Q28 <br>
            <b>Pregunta</b>: que tan satisfecho está con LA ACCESIBILIDAD DE LA UBICACIÓN DE SU VIVIENDA? <br>
             <b>Escala</b>: 1-10",
      "Comparativa" = "<b>ID</b>: PER Q26 - Q28 <br>
            <b>Pregunta</b>: Satisfacción en multiples rasgos <br>
             <b>Escala</b>: 1-10",
      "<b>ID</b>: PER Q26 <br>
            <b>Pregunta</b>: Que tan satisfecho esta con LA CALIDAD DE LOS MATERIALES DE LA VIVIENDA? <br>
             <b>Escala</b>: 1-10"
    )
    
    update_tooltip_content(session, "housing_tooltip", tooltip_content)
  })
  
  # Set initial tooltip
  observeEvent(session$clientData$url_protocol, {
    initial_tooltip <- "<b>ID</b>: PER Q26 <br>
            <b>Pregunta</b>: Que tan satisfecho esta con LA CALIDAD DE LOS MATERIALES DE LA VIVIENDA? <br>
             <b>Escala</b>: 1-10"
    
    update_tooltip_content(session, "housing_tooltip", initial_tooltip)
  }, once = TRUE)
  
  # Render outputs using pre-saved plots
  output$comparison_plot <- renderPlotly({
    plots()$housing_comparison_plot
  })
  
  # Render maps using pre-saved maps
  output$materials_map <- renderLeaflet({
    maps()$housing_materials_map
  })
  
  output$spaces_map <- renderLeaflet({
    maps()$housing_spaces_map
  })
  
  output$location_map <- renderLeaflet({
    maps()$housing_location_map
  })
  
  # Render percentage values for value boxes
  output$materials_avg <- renderText({
    percentages()$materials_avg
  })
  
  output$spaces_avg <- renderText({
    percentages()$spaces_avg
  })
  
  output$location_avg <- renderText({
    percentages()$location_avg
  })
  
  output$materials_best_district <- renderText({
    percentages()$materials_best_district
  })
  
  output$spaces_best_district <- renderText({
    percentages()$spaces_best_district
  })
  
  output$location_best_district <- renderText({
    percentages()$location_best_district
  })
  
  # Show/hide download button based on tab
  observeEvent(input$housing_tabs, {
    if (input$housing_tabs == "Comparativa") {
      shinyjs::hide("download_house_satis_map")
    } else {
      shinyjs::show("download_house_satis_map")
    }
  }, ignoreInit = FALSE)
  
  # Download handler using pre-saved PNG files
  output$download_house_satis_map <- downloadHandler(
    filename = function() {
      map_type <- switch(input$housing_tabs,
        "Calidad de Materiales" = "Materiales",
        "Tamaño y Espacios" = "Espacios",
        "Ubicación y Accesibilidad" = "Ubicación",
        "Materiales"
      )
      
      paste("mapa_vivienda_", map_type, "_", selectedYear(), "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Determine map name based on active tab
      map_name <- switch(input$housing_tabs,
        "Calidad de Materiales" = "mapa_vivienda_Materiales",
        "Tamaño y Espacios" = "mapa_vivienda_Espacios",
        "Ubicación y Accesibilidad" = "mapa_vivienda_Ubicación",
        "mapa_vivienda_Materiales"
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