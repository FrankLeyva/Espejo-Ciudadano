# identity_server.R - Simplified with Data Manager Integration

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
  
  # Load pre-saved plots
  plots <- reactive({
    req(selectedYear())
    data_manager$get_plots("identity", selectedYear())
  })
  
  # Load pre-saved maps
  maps <- reactive({
    req(selectedYear())
    data_manager$get_maps("identity", selectedYear())
  })
  
  # Load pre-calculated percentages
  percentages <- reactive({
    req(selectedYear())
    data_manager$get_percentages("identity", selectedYear())
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
  
  # Render outputs using pre-saved plots
  output$monuments_bar <- renderPlotly({
    plots()$monuments_bar
  })
  
  output$city_pride_pie <- renderPlotly({
    plots()$city_pride_pie
  })
  
  # Render maps using pre-saved maps
  output$neighborhood_connection_map <- renderLeaflet({
    maps()$neighborhood_connection_map
  })
  
  output$neighbors_connection_map <- renderLeaflet({
    maps()$neighbors_connection_map
  })
  
  # Render percentage values for value boxes
  output$neighborhood_connection_pct <- renderText({
    percentages()$neighborhood_connection_pct
  })
  
  output$neighbors_connection_pct <- renderText({
    percentages()$neighbors_connection_pct
  })
  
  output$city_pride_pct <- renderText({
    percentages()$city_pride_pct
  })
  
  # Download handler using pre-saved PNG files
  output$download_identity_map <- downloadHandler(
    filename = function() {
      paste0(map_name, "_", selectedYear(), "_", Sys.Date(), ".png")
    },
    content = function(file) {
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