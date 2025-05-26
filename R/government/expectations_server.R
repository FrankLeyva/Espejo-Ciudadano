# expectations_server.R - Simplified with Data Manager Integration

expectationsServer <- function(input, output, session, current_theme = NULL) {
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
      get_section_theme("gobierno")
    }
  })
  
  # Load pre-saved plots
  plots <- reactive({
    req(selectedYear())
    data_manager$get_plots("expectations", selectedYear())
  })
  
  # Load pre-saved maps
  maps <- reactive({
    req(selectedYear())
    data_manager$get_maps("expectations", selectedYear())
  })
  
  # Load pre-calculated percentages
  percentages <- reactive({
    req(selectedYear())
    data_manager$get_percentages("expectations", selectedYear())
  })
  
  # Update tooltip content based on selected tab
  observe({
    req(input$expectations_tabs)
    
    active_tab <- input$expectations_tabs
    
    tooltip_content <- switch(active_tab,
      "Gobierno Municipal" = "<b>ID</b>: PAR Q19  <br>
              <b>Pregunta</b>: Como calificaria la expectativa que tiene en este momento del gobierno Municipal ?<br>
               <b>Escala</b>: 1-10",
      "Gobierno Estatal" = "<b>ID</b>: PAR Q20  <br>
              <b>Pregunta</b>: Como calificaria la expectativa que tiene en este momento del gobierno Estatal ?<br>
               <b>Escala</b>: 1-10",
      "Gobierno Federal" = "<b>ID</b>: PAR Q21  <br>
              <b>Pregunta</b>: Como calificaria la expectativa que tiene en este momento del gobierno Federal ?<br>
               <b>Escala</b>: 1-10",
      "<b>ID</b>: PAR Q19  <br>
              <b>Pregunta</b>: Como calificaria la expectativa que tiene en este momento del gobierno Municipal ?<br>
               <b>Escala</b>: 1-10"
    )
    
    update_tooltip_content(session, "expectations_tooltip", tooltip_content)
  })

  # Set initial tooltip for expectations
  observeEvent(session$clientData$url_protocol, {
    initial_tooltip <- "<b>ID</b>: PAR Q19  <br>
              <b>Pregunta</b>: Como calificaria la expectativa que tiene en este momento del gobierno Municipal ?<br>
               <b>Escala</b>: 1-10"
    
    update_tooltip_content(session, "expectations_tooltip", initial_tooltip)
  }, once = TRUE)
  
  observe({
    req(input$perception_tabs)
    
    active_tab <- input$perception_tabs
    
    tooltip_content <- switch(active_tab,
      "Toma en cuenta a ciudadanos" = "<b>ID</b>: PAR Q15.2 Q16.2 Q17.2  <br>
              <b>Pregunta</b>: Tomó en cuenta a los ciudadanos (Municipal/Estatal/Federal) <br>
               <b>Escala</b>: 1=Nunca (nunca lo hace); 2=Poco (con poca frecuencia); 3=Algo (con algo de frecuencia); 4=Mucho (con mucha frecuencia); 5=NS/NC",
      "Cumple compromisos y metas" = "<b>ID</b>: PAR Q15.3 Q16.3 Q17.3  <br>
              <b>Pregunta</b>: 	Cumplio con sus compromisos y promesas (Municipal/Estatal/Federal) <br>
               <b>Escala</b>: 1=Nunca (nunca lo hace); 2=Poco (con poca frecuencia); 3=Algo (con algo de frecuencia); 4=Mucho (con mucha frecuencia); 5=NS/NC",
      "Aplica la ley de manera imparcial" = "<b>ID</b>: PAR Q15.4 Q16.4 Q17.4  <br>
              <b>Pregunta</b>: Aplico imparcialmente las leyes (Municipal/Estatal/Federal) <br>
               <b>Escala</b>: 1=Nunca (nunca lo hace); 2=Poco (con poca frecuencia); 3=Algo (con algo de frecuencia); 4=Mucho (con mucha frecuencia); 5=NS/NC",
      "<b>ID</b>: PAR Q15.2 Q16.2 Q17.2  <br>
              <b>Pregunta</b>: Tomó en cuenta a los ciudadanos (Municipal/Estatal/Federal) <br>
               <b>Escala</b>: 1=Nunca (nunca lo hace); 2=Poco (con poca frecuencia); 3=Algo (con algo de frecuencia); 4=Mucho (con mucha frecuencia); 5=NS/NC"
    )
    
    update_tooltip_content(session, "perception_tooltip", tooltip_content)
  })

  # Set initial tooltip for perception
  observeEvent(session$clientData$url_protocol, {
    initial_tooltip <- "<b>ID</b>: PAR Q15.2 Q16.2 Q17.2  <br>
              <b>Pregunta</b>: Tomó en cuenta a los ciudadanos (Municipal/Estatal/Federal) <br>
               <b>Escala</b>: 1=Nunca (nunca lo hace); 2=Poco (con poca frecuencia); 3=Algo (con algo de frecuencia); 4=Mucho (con mucha frecuencia); 5=NS/NC"
    
    update_tooltip_content(session, "perception_tooltip", initial_tooltip)
  }, once = TRUE)
  
  # Render outputs using pre-saved plots
  output$gov_comparison_plot1 <- renderPlotly({
    plots()$gov_comparison_plot1
  })
  
  output$gov_comparison_plot2 <- renderPlotly({
    plots()$gov_comparison_plot2
  })
  
  output$gov_comparison_plot3 <- renderPlotly({
    plots()$gov_comparison_plot3
  })
  
  # Render maps using pre-saved maps
  output$municipal_expectations_map <- renderLeaflet({
    maps()$municipal_expectations_map
  })
  
  output$state_expectations_map <- renderLeaflet({
    maps()$state_expectations_map
  })
  
  output$federal_expectations_map <- renderLeaflet({
    maps()$federal_expectations_map
  })
  
  # Render percentage values for value boxes
  output$municipal_expectation_mean <- renderText({
    percentages()$municipal_expectation_mean
  })
  
  output$state_expectation_mean <- renderText({
    percentages()$state_expectation_mean
  })
  
  output$federal_expectation_mean <- renderText({
    percentages()$federal_expectation_mean
  })
  
  # Download handler for expectations maps using pre-saved PNGs
  output$download_expectations_map <- downloadHandler(
    filename = function() {
      map_type <- if(input$expectations_tabs == "Gobierno Municipal"){ 
                        "Municipal"} else if (input$expectations_tabs == "Gobierno Estatal"){
                          "Estatal" } else {
                          "Federal"
                        }
      paste("mapa_expectativas_gobierno_", map_type, "_", selectedYear(), "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Determine map name based on active tab
      map_name <- if(input$expectations_tabs == "Gobierno Municipal") {
        "mapa_expectativas_gobierno_Municipal"
      } else if(input$expectations_tabs == "Gobierno Estatal") {
        "mapa_expectativas_gobierno_Estatal"
      } else {
        "mapa_expectativas_gobierno_Federal"
      }
      
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