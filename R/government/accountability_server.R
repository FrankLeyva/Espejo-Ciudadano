# accountability_server.R - Simplified with Data Manager Integration

accountabilityServer <- function(input, output, session, current_theme = NULL) {
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
    data_manager$get_plots("accountability", selectedYear())
  })
  
  # Load pre-saved maps (if any)
  maps <- reactive({
    req(selectedYear())
    data_manager$get_maps("accountability", selectedYear())
  })
  
  # Load pre-calculated percentages
  percentages <- reactive({
    req(selectedYear())
    data_manager$get_percentages("accountability", selectedYear())
  })
  
  # Update tooltip content based on selected tab
  observe({
    req(input$punishment_tabs)
    
    active_tab <- input$punishment_tabs
    
    tooltip_content <- switch(active_tab,
      "Gobierno Municipal" = "<b>ID</b>: PAR Q123 <br>
            <b>Pregunta</b>:		¿Usted cree que el GOBIERNO MUNICIPAL sanciona a los servidores públicos que son sorprendidos en actos de corrupción? <br>
             <b>Escala</b>:  1=Nunca (nunca lo hacen); 2=Poco (con poca frecuencia); 3=Algo (con algo de frecuencia); 4=Mucho (con mucha frecuencia); 5=NS/NC",
      "Gobierno Estatal" = "<b>ID</b>: PAR Q124 <br>
            <b>Pregunta</b>:	¿Usted cree que el GOBIERNO ESTATAL  sanciona a los servidores públicos que son sorprendidos en actos de corrupción? <br>
             <b>Escala</b>:  1=Nunca (nunca lo hacen); 2=Poco (con poca frecuencia); 3=Algo (con algo de frecuencia); 4=Mucho (con mucha frecuencia); 5=NS/NC",
      "Gobierno Federal" = "<b>ID</b>: PAR Q124 <br>
            <b>Pregunta</b>:	¿Usted cree que el GOBIERNO FEDERAL sanciona a los servidores públicos que son sorprendidos en actos de corrupción? <br>
             <b>Escala</b>:  1=Nunca (nunca lo hacen); 2=Poco (con poca frecuencia); 3=Algo (con algo de frecuencia); 4=Mucho (con mucha frecuencia); 5=NS/NC",
      "<b>ID</b>: PAR Q123 <br>
            <b>Pregunta</b>:		¿Usted cree que el GOBIERNO MUNICIPAL sanciona a los servidores públicos que son sorprendidos en actos de corrupción? <br>
             <b>Escala</b>:  1=Nunca (nunca lo hacen); 2=Poco (con poca frecuencia); 3=Algo (con algo de frecuencia); 4=Mucho (con mucha frecuencia); 5=NS/NC"
    )
    
    update_tooltip_content(session, "punishment_tooltip", tooltip_content)
  })

  # Set initial tooltip for punishment
  observeEvent(session$clientData$url_protocol, {
    initial_tooltip <- "<b>ID</b>: PAR Q123 <br>
            <b>Pregunta</b>:		¿Usted cree que el GOBIERNO MUNICIPAL sanciona a los servidores públicos que son sorprendidos en actos de corrupción? <br>
             <b>Escala</b>:  1=Nunca (nunca lo hacen); 2=Poco (con poca frecuencia); 3=Algo (con algo de frecuencia); 4=Mucho (con mucha frecuencia); 5=NS/NC"
    
    update_tooltip_content(session, "punishment_tooltip", initial_tooltip)
  }, once = TRUE)

  observe({
    req(input$corruption_tabs)
    
    active_tab <- input$corruption_tabs
    
    tooltip_content <- switch(active_tab,
      "Gobierno Municipal" = "<b>ID</b>: PAR Q15.1 <br>
            <b>Pregunta</b>:		Llevó a cabo actos de corrupcion <br>
             <b>Escala</b>:  1=Nunca (nunca lo hacen); 2=Poco (con poca frecuencia); 3=Algo (con algo de frecuencia); 4=Mucho (con mucha frecuencia); 5=NS/NC",
      "Gobierno Estatal" = "<b>ID</b>: PAR Q15.2 <br>
            <b>Pregunta</b>:	Llevó a cabo actos de corrupcion <br>
             <b>Escala</b>:  1=Nunca (nunca lo hacen); 2=Poco (con poca frecuencia); 3=Algo (con algo de frecuencia); 4=Mucho (con mucha frecuencia); 5=NS/NC",
      "Gobierno Federal" = "<b>ID</b>: PAR Q15.3 <br>
            <b>Pregunta</b>:	Llevó a cabo actos de corrupcion <br>
             <b>Escala</b>:  1=Nunca (nunca lo hacen); 2=Poco (con poca frecuencia); 3=Algo (con algo de frecuencia); 4=Mucho (con mucha frecuencia); 5=NS/NC",
      "<b>ID</b>: PAR Q15.1 <br>
            <b>Pregunta</b>:		Llevó a cabo actos de corrupcion <br>
             <b>Escala</b>:  1=Nunca (nunca lo hacen); 2=Poco (con poca frecuencia); 3=Algo (con algo de frecuencia); 4=Mucho (con mucha frecuencia); 5=NS/NC"
    )
    
    update_tooltip_content(session, "corruption_tooltip", tooltip_content)
  })

  # Set initial tooltip for corruption
  observeEvent(session$clientData$url_protocol, {
    initial_tooltip <- "<b>ID</b>: PAR Q15.1 <br>
            <b>Pregunta</b>:		Llevó a cabo actos de corrupcion <br>
             <b>Escala</b>:  1=Nunca (nunca lo hacen); 2=Poco (con poca frecuencia); 3=Algo (con algo de frecuencia); 4=Mucho (con mucha frecuencia); 5=NS/NC"
    
    update_tooltip_content(session, "corruption_tooltip", initial_tooltip)
  }, once = TRUE)

  # Render outputs using pre-saved plots
  output$municipal_punishment_hist <- renderPlotly({
    plots()$municipal_punishment_hist
  })
  
  output$state_punishment_hist <- renderPlotly({
    plots()$state_punishment_hist
  })
  
  output$federal_punishment_hist <- renderPlotly({
    plots()$federal_punishment_hist
  })
  
  output$municipal_corruption_pie <- renderPlotly({
    plots()$municipal_corruption_pie
  })
  
  output$state_corruption_pie <- renderPlotly({
    plots()$state_corruption_pie
  })
  
  output$federal_corruption_pie <- renderPlotly({
    plots()$federal_corruption_pie
  })
  
  # Render percentage values for value boxes
  output$justice_perception <- renderText({
    percentages()$justice_perception
  })
}