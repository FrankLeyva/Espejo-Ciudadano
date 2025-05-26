# mobility_server.R - Simplified with Data Manager Integration

mobilityServer <- function(input, output, session, current_theme = NULL) {
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
      get_section_theme("movilidad")
    }
  })
  
  # Load pre-saved plots
  plots <- reactive({
    req(selectedYear())
    data_manager$get_plots("mobility", selectedYear())
  })
  
  # Load pre-saved maps
  maps <- reactive({
    req(selectedYear())
    data_manager$get_maps("mobility", selectedYear())
  })
  
  # Load pre-calculated percentages
  percentages <- reactive({
    req(selectedYear())
    data_manager$get_percentages("mobility", selectedYear())
  })
  
  # Update tooltip content based on selected tab
  observe({
    req(input$transport_tabs)
    
    active_tab <- input$transport_tabs
    
    tooltip_content <- switch(active_tab,
      "Transporte al Trabajo" = "<b>ID</b>: PER Q72.1 - Q72.10 <br>
            <b>Pregunta</b>:	Caminando / Bicicleta / Autobús escolar	/ Autobús especial (transporte de personal)	 / Taxi / Uber/Didi/InDriver o cualquier otro servicio por aplicación / Motocicleta / Vehículo propio	/ Camion / Juarez Bus <br>
             <b>Escala</b>:  1=Sí; 2=No	",
      "Transporte General"= "<b>ID</b>: PER Q73.1 - Q73.10 <br>
            <b>Pregunta</b>:	Caminando / Bicicleta / Autobús escolar	/ Autobús especial (transporte de personal)	 / Taxi / Uber/Didi/InDriver o cualquier otro servicio por aplicación / Motocicleta / Vehículo propio	/ Camion / Juarez Bus <br>
             <b>Escala</b>:  1=Sí; 2=No	",
      "<b>ID</b>: PER Q72.1 - Q72.10 <br>
            <b>Pregunta</b>:	Caminando / Bicicleta / Autobús escolar	/ Autobús especial (transporte de personal)	 / Taxi / Uber/Didi/InDriver o cualquier otro servicio por aplicación / Motocicleta / Vehículo propio	/ Camion / Juarez Bus <br>
             <b>Escala</b>:  1=Sí; 2=No	"
    )
    
    update_tooltip_content(session, "transportation_tooltip", tooltip_content)
  })
  
  # Set initial tooltip
  observeEvent(session$clientData$url_protocol, {
    initial_tooltip <- "<b>ID</b>: PER Q72.1 - Q72.10 <br>
            <b>Pregunta</b>:	Caminando / Bicicleta / Autobús escolar	/ Autobús especial (transporte de personal)	 / Taxi / Uber/Didi/InDriver o cualquier otro servicio por aplicación / Motocicleta / Vehículo propio	/ Camion / Juarez Bus <br>
             <b>Escala</b>:  1=Sí; 2=No	"
    
    update_tooltip_content(session, "transportation_tooltip", initial_tooltip)
  }, once = TRUE)
  
  # Render outputs using pre-saved plots
  output$bicycles_pie <- renderPlotly({
    plots()$bicycles_pie
  })
  
  output$vehicles_pie <- renderPlotly({
    plots()$vehicles_pie
  })
  
  output$work_transport_plot <- renderPlotly({
    plots()$work_transport_plot
  })
  
  output$general_transport_plot <- renderPlotly({
    plots()$general_transport_plot
  })
}