mobilityServer <- function(input, output, session,current_theme = NULL) {
  # Get the selected year from userData
  selectedYear <- session$userData$selectedYear
  
  survey_data <- session$userData$perSurveyData
  
  
  active_theme <- reactive({
    if (is.function(current_theme)) {
      # If current_theme is a reactive function, call it to get the value
      current_theme()
    } else if (!is.null(current_theme)) {
      # If it's a direct value, use it
      current_theme
    } else {
      # Default to movilidad theme if nothing provided
      get_section_theme("movilidad")
    }
  })
  
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
      "Transporte al Trabajo" = "<b>ID</b>: PER Q72.1 - Q72.10 <br>
            <b>Pregunta</b>:	Caminando / Bicicleta / Autobús escolar	/ Autobús especial (transporte de personal)	 / Taxi / Uber/Didi/InDriver o cualquier otro servicio por aplicación / Motocicleta / Vehículo propio	/ Camion / Juarez Bus <br>
             <b>Escala</b>:  1=Sí; 2=No	"
    )
    
    update_tooltip_content(session, "transportation_tooltip", tooltip_content)
  })

  observeEvent(session$clientData$url_protocol, {
    initial_tooltip <- "<b>ID</b>: PER Q89 <br>
            <b>Pregunta</b>:	¿Qué tan satisfecho está con LA CALIDAD DEL AIRE? <br>
             <b>Escala</b>:  1-10"
    
    update_tooltip_content(session, "transportation_tooltip", initial_tooltip)
  }, once = TRUE)  




  # Create bicycle distribution pie chart
  output$bicycles_pie <- renderPlotly({
    req(survey_data())
    create_bicycle_distribution(survey_data()$responses, active_theme())%>% apply_plotly_theme()
  })
  
  # Create vehicle distribution pie chart
  output$vehicles_pie <- renderPlotly({
    req(survey_data())
    create_vehicle_distribution(survey_data()$responses, active_theme())%>% apply_plotly_theme()
  })
  
  # Create work transportation mode plot
  output$work_transport_plot <- renderPlotly({
    req(survey_data())
    create_transport_modes_plot(
      survey_data()$responses, 
      mode_type = "work", 
      custom_theme = active_theme()
    ) 
  })
  
  # Create general transportation mode plot
  output$general_transport_plot <- renderPlotly({
    req(survey_data())
    create_transport_modes_plot(
      survey_data()$responses, 
      mode_type = "general", 
      custom_theme = active_theme()
    )
  })
}