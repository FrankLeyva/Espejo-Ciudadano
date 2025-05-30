# government_server.R - Simplified with Data Manager Integration

governmentServer <- function(input, output, session, current_theme = NULL) {
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
    data_manager$get_plots("government", selectedYear())
  })
  
 
  
  # Update tooltip content based on selected tab
  observe({
    req(input$env_satisfaction_tabs)
    
    active_tab <- input$env_satisfaction_tabs
    
    tooltip_content <- switch(active_tab,
      "Regidor/a" = "<b>ID</b>: PAR Q5 <br>
            <b>Pregunta</b>: ¿Conoce o puede mencionar el nombre de los actuales regidores? <br>
             <b>Escala</b>:  1=Sí puede mencionar por lo menos 1 regidor; 2=No conoce ningún regidor",
      "Síndico/a" = "<b>ID</b>: PAR Q7 <br>
            <b>Pregunta</b>:	¿Puede decirme el nombre del síndico o síndica municipal? <br>
             <b>Escala</b>:  1=No conoce el nombre del/la síndico(a); 2=Sí conoce",
      "Diputado/a Local y/o Estatal" = "<b>ID</b>: PAR Q8 <br>
            <b>Pregunta</b>:		Conoce o puede nombrar algun diputado local/ estatal de su distrito (Computada) <br>
             <b>Escala</b>:  1 = No conoce el nombre de algun diputado local; 2 = Si conoce algun diputado local",
      "Diputado/a Federal" = "<b>ID</b>: PAR Q9 <br>
            <b>Pregunta</b>:		¿Puede decirme el nombre del (la) diputado(a) federal de su distrito? NO AYUDAR CON NOMBRES <br>
             <b>Escala</b>:  	1=Sí conoce diputado(a) federal; 2=No conoce diputado(a) federal",
      "<b>ID</b>: PAR Q5 <br>
            <b>Pregunta</b>: ¿Conoce o puede mencionar el nombre de los actuales regidores? <br>
             <b>Escala</b>:  1=Sí puede mencionar por lo menos 1 regidor; 2=No conoce ningún regidor"
    )
    
    update_tooltip_content(session, "knowledge_pub_tooltip", tooltip_content)
  })
  
  # Set initial tooltip
  observeEvent(session$clientData$url_protocol, {
    initial_tooltip <- "<b>ID</b>: PAR Q5 <br>
            <b>Pregunta</b>: ¿Conoce o puede mencionar el nombre de los actuales regidores? <br>
             <b>Escala</b>:  1=Sí puede mencionar por lo menos 1 regidor; 2=No conoce ningún regidor"
    
    update_tooltip_content(session, "knowledge_pub_tooltip", initial_tooltip)
  }, once = TRUE)
  
  # Render outputs using pre-saved plots
  output$officials_knowledge_regidor_plot <- renderPlotly({
    plots()$officials_knowledge_regidor
  })
  
  output$officials_knowledge_sindico_plot <- renderPlotly({
    plots()$officials_knowledge_sindico
  })
  
  output$officials_knowledge_dipupadol_plot <- renderPlotly({
    plots()$officials_knowledge_dipupadol
  })
  
  output$officials_knowledge_diputadof_plot <- renderPlotly({
    plots()$officials_knowledge_diputadof
  })
  
  output$inequality_perception_plot <- renderPlotly({
    plots()$inequality_perception
  })
  
  output$government_expectations_plot <- renderPlotly({
    plots()$government_expectations
  })
  
  output$important_problems_plot <- renderPlotly({
    plots()$important_problems
  })
  
}