# government_server.R
governmentServer <- function(input, output, session,current_theme = NULL) {
  selectedYear <- session$userData$selectedYear
  # Load survey data with dynamic year
  perception_data <- session$userData$perSurveyData
  participation_data <- session$userData$parSurveyData
  
  # Use the current theme
  active_theme <- reactive({
    if (is.function(current_theme)) {
      # If current_theme is a reactive function, call it to get the value
      current_theme()
    } else if (!is.null(current_theme)) {
      # If it's a direct value, use it
      current_theme
    } else {
      # Default to gobierno theme if nothing provided
      get_section_theme("gobierno")
    }
  })
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

  observeEvent(session$clientData$url_protocol, {
    initial_tooltip <- "<b>ID</b>: PAR Q5 <br>
            <b>Pregunta</b>: ¿Conoce o puede mencionar el nombre de los actuales regidores? <br>
             <b>Escala</b>:  1=Sí puede mencionar por lo menos 1 regidor; 2=No conoce ningún regidor"
    
    update_tooltip_content(session, "knowledge_pub_tooltip", initial_tooltip)
  }, once = TRUE)  	  


  # Card 1: Knowledge of Officials Plots
  output$officials_knowledge_regidor_plot <- renderPlotly({
    req(participation_data())
    create_officials_knowledge_pie(participation_data()$responses, "Q5", "Regidor/a", active_theme()) %>% apply_plotly_theme()
  })
  
  output$officials_knowledge_sindico_plot <- renderPlotly({
    req(participation_data())
    create_officials_knowledge_pie(participation_data()$responses, "Q7", "Síndico/a", active_theme())%>% apply_plotly_theme()
  })
  output$officials_knowledge_dipupadol_plot <- renderPlotly({
    req(participation_data())
    create_officials_knowledge_pie(participation_data()$responses, "Q8", "Diputado/a Local y/o Estatal", active_theme()) %>% apply_plotly_theme()
  })
  
  output$officials_knowledge_diputadof_plot <- renderPlotly({
    req(participation_data())
    create_officials_knowledge_pie(participation_data()$responses, "Q10", "Diputado/a Federal", active_theme())%>% apply_plotly_theme()
  })
  # Card 2: Inequality Perception Plot
  output$inequality_perception_plot <- renderPlotly({
    req(perception_data())
    create_inequality_perception_pie(perception_data()$responses, active_theme())%>% apply_plotly_theme()
  })
  
  # Card 3: Government Expectations Plot
  output$government_expectations_plot <- renderPlotly({
    req(participation_data())
    create_government_expectations_plot(participation_data()$responses, active_theme())%>% apply_plotly_theme()
  })
  
  # Card 4: Important Problems Plot
  output$important_problems_plot <- renderPlotly({
    req(perception_data())
    create_important_problems_plot(perception_data()$responses, active_theme())%>% apply_plotly_theme()
  })

}