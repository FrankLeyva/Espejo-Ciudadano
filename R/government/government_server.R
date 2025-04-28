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