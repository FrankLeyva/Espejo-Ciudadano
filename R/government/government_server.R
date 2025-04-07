# government_server.R
governmentServer <- function(input, output, session,current_theme = NULL) {
  selectedYear <- session$userData$selectedYear

  # Load survey data with dynamic year
  perception_data <- reactive({
    survey_id <- paste0("PER_", selectedYear())
    load_survey_data(survey_id)
  })
  participation_data <- reactive({
    survey_id <- paste0("PAR_", selectedYear())
    load_survey_data(survey_id)
  })
  
  # Use the current theme
  current_theme <- reactiveVal(theme_config)
  
  # Card 1: Knowledge of Officials Plots
  output$officials_knowledge_regidor_plot <- renderPlotly({
    req(participation_data())
    create_officials_knowledge_pie(participation_data()$responses, "Q5", "Regidor/a", current_theme())
  })
  
  output$officials_knowledge_sindico_plot <- renderPlotly({
    req(participation_data())
    create_officials_knowledge_pie(participation_data()$responses, "Q7", "Síndico/a", current_theme())
  })
  output$officials_knowledge_dipupadol_plot <- renderPlotly({
    req(participation_data())
    create_officials_knowledge_pie(participation_data()$responses, "Q8", "Síndico/a", current_theme())
  })
  
  output$officials_knowledge_diputadof_plot <- renderPlotly({
    req(participation_data())
    create_officials_knowledge_pie(participation_data()$responses, "Q10", "Diputado/a Federal", current_theme())
  })
  
  # Card 2: Inequality Perception Plot
  output$inequality_perception_plot <- renderPlotly({
    req(perception_data())
    create_inequality_perception_pie(perception_data()$responses, current_theme())
  })
  
  # Card 3: Government Expectations Plot
  output$government_expectations_plot <- renderPlotly({
    req(participation_data())
    create_government_expectations_plot(participation_data()$responses, current_theme())
  })
  
  # Card 4: Important Problems Plot
  output$important_problems_plot <- renderPlotly({
    req(perception_data())
    create_important_problems_plot(perception_data()$responses, current_theme())
  })

}