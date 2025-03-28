mobilityServer <- function(input, output, session) {
  # Load survey data
  survey_data <- reactive({
    load_survey_data("PER_2024")
  })
  
  # Use the current theme
  current_theme <- reactiveVal(theme_config)
  
  # Create bicycle distribution pie chart
  output$bicycles_pie <- renderPlotly({
    req(survey_data())
    create_bicycle_distribution(survey_data()$responses, current_theme())
  })
  
  # Create vehicle distribution pie chart
  output$vehicles_pie <- renderPlotly({
    req(survey_data())
    create_vehicle_distribution(survey_data()$responses, current_theme())
  })
  
  # Create work transportation mode plot
  output$work_transport_plot <- renderPlotly({
    req(survey_data())
    create_transport_modes_plot(
      survey_data()$responses, 
      mode_type = "work", 
      custom_theme = current_theme()
    )
  })
  
  # Create general transportation mode plot
  output$general_transport_plot <- renderPlotly({
    req(survey_data())
    create_transport_modes_plot(
      survey_data()$responses, 
      mode_type = "general", 
      custom_theme = current_theme()
    )
  })
}