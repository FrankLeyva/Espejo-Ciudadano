mobilityServer <- function(input, output, session,current_theme = NULL) {
  # Get the selected year from userData
  selectedYear <- session$userData$selectedYear
  
  # Load survey data with dynamic year
  survey_data <- reactive({
    survey_id <- paste0("PER_", selectedYear())
    load_survey_data(survey_id)
  })
  
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