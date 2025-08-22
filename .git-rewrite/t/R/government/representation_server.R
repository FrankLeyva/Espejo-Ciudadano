# representation_server.R - Simplified with Data Manager Integration

representationServer <- function(input, output, session, current_theme = NULL) {
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
    data_manager$get_plots("representation", selectedYear())
  })
  
  # Load pre-saved maps
  maps <- reactive({
    req(selectedYear())
    data_manager$get_maps("representation", selectedYear())
  })
  
  # Load pre-calculated percentages
  percentages <- reactive({
    req(selectedYear())
    data_manager$get_percentages("representation", selectedYear())
  })
  
  # Update tooltip content based on selected tab
  observe({
    req(input$knowledge_tabs)
    
    active_tab <- input$knowledge_tabs
    
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
    
    update_tooltip_content(session, "political_knowledge_tooltip", tooltip_content)
  })

  # Set initial tooltip
  observeEvent(session$clientData$url_protocol, {
    initial_tooltip <- "<b>ID</b>: PAR Q5 <br>
        <b>Pregunta</b>: ¿Conoce o puede mencionar el nombre de los actuales regidores? <br>
         <b>Escala</b>:  1=Sí puede mencionar por lo menos 1 regidor; 2=No conoce ningún regidor"
    
    update_tooltip_content(session, "political_knowledge_tooltip", initial_tooltip)
  }, once = TRUE)

  observe({
    req(input$specific_knowledge_tabs)
    
    active_tab <- input$specific_knowledge_tabs
    
    tooltip_content <- switch(active_tab,
      "Regidores" = "<b>ID</b>: PAR Q6.1 - Q6.20 <br>
        <b>Pregunta</b>: Nombres de los regidores en la gráfica <br>
         <b>Escala</b>:  	0=No seleccionado; 1=Seleccionado",
      "Diputados Locales" = "<b>ID</b>: PAR Q8.1 - Q8.10 <br>
        <b>Pregunta</b>:	Nombres de los diputados locales en la gráfica <br>
         <b>Escala</b>:  0=No seleccionado; 1=Seleccionado",
      "Diputados Federales" = "<b>ID</b>: PAR Q10.1 - Q10.4 <br>
        <b>Pregunta</b>:		Nombres de los diputados federales en la gráfica <br>
         <b>Escala</b>:  0=No seleccionado; 1=Seleccionado",
      "<b>ID</b>: PAR Q6.1 - Q6.20 <br>
        <b>Pregunta</b>: Nombres de los regidores en la gráfica <br>
         <b>Escala</b>:  	0=No seleccionado; 1=Seleccionado"
    )
    
    update_tooltip_content(session, "specific_knowledge_tooltip", tooltip_content)
  })

  observeEvent(session$clientData$url_protocol, {
    initial_tooltip <- "<b>ID</b>: PAR Q6.1 - Q6.20 <br>
        <b>Pregunta</b>: Nombres de los regidores en la gráfica <br>
         <b>Escala</b>:  	0=No seleccionado; 1=Seleccionado"
    
    update_tooltip_content(session, "specific_knowledge_tooltip", initial_tooltip)
  }, once = TRUE)

  # Render outputs using pre-saved plots
  output$regidores_knowledge_chart <- renderPlotly({
    plots()$regidores_knowledge_chart
  })
  
  output$diputados_locales_knowledge_chart <- renderPlotly({
    plots()$diputados_locales_knowledge_chart
  })
  
  output$diputados_federales_knowledge_chart <- renderPlotly({
    plots()$diputados_federales_knowledge_chart
  })
  
  # Render maps using pre-saved maps
  output$regidor_knowledge_map <- renderLeaflet({
    maps()$regidor_knowledge_map
  })
  
  output$sindico_knowledge_map <- renderLeaflet({
    maps()$sindico_knowledge_map
  })
    
  output$diputadol_knowledge_map <- renderLeaflet({
    maps()$diputadol_knowledge_map
  })
  
  output$diputadof_knowledge_map <- renderLeaflet({
    maps()$diputadof_knowledge_map
  })
  
  # Render percentage values for value boxes
  output$regidores_rating <- renderText({
    percentages()$regidores_rating
  })
  
  output$sindico_rating <- renderText({
    percentages()$sindico_rating
  })
  
  output$diputado_local_rating <- renderText({
    percentages()$diputado_local_rating
  })
  
  output$diputado_federal_rating <- renderText({
    percentages()$diputado_federal_rating
  })

  # Download handler for political knowledge maps using pre-saved PNGs
  output$download_political_knowledge_map <- downloadHandler(
    filename = function() {
      map_type <- if(input$knowledge_tabs == "Regidor(a)"){ 
                        "Regidor"} else if (input$knowledge_tabs == "Síndico(a)"){
                          "Sindico" } else if (input$knowledge_tabs == "Diputado(a) Local y/o Estatal"){
                          "Diputado_local_estatal" }else {
                          "Diputado_federal"
                        }
      paste("mapa_conocimiento_representantes_", map_type, "_", selectedYear(), "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Determine map name based on active tab
      map_name <- if(input$knowledge_tabs == "Regidor(a)") {
        "mapa_conocimiento_representantes_Regidor"
      } else if(input$knowledge_tabs == "Síndico(a)") {
        "mapa_conocimiento_representantes_Sindico"
      } else if(input$knowledge_tabs == "Diputado(a) Local y/o Estatal") {
        "mapa_conocimiento_representantes_Diputado_local_estatal"
      } else {
        "mapa_conocimiento_representantes_Diputado_federal"
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