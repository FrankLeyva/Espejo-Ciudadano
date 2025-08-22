# transportation_ui.R - Updated to follow new styling patterns
transportationUI <- function() {
  page_fluid(
    class = "section-movilidad", 
    useShinyjs(),
    init_tooltips(),

    # Back navigation
    div(
      class = "mb-4",
      tags$a(
        href = "#",
        class = "text-decoration-none",
        onclick = "Shiny.setInputValue('nav_target', 'urban', {priority: 'event'}); return false;",
        tags$i(class = "fas fa-arrow-left me-2"),
        "Volver a Movilidad Urbana"
      )
    ),
    
    # Header
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          style = paste0("background-color: var(--movilidad-color) !important; 
            color: white !important; 
            font-family: var(--font-display) !important;
            font-weight: bolder !important; 
            text-align: center !important; 
            border-bottom: none !important;"),
          h1("Transporte Público", class = "dashboard-header")
        )
      )
    ),
    
    # Tab Navigation with Plot Cards (Transportation Satisfaction)
    div(
      class = "plot-tabs-container",
      
      navset_tab(
        id = "transport_satisfaction_tabs",
        
        # Bus/Rutera Tab
        nav_panel(
          title = "Camión/Rutera",
          value = "bus_satisfaction",
          
          div(
            class = "plot-card plot-card-movilidad",
            
            # Plot Header
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Satisfacción con el Servicio de Camión/Rutera por Distrito",
                  create_tooltip("<b>ID</b>: PER Q75 <br>
                    <b>Pregunta</b>: En una escala del 1 al 10, que tan satisfecho está con la calidad del servicio del camión/rutera? <br>
                     <b>Escala</b>: 1-10 (1=Muy insatisfecho, 10=Muy satisfecho)")
                )
              ),
              div(
                class = "plot-actions",
                downloadButton(
                  "download_bus_satisfaction_map", 
                  "", 
                  icon = icon("download"), 
                  class = "plot-action-btn",
                  title = "Descargar mapa"
                )
              )
            ),
            
            # Plot Content
            div(
              class = "plot-content",
              leafletOutput("bus_satisfaction_map", height = "500px")
            )
          )
        ),
        
        # Juarez Bus Tab
        nav_panel(
          title = "Juárez Bus",
          value = "juarez_bus_satisfaction",
          
          div(
            class = "plot-card plot-card-movilidad",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Satisfacción con el Servicio de Juárez Bus por Distrito",
                  create_tooltip("<b>ID</b>: PER Q78 <br>
                    <b>Pregunta</b>: En una escala del 1 al 10, que tan satisfecho está con la calidad del servicio del BravoBus/EcoBus/ViveBus/Juarez Bus? <br>
                     <b>Escala</b>: 1-10 (1=Muy insatisfecho, 10=Muy satisfecho)")
                )
              ),
              div(
                class = "plot-actions",
                downloadButton(
                  "download_juarez_bus_satisfaction_map", 
                  "", 
                  icon = icon("download"), 
                  class = "plot-action-btn",
                  title = "Descargar mapa"
                )
              )
            ),
            
            div(
              class = "plot-content",
              leafletOutput("juarez_bus_satisfaction_map", height = "500px")
            )
          )
        )
      )
    ),
    
    # Second Tab Navigation with Plot Cards (Service Issues)
    div(
      class = "plot-tabs-container",
      
      navset_tab(
        id = "service_issues_tabs",
        
        # Bus Issues Tab
        nav_panel(
          title = "Problemas con Camión/Rutera",
          value = "bus_issues",
          
          div(
            class = "plot-card plot-card-movilidad",
            
            # Plot Header
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Aspectos con los que NO están satisfechos - Camión/Rutera",
                  create_tooltip("<b>ID</b>: PER Q76 <br>
                    <b>Pregunta</b>: ¿Con cuáles de los siguientes aspectos del servicio del camión/rutera NO está satisfecho? <br>
                     <b>Escala</b>: Aspectos múltiples de insatisfacción")
                )
              )
            ),
            
            # Plot Content
            div(
              class = "plot-content",
              plotlyOutput("bus_issues_plot", height = "500px")
            )
          )
        ),
        
        # Juarez Bus Issues Tab
        nav_panel(
          title = "Problemas con Juárez Bus",
          value = "juarez_bus_issues",
          
          div(
            class = "plot-card plot-card-movilidad",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Aspectos con los que NO están satisfechos - Juárez Bus",
                  create_tooltip("<b>ID</b>: PER Q79 <br>
                    <b>Pregunta</b>: ¿Con cuáles de los siguientes aspectos del servicio del BravoBus/EcoBus/ViveBus/Juarez Bus NO está satisfecho? <br>
                     <b>Escala</b>: Aspectos múltiples de insatisfacción")
                )
              )
            ),
            
            div(
              class = "plot-content",
              plotlyOutput("juarez_bus_issues_plot", height = "500px")
            )
          )
        )
      )
    )
  )
}