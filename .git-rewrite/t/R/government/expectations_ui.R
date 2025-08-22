# expectations_ui.R - Updated to match new styling system
expectationsUI <- function() {
  page_fluid(
    class = "section-gobierno",
    useShinyjs(),
    init_tooltips(),

    # Back navigation
    div(
      class = "mb-4",
      tags$a(
        href = "#",
        class = "text-decoration-none",
        onclick = "Shiny.setInputValue('nav_target', 'government', {priority: 'event'}); return false;",
        tags$i(class = "fas fa-arrow-left me-2"),
        "Volver a Instituciones"
      )
    ),
    
    # Header
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          style = paste0("background-color: var(--gobierno-color) !important; 
            color: white !important; 
            font-family: var(--font-display) !important;
            font-weight: bolder !important; 
            text-align: center !important; 
            border-bottom: none !important;"),
          h1("Expectativas de Gobierno", class = "dashboard-header")
        )
      )
    ),
    
    # Content Section 1: Government Expectations Maps
    div(
      class = "plot-tabs-container",
      
      navset_tab(
        id = "expectations_tabs",
        
        # Municipal Government Tab
        nav_panel(
          title = "Expectativas del Gobierno Municipal",
          value = "municipal_expectations",
          
          div(
            class = "plot-card plot-card-gobierno",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Expectativas Ciudadanas del Gobierno Municipal por Distrito",
                  create_tooltip("<b>ID</b>: PAR Q19 <br>
                    <b>Pregunta</b>: ¿Cómo calificaría la expectativa que tiene en este momento del gobierno Municipal? <br>
                     <b>Escala</b>: 1-10")
                )
              ),
              div(
                class = "plot-actions",
                downloadButton(
                  "download_municipal_expectations_map", 
                  "", 
                  icon = icon("download"), 
                  class = "plot-action-btn",
                  title = "Descargar mapa"
                )
              )
            ),
            
            div(
              class = "plot-content",
              leafletOutput("municipal_expectations_map", height = "500px")
            )
          )
        ),
        
        # State Government Tab
        nav_panel(
          title = "Expectativas del Gobierno Estatal",
          value = "state_expectations",
          
          div(
            class = "plot-card plot-card-gobierno",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Expectativas Ciudadanas del Gobierno Estatal por Distrito",
                  create_tooltip("<b>ID</b>: PAR Q20 <br>
                    <b>Pregunta</b>: ¿Cómo calificaría la expectativa que tiene en este momento del gobierno Estatal? <br>
                     <b>Escala</b>: 1-10")
                )
              ),
              div(
                class = "plot-actions",
                downloadButton(
                  "download_state_expectations_map", 
                  "", 
                  icon = icon("download"), 
                  class = "plot-action-btn",
                  title = "Descargar mapa"
                )
              )
            ),
            
            div(
              class = "plot-content",
              leafletOutput("state_expectations_map", height = "500px")
            )
          )
        ),
        
        # Federal Government Tab
        nav_panel(
          title = "Expectativas del Gobierno Federal",
          value = "federal_expectations",
          
          div(
            class = "plot-card plot-card-gobierno",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Expectativas Ciudadanas del Gobierno Federal por Distrito",
                  create_tooltip("<b>ID</b>: PAR Q21 <br>
                    <b>Pregunta</b>: ¿Cómo calificaría la expectativa que tiene en este momento del gobierno Federal? <br>
                     <b>Escala</b>: 1-10")
                )
              ),
              div(
                class = "plot-actions",
                downloadButton(
                  "download_federal_expectations_map", 
                  "", 
                  icon = icon("download"), 
                  class = "plot-action-btn",
                  title = "Descargar mapa"
                )
              )
            ),
            
            div(
              class = "plot-content",
              leafletOutput("federal_expectations_map", height = "500px")
            )
          )
        )
      )
    ),
    
    # Content Section 2: Government Perception Comparison
    div(
      class = "plot-tabs-container",
      
      navset_tab(
        id = "perception_tabs",
        
        # Citizens Consideration Tab
        nav_panel(
          title = "Toma en Cuenta a Ciudadanos",
          value = "citizens_consideration",
          
          div(
            class = "plot-card plot-card-gobierno",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Percepción: El Gobierno Toma en Cuenta a los Ciudadanos",
                  create_tooltip("<b>Descripción</b>: Comparación de la percepción ciudadana sobre qué tanto cada nivel de gobierno toma en cuenta las opiniones y necesidades de los ciudadanos")
                )
              )
            ),
            
            div(
              class = "plot-content",
              plotlyOutput("gov_comparison_plot1", height = "500px")
            )
          )
        ),
        
        # Commitments Fulfillment Tab
        nav_panel(
          title = "Cumple Compromisos y Metas",
          value = "commitments_fulfillment",
          
          div(
            class = "plot-card plot-card-gobierno",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Percepción: El Gobierno Cumple Compromisos y Metas",
                  create_tooltip("<b>Descripción</b>: Comparación de la percepción ciudadana sobre qué tanto cada nivel de gobierno cumple con sus compromisos y metas establecidas")
                )
              )
            ),
            
            div(
              class = "plot-content",
              plotlyOutput("gov_comparison_plot2", height = "500px")
            )
          )
        ),
        
        # Law Application Tab
        nav_panel(
          title = "Aplica la Ley de Manera Imparcial",
          value = "law_application",
          
          div(
            class = "plot-card plot-card-gobierno",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Percepción: El Gobierno Aplica la Ley de Manera Imparcial",
                  create_tooltip("<b>Descripción</b>: Comparación de la percepción ciudadana sobre qué tanto cada nivel de gobierno aplica las leyes de manera justa e imparcial")
                )
              )
            ),
            
            div(
              class = "plot-content",
              plotlyOutput("gov_comparison_plot3", height = "500px")
            )
          )
        )
      )
    )
  )
}