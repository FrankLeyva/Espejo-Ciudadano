# accountability_ui.R - Updated to match new styling system
accountabilityUI <- function() {
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
          h1("Rendición de Cuentas", class = "dashboard-header")
        )
      )
    ),
    
    # Corruption Punishment Tabs
    div(
      class = "plot-tabs-container",
      
      navset_tab(
        id = "punishment_tabs",
        
        # Municipal Government Tab
        nav_panel(
          title = "Castigo Municipal",
          value = "municipal_punishment",
          
          div(
            class = "plot-card plot-card-gobierno",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Castigo a Servidores Públicos Corruptos - Gobierno Municipal",
                  create_tooltip("<b>ID</b>: PAR Q123 <br>
                    <b>Pregunta</b>: ¿Qué tan frecuente considera usted que se castiga a los servidores públicos corruptos en el gobierno municipal? <br>
                     <b>Escala</b>: 1=Siempre; 2=Casi siempre; 3=Casi nunca; 4=Nunca; 5=NS/NC")
                )
              )
            ),
            
            div(
              class = "plot-content",
              plotlyOutput("municipal_punishment_hist", height = "450px")
            )
          )
        ),
        
        # State Government Tab
        nav_panel(
          title = "Castigo Estatal",
          value = "state_punishment",
          
          div(
            class = "plot-card plot-card-gobierno",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Castigo a Servidores Públicos Corruptos - Gobierno Estatal",
                  create_tooltip("<b>ID</b>: PAR Q124 <br>
                    <b>Pregunta</b>: ¿Qué tan frecuente considera usted que se castiga a los servidores públicos corruptos en el gobierno estatal? <br>
                     <b>Escala</b>: 1=Siempre; 2=Casi siempre; 3=Casi nunca; 4=Nunca; 5=NS/NC")
                )
              )
            ),
            
            div(
              class = "plot-content",
              plotlyOutput("state_punishment_hist", height = "450px")
            )
          )
        ),
        
        # Federal Government Tab
        nav_panel(
          title = "Castigo Federal",
          value = "federal_punishment",
          
          div(
            class = "plot-card plot-card-gobierno",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Castigo a Servidores Públicos Corruptos - Gobierno Federal",
                  create_tooltip("<b>ID</b>: PAR Q125 <br>
                    <b>Pregunta</b>: ¿Qué tan frecuente considera usted que se castiga a los servidores públicos corruptos en el gobierno federal? <br>
                     <b>Escala</b>: 1=Siempre; 2=Casi siempre; 3=Casi nunca; 4=Nunca; 5=NS/NC")
                )
              )
            ),
            
            div(
              class = "plot-content",
              plotlyOutput("federal_punishment_hist", height = "450px")
            )
          )
        )
      )
    ),
    
    # Corruption Acts Tabs
    div(
      class = "plot-tabs-container",
      
      navset_tab(
        id = "corruption_tabs",
        
        # Municipal Corruption Tab
        nav_panel(
          title = "Corrupción Municipal",
          value = "municipal_corruption",
          
          div(
            class = "plot-card plot-card-gobierno",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Percepción de Actos de Corrupción - Gobierno Municipal",
                  create_tooltip("<b>ID</b>: PAR Q15.1 <br>
                    <b>Pregunta</b>: ¿Qué tan frecuentes considera usted que son los actos de corrupción en el gobierno municipal? <br>
                     <b>Escala</b>: 1=Muy frecuentes; 2=Frecuentes; 3=Poco frecuentes; 4=Nada frecuentes; 5=NS/NC")
                )
              )
            ),
            
            div(
              class = "plot-content",
              plotlyOutput("municipal_corruption_pie", height = "450px")
            )
          )
        ),
        
        # State Corruption Tab
        nav_panel(
          title = "Corrupción Estatal",
          value = "state_corruption",
          
          div(
            class = "plot-card plot-card-gobierno",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Percepción de Actos de Corrupción - Gobierno Estatal",
                  create_tooltip("<b>ID</b>: PAR Q16.1 <br>
                    <b>Pregunta</b>: ¿Qué tan frecuentes considera usted que son los actos de corrupción en el gobierno estatal? <br>
                     <b>Escala</b>: 1=Muy frecuentes; 2=Frecuentes; 3=Poco frecuentes; 4=Nada frecuentes; 5=NS/NC")
                )
              )
            ),
            
            div(
              class = "plot-content",
              plotlyOutput("state_corruption_pie", height = "450px")
            )
          )
        ),
        
        # Federal Corruption Tab
        nav_panel(
          title = "Corrupción Federal",
          value = "federal_corruption",
          
          div(
            class = "plot-card plot-card-gobierno",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Percepción de Actos de Corrupción - Gobierno Federal",
                  create_tooltip("<b>ID</b>: PAR Q17.1 <br>
                    <b>Pregunta</b>: ¿Qué tan frecuentes considera usted que son los actos de corrupción en el gobierno federal? <br>
                     <b>Escala</b>: 1=Muy frecuentes; 2=Frecuentes; 3=Poco frecuentes; 4=Nada frecuentes; 5=NS/NC")
                )
              )
            ),
            
            div(
              class = "plot-content",
              plotlyOutput("federal_corruption_pie", height = "450px")
            )
          )
        )
      )
    )
  )
}