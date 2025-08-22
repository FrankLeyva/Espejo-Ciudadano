# trust_ui.R - Updated to match new styling system
trustUI <- function() {
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
          h1("Confianza Institucional", class = "dashboard-header")
        )
      )
    ),
    
    # Content Section: Trust in Different Institution Types
    div(
      class = "plot-cards-grid single-column",
      
      # Popular Election Institutions Plot Card
      div(
        class = "plot-card plot-card-gobierno",
        
        div(
          class = "plot-header",
          div(
            class = "plot-header-content",
            h6(
              class = "plot-title",
              "Confianza en Instituciones de Elección Popular",
              create_tooltip("<b>ID</b>: PAR Q21.1, PAR Q21.17 - PAR Q21.24 <br>
                <b>Pregunta</b>: ¿Qué tanta confianza tiene usted en las siguientes instituciones de elección popular? <br>
                 <b>Escala</b>: 1=Mucha desconfianza; 2=Algo de desconfianza; 3=Algo de confianza; 4=Mucha confianza; 5=NS/NC")
            )
          )
        ),
        
        div(
          class = "plot-content",
          plotlyOutput("popular_election_institutions_plot", height = "500px")
        )
      ),
      
      # Public Institutions and Media Plot Card
      div(
        class = "plot-card plot-card-gobierno",
        
        div(
          class = "plot-header",
          div(
            class = "plot-header-content",
            h6(
              class = "plot-title",
              "Confianza en Instituciones Públicas y Medios de Comunicación",
              create_tooltip("<b>ID</b>: PAR Q21.2 - PAR Q21.9 <br>
                <b>Pregunta</b>: ¿Qué tanta confianza tiene usted en las siguientes instituciones públicas y medios de comunicación? <br>
                 <b>Escala</b>: 1=Mucha desconfianza; 2=Algo de desconfianza; 3=Algo de confianza; 4=Mucha confianza; 5=NS/NC")
            )
          )
        ),
        
        div(
          class = "plot-content",
          plotlyOutput("public_institutions_media_plot", height = "500px")
        )
      ),
      
      # Public Safety Institutions Plot Card
      div(
        class = "plot-card plot-card-gobierno",
        
        div(
          class = "plot-header",
          div(
            class = "plot-header-content",
            h6(
              class = "plot-title",
              "Confianza en Instituciones de Seguridad Pública",
              create_tooltip("<b>ID</b>: PAR Q21.10 - PAR Q21.16 <br>
                <b>Pregunta</b>: ¿Qué tanta confianza tiene usted en las siguientes instituciones de seguridad pública? <br>
                 <b>Escala</b>: 1=Mucha desconfianza; 2=Algo de desconfianza; 3=Algo de confianza; 4=Mucha confianza; 5=NS/NC")
            )
          )
        ),
        
        div(
          class = "plot-content",
          plotlyOutput("public_safety_institutions_plot", height = "500px")
        )
      )
    )
  )
}