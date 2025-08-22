# civic_ui.R - Updated to match style guide and modern plot card pattern

civicUI <- function() {
  page_fluid(
    class = "section-participacion",
    useShinyjs(),
    init_tooltips(),

    # Back navigation
    div(
      class = "mb-4",
      tags$a(
        href = "#",
        class = "text-decoration-none",
        onclick = "Shiny.setInputValue('nav_target', 'participation', {priority: 'event'}); return false;",
        tags$i(class = "fas fa-arrow-left me-2"),
        "Volver a Participación Ciudadana"
      )
    ),
    
    # Header
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          style = paste0("background-color: var(--participacion-color) !important; 
            color: white !important; 
            font-family: var(--font-display) !important;
            font-weight: bolder !important; 
            text-align: center !important; 
            border-bottom: none !important;"),
          h1("Participación Cívica", class = "dashboard-header")
        )
      )
    ),
    
    # PLOT CARDS SECTION - Using responsive grid
    
    # Content section 1: Interest map (full width)
    div(
      class = "plot-cards-grid single-column",
      
      # Political Interest Map Plot Card
      div(
        class = "plot-card plot-card-participacion",
        
        # Plot Header with download button
        div(
          class = "plot-header",
          div(
            class = "plot-header-content",
            h6(
              class = "plot-title",
              "Interés en participar en política municipal por distrito",
              create_tooltip("<b>ID</b>: PAR Q130 <br>
                <b>Pregunta</b>: ¿Qué tanto le interesa participar en la política del municipio, interviniendo en decisiones o realizando acciones de interés público en beneficio de la ciudad? <br>
                 <b>Escala</b>: 1=NADA; 2=POCO; 3=REGULAR; 4=ALGO; 5=MUCHO")
            )
          ),
          div(
            class = "plot-actions",
            downloadButton(
              "download_interest_map", 
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
          leafletOutput("interest_map", height = "500px")
        )
      )
    ),
    
    # Content section 2: Requirements and mechanisms charts
    div(
      class = "plot-cards-grid two-columns",
      
      # Participation Requirements Plot Card
      div(
        class = "plot-card plot-card-participacion",
        
        # Plot Header
        div(
          class = "plot-header",
          div(
            class = "plot-header-content",
            h6(
              class = "plot-title",
              "¿Qué necesita para participar en asuntos públicos?",
              create_tooltip("<b>ID</b>: PAR Q131.1 - Q131.5 <br>
                <b>Pregunta</b>: Para que usted pueda participar en asuntos públicos, ¿necesita?: Información / Espacios para participar / Tiempo / Dinero / Confianza en las instituciones <br>
                 <b>Escala</b>: 0=No necesario; 1=Necesario")
            )
          )
        ),
        
        # Plot Content
        div(
          class = "plot-content",
          plotlyOutput("requirements_plot", height = "450px")
        )
      ),
      
      # Participation Mechanisms Plot Card
      div(
        class = "plot-card plot-card-participacion",
        
        # Plot Header
        div(
          class = "plot-header",
          div(
            class = "plot-header-content",
            h6(
              class = "plot-title",
              "Conocimiento de mecanismos de participación ciudadana",
              create_tooltip("<b>ID</b>: PAR Q138.1 - Q138.13 <br>
                <b>Pregunta</b>: ¿Conoce usted los siguientes mecanismos de participación ciudadana?: Presupuesto participativo, Audiencias públicas, Consultas ciudadanas, Consejos ciudadanos, Comités de vecinos, Referéndum, Plebiscito, Iniciativa ciudadana, Revocación de mandato, Contraloría social, Sesiones abiertas de cabildo, Acceso a la información pública, Rendición de cuentas <br>
                 <b>Escala</b>: 1=Sí; 2=No")
            )
          )
        ),
        
        # Plot Content
        div(
          class = "plot-content",
          plotlyOutput("mechanisms_plot", height = "450px")
        )
      )
    )
  )
}