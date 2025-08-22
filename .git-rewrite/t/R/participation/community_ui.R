# community_ui.R - Updated to match style guide and modern plot card pattern

communityUI <- function() {
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
          h1("Participación Comunitaria", class = "dashboard-header")
        )
      )
    ),
    
    # PLOT CARDS SECTION - Using responsive grid
    
    # Content section: Organization participation and problem-solving activities
    div(
      class = "plot-cards-grid two-columns",
      
      # Organization Participation Plot Card
      div(
        class = "plot-card plot-card-participacion",
        
        # Plot Header
        div(
          class = "plot-header",
          div(
            class = "plot-header-content",
            h6(
              class = "plot-title",
              "Participación en organizaciones",
              create_tooltip("<b>ID</b>: PAR Q132.1 - Q132.11 <br>
                <b>Pregunta</b>: ¿Participa usted en alguna de las siguientes organizaciones?: Partido político, Organización religiosa, Organización deportiva, Organización cultural, Organización de padres de familia, Organización de vecinos, Sindicato, Organización empresarial, Organización profesional, Organización de derechos humanos, Otra organización civil <br>
                 <b>Escala</b>: 1=Sí; 2=No")
            )
          )
        ),
        
        # Plot Content
        div(
          class = "plot-content",
          plotlyOutput("organizations_plot", height = "450px")
        )
      ),
      
      # Problem-solving Activities Plot Card
      div(
        class = "plot-card plot-card-participacion",
        
        # Plot Header
        div(
          class = "plot-header",
          div(
            class = "plot-header-content",
            h6(
              class = "plot-title",
              "Actividades para resolver problemas comunitarios",
              create_tooltip("<b>ID</b>: PAR Q136.1 - Q136.14 <br>
                <b>Pregunta</b>: Durante el año: ¿Realizó alguna de las siguientes actividades para resolver problemas en su comunidad?: Firmó una petición, Contactó a un funcionario público, Participó en una reunión pública, Participó en una protesta pacífica, Contactó a medios de comunicación, Participó en redes sociales, Donó dinero, Hizo trabajo voluntario, Participó en una campaña política, Contactó a una organización civil, Participó en un boicot, Se unió a una organización, Organizó o participó en una asamblea comunitaria, Otras actividades <br>
                 <b>Escala</b>: 1=Sí; 2=No")
            )
          )
        ),
        
        # Plot Content
        div(
          class = "plot-content",
          plotlyOutput("activities_plot", height = "450px")
        )
      )
    )
  )
}