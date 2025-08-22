# inequality_ui.R - Updated to match new styling system
inequalityUI <- function() {
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
          h1("Desigualdad", class = "dashboard-header")
        )
      )
    ),
    
    # Content Section 1: Maps
    div(
      class = "plot-cards-grid two-columns",
      
      # Rights Violation Map Card
      div(
        class = "plot-card plot-card-gobierno",
        
        div(
          class = "plot-header",
          div(
            class = "plot-header-content",
            h6(
              class = "plot-title",
              "Violación de Derechos por Distrito",
              create_tooltip("<b>ID</b>: PER Q84 <br>
                <b>Pregunta</b>: En el último año (2024), ¿siente que sus garantías o derechos han sido violentados por alguna autoridad o dependencia gubernamental? <br>
                 <b>Escala</b>: 1=Sí; 2=No")
            )
          ),
          div(
            class = "plot-actions",
            downloadButton(
              "download_rights_violation_map", 
              "", 
              icon = icon("download"), 
              class = "plot-action-btn",
              title = "Descargar mapa"
            )
          )
        ),
        
        div(
          class = "plot-content",
          leafletOutput("rights_violation_map", height = "500px")
        )
      ),
      
      # Inequality Perception Map Card
      div(
        class = "plot-card plot-card-gobierno",
        
        div(
          class = "plot-header",
          div(
            class = "plot-header-content",
            h6(
              class = "plot-title",
              "Percepción de la Desigualdad por Distrito",
              create_tooltip("<b>ID</b>: PER Q87 <br>
                <b>Pregunta</b>: Por distintos motivos, no todas las personas que habitan en Juárez pueden acceder en condiciones de igualdad a los bienes y servicios, ni tienen las mismas oportunidades en la vida. ¿Cómo describiría la desigualdad que se vive hoy en día? <br>
                 <b>Escala</b>: 1=Muy alta; 2=Alta; 3=Media; 4=Baja; 5=Muy baja; 6=No sabe/No contestó")
            )
          ),
          div(
            class = "plot-actions",
            downloadButton(
              "download_inequality_map", 
              "", 
              icon = icon("download"), 
              class = "plot-action-btn",
              title = "Descargar mapa"
            )
          )
        ),
        
        div(
          class = "plot-content",
          leafletOutput("inequality_perception_map", height = "500px")
        )
      )
    ),
    
    # Content Section 2: Institution Contribution (Full Width)
    div(
      class = "plot-cards-grid single-column",
      
      div(
        class = "plot-card plot-card-gobierno",
        
        div(
          class = "plot-header",
          div(
            class = "plot-header-content",
            h6(
              class = "plot-title",
              "Instituciones que Contribuyen a Reducir la Desigualdad",
              create_tooltip("<b>ID</b>: PER Q88 <br>
                <b>Pregunta</b>: ¿Qué institución contribuye más a reducir la desigualdad en Juárez? <br>
                 <b>Escala</b>: 11 categorías")
            )
          )
        ),
        
        div(
          class = "plot-content",
          plotlyOutput("inequality_reduction_plot", height = "500px")
        )
      )
    )
  )
}