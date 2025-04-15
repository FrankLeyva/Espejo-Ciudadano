# UI para Dashboard de Participación Cívica
civicUI <- function() {
  page_fluid(
    class = "section-participacion",

    useShinyjs(),
      
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
          style="border-top: 4px solid var(--participacion-color)",

          h2("Participación Cívica", class = "text-center")
        )
      )
    ),
    
    # Map section
    card(
      card_header(
        div(
          class = "d-flex justify-content-between align-items-center",
          "Interés en participar en política municipal por distrito",
          downloadButton(
            "download_interest_map", 
            "", 
            icon = icon("download"), 
            class = "btn-sm"
          )
        )
      ),
      leafletOutput("interest_map", height = "500px")
    ),
    # Participation requirements
    card(
      card_header("¿Qué necesita para participar en asuntos públicos?"),
      plotlyOutput("requirements_plot", height = "350px")
    ),
    # Participation mechanisms knowledge
    card(
      card_header("Conocimiento de mecanismos de participación ciudadana"),
      plotlyOutput("mechanisms_plot", height = "450px")
    )
    
    
  )
}