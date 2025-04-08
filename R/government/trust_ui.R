trustUI <- function() {
  page_fluid(
    useShinyjs(),
    div(
      class = "mb-4",
      tags$a(
        href = "#",
        class = "text-decoration-none",
        onclick = "Shiny.setInputValue('nav_target', 'government', {priority: 'event'}); return false;",
        tags$i(class = "fas fa-arrow-left me-2"),
        "Volver a Gobierno"
      )
    ),
    # Header
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          h2("Confianza en las Instituciones", class = "text-center")
        )
      )
    ),
    
    
    # First bar plot: Institutions of popular election
    card(
      card_header(
        div(
          class = "d-flex justify-content-between align-items-center",
          h4("Instituciones de Elección Popular", class = "m-0")
        )
      ),
      card_body(
        plotlyOutput("popular_election_institutions_plot", height = "500px")
      )
    ),
    
    # Second bar plot: Public institutions and media
    card(
      card_header(
        div(
          class = "d-flex justify-content-between align-items-center",
          h4("Instituciones Públicas y Medios de Comunicación", class = "m-0")
        )
      ),
      card_body(
        plotlyOutput("public_institutions_media_plot", height = "500px")
      )
    ),
    
    # Third bar plot: Public safety institutions
    card(
      card_header(
        div(
          class = "d-flex justify-content-between align-items-center",
          h4("Instituciones de Seguridad Pública", class = "m-0")
        )
      ),
      card_body(
        plotlyOutput("public_safety_institutions_plot", height = "500px")
      )
    ),
    
    # Source note card
    card(
      card_body(
        p(
          class = "text-muted small",
          "Fuente: Encuesta de Participación Ciudadana (PAR) 2024.",
          "Notas: Se excluyen las respuestas \"No sabe/No contesta\" del análisis."
        )
      )
    )
  )
}