trustUI <- function() {
  page_fluid(
    class = "section-gobierno",

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
          style="border-top: 4px solid var(--gobierno-color)",

          h2("Confianza en las Instituciones", class = "text-center")
        )
      )
    ),
    
    
    # First bar plot: Institutions of popular election
    card(
      card_header(
          "Instituciones de Elección Popular"
        
      ),
      card_body(
        plotlyOutput("popular_election_institutions_plot", height = "500px")
      )
    ),
    
    # Second bar plot: Public institutions and media
    card(
      card_header(
"Instituciones Públicas y Medios de Comunicación"
      ),
      card_body(
        plotlyOutput("public_institutions_media_plot", height = "500px")
      )
    ),
    
    # Third bar plot: Public safety institutions
    card(
      card_header(

"Instituciones de Seguridad Pública"
      ),
      card_body(
        plotlyOutput("public_safety_institutions_plot", height = "500px")
      )
    )
  )
}