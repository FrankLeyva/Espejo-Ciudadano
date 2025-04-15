# UI para Dashboard de Participación Comunitaria
communityUI <- function() {
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

          h2("Participación Comunitaria", class = "text-center")
        )
      )
    ),
    
    # Organization participation
    card(
      card_header("Participación en organizaciones"),
      plotlyOutput("organizations_plot", height = "450px")
    ),
    
    # Problem-solving activities
    card(
      card_header("Actividades para resolver problemas comunitarios"),
      plotlyOutput("activities_plot", height = "500px")
    )
  )
}