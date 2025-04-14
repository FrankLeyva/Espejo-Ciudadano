# environmentUI.R
environmentUI <- function() {
  page_fluid(
    class = "section-bienestar", 
    useShinyjs(),
    
   div(
      class = "mb-4",
      tags$a(
        href = "#",
        class = "text-decoration-none",
        onclick = "Shiny.setInputValue('nav_target', 'wellness', {priority: 'event'}); return false;",
        tags$i(class = "fas fa-arrow-left me-2"),
        "Volver a Bienestar Social y Económico"
      )
    ),
    # Header
    layout_columns(
      fill = FALSE,
      card(
        style = "border-top: 4px solid var(--bienestar-color);", 
        card_header(
          h2("Medio Ambiente", class = "text-center")
        )
      )
    ),
    
    # Environmental satisfaction maps
    card(
      card_header(
          "Satisfacción con Aspectos Ambientales",
      ),
      tabsetPanel(
        id = "env_satisfaction_tabs",
        tabPanel(
          "Calidad del Aire",
          leafletOutput("air_quality_map", height = "500px")
        ),
        tabPanel(
          "Arbolado Urbano",
          leafletOutput("urban_trees_map", height = "500px")
        ),
        tabPanel(
          "Limpieza de Calles",
          leafletOutput("street_cleanliness_map", height = "500px")
        ),
        tabPanel(
          "Calidad del Agua",
          leafletOutput("water_quality_map", height = "500px")
        )
      )
    ),
    
    # Environmental problems bar chart
    card(
      card_header(

          "Principales Problemas Ambientales por Colonia",
      ),
      plotlyOutput("env_problems_plot", height = "500px")
    ),

  )
}