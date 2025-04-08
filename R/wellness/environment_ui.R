# environmentUI.R
environmentUI <- function() {
  page_fluid(
    useShinyjs(),
    
    tags$head(
      tags$link(
        rel = "stylesheet", 
        href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.5/font/bootstrap-icons.css"
      ),
      tags$style(HTML("
        .tab-content {
          padding-top: 20px;
        }
        .nav-tabs .nav-link.active {
          font-weight: bold;
          border-top: 3px solid #0d6efd;
        }
      "))
    ),
    
    theme = bs_theme(
      version = 5,
      bootswatch = "litera",
      primary = "#0d6efd"
    ),
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
        card_header(
          h2("Medio Ambiente", class = "text-center")
        )
      )
    ),
    
    # Environmental satisfaction maps
    card(
      card_header(
        div(
          style = "background-color: transparent; border-bottom: none;",
          "Satisfacción con Aspectos Ambientales",
          class = "h5 fw-bold"
        )
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
        div(
          style = "background-color: transparent; border-bottom: none;",
          "Principales Problemas Ambientales por Colonia",
          class = "h5 fw-bold"
        )
      ),
      plotlyOutput("env_problems_plot", height = "500px")
    ),

  )
}