# environmentUI.R
environmentUI <- function() {
  page_fluid(
    class = "section-bienestar", 
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        /* Override pill navigation styling for this page */
        .bienestar-pills .nav-pills .nav-link:not(.active) {
          background-color: rgba(240, 240, 240, 0.8);
color: var(--bienestar-color) !important;
            border: 1px solid rgba(30, 136, 229, 0.2);
          font-weight: bold !important;
        }
        
        .bienestar-pills .nav-pills .nav-link:hover:not(.active) {
          background-color: rgba(30, 136, 229, 0.1);
        }
             .bienestar-pills .nav-pills .nav-link.active {
          background-color: var(--bienestar-color) !important; /* Bienestar primary color */
          color: white !important;
          font-weight: bold !important;
          border: none !important;
        }
      "))
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
        style = "border-top: 4px solid var(--bienestar-color);", 
        card_header(
          h2("Medio Ambiente", class = "text-center")
        )
      )
    ),
    
    # Environmental satisfaction maps
    card(
      card_header(
        div(
          class = "d-flex justify-content-between align-items-center",
          "Satisfacción con Aspectos Ambientales",
          downloadButton(
            "download_environment_map", 
            "", 
            icon = icon("download"), 
            class = "btn-sm"
          )
        )
      ),
      div(class = "bienestar-pills",
      navset_pill(
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