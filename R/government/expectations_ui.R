# UI for Government Expectations Dashboard
expectationsUI <- function() {
  page_fluid(
    class = "section-gobierno",

    useShinyjs(),
    tags$head(
      tags$style(HTML("
        /* Override pill navigation styling for this page */
        .gobierno-pills .nav-pills .nav-link:not(.active) {
          background-color: rgba(240, 240, 240, 0.8);
color: var(--gobierno-color) !important;
            border: 1px solid rgba(229, 126, 30, 0.2);
          font-weight: bold !important;
        }
        
        .gobierno-pills .nav-pills .nav-link:hover:not(.active) {
          background-color: rgba(160, 115, 67, 0.1);
        }
             .gobierno-pills .nav-pills .nav-link.active {
          background-color: var(--gobierno-color) !important; 
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

          h2("Expectativas Ciudadanas en Gobiernos", class = "text-center")
        )
      )
    ),
    
    # Government expectations maps
    card(
      card_header(
        div(
        class = "d-flex justify-content-between align-items-center",
      "Expectativas de los Ciudadanos sobre el Gobierno",
      downloadButton(
        "download_expectations_map", 
        "", 
        icon = icon("download"), 
        class = "btn-sm"
      ))),
      div(class = "gobierno-pills",
      navset_pill(
        id = "expectations_tabs",

        nav_panel(
          title = "Gobierno Municipal",
          leafletOutput("municipal_expectations_map", height = "500px")
        ),
        nav_panel(
          title = "Gobierno Estatal",
          leafletOutput("state_expectations_map", height = "500px")
        ),
        nav_panel(
          title = "Gobierno Federal",
          leafletOutput("federal_expectations_map", height = "500px")
        )
      )
    )
    ),
    
    # Comparison bar chart
    card(
      card_header("Comparación de Percepción Ciudadana por Nivel de Gobierno"),
      plotlyOutput("government_comparison_plot", height = "500px")
    )
  )
}