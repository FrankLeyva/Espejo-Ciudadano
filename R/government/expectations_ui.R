# UI for Government Expectations Dashboard
expectationsUI <- function() {
  page_fluid(
    useShinyjs(),
    
    tags$head(
      tags$link(
        rel = "stylesheet", 
        href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.5/font/bootstrap-icons.css"
      ),
      tags$style(HTML("
        .nav-tabs .nav-link.active {
          font-weight: bold;
          border-bottom-color: #0d6efd;
        }
        
        .tab-content {
          padding-top: 20px;
        }
        
        .info-box {
          padding: 15px;
          border-radius: 5px;
          margin-bottom: 15px;
          display: flex;
          align-items: flex-start;
        }
        
        .info-box-icon {
          margin-right: 15px;
          font-size: 24px;
          padding-top: 3px;
        }
        
        .info-box-content {
          flex-grow: 1;
        }
        
        .info-box-title {
          font-weight: bold;
          margin-bottom: 10px;
          font-size: 16px;
        }
        
        .info-box-value {
          font-size: 18px;
          line-height: 1.4;
        }
        
        .info-box-info {
          background-color: #d1ecf1;
          color: #0c5460;
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
          h2("Expectativas Ciudadanas en Gobiernos", class = "text-center")
        )
      )
    ),
    
    # Government expectations maps
    card(
      card_header("Calificación de Expectativas Ciudadanas por Distrito"),
      
      # Create tabset for the three maps
      navset_tab(
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
    ),
    
    # Comparison bar chart
    card(
      card_header("Comparación de Percepción Ciudadana por Nivel de Gobierno"),
      plotlyOutput("government_comparison_plot", height = "500px")
    )
  )
}