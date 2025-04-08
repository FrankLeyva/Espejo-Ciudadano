# UI para Dashboard de Participación Cívica
civicUI <- function() {
  page_fluid(
    useShinyjs(),
      
    tags$head(
      tags$link(
        rel = "stylesheet", 
        href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.5/font/bootstrap-icons.css"
      ),
      tags$style(HTML("
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
          h2("Participación Cívica", class = "text-center")
        )
      )
    ),
    
    # Map section
    card(
      card_header("Interés en participar en política municipal por distrito"),
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