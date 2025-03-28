# UI for Inequality Dashboard
inequalityUI <- function() {
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
        
        .info-box-warning {
          background-color: #fff3cd;
          color: #856404;
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
          h2("Desigualdad en Ciudad Juárez", class = "text-center")
        )
      )
    ),
    
    # Maps row
    layout_columns(
      col_widths = c(6, 6),
      
      # Map for Q84 - Rights Violation
      card(
        card_header("Violación de derechos por distrito"),
        leafletOutput("rights_violation_map", height = "500px")
      ),
      
      # Map for Q87 - Inequality Perception
      card(
        card_header("Percepción de la desigualdad por distrito"),
        leafletOutput("inequality_perception_map", height = "500px")
      )
    ),
    
    # Institution bar chart
    card(
      card_header("Instituciones que contribuyen a reducir la desigualdad"),
      plotlyOutput("inequality_reduction_plot", height = "450px")
    ),
    
    # Information box explaining inequality in Juárez
    card(
      div(
        class = "info-box info-box-warning",
        div(class = "info-box-icon", bsicons::bs_icon("info-circle")),
        div(
          class = "info-box-content",
          div(class = "info-box-title", "Acerca de la desigualdad en Ciudad Juárez"),
          div(class = "info-box-value", "La percepción de desigualdad es un indicador importante del bienestar social y la cohesión comunitaria. Los datos presentados muestran cómo los ciudadanos perciben la desigualdad en diferentes distritos y qué instituciones consideran más efectivas en reducirla.")
        )
      )
    )
  )
}