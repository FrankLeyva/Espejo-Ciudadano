# UI para Dashboard de Participación Comunitaria
communityUI <- function() {
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
          h2("Participación Comunitaria", class = "text-center")
        )
      )
    ),
    
    # Organization participation
    card(
      card_header("Participación en organizaciones"),
      p("Porcentaje de personas que participan activamente en las siguientes organizaciones:"),
      plotlyOutput("organizations_plot", height = "450px")
    ),
    
    # Problem-solving activities
    card(
      card_header("Actividades para resolver problemas comunitarios"),
      p("Porcentaje de personas que han participado en las siguientes actividades para resolver problemas que les afectan:"),
      plotlyOutput("activities_plot", height = "500px")
    ),
    
    # Optional summary card
    card(
      card_header("Resumen de hallazgos clave"),
      div(
        class = "info-box info-box-info",
        div(class = "info-box-icon", bsicons::bs_icon("lightbulb")),
        div(
          class = "info-box-content",
          div(class = "info-box-title", "Participación comunitaria en Ciudad Juárez"),
          div(class = "info-box-value", "Los datos mostrados en este dashboard reflejan las formas actuales de participación comunitaria en Ciudad Juárez, destacando tanto la afiliación a organizaciones formales como las acciones específicas que toman los ciudadanos para resolver problemas colectivos.")
        )
      )
    )
  )
}