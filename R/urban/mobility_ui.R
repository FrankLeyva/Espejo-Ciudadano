mobilityUI <- function() {
  page_fluid(
    useShinyjs(),
    
    tags$head(
      tags$link(
        rel = "stylesheet", 
        href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.5/font/bootstrap-icons.css"
      ),
      tags$style(HTML("
        /* Card styling */
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
        
        .tab-content {
          padding-top: 20px;
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
        onclick = "Shiny.setInputValue('nav_target', 'urban', {priority: 'event'}); return false;",
        tags$i(class = "fas fa-arrow-left me-2"),
        "Volver a Desarrollo Urbano"
      )
    ),
    # Header
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          h2("Movilidad Urbana", class = "text-center")
        )
      )
    ),
    
    # Vehicle and bicycle distribution
    layout_columns(
      col_widths = c(6, 6),
      
      # Bicycles card
      card(
        card_header(
          div(
            style = "background-color: transparent; border-bottom: none;",
            "Bicicletas por Hogar",
            class = "h5 fw-bold"
          )
        ),
        plotlyOutput("bicycles_pie", height = "400px")
      ),
      
      # Vehicles card
      card(
        card_header(
          div(
            style = "background-color: transparent; border-bottom: none;",
            "Vehículos Motorizados por Hogar",
            class = "h5 fw-bold"
          )
        ),
        plotlyOutput("vehicles_pie", height = "400px")
      )
    ),
    
    # Transportation modes tabset panel
    card(
      card_header(
        div(
          style = "background-color: transparent; border-bottom: none;",
          "Modos de Transporte",
          class = "h5 fw-bold"
        )
      ),
      tabsetPanel(
        id = "transport_tabs",
        tabPanel(
          "Transporte al Trabajo",
          plotlyOutput("work_transport_plot", height = "550px")
        ),
        tabPanel(
          "Transporte General",
          plotlyOutput("general_transport_plot", height = "550px")
        )
      )
    ),
    
    # Additional info card
    card(
      div(
        class = "info-box",
        div(class = "info-box-icon", bsicons::bs_icon("info-circle-fill")),
        div(
          class = "info-box-content",
          div(class = "info-box-title", "Sobre este Dashboard"),
          div(class = "info-box-value", "Este dashboard muestra los patrones de movilidad urbana de los habitantes de Ciudad Juárez, incluyendo la distribución de vehículos personales y los medios de transporte utilizados para desplazamientos diarios.")
        )
      )
    )
  )
}