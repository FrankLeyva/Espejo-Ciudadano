# UI para Dashboard de Vivienda
housingUI <- function() {
  page_fluid(
    useShinyjs(),
      
    tags$head(
      tags$link(
        rel = "stylesheet", 
        href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.5/font/bootstrap-icons.css"
      ),
      tags$style(HTML("
        /* General styling */
        .nav-tabs .nav-link.active {
          font-weight: bold;
          color: #0d6efd;
          background-color: #f8f9fa;
          border-bottom: 3px solid #0d6efd;
        }
        
        .nav-tabs .nav-link {
          color: #495057;
          transition: all 0.3s ease;
        }
        
        .nav-tabs .nav-link:hover {
          background-color: #f1f1f1;
        }
        
        /* Custom info box styles */
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
        
        .housing-intro {
          background-color: #f8f9fa;
          padding: 20px;
          border-radius: 5px;
          margin-bottom: 20px;
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
        onclick = "Shiny.setInputValue('nav_target', 'infrastructure', {priority: 'event'}); return false;",
        tags$i(class = "fas fa-arrow-left me-2"),
        "Volver a Infraestructura"
      )
    ),
    # Encabezado
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          h2("Satisfacción con la Vivienda", class = "text-center")
        )
      )
    ),

    
    # Pestañas para los diferentes aspectos de vivienda
    navset_tab(
      id = "housing_tabs",
      
      # Pestaña: Calidad de materiales
      nav_panel(
        title = "Calidad de Materiales",
        icon = bsicons::bs_icon("bricks"),
        

        
        card(
          leafletOutput("materials_map", height = "500px")
        )
      ),
      
      # Pestaña: Tamaño y espacios
      nav_panel(
        title = "Tamaño y Espacios",
        icon = bsicons::bs_icon("layout-text-window"),
        
        
        card(

          leafletOutput("spaces_map", height = "500px")
        )
      ),
      
      # Pestaña: Accesibilidad de la ubicación
      nav_panel(
        title = "Ubicación y Accesibilidad",
        icon = bsicons::bs_icon("geo-alt"),
      
        card(

          leafletOutput("location_map", height = "500px")
        )
      ),
      
      # Pestaña: Comparativa
      nav_panel(
        title = "Comparativa",
        icon = bsicons::bs_icon("graph-up"),
        
        card(

          plotlyOutput("comparison_plot", height = "500px")
        )
      )
    ),
    

  )
}