# UI para Dashboard de Servicios de Salud
healthcareUI <- function() {
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
        
        /* Info box styles */
        .info-box {
          padding: 15px;
          border-radius: 5px;
          margin-bottom: 15px;
          display: flex;
          align-items: flex-start;
          background-color: #d1ecf1;
          color: #0c5460;
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
          font-size: 16px;
          line-height: 1.4;
        }
        
        /* Healthcare provider styles */
        .provider-card {
          margin-bottom: 20px;
          border-radius: 5px;
          box-shadow: 0 2px 5px rgba(0,0,0,0.1);
        }
        
        .provider-chart-container {
          padding: 20px;
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
          h2("Servicios de Salud", class = "text-center")
        )
      )
    ),
    
    
    # Primera sección: Mapas de satisfacción
    card(

      
      # Selector de preguntas sobre satisfacción
      navset_tab(
        id = "satisfaction_tabs",
        
        # Tab: Servicios de salud en general
        nav_panel(
          title = "Servicios en General",
          icon = bsicons::bs_icon("hospital"),
          
          card(
            card_header(
              "Satisfacción con los Servicios de Salud (General)",
              class = "bg-light"
            ),
            leafletOutput("health_services_map", height = "500px")
          )
        ),
        
        # Tab: Instalaciones
        nav_panel(
          title = "Instalaciones",
          icon = bsicons::bs_icon("building"),
          
          card(

            leafletOutput("facilities_map", height = "500px")
          ),
          
          layout_columns(
            col_widths = c(6, 6),
            value_box(
              title = "Promedio General",
              value = textOutput("facilities_avg"),
              showcase = bsicons::bs_icon("bar-chart"),
              theme = value_box_theme(bg = "#2A9D8F", fg = "white")
            ),
            value_box(
              title = "Distrito con Mayor Satisfacción",
              value = textOutput("facilities_best_district"),
              showcase = bsicons::bs_icon("trophy"),
              theme = value_box_theme(bg = "#E9C46A", fg = "white")
            )
          )
        ),
        
        # Tab: Tiempo de atención
        nav_panel(
          title = "Tiempo de Atención",
          icon = bsicons::bs_icon("stopwatch"),
          
          card(

            leafletOutput("attention_time_map", height = "500px")
          )
          
          
        ),
        
        # Tab: Disponibilidad de medicamentos
        nav_panel(
          title = "Medicamentos",
          icon = bsicons::bs_icon("capsule"),
          
          card(

            leafletOutput("medication_map", height = "500px")
          )
        
        ),
        
        # Tab: Calidad del servicio
        nav_panel(
          title = "Calidad de Servicio",
          icon = bsicons::bs_icon("star"),
          
          card(

            leafletOutput("service_quality_map", height = "500px")
          )
        ),
        
        # Tab: Distancia al centro de salud
        nav_panel(
          title = "Distancia",
          icon = bsicons::bs_icon("geo-alt"),
          
          card(
            card_header(
              "Satisfacción con la Distancia al Centro de Salud",
              class = "bg-light"
            ),
            leafletOutput("distance_map", height = "500px")
          )
        )
      )
    ),
    
    # Segunda sección: Proveedores de servicios de salud
    card(
      class = "provider-card",
      card_header(
        h4("Proveedores de Servicios de Salud")
      ),
      plotlyOutput('healthcare_providers_chart')
    )
  
  )
}