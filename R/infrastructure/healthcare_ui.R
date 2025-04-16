# UI para Dashboard de Servicios de Salud
healthcareUI <- function() {
  page_fluid(
    class = "section-infraestructura",

    useShinyjs(),
      
    tags$head(
      tags$style(HTML("
        /* Override pill navigation styling for this page */
        .infraestructura-pills .nav-pills .nav-link:not(.active) {
          background-color: rgba(240, 240, 240, 0.8);
color: var(--infraestructura-color) !important;
            border: 1px solid rgba(142, 36, 170, 0.2);
          font-weight: bold !important;
        }
        
        .infraestructura-pills .nav-pills .nav-link:hover:not(.active) {
          background-color: rgba(142, 36, 170, 0.1);
        }
             .infraestructura-pills .nav-pills .nav-link.active {
          background-color: var(--infraestructura-color) !important; 
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
          style="border-top: 4px solid var(--infraestructura-color)",

          h2("Servicios de Salud", class = "text-center")
        )
      )
    ),
    
    
    # Primera sección: Mapas de satisfacción
    card(
      card_header(
      div(
        class = "d-flex justify-content-between align-items-center",
      "Satisfacción con los Servicios de Salud",
      downloadButton(
        "download_healthcare_map", 
        "", 
        icon = icon("download"), 
        class = "btn-sm"
      )
    )
    ),
    div(class = "infraestructura-pills",
    navset_pill(
        id = "healthcare_tabs",
        
        # Tab: Servicios de salud en general
        nav_panel(
          title = "Servicios en General",
          icon = bsicons::bs_icon("hospital"),
          
          card(
            leafletOutput("health_services_map", height = "500px")
          )
        ),
        
        nav_panel(
          title = "Instalaciones",
          icon = bsicons::bs_icon("building"),
          
          card(
            leafletOutput("facilities_map", height = "500px")
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

            leafletOutput("distance_map", height = "500px")
          )
        )
      )
    )
    ),
    
    # Segunda sección: Proveedores de servicios de salud
    card(
      class = "provider-card",
      card_header(
"Proveedores de Servicios de Salud"
      ),
      plotlyOutput('healthcare_providers_chart')
    )
  
  )
}