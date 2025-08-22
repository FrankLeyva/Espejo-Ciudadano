# public_services_ui.R - Updated to match style guide while preserving sidebar functionality
publicServicesUI <- function() {
  page_fluid(
    class = "section-infraestructura",
    useShinyjs(),
    init_tooltips(),

    # Back navigation
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
    
    # Header
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          style = paste0("background-color: var(--infraestructura-color) !important; 
            color: white !important; 
            font-family: var(--font-display) !important;
            font-weight: bolder !important; 
            text-align: center !important; 
            border-bottom: none !important;"),
          h1("Servicios Públicos", class = "dashboard-header")
        )
      )
    ),
    
    # Main layout with sidebar - Enhanced styling
    layout_sidebar(
      sidebar = sidebar(
        title = div(
          class = "sidebar-title",
          style = "color: var(--infraestructura-color); font-weight: 600; margin-bottom: 1rem;",
          "Seleccionar Servicio"
        ),
        width = 300,
        
        # Enhanced service selection with better styling
        div(
          class = "service-selection-container",
          style = "background: #f8f9fa; padding: 1rem; border-radius: 8px; border-left: 4px solid var(--infraestructura-color);",
          
          div(
            class = "service-list",
            radioButtons(
              "selected_service",
              label = div(
                style = "font-weight: 600; color: #495057; margin-bottom: 0.75rem;",
                "Servicio a evaluar:"
              ),
              choices = c(
                "Agua" = "Q29",
                "Drenaje y Alcantarillado" = "Q30",
                "Comisión Federal de Electricidad" = "Q35",
                "Recolección de Basura" = "Q40"
              ),
              selected = "Q29"
            )
          ),
          
          # Service description helper
          div(
            class = "service-help-text",
            style = "margin-top: 1rem; padding: 0.75rem; background: white; border-radius: 6px; font-size: 0.875rem; color: #6c757d;",
            "Seleccione un servicio para ver su evaluación por distrito en el mapa."
          )
        )
      ),
      
      # Main content area with plot card structure
      div(
        class = "plot-card plot-card-infraestructura",
        
        # Plot Header
        div(
          class = "plot-header",
          div(
            class = "plot-header-content",
            h6(
              class = "plot-title",
              textOutput("service_title", inline = TRUE),
              create_dynamic_tooltip("utilities_tooltip")
            )
          ),
          div(
            class = "plot-actions",
            downloadButton(
              "download_service_map", 
              "", 
              icon = icon("download"), 
              class = "plot-action-btn",
              title = "Descargar mapa"
            )
          )
        ),
        
        # Plot Content
        div(
          class = "plot-content",
          leafletOutput("service_map", height = "600px")
        )
      )
    ),
    
    # Service-specific information cards with enhanced styling
    div(
      class = "service-info-cards mt-4",
      
      # Water service info
      conditionalPanel(
        condition = "input.selected_service == 'Q29'",
        div(
          class = "plot-card plot-card-infraestructura",
          div(
            class = "plot-content with-padding",
            div(
              class = "info-box",
              style = "background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%); border-left: 4px solid #2196f3;",
              div(
                class = "info-box-icon",
                style = "color: #1976d2;",
                bsicons::bs_icon("droplet-fill", size = "1.5em")
              ),
              div(
                class = "info-box-content",
                div(class = "info-box-title", style = "color: #1565c0; font-weight: 600;", "Suministro de Agua"),
                div(
                  class = "info-box-value",
                  style = "color: #0d47a1; line-height: 1.5;",
                  "En promedio, los ciudadanos reportaron tener acceso al suministro de agua ",
                  tags$strong(textOutput("water_days", inline = TRUE)),
                  " días por semana. Este valor refleja la frecuencia promedio del servicio de agua en toda la ciudad."
                )
              )
            )
          )
        )
      ),
      
      # Electricity service info
      conditionalPanel(
        condition = "input.selected_service == 'Q35'",
        div(
          class = "plot-card plot-card-infraestructura",
          div(
            class = "plot-content with-padding",
            div(
              class = "info-box",
              style = "background: linear-gradient(135deg, #fff3e0 0%, #ffe0b2 100%); border-left: 4px solid #ff9800;",
              div(
                class = "info-box-icon",
                style = "color: #f57c00;",
                bsicons::bs_icon("lightning-fill", size = "1.5em")
              ),
              div(
                class = "info-box-content",
                div(class = "info-box-title", style = "color: #ef6c00; font-weight: 600;", "Servicio Eléctrico"),
                div(
                  class = "info-box-value",
                  style = "color: #e65100; line-height: 1.5;",
                  "La experiencia más común reportada por los ciudadanos con respecto a los cortes de luz en el último mes fue: ",
                  tags$strong(textOutput("power_outages", inline = TRUE)),
                  ". Este dato representa la situación más frecuente reportada por los encuestados."
                )
              )
            )
          )
        )
      ),
      
      # Trash collection service info
      conditionalPanel(
        condition = "input.selected_service == 'Q40'",
        div(
          class = "plot-card plot-card-infraestructura",
          div(
            class = "plot-content with-padding",
            div(
              class = "info-box",
              style = "background: linear-gradient(135deg, #e8f5e8 0%, #c8e6c9 100%); border-left: 4px solid #4caf50;",
              div(
                class = "info-box-icon",
                style = "color: #388e3c;",
                bsicons::bs_icon("trash-fill", size = "1.5em")
              ),
              div(
                class = "info-box-content",
                div(class = "info-box-title", style = "color: #2e7d32; font-weight: 600;", "Recolección de Basura"),
                div(
                  class = "info-box-value",
                  style = "color: #1b5e20; line-height: 1.5;",
                  "En promedio, el servicio de recolección de basura pasa ",
                  tags$strong(textOutput("trash_pickup", inline = TRUE)),
                  " veces por semana en los hogares de la ciudad."
                )
              )
            )
          )
        )
      )
    )
  )
}