# healthcare_ui.R

healthcareUI <- function() {
  page_fluid(
    class = "section-bienestar",
    useShinyjs(),
    init_tooltips(),

    # Back navigation
    div(
      class = "mb-4",
      tags$a(
        href = "#",
        class = "text-decoration-none",
        onclick = "Shiny.setInputValue('nav_target', 'wellness', {priority: 'event'}); return false;",
        tags$i(class = "fas fa-arrow-left me-2"),
        "Volver a Calidad de Vida"
      )
    ),
    
    # Header
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          style = paste0("background-color: var(--bienestar-color) !important; 
            color: white !important; 
            font-family: var(--font-display) !important;
            font-weight: bolder !important; 
            text-align: center !important; 
            border-bottom: none !important;"),
          h1("Servicios de Salud", class = "dashboard-header")
        )
      )
    ),
    
    # Tab Navigation with Plot Cards (Healthcare Satisfaction Maps)
    div(
      class = "plot-tabs-container",
      
      navset_tab(
        id = "healthcare_tabs",
        
        # Servicios en General Tab
        nav_panel(
          title = "Servicios en General",
          value = "general_services",
          
          div(
            class = "plot-card plot-card-bienestar",
            
            # Plot Header
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Satisfacción con los Servicios de Salud en General por Distrito",
                  create_dynamic_tooltip("healthcare_tooltip")
                )
              ),
              div(
                class = "plot-actions",
                downloadButton(
                  "download_healthcare_map", 
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
              leafletOutput("health_services_map", height = "500px")
            )
          )
        ),
        
        # Instalaciones Tab
        nav_panel(
          title = "Instalaciones",
          value = "facilities",
          
          div(
            class = "plot-card plot-card-bienestar",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Satisfacción con las Instalaciones de Salud por Distrito",
                  create_dynamic_tooltip("facilities_tooltip")
                )
              ),
              div(
                class = "plot-actions",
                downloadButton(
                  "download_facilities_map", 
                  "", 
                  icon = icon("download"), 
                  class = "plot-action-btn",
                  title = "Descargar mapa"
                )
              )
            ),
            
            div(
              class = "plot-content",
              leafletOutput("facilities_map", height = "500px")
            )
          )
        ),
        
        # Tiempo de Atención Tab
        nav_panel(
          title = "Tiempo de Atención",
          value = "attention_time",
          
          div(
            class = "plot-card plot-card-bienestar",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Satisfacción con el Tiempo de Atención por Distrito",
                  create_dynamic_tooltip("attention_time_tooltip")
                )
              ),
              div(
                class = "plot-actions",
                downloadButton(
                  "download_attention_time_map", 
                  "", 
                  icon = icon("download"), 
                  class = "plot-action-btn",
                  title = "Descargar mapa"
                )
              )
            ),
            
            div(
              class = "plot-content",
              leafletOutput("attention_time_map", height = "500px")
            )
          )
        ),
        
        # Medicamentos Tab
        nav_panel(
          title = "Medicamentos",
          value = "medication",
          
          div(
            class = "plot-card plot-card-bienestar",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Satisfacción con Disponibilidad de Medicamentos por Distrito",
                  create_dynamic_tooltip("medication_tooltip")
                )
              ),
              div(
                class = "plot-actions",
                downloadButton(
                  "download_medication_map", 
                  "", 
                  icon = icon("download"), 
                  class = "plot-action-btn",
                  title = "Descargar mapa"
                )
              )
            ),
            
            div(
              class = "plot-content",
              leafletOutput("medication_map", height = "500px")
            )
          )
        ),
        
        # Calidad de Servicio Tab
        nav_panel(
          title = "Calidad de Servicio",
          value = "service_quality",
          
          div(
            class = "plot-card plot-card-bienestar",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Satisfacción con la Calidad del Servicio por Distrito",
                  create_dynamic_tooltip("service_quality_tooltip")
                )
              ),
              div(
                class = "plot-actions",
                downloadButton(
                  "download_service_quality_map", 
                  "", 
                  icon = icon("download"), 
                  class = "plot-action-btn",
                  title = "Descargar mapa"
                )
              )
            ),
            
            div(
              class = "plot-content",
              leafletOutput("service_quality_map", height = "500px")
            )
          )
        ),
        
        # Distancia Tab
        nav_panel(
          title = "Distancia",
          value = "distance",
          
          div(
            class = "plot-card plot-card-bienestar",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Satisfacción con la Distancia al Centro de Salud por Distrito",
                  create_dynamic_tooltip("distance_tooltip")
                )
              ),
              div(
                class = "plot-actions",
                downloadButton(
                  "download_distance_map", 
                  "", 
                  icon = icon("download"), 
                  class = "plot-action-btn",
                  title = "Descargar mapa"
                )
              )
            ),
            
            div(
              class = "plot-content",
              leafletOutput("distance_map", height = "500px")
            )
          )
        )
      )
    ),
    
    # Healthcare Providers Chart Section (separate plot card)
    div(
      class = "plot-cards-grid single-column",
      
      div(
        class = "plot-card plot-card-bienestar",
        
        div(
          class = "plot-header",
          div(
            class = "plot-header-content",
            h6(
              class = "plot-title",
              "Proveedores de Servicios de Salud",
              create_tooltip("<b>ID</b>: PER Q17.1 -17.8  <br>
                <b>Pregunta</b>: Ver grafica para los proveedores de servicios de salud <br>
                 <b>Escala</b>: 1=Sí; 2=No ")
            )
          )
        ),
        
        div(
          class = "plot-content",
          plotlyOutput('healthcare_providers_chart', height = "500px")
        )
      )
    )
  )
}