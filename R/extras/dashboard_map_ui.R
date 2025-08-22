# dashboard_map_ui.R - Updated to match new style guide

dashboardMapUI <- function(id) {
  ns <- NS(id)
  
  page_fluid(
    class = "section-extras",
    useShinyjs(),
    init_tooltips(),
    
    # Header
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          style = paste0("background-color: var(--extras-color) !important; 
            color: white !important; 
            font-family: var(--font-display) !important;
            font-weight: bolder !important; 
            text-align: center !important; 
            border-bottom: none !important;"),
          h1("Mapa del Dashboard", class = "dashboard-header")
        )
      )
    ),
    
    # Introduction - Full width card
    div(
      class = "plot-cards-grid single-column",
      
      div(
        class = "page-card page-card-extras",
        
        # Card Header
        div(
          class = "page-header",
          div(
            class = "page-header-content",
            h4(class = "page-card-title", style = "font-family: var(--font-display);", "Navegación del Dashboard")
          )
        ),
        
        # Card Content
        div(
          class = "page-content",
          div(
            class = "text-center mb-4",
            p("Este mapa proporciona una visión general de todos los contenidos disponibles en el dashboard Espejo Ciudadano.", 
              class = "mb-3"),
            p("Seleccione una sección para ver su contenido detallado y navegar directamente a ella.", 
              class = "text-muted")
          ),
          
          # Section selection dropdown
          div(
            style = "margin-bottom: 25px;",
            div(
              style = "background-color: #f8f9fa; padding: 1rem; border-radius: var(--border-radius); border-left: 4px solid var(--extras-color);",
              selectInput(
                ns("selected_section"),
                label = div(
                  style = "font-weight: 600; font-size: 1.1rem; margin-bottom: 8px; font-family: var(--font-display);",
                  "Seleccionar Sección del Dashboard:"
                ),
                choices = c(
                  "Calidad de Vida" = "bienestar",
                  "Movilidad Urbana" = "movilidad", 
                  "Instituciones" = "gobierno",
                  "Infraestructura" = "infraestructura",
                  "Participación" = "participacion",
                  "Extras" = "extras"
                ),
                width = "100%",
                selectize = TRUE
              )
            )
          ),
          
          # Section contents will be rendered here
          uiOutput(ns("section_contents"))
        )
      )
    ),
    
    # Navigation helper - Full width card
    div(
      class = "plot-cards-grid single-column",
      
      div(
        class = "page-card page-card-extras",
        
        # Card Header
        div(
          class = "page-header",
          div(
            class = "page-header-content",
            h4(class = "page-card-title", style = "font-family: var(--font-display);", "Navegación Rápida")
          )
        ),
        
        # Card Content
        div(
          class = "page-content",
          p("Use los siguientes enlaces para navegar rápidamente a las secciones principales del dashboard:"),
          
          div(
            class = "row g-3 mt-3",
            
            # Main sections quick navigation
            div(
              class = "col-md-4",
              div(
                class = "d-grid",
                actionButton(
                  inputId = ns("goto_overview"),
                  label = div(
                    icon("home"), " Inicio",
                    style = "display: flex; align-items: center; gap: 0.5rem;"
                  ),
                  class = "btn btn-outline-primary",
                  style = "border-color: var(--extras-color); color: var(--extras-color);",
                  onclick = "Shiny.setInputValue('nav_target', 'overview', {priority: 'event'})"
                )
              )
            ),
            div(
              class = "col-md-4",
              div(
                class = "d-grid",
                actionButton(
                  inputId = ns("goto_explorer"),
                  label = div(
                    icon("search"), " Explorador",
                    style = "display: flex; align-items: center; gap: 0.5rem;"
                  ),
                  class = "btn btn-outline-primary",
                  style = "border-color: var(--extras-color); color: var(--extras-color);",
                  onclick = "Shiny.setInputValue('nav_target', 'explorer', {priority: 'event'})"
                )
              )
            ),
            div(
              class = "col-md-4",
              div(
                class = "d-grid",
                actionButton(
                  inputId = ns("goto_methodology"),
                  label = div(
                    icon("download"), " Metodología",
                    style = "display: flex; align-items: center; gap: 0.5rem;"
                  ),
                  class = "btn btn-outline-primary",
                  style = "border-color: var(--extras-color); color: var(--extras-color);",
                  onclick = "Shiny.setInputValue('nav_target', 'methodology', {priority: 'event'})"
                )
              )
            )
          )
        )
      )
    ),
    
    # Footer
    create_dashboard_footer()
  )
}