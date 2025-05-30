explorerUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # CSS for the warning banner and improved styling
    tags$style(HTML("
      .warning-banner {
        background-color: #fff3cd;
        border-left: 4px solid #ffc107;
        padding: 10px 15px;
        margin-bottom: 20px;
        border-radius: 4px;
      }
      .warning-banner p {
        margin: 0;
        color: #856404;
      }
      
      /* Improve filter panel styling */
      .filter-panel {
        border-radius: 8px;
        background-color: #f8f9fa;
        padding: 15px;
        margin-bottom: 20px;
      }
      
      /* Custom styling for search box */
      .search-box {
        border-radius: 20px;
        border: 1px solid #ced4da;
        padding: 10px 15px;
        transition: box-shadow 0.3s;
      }
      .search-box:focus {
        box-shadow: 0 0 0 0.2rem rgba(13, 110, 253, 0.25);
        border-color: #80bdff;
      }
      
      /* Better visualization selector */
      .viz-selector {
        margin-top: 15px;
      }
      
      /* Custom card with shadow */
      .custom-card {
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        border-radius: 8px;
        overflow: hidden;
      }
      
      /* Improve download button styling */
      .download-btn {
        margin-top: 10px;
      }
      
      .value-box-border-left {
        border-left-width: 0 !important; 
      }
      
      .value-box-title {
        font-size: 1rem !important;
        font-weight: 600 !important;
      }
      
      .value-box-value {
        font-size: 1.5rem !important;
        font-weight: 700 !important;
      }
      
      .dataTables_wrapper .dataTable {
        width: 100% !important;
      }
      
      /* Improve table aesthetics */
      .dataTables_wrapper {
        padding: 0;
        margin-bottom: 20px;
      }
      
      .dataTables_wrapper .dataTable thead th {
        background-color: #f5f5f5;
        color: #333;
        font-weight: 600;
      }
      
      .dataTables_wrapper .dataTable.stripe tbody tr.odd {
        background-color: rgba(0,0,0,.02);
      }
      
      /* Add spacing */
      .mb-3 {
        margin-bottom: 1rem;
      }
      
      .mt-4 {
        margin-top: 1.5rem;
      }
      
      /* Theme indicator styling */
      .theme-indicator {
        display: inline-block;
        padding: 2px 8px;
        border-radius: 12px;
        font-size: 0.75rem;
        font-weight: 500;
        margin-left: 8px;
      }
      
      .theme-bienestar { background-color: #e3f2fd; color: #1976d2; }
      .theme-movilidad { background-color: #e8f5e8; color: #388e3c; }
      .theme-gobierno { background-color: #fce4ec; color: #c2185b; }
      .theme-infraestructura { background-color: #f3e5f5; color: #7b1fa2; }
      .theme-participacion { background-color: #fff3e0; color: #f57c00; }
      
      /* Search results styling */
      .search-results-info {
        background-color: #d1ecf1;
        border: 1px solid #bee5eb;
        border-radius: 4px;
        padding: 8px 12px;
        margin-bottom: 10px;
        font-size: 0.9rem;
        color: #0c5460;
      }
      
      /* Tab content styling */
      .search-tab-content {
        min-height: 400px;
      }
      
      /* Search button styling */
      .search-btn-container {
        margin-top: 10px;
        margin-bottom: 15px;
      }
    ")),
    
    div(
      class = "container-fluid mt-4",
      
      # Header and description
      div(
        class = "row",
        div(
          class = "col-12",
          h1("Explorador de Encuestas", class = "mb-3"),
          p("Esta herramienta le permite explorar preguntas de ambas encuestas. Use la búsqueda por tema para navegar por categorías o la búsqueda por texto para encontrar preguntas específicas.", class = "lead")
        )
      ),
      
      # Main content
      div(
        class = "row",
        
        # Left sidebar with search tabs
        div(
          class = "col-md-4",
          div(
            class = "custom-card",
            div(
              class = "card-body",
              
              # Navigation tabs
              navset_tab(
                id = ns("search_tabs"),
                
                # Theme-based navigation tab
                nav_panel(
                  title = "Búsqueda por Tema",
                  value = "theme_search",
                  div(
                    class = "search-tab-content",
                    h5("Navegación por Temas", class = "mb-3"),
                    div(
                      class = "filter-panel",
                      selectInput(
                        ns("theme_filter"),
                        "Seleccionar Tema:",
                        choices = c("Seleccione un tema..." = ""),
                        selected = ""
                      ),
                      selectInput(
                        ns("subtheme_filter"),
                        "Seleccionar Subtema:",
                        choices = c("Primero seleccione un tema" = ""),
                        selected = ""
                      ),
                      selectInput(
                        ns("question_select_theme"),
                        "Seleccionar Pregunta:",
                        choices = c("Primero seleccione un subtema" = ""),
                        width = "100%"
                      )
                    )
                  )
                ),
                
                # Text search tab
                nav_panel(
                  title = "Búsqueda por Texto",
                  value = "text_search",
                  div(
                    class = "search-tab-content",
                    h5("Búsqueda de Preguntas", class = "mb-3"),
                    div(
                      class = "filter-panel",
                      textInput(
                        ns("search_query"),
                        "Buscar preguntas:",
                        placeholder = "Ingrese palabras clave...",
                        width = "100%"
                      ),
                      div(
                        class = "search-btn-container",
                        actionButton(
                          ns("search_button"),
                          "Buscar",
                          class = "btn btn-primary",
                          icon = icon("search")
                        )
                      ),
                      div(
                        id = ns("search_results_container"),
                        uiOutput(ns("search_results_info")),
                        selectInput(
                          ns("question_select_search"),
                          "Resultados de búsqueda:",
                          choices = c("Haga clic en 'Buscar' para ver resultados" = ""),
                          width = "100%"
                        )
                      )
                    ),
                    tags$small(
                      class = "form-text text-muted",
                      "La búsqueda incluye ambas encuestas (PER y PAR) de 2023 y 2024."
                    )
                  )
                )
              ),
              
              # Data source indicator (shown for both tabs)
              conditionalPanel(
                condition = sprintf("(input['%s'] != '' && input['%s'] != 'Primero seleccione un subtema') || (input['%s'] != '' && input['%s'] != 'Haga clic en \\'Buscar\\' para ver resultados')", 
                                  ns("question_select_theme"), ns("question_select_theme"),
                                  ns("question_select_search"), ns("question_select_search")),
                div(
                  class = "mt-3 p-2",
                  style = "background-color: #f8f9fa; border-radius: 4px;",
                  h6("Fuente de Datos:", class = "mb-1"),
                  textOutput(ns("data_source_info"))
                )
              ),
              
              # Visualization options (shown for both tabs)
              h4("Opciones de Visualización", class = "card-title mt-4 mb-3"),
              div(
                class = "filter-panel viz-selector",
                uiOutput(ns("viz_options")),
                
                # Filter options (hidden until a question is selected)
                conditionalPanel(
                  condition = sprintf("(input['%s'] != '' && input['%s'] != 'Primero seleccione un subtema') || (input['%s'] != '' && input['%s'] != 'Haga clic en \\'Buscar\\' para ver resultados')", 
                                    ns("question_select_theme"), ns("question_select_theme"),
                                    ns("question_select_search"), ns("question_select_search")),
                  hr(),
                  h5("Filtros"),
                  selectInput(
                    ns("district_filter"),
                    "Distritos:", 
                    choices = NULL,
                    multiple = TRUE
                  ),
                  
                  # Custom options for visualization types
                  uiOutput(ns("custom_viz_options"))
                )
              )
            )
          )
        ),
        
        # Right content area with visualization
        div(
          class = "col-md-8",
          div(
            class = "custom-card",
            div(
              class = "card-body",
              h4(textOutput(ns("viz_title")), class = "card-title mb-3"),
              div(
                class = "card-text",
                # Question information panel
                conditionalPanel(
                  condition = sprintf("(input['%s'] != '' && input['%s'] != 'Primero seleccione un subtema') || (input['%s'] != '' && input['%s'] != 'Haga clic en \\'Buscar\\' para ver resultados')", 
                                    ns("question_select_theme"), ns("question_select_theme"),
                                    ns("question_select_search"), ns("question_select_search")),
                  div(
                    class = "alert alert-info",
                    htmlOutput(ns("question_text"))
                  )
                ),
                
                # Visualization container
                div(
                  id = ns("viz_container"),
                  uiOutput(ns("visualization"))
                ),
                
                # Download options
                uiOutput(ns("download_options"))
              )
            )
          )
        )
      ),
      
      # Warning banner
      div(
        class = "row mt-4",
        div(
          class = "col-12",
          div(
            class = "warning-banner",
            p(icon("exclamation-triangle"), " Las conclusiones derivadas de las visualizaciones generadas por esta herramienta NO son representativas de Plan Estratégico de Juárez y deben ser interpretadas cuidadosamente. Este es solo un explorador de datos para referencia.")
          )
        )
      ),
      
      # Footer
      div(
        class = "row mt-4",
        div(
          class = "col-12 text-center",
          p("Desarrollado por Plan Estratégico de Juárez - 2024", class = "text-muted")
        )
      )
    )
  )
}