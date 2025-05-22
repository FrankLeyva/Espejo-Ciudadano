explorerUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # CSS for the warning banner
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
    ")),
    
    div(
      class = "container-fluid mt-4",
      
      # Header and description
      div(
        class = "row",
        div(
          class = "col-12",
          h1("Explorador de Encuestas", class = "mb-3"),
          p("Esta herramienta le permite explorar los datos de las encuestas de manera interactiva. Seleccione los parámetros de interés y visualice los resultados.", class = "lead")
        )
      ),
      

      
      # Main content
      div(
        class = "row",
        
        # Left sidebar with controls
        div(
          class = "col-md-4",
          div(
            class = "custom-card",
            div(
              class = "card-body",
              
              # Survey and year selection
              h4("Selección de Encuesta", class = "card-title mb-3"),
              div(
                class = "filter-panel",
                div(
                  class = "row mb-3",
                  div(
                    class = "col-sm-6",
                    selectInput(
                      ns("survey_type"),
                      "Tipo de Encuesta:",
                      choices = c(
                        "Percepción Ciudadana" = "PER",
                        "Participación Ciudadana y Buen Gobierno" = "PAR"
                      ),
                      selected = "PER"
                    )
                  ),
                  div(
                    class = "col-sm-6",
                    selectInput(
                      ns("survey_year"),
                      "Año:",
                      choices = c("2023", "2024"),
                      selected = "2024"
                    )
                  )
                )
              ),
              
              # Search and filter
              h4("Búsqueda de Preguntas", class = "card-title mt-4 mb-3"),
              div(
                class = "filter-panel",
                textInput(
                  ns("search_query"),
                  "Buscar:",
                  placeholder = "Ingrese palabras clave...",
                  width = "100%"
                ),
                div(class = "mb-2"),
                selectInput(
                  ns("theme_filter"),
                  "Filtrar por Tema:",
                  choices = c("Todos" = "all"),
                  selected = "all"
                ),
                div(class = "mb-2"),
                selectInput(
                  ns("subtheme_filter"),
                  "Filtrar por Subtema:",
                  choices = c("Todos" = "all"),
                  selected = "all"
                ),
                div(class = "mb-3"),
                selectInput(
                  ns("question_select"),
                  "Seleccionar Pregunta:",
                  choices = NULL,
                  width = "100%"
                )
              ),
              
              # Visualization options
              h4("Opciones de Visualización", class = "card-title mt-4 mb-3"),
              div(
                class = "filter-panel viz-selector",
                uiOutput(ns("viz_options")),
                
                # Filter options (hidden until a question is selected)
                conditionalPanel(
                  condition = sprintf("input['%s'] != null", ns("question_select")),
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
                p(htmlOutput(ns("question_text"))),
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
              class = "row",
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