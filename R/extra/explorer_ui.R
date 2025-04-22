# R/extra/explorer_ui.R

explorerUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "container-fluid mt-4",
      
      # Title and description
      div(
        class = "row",
        div(
          class = "col-12",
          create_dashboard_header("Explorador de Encuesta", 
                                "Herramienta para exploración interactiva de preguntas de la encuesta")
        )
      ),
      
      div(
        class = "row",
        div(
          class = "col-12",
          # Main interface using the layout_sidebar component
          layout_sidebar(
            sidebar = sidebar(
              width = 300,
              
              # Module selection
              selectInput(
                ns("module_type"),
                "Tipo de Pregunta",
                choices = c(
                  "Razón" = "razon",
                  "Intervalo" = "intervalo",
                  "Ordinal" = "ordinal",
                  "Categórico" = "categorico",
                  "Binaria" = "binaria",
                  "Nominal" = "nominal"
                ),
                selected = "razon"
              ),
              
              # Theme filtering
              selectInput(
                ns("theme_filter"),
                "Filtrar por Tema:",
                choices = c("Todos" = "all")  # Will be populated in server
              ),
              
              # Subtheme filtering
              selectInput(
                ns("subtheme_filter"),
                "Filtrar por Subtema:",
                choices = c("Todos" = "all")  # Will be populated in server
              ),
              
              # Question selection - with improved styling
              div(
                style = "margin-bottom: 15px;",
                selectInput(
                  ns("selected_question"),
                  "Seleccionar Pregunta:",
                  choices = NULL,
                  width = "100%"
                )
              ),
              
              # Question info box
              div(
                class = "question-info-box",
                style = "margin-top: 15px; padding: 10px; border: 1px solid #ddd; border-radius: 5px; background-color: #f9f9f9;",
                h5("Información de la Pregunta:"),
                div(
                  style = "font-style: italic; margin-bottom: 10px;",
                  textOutput(ns("question_label"))
                ),
                hr(),
                uiOutput(ns("question_theme_info"))
              ),
              
              # Help text
              div(
                class = "mt-4 small text-muted",
                p("Seleccione un tipo de pregunta, tema y pregunta específica para explorar datos de la encuesta."),
                p("Las visualizaciones y controles se adaptarán automáticamente al tipo de pregunta seleccionada."),
                p("Utilice los filtros de tema y subtema para encontrar preguntas más fácilmente.")
              )
            ),
            
            # Main content with dynamic modules
            div(
              id = ns("module_container"),
              
              conditionalPanel(
                condition = sprintf("input['%s'] == 'razon'", ns("module_type")),
                razonUI(ns("razon_module"))
              ),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'intervalo'", ns("module_type")),
                intervalUI(ns("interval_module"))
              ),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'ordinal'", ns("module_type")),
                ordinalUI(ns("ordinal_module"))
              ),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'categorico'", ns("module_type")),
                categoricoUI(ns("categorico_module"))
              ),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'binaria'", ns("module_type")),
                binaryUI(ns("binary_module"))
              ),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'nominal'", ns("module_type")),
                nominalUI(ns("nominal_module"))
              )
            )
          )
        )
      ),
      
      # Footer
      create_dashboard_footer()
    )
  )
}