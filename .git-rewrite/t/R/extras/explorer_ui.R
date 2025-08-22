# explorer_ui.R - Updated with dropdown fixes and proper height

explorerUI <- function(id) {
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
          h1("Así Vemos Juárez", class = "dashboard-header")
        )
      )
    ),

    # Description and Year Indicator
    div(
      class = "mb-4",
      p("Esta herramienta le permite visualizar los resultados de las encuestas utilizadas en el informe. Use la búsqueda por tema para navegar por categorías o la búsqueda por texto para encontrar preguntas específicas.", 
        class = "lead"),
      
      div(
        class = "alert alert-info",
        icon("calendar"), 
        " Explorando datos del año: ", 
        textOutput(ns("current_year_info"), inline = TRUE),
        br(),
        tags$small("Para cambiar el año, use el selector en la barra de navegación superior.")
      )
    ),

    # Main Content Grid - 4:8 column proportion with improved dropdown handling
    div(
      class = "row",
      style = "overflow: visible;", # Ensure row allows dropdown overflow
      
      # Left Panel - Search and Filters (4 columns = 33% width)
      div(
        class = "col-md-4",
        style = "overflow: visible;", # Ensure column allows dropdown overflow
        div(
          class = "plot-card plot-card-extras explorer-search-card",
          style = "min-height: 650px; overflow: visible;", # Inline style for immediate effect
          
          # Search Header
          div(
            class = "plot-header",
            div(
              class = "plot-header-content",
              h6(class = "plot-title", "Búsqueda de Preguntas")
            )
          ),
          
          # Search Content - marked with dropdowns class
          div(
            class = "plot-content with-padding with-dropdowns",
            style = "overflow: visible; position: relative;", # Ensure dropdowns can extend
            
            # Search Method Tabs
            navset_tab(
              id = ns("search_tabs"),
              
              # Theme-based navigation tab
              nav_panel(
                title = "Por Tema",
                value = "theme_search",
                
                div(
                  class = "mb-3",
                  style = "overflow: visible;", # Each dropdown container
                  
                  # Theme selector with improved container
                  div(
                    style = "position: relative; z-index: 100; margin-bottom: 20px;",
                    selectInput(
                      ns("theme_filter"),
                      "Tema:",
                      choices = c("Seleccione un tema..." = ""),
                      selected = ""
                    )
                  ),
                  
                  # Subtheme selector with improved container
                  div(
                    style = "position: relative; z-index: 99; margin-bottom: 20px;",
                    selectInput(
                      ns("subtheme_filter"),
                      "Subtema:",
                      choices = c("Primero seleccione un tema" = ""),
                      selected = ""
                    )
                  ),
                  
                  # Question selector with improved container
                  div(
                    style = "position: relative; z-index: 98; margin-bottom: 20px;",
                    selectInput(
                      ns("question_select_theme"),
                      "Pregunta:",
                      choices = c("Primero seleccione un subtema" = ""),
                      width = "100%"
                    )
                  )
                )
              ),
              
              # Text search tab
              nav_panel(
                title = "Por Texto",
                value = "text_search",
                
                div(
                  class = "mb-3",
                  style = "overflow: visible;",
                  
                  # Text input with improved container
                  div(
                    style = "margin-bottom: 15px;",
                    textInput(
                      ns("search_query"),
                      "Buscar:",
                      placeholder = "Ingrese palabras clave...",
                      width = "100%"
                    )
                  ),
                  
                  div(
                    class = "mb-3",
                    actionButton(
                      ns("search_button"),
                      "Buscar",
                      class = "btn btn-primary",
                      icon = icon("search")
                    )
                  ),
                  
                  uiOutput(ns("search_results_info")),
                  
                  # Search results selector with improved container
                  div(
                    style = "position: relative; z-index: 97; margin-bottom: 20px;",
                    selectInput(
                      ns("question_select_search"),
                      "Resultados:",
                      choices = c("Haga clic en 'Buscar' para ver resultados" = ""),
                      width = "100%"
                    )
                  ),
                  
                  tags$small(
                    class = "text-muted",
                    "La búsqueda incluye ambas encuestas del año seleccionado."
                  )
                )
              )
            ),
            
            # Data Source Info
            conditionalPanel(
              condition = sprintf("(input['%s'] != '' && input['%s'] != 'Primero seleccione un subtema') || (input['%s'] != '' && input['%s'] != 'Haga clic en \\'Buscar\\' para ver resultados')", 
                                ns("question_select_theme"), ns("question_select_theme"),
                                ns("question_select_search"), ns("question_select_search")),
              div(
                class = "alert alert-secondary mt-3",
                h6("Fuente de Datos:", class = "mb-1"),
                textOutput(ns("data_source_info"))
              )
            ),
            
            # Visualization Options
            conditionalPanel(
              condition = sprintf("(input['%s'] != '' && input['%s'] != 'Primero seleccione un subtema') || (input['%s'] != '' && input['%s'] != 'Haga clik en \\'Buscar\\' para ver resultados')", 
                                ns("question_select_theme"), ns("question_select_theme"),
                                ns("question_select_search"), ns("question_select_search")),
              hr(),
              h6("Opciones de Visualización", class = "mb-3"),
              
              # Visualization options with improved container
              div(
                style = "position: relative; z-index: 96; margin-bottom: 15px;",
                uiOutput(ns("viz_options"))
              ),
              
              hr(),
              h6("Filtros", class = "mb-3"),
              
              # District filter with improved container
              div(
                style = "position: relative; z-index: 95; margin-bottom: 15px;",
                selectInput(
                  ns("district_filter"),
                  "Distritos:", 
                  choices = NULL,
                  multiple = TRUE
                )
              ),
              
              # Custom options for visualization types
              div(
                style = "position: relative; z-index: 94;",
                uiOutput(ns("custom_viz_options"))
              )
            )
          )
        )
      ),
      
      # Right Panel - Visualization (8 columns = 67% width)
      div(
        class = "col-md-8",
        style = "overflow: visible;", # Ensure column allows overflow if needed
        div(
          class = "plot-card plot-card-extras",
          style = "min-height: 650px;", # Match the height of the left panel
          
          # Visualization Header
          div(
            class = "plot-header",
            div(
              class = "plot-header-content",
              h6(
                class = "plot-title",
                textOutput(ns("viz_title"), inline = TRUE)
              )
            ),
            # Download options in header actions
            div(
              class = "plot-actions",
              uiOutput(ns("download_options"))
            )
          ),
          
          # Visualization Content
          div(
            class = "plot-content",
            
            # Question Information Panel
            conditionalPanel(
              condition = sprintf("(input['%s'] != '' && input['%s'] != 'Primero seleccione un subtema') || (input['%s'] != '' && input['%s'] != 'Haga clic en \\'Buscar\\' para ver resultados')", 
                                ns("question_select_theme"), ns("question_select_theme"),
                                ns("question_select_search"), ns("question_select_search")),
              div(
                class = "alert alert-info mx-3 mt-3",
                htmlOutput(ns("question_text"))
              )
            ),
            
            # Visualization Container
            div(
              id = ns("viz_container"),
              style = "min-height: 400px;",
              uiOutput(ns("visualization"))
            )
          )
        )
      )
    ),

    # Warning Banner
    div(
      class = "alert alert-warning mt-4",
      icon("exclamation-triangle"), 
      " Las conclusiones derivadas de las visualizaciones generadas por esta herramienta NO son representativas de Plan Estratégico de Juárez y deben ser interpretadas cuidadosamente. Este es solo un explorador de datos para referencia."
    ),
    
    # Additional CSS injection for immediate effect
    tags$style(HTML("
      .explorer-search-card {
        min-height: 650px !important;
        overflow: visible !important;
      }
      
      .explorer-search-card .selectize-dropdown {
        z-index: 1000 !important;
        position: absolute !important;
      }
      
      .explorer-search-card .with-dropdowns {
        overflow: visible !important;
        position: relative !important;
      }
      
      /* Responsive adjustments */
      @media (max-width: 768px) {
        .explorer-search-card {
          min-height: 750px !important;
        }
      }
      
      @media (min-width: 769px) and (max-width: 992px) {
        .explorer-search-card {
          min-height: 700px !important;
        }
      }
    ")),
    tags$script(HTML("
$(document).ready(function() {
  // Function to prevent mobile keyboard
  function preventMobileKeyboard() {
    // Set font-size to 16px or larger to prevent zoom on iOS
    $('.selectize-input input, select').css({
      'font-size': '16px',
      'line-height': '1.5'
    });
    
    // Add inputmode='none' to all selectize inputs
    $('.selectize-input input').attr('inputmode', 'none');
    
    // For mobile devices, make inputs readonly temporarily on focus
    if (/Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini/i.test(navigator.userAgent)) {
      $(document).on('focusin', '.selectize-input input', function() {
        var input = this;
        $(input).attr('readonly', 'readonly');
        setTimeout(function() {
          $(input).removeAttr('readonly');
        }, 100);
      });
    }
  }
  
  // Apply on page load
  preventMobileKeyboard();
  
  // Reapply when Shiny updates the UI
  $(document).on('shiny:inputchanged', function() {
    setTimeout(preventMobileKeyboard, 100);
  });
  
  // Reapply when new elements are added
  $(document).on('DOMNodeInserted', function() {
    setTimeout(preventMobileKeyboard, 100);
  });
});
")),

# Additional CSS for mobile keyboard prevention
tags$style(HTML("
  /* Mobile-specific styles */
  @media (max-width: 768px) {
    .selectize-input input {
      font-size: 16px !important; /* Prevents zoom on iOS */
      caret-color: transparent;
      -webkit-user-select: none;
      -moz-user-select: none;
      -ms-user-select: none;
      user-select: none;
    }
    
    .selectize-input {
      -webkit-touch-callout: none;
      -webkit-user-select: none;
      -khtml-user-select: none;
      -moz-user-select: none;
      -ms-user-select: none;
      user-select: none;
    }
    
    /* Prevent text cursor */
    .selectize-input input:focus {
      caret-color: transparent;
    }
  }
  
  /* iOS specific fixes */
  @supports (-webkit-touch-callout: none) {
    .selectize-input input {
      -webkit-appearance: none;
      border-radius: 0;
    }
  }
"))
  )
}