# education_ui.R

educationUI <- function() {
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
          h1("Educación", class = "dashboard-header")
        )
      )
    ),
    
    # SECCIÓN 1: Hogares con Estudiantes - Tab Navigation
    div(
      class = "plot-tabs-container",
      
      navset_tab(
        id = "students_tabs",
        
        # General Tab
        nav_panel(
          title = "General",
          value = "general_students",
          
          div(
            class = "plot-card plot-card-bienestar",
            
            # Plot Header
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Hogares con Estudiantes - General",
                  create_dynamic_tooltip("students_tooltip")
                )
              ),
              div(
                class = "plot-actions",
                downloadButton(
                  "download_students_map", 
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
              leafletOutput("students_map", height = "500px")
            )
          )
        ),
        
        # Educación Básica Tab
        nav_panel(
          title = "Educación Básica",
          value = "basic_students",
          
          div(
            class = "plot-card plot-card-bienestar",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Hogares con Estudiantes - Educación Básica",
                  create_dynamic_tooltip("basic_students_tooltip")
                )
              ),
              div(
                class = "plot-actions",
                downloadButton(
                  "download_basic_students_map", 
                  "", 
                  icon = icon("download"), 
                  class = "plot-action-btn",
                  title = "Descargar mapa"
                )
              )
            ),
            
            div(
              class = "plot-content",
              leafletOutput("basic_students_map", height = "500px")
            )
          )
        ),
        
        # Educación Media Superior Tab
        nav_panel(
          title = "Educación Media Superior",
          value = "highschool_students",
          
          div(
            class = "plot-card plot-card-bienestar",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Hogares con Estudiantes - Educación Media Superior",
                  create_dynamic_tooltip("highschool_students_tooltip")
                )
              ),
              div(
                class = "plot-actions",
                downloadButton(
                  "download_highschool_students_map", 
                  "", 
                  icon = icon("download"), 
                  class = "plot-action-btn",
                  title = "Descargar mapa"
                )
              )
            ),
            
            div(
              class = "plot-content",
              leafletOutput("highschool_students_map", height = "500px")
            )
          )
        ),
        
        # Educación Superior Tab
        nav_panel(
          title = "Educación Superior",
          value = "college_students",
          
          div(
            class = "plot-card plot-card-bienestar",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Hogares con Estudiantes - Educación Superior",
                  create_dynamic_tooltip("college_students_tooltip")
                )
              ),
              div(
                class = "plot-actions",
                downloadButton(
                  "download_college_students_map", 
                  "", 
                  icon = icon("download"), 
                  class = "plot-action-btn",
                  title = "Descargar mapa"
                )
              )
            ),
            
            div(
              class = "plot-content",
              leafletOutput("college_students_map", height = "500px")
            )
          )
        )
      )
    ),
    
    # SECCIÓN 2: Satisfacción con Niveles Educativos - Tab Navigation
    div(
      class = "plot-tabs-container",
      
      navset_tab(
        id = "education_tabs",
        
        # Educación Básica Tab
        nav_panel(
          title = "Satisfacción - Educación Básica",
          value = "basic_education",
          
          div(
            class = "plot-card plot-card-bienestar",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Satisfacción con Educación Básica por Distrito",
                  create_dynamic_tooltip("edu_satis_tooltip")
                )
              ),
              div(
                class = "plot-actions",
                downloadButton(
                  "download_basic_education_map", 
                  "", 
                  icon = icon("download"), 
                  class = "plot-action-btn",
                  title = "Descargar mapa"
                )
              )
            ),
            
            div(
              class = "plot-content",
              leafletOutput("basic_education_map", height = "500px")
            )
          )
        ),
        
        # Educación Media Superior Tab
        nav_panel(
          title = "Satisfacción - Media Superior",
          value = "highschool_education",
          
          div(
            class = "plot-card plot-card-bienestar",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Satisfacción con Educación Media Superior por Distrito",
                  create_dynamic_tooltip("highschool_edu_tooltip")
                )
              ),
              div(
                class = "plot-actions",
                downloadButton(
                  "download_highschool_education_map", 
                  "", 
                  icon = icon("download"), 
                  class = "plot-action-btn",
                  title = "Descargar mapa"
                )
              )
            ),
            
            div(
              class = "plot-content",
              leafletOutput("highschool_education_map", height = "500px")
            )
          )
        ),
        
        # Educación Superior Tab
        nav_panel(
          title = "Satisfacción - Superior",
          value = "college_education",
          
          div(
            class = "plot-card plot-card-bienestar",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Satisfacción con Educación Superior por Distrito",
                  create_dynamic_tooltip("college_edu_tooltip")
                )
              ),
              div(
                class = "plot-actions",
                downloadButton(
                  "download_college_education_map", 
                  "", 
                  icon = icon("download"), 
                  class = "plot-action-btn",
                  title = "Descargar mapa"
                )
              )
            ),
            
            div(
              class = "plot-content",
              leafletOutput("college_education_map", height = "500px")
            )
          )
        ),
        
        # Comparativa Tab
        nav_panel(
          title = "Comparativa",
          value = "education_comparison",
          
          div(
            class = "plot-card plot-card-bienestar",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Comparativa de Satisfacción por Nivel Educativo",
                  create_dynamic_tooltip("education_comparison_tooltip")
                )
              )
            ),
            
            div(
              class = "plot-content",
              plotlyOutput("education_comparison_plot", height = "500px")
            )
          )
        )
      )
    ),
    
    # Footer
    create_dashboard_footer()
  )
}