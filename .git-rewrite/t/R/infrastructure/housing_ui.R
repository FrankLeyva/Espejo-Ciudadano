# housing_ui.R - Updated to match style guide and environment_ui pattern
housingUI <- function() {
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
          h1("Vivienda", class = "dashboard-header")
        )
      )
    ),
    
    # Tab Navigation with Plot Cards (Housing Satisfaction Maps)
    div(
      class = "plot-tabs-container",
      
      navset_tab(
        id = "housing_tabs",
        
        # Materials Quality Tab
        nav_panel(
          title = "Calidad de Materiales",
          value = "materials_quality",
          
          div(
            class = "plot-card plot-card-infraestructura",
            
            # Plot Header
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Satisfacción con la Calidad de Materiales por Distrito",
                  create_tooltip("<b>ID</b>: PER Q26.1 <br>
                    <b>Pregunta</b>: En una escala del 1 al 10, qué tan satisfecho está usted con la calidad de los materiales de su vivienda? <br>
                     <b>Escala</b>: 1-10 (1=Muy insatisfecho, 10=Muy satisfecho)")
                )
              ),
              div(
                class = "plot-actions",
                downloadButton(
                  "download_materials_map", 
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
              leafletOutput("materials_map", height = "500px")
            )
          )
        ),
        
        # Size and Spaces Tab
        nav_panel(
          title = "Tamaño y Espacios",
          value = "size_spaces",
          
          div(
            class = "plot-card plot-card-infraestructura",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Satisfacción con el Tamaño y Espacios por Distrito",
                  create_tooltip("<b>ID</b>: PER Q26.2 <br>
                    <b>Pregunta</b>: En una escala del 1 al 10, qué tan satisfecho está usted con el tamaño y espacios de su vivienda? <br>
                     <b>Escala</b>: 1-10 (1=Muy insatisfecho, 10=Muy satisfecho)")
                )
              ),
              div(
                class = "plot-actions",
                downloadButton(
                  "download_spaces_map", 
                  "", 
                  icon = icon("download"), 
                  class = "plot-action-btn",
                  title = "Descargar mapa"
                )
              )
            ),
            
            div(
              class = "plot-content",
              leafletOutput("spaces_map", height = "500px")
            )
          )
        ),
        
        # Location and Accessibility Tab
        nav_panel(
          title = "Ubicación y Accesibilidad",
          value = "location_accessibility",
          
          div(
            class = "plot-card plot-card-infraestructura",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Satisfacción con la Ubicación y Accesibilidad por Distrito",
                  create_tooltip("<b>ID</b>: PER Q26.3 <br>
                    <b>Pregunta</b>: En una escala del 1 al 10, qué tan satisfecho está usted con la ubicación y accesibilidad de su vivienda? <br>
                     <b>Escala</b>: 1-10 (1=Muy insatisfecho, 10=Muy satisfecho)")
                )
              ),
              div(
                class = "plot-actions",
                downloadButton(
                  "download_location_map", 
                  "", 
                  icon = icon("download"), 
                  class = "plot-action-btn",
                  title = "Descargar mapa"
                )
              )
            ),
            
            div(
              class = "plot-content",
              leafletOutput("location_map", height = "500px")
            )
          )
        ),
        
        # Comparative Analysis Tab
        nav_panel(
          title = "Análisis Comparativo",
          value = "comparative_analysis",
          
          div(
            class = "plot-card plot-card-infraestructura",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Comparación de Satisfacción con Aspectos de la Vivienda",
                  create_tooltip("<b>ID</b>: PER Q26.1, Q26.2, Q26.3 <br>
                    <b>Pregunta</b>: Comparación entre satisfacción con materiales, espacios y ubicación de la vivienda <br>
                     <b>Escala</b>: 1-10 (1=Muy insatisfecho, 10=Muy satisfecho)")
                )
              )
            ),
            
            div(
              class = "plot-content",
              plotlyOutput("housing_comparison_plot", height = "500px")
            )
          )
        )
      )
    )
  )
}