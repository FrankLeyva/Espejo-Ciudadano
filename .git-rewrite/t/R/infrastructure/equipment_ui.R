# equipment_ui.R - Updated to match style guide while preserving sidebar and value boxes functionality
equipmentUI <- function() {
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
          h1("Equipamiento Público", class = "dashboard-header")
        )
      )
    ),
    
    # Main layout with enhanced sidebar
    layout_sidebar(
      sidebar = sidebar(
        title = div(
          class = "sidebar-title",
          style = "color: var(--infraestructura-color); font-weight: 600; margin-bottom: 1rem;",
          "Seleccionar Equipamiento"
        ),
        width = 320,  # Slightly wider for longer equipment names
        
        # Enhanced equipment selection with better styling
        div(
          class = "equipment-selection-container",
          style = "background: #f8f9fa; padding: 1rem; border-radius: 8px; border-left: 4px solid var(--infraestructura-color);",
          
          div(
            class = "equipment-list",
            radioButtons(
              "selected_equipment",
              label = div(
                style = "font-weight: 600; color: #495057; margin-bottom: 0.75rem;",
                "Equipamiento a evaluar:"
              ),
              choices = c(
                "Alumbrado Público" = "Q45",
                "Calles y Pavimentación" = "Q51",
                "Semaforización" = "Q55",
                "Áreas verdes y Espacios públicos" = "Q56",
                "Unidades deportivas" = "Q58",
                "Bibliotecas" = "Q59",
                "Centros comunitarios" = "Q60",
                "Banquetas" = "Q61",
                "Espacios para personas con discapacidad" = "Q62"
              ),
              selected = "Q45"
            )
          ),
          
          # Equipment description helper
          div(
            class = "equipment-help-text",
            style = "margin-top: 1rem; padding: 0.75rem; background: white; border-radius: 6px; font-size: 0.875rem; color: #6c757d;",
            "Seleccione un tipo de equipamiento para ver su evaluación por distrito en el mapa."
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
              textOutput("equipment_title", inline = TRUE),
              create_dynamic_tooltip("equipment_tooltip")
            )
          ),
          div(
            class = "plot-actions",
            downloadButton(
              "download_equipment_map", 
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
          leafletOutput("equipment_map", height = "600px")
        )
      )
    ),
    
    # Special detailed evaluation for green areas and public spaces
    conditionalPanel(
      condition = "input.selected_equipment == 'Q56'",
      div(
        class = "mt-4",
        div(
          class = "plot-card plot-card-infraestructura",
          
          # Plot Header for value boxes section
          div(
            class = "plot-header",
            div(
              class = "plot-header-content",
              h6(
                class = "plot-title",
                "Evaluación Detallada: Áreas Verdes y Espacios Públicos",
                create_tooltip("<b>ID</b>: PER Q57.1-Q57.4 <br>
                  <b>Pregunta</b>: Evaluación específica de diferentes aspectos de áreas verdes <br>
                   <b>Escala</b>: 1-10 (1=Muy insatisfecho, 10=Muy satisfecho)")
              )
            )
          ),
          
          # Plot Content with value boxes
          div(
            class = "plot-content with-padding",
            layout_columns(
              col_widths = c(3, 3, 3, 3),
              
              value_box_with_title_tooltip(
                title = "Equipamiento",
                value = textOutput("green_areas_equipment"),
                showcase = bsicons::bs_icon("tools"),
                theme = value_box_theme(bg = "#2A9D8F", fg = "white"),
                tooltip_text = "<b>ID</b>: PER Q57.1 <br>
                       <b>Pregunta</b>: Satisfacción con el equipamiento de áreas verdes <br>
                        <b>Escala</b>: 1-10 (1=Muy insatisfecho, 10=Muy satisfecho)"
              ),
              
              value_box_with_title_tooltip(
                title = "Iluminación",
                value = textOutput("green_areas_lighting"),
                showcase = bsicons::bs_icon("lightbulb-fill"),
                theme = value_box_theme(bg = "#6969B3", fg = "white"),
                tooltip_text = "<b>ID</b>: PER Q57.2 <br>
                       <b>Pregunta</b>: Satisfacción con la iluminación de áreas verdes <br>
                        <b>Escala</b>: 1-10 (1=Muy insatisfecho, 10=Muy satisfecho)"
              ),
              
              value_box_with_title_tooltip(
                title = "Cuidado y Mantenimiento",
                value = textOutput("green_areas_maintenance"),
                showcase = bsicons::bs_icon("brush"),
                theme = value_box_theme(bg = "#F4A261", fg = "white"),
                tooltip_text = "<b>ID</b>: PER Q57.3 <br>
                       <b>Pregunta</b>: Satisfacción con el cuidado (limpieza y mantenimiento) <br>
                        <b>Escala</b>: 1-10 (1=Muy insatisfecho, 10=Muy satisfecho)",
                force_icon_color = "rgba(255, 255, 255, 0.8)"
              ),
              
              value_box_with_title_tooltip(
                title = "Seguridad",
                value = textOutput("green_areas_security"),
                showcase = bsicons::bs_icon("shield-lock"),
                theme = value_box_theme(bg = "#E86486", fg = "white"),
                tooltip_text = "<b>ID</b>: PER Q57.4 <br>
                       <b>Pregunta</b>: Satisfacción con la seguridad en áreas verdes <br>
                        <b>Escala</b>: 1-10 (1=Muy insatisfecho, 10=Muy satisfecho)"
              )
            )
          )
        )
      )
    )
  )
}