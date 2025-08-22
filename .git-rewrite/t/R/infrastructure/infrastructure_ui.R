# infrastructure_ui.R - Updated to match style guide and wellness_ui pattern
infrastructureUI <- function() {
  svg_icon <- function(filename, class = "nav-card-icon", width = "48", height = "48") {
    svg_path <- file.path("svg", filename)
    
    # Check if file exists
    full_path <- file.path("www", svg_path)
    if (!file.exists(full_path)) {
      warning(paste("SVG file not found:", full_path))
      return(div(class = class, "⚠️"))  # Fallback icon
    }
    
    tags$div(
      class = class,
      tags$img(
        src = svg_path,
        alt = tools::file_path_sans_ext(filename),
        width = width,
        height = height,
        style = "max-width: 100%; height: auto;"
      )
    )
  }
  
  page_fluid(
    class = "section-infraestructura",
    useShinyjs(),
    init_tooltips(),

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
          h1("Infraestructura", class = "dashboard-header")
        )
      )
    ),
    
    # Navigation Cards Section with improved responsive grid
    div(
      class = "section-nav-grid mb-4",
      
      # Public Services Card
      div(
        class = "page-card page-card-infraestructura section-nav-card",
        onclick = "Shiny.setInputValue('nav_target', 'public_services', {priority: 'event'})",
        
        # Card Header
        div(
          class = "page-header",
          div(
            class = "page-header-content",
            h4(class = "page-card-title", "Servicios Públicos"),
            p(class = "page-card-subtitle", "¿Qué tan satisfechos están con los servicios básicos?")
          ),
          div(
            class = "page-icon",
            svg_icon("Cap--50.svg", width = "40", height = "40")
          )
        )
      ),
      
      # Equipment Card
      div(
        class = "page-card page-card-infraestructura section-nav-card",
        onclick = "Shiny.setInputValue('nav_target', 'equipment', {priority: 'event'})",
        
        div(
          class = "page-header",
          div(
            class = "page-header-content",
            h4(class = "page-card-title", "Equipamiento Público"),
            p(class = "page-card-subtitle", "¿Cómo evalúan la infraestructura comunitaria?")
          ),
          div(
            class = "page-icon",
            svg_icon("Cap--38.svg", width = "40", height = "40")
          )
        )
      ),
      
      # Housing Card
      div(
        class = "page-card page-card-infraestructura section-nav-card",
        onclick = "Shiny.setInputValue('nav_target', 'housing', {priority: 'event'})",
        
        div(
          class = "page-header",
          div(
            class = "page-header-content",
            h4(class = "page-card-title", "Vivienda"),
            p(class = "page-card-subtitle", "¿Cómo perciben las condiciones de su vivienda?")
          ),
          div(
            class = "page-icon",
            svg_icon("Cap--38.svg", width = "40", height = "40")
          )
        )
      )
    ),
    
    # PLOT CARDS SECTION - Using responsive grid
    
    # Content section 1: Public Services and Housing
    div(
      class = "plot-cards-grid two-columns",
      
      # Public Services Plot Card
      div(
        class = "plot-card plot-card-infraestructura",
        
        # Plot Header
        div(
          class = "plot-header",
          div(
            class = "plot-header-content",
            h6(
              class = "plot-title",
              "Servicios Públicos: Satisfacción por servicio",
              create_tooltip("<b>ID</b>: PER Q29 - Q56 <br>
                <b>Pregunta</b>: Satisfacción con los servicios en la gráfica <br>
                 <b>Escala</b>: 1-10")
            )
          )
        ),
        
        # Plot Content
        div(
          class = "plot-content",
          plotlyOutput("utilities_plot", height = "500px")
        )
      ),
      
      # Housing Plot Card
      div(
        class = "plot-card plot-card-infraestructura",
        
        # Plot Header with download button
        div(
          class = "plot-header",
          div(
            class = "plot-header-content",
            h6(
              class = "plot-title",
              "Vivienda: Satisfacción por distrito",
              create_tooltip("<b>ID</b>: PER Q25 <br>
                <b>Pregunta</b>: En una escala del 1 al 10, Que tan satisfecho esta con LA CASA EN LA QUE VIVE? <br>
                 <b>Escala</b>: 1-10")
            )
          ),
          div(
            class = "plot-actions",
            downloadButton(
              "download_housing_map", 
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
          leafletOutput("housing_map", height = "500px")
        )
      )
    ),
    
    # Content section 2: Reports chart (full width)
    div(
      class = "plot-cards-grid single-column",
      
      div(
        class = "plot-card plot-card-infraestructura",
        
        div(
          class = "plot-header",
          div(
            class = "plot-header-content",
            h6(
              class = "plot-title",
              "Reportes de Servicios Públicos",
              create_tooltip("<b>ID</b>: PER Q32 Q33 Q37 Q38 Q42 Q43 Q48 Q49 Q52 Q53 <br>
                <b>Pregunta</b>: Durante este año Interpuso algún reporte? El problema fue atendido por la dependencia? <br>
                 <b>Escala</b>: 1=Si;2=No;3=no sabe")
            )
          )
        ),
        
        div(
          class = "plot-content",
          uiOutput("report_statistics_plot")
        )
      )
    )
  )
}