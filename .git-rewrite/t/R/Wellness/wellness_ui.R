# wellness_ui.R - Improved responsive version
wellnessUI <- function() {
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
    class = "section-bienestar",
    useShinyjs(),
    init_tooltips(),

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
          h1("Calidad de Vida", class = "dashboard-header")
        )
      )
    ),
    
    # Navigation Cards Section with improved responsive grid
    div(
      class = "section-nav-grid has-many-cards mb-4",
      
      # Economy Card
      div(
        class = "page-card page-card-bienestar section-nav-card",
        onclick = "Shiny.setInputValue('nav_target', 'economic', {priority: 'event'})",
        
        # Card Header
        div(
          class = "page-header",
          div(
            class = "page-header-content",
            h4(class = "page-card-title", "Economía"),
            p(class = "page-card-subtitle", "¿Cómo perciben los juarenses su situación económica?")
          ),
          div(
            class = "page-icon",
            svg_icon("Cap--39.svg", width = "40", height = "40")
          )
        )
      ),
      
      # Cultural Card
      div(
        class = "page-card page-card-bienestar section-nav-card",
        onclick = "Shiny.setInputValue('nav_target', 'cultural', {priority: 'event'})",
        
        div(
          class = "page-header",
          div(
            class = "page-header-content",
            h4(class = "page-card-title", "Participación Cultural"),
            p(class = "page-card-subtitle", "¿Qué actividades culturales realizan los ciudadanos?")
          ),
          div(
            class = "page-icon",
            svg_icon("Cap--41.svg", width = "40", height = "40")
          )
        )
      ),
      
      # Identity Card
      div(
        class = "page-card page-card-bienestar section-nav-card",
        onclick = "Shiny.setInputValue('nav_target', 'identity', {priority: 'event'})",
        
        div(
          class = "page-header",
          div(
            class = "page-header-content",
            h4(class = "page-card-title", "Identidad y Pertenencia"),
            p(class = "page-card-subtitle", "¿Qué tan conectados se sienten con su ciudad?")
          ),
          div(
            class = "page-icon",
            svg_icon("Cap--40.svg", width = "40", height = "40")
          )
        )
      ),
      
      # Environment Card
      div(
        class = "page-card page-card-bienestar section-nav-card",
        onclick = "Shiny.setInputValue('nav_target', 'environment', {priority: 'event'})",
        
        div(
          class = "page-header",
          div(
            class = "page-header-content",
            h4(class = "page-card-title", "Medio Ambiente"),
            p(class = "page-card-subtitle", "¿Cómo evalúan la calidad ambiental de su entorno?")
          ),
          div(
            class = "page-icon",
            svg_icon("Cap--49.svg", width = "40", height = "40")
          )
        )
      ),
      
      # Education Card
      div(
        class = "page-card page-card-bienestar section-nav-card",
        onclick = "Shiny.setInputValue('nav_target', 'education', {priority: 'event'})",
        
        div(
          class = "page-header",
          div(
            class = "page-header-content",
            h4(class = "page-card-title", "Educación"),
            p(class = "page-card-subtitle", "¿Cómo perciben la calidad educativa en la ciudad?")
          ),
          div(
            class = "page-icon",
            svg_icon("Cap--47.svg", width = "40", height = "40")
          )
        )
      ),
      
      # Healthcare Card
      div(
        class = "page-card page-card-bienestar section-nav-card",
        onclick = "Shiny.setInputValue('nav_target', 'healthcare', {priority: 'event'})",
        
        div(
          class = "page-header",
          div(
            class = "page-header-content",
            h4(class = "page-card-title", "Salud"),
            p(class = "page-card-subtitle", "¿Qué tan satisfechos están con los servicios de salud?")
          ),
          div(
            class = "page-icon",
            svg_icon("Cap--37.svg", width = "40", height = "40")
          )
        )
      )
    ),
    
    # PLOT CARDS SECTION - Using responsive grid
    
    # Content section 1: Economic situation and Migration intention
    div(
      class = "plot-cards-grid two-columns",
      
      # Economic Situation Plot Card
      div(
        class = "plot-card plot-card-bienestar",
        
        # Plot Header
        div(
          class = "plot-header",
          div(
            class = "plot-header-content",
            h6(
              class = "plot-title",
              "Percepción de situación económica personal",
              create_tooltip("<b>ID</b>: PER Q4 <br>
                 <b>Pregunta</b>: En comparación con el año pasado, Diría usted que su situación económica personal ha mejorado, ha empeorado o sigue igual? <br>
                  <b>Escala</b>: 1=Empeorado mucho; 2=Empeorado algo; 3=Igual; 4=Mejorado algo; 5=Mejorado mucho; 6=NS/NC")
            )
          )
        ),
        
        # Plot Content
        div(
          class = "plot-content",
          plotlyOutput("economic_situation_pie", height = "450px")
        )
      ),
      
      # Migration Intention Plot Card
      div(
        class = "plot-card plot-card-bienestar",
        
        # Plot Header with download button
        div(
          class = "plot-header",
          div(
            class = "plot-header-content",
            h6(
              class = "plot-title",
              "Frecuencia con que piensa en irse de la ciudad",
              create_tooltip("<b>ID</b>: PER Q5 <br>
                 <b>Pregunta</b>: Con el total del ingreso familiar, usted diría que...? <br>
                  <b>Escala</b>: 1=Les alcanza bien y pueden ahorrar; 2=Les alcanza justo, sin grandes dificultades; 3=No les alcanza y tienen dificultades; 4=No les alcanza y tienen grandes dificultades; 5=NS/NC")
            )
          ),
          div(
            class = "plot-actions",
            downloadButton(
              "download_migration_map", 
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
          leafletOutput("migration_intention_map", height = "450px")
        )
      )
    ),
    
    # Content section 2: Activities chart (full width)
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
              "Actividades realizadas en los últimos 3 meses",
              create_tooltip("<b>ID</b>: PER Q16 <br>
                 <b>Pregunta</b>: ¿Qué actividades culturales, recreativas realizó en la ciudad en los últimos tres meses?  <br>
                  <b>Escala</b>: 1=Sí; 2=No")
            )
          )
        ),
        
        div(
          class = "plot-content",
          plotlyOutput("activities_chart", height = "550px")
        )
      )
    ),

    # Content section 3: Education and Healthcare
    div(
      class = "plot-cards-grid two-columns",
      
      # Education Plot Card
      div(
        class = "plot-card plot-card-bienestar",
        
        div(
          class = "plot-header",
          div(
            class = "plot-header-content",
            h6(
              class = "plot-title",
              "Educación: Hogares con estudiantes por distrito",
              create_tooltip("<b>ID</b>: PER Q6 <br>
                <b>Pregunta</b>: En su familia, hay por lo menos 1 o más estudiantes de cualquier nivel educativo?v <br>
                 <b>Escala</b>: 1=Sí; 2=No")
            )
          ),
          div(
            class = "plot-actions",
            downloadButton(
              "download_gen_students_map", 
              "", 
              icon = icon("download"), 
              class = "plot-action-btn",
              title = "Descargar mapa"
            )
          )
        ),
        
        div(
          class = "plot-content",
          leafletOutput("education_plot", height = "400px")
        )
      ),
      
      # Healthcare Plot Card
      div(
        class = "plot-card plot-card-bienestar",
        
        div(
          class = "plot-header",
          div(
            class = "plot-header-content",
            h6(
              class = "plot-title",
              "Salud: Satisfacción con servicios de salud",
              create_tooltip("<b>ID</b>: PER Q19 <br>
                <b>Pregunta</b>: En una escala del 1 al 10, Que tan satisfecho/a esta en GENERAL con los servicios de salud que recibe del servicio medico que mas USA? <br>
                 <b>Escala</b>: 1-10")
            )
          )
        ),
        
        div(
          class = "plot-content",
          plotlyOutput("healthcare_plot", height = "400px")
        )
      )
    )
  )
}