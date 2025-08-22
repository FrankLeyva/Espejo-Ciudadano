# government_ui.R - Updated to match new styling system
governmentUI <- function() {
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
    class = "section-gobierno",
    useShinyjs(),
    init_tooltips(),

    # Header
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          style = paste0("background-color: var(--gobierno-color) !important; 
            color: white !important; 
            font-family: var(--font-display) !important;
            font-weight: bolder !important; 
            text-align: center !important; 
            border-bottom: none !important;"),
          h1("Instituciones", class = "dashboard-header")
        )
      )
    ),
    
    # Navigation Cards Section
    div(
      class = "section-nav-grid",
      
      # Inequality Card
      div(
        class = "page-card page-card-gobierno section-nav-card",
        onclick = "Shiny.setInputValue('nav_target', 'inequality', {priority: 'event'})",
        
        div(
          class = "page-header",
          div(
            class = "page-header-content",
            h4(class = "page-card-title", "Desigualdad"),
            p(class = "page-card-subtitle", "¿Cómo perciben los ciudadanos la desigualdad en la ciudad?")
          ),
          div(
            class = "page-icon",
            svg_icon("Cap--12.svg", width = "40", height = "40")
          )
        )
      ),
      
      # Trust Card
      div(
        class = "page-card page-card-gobierno section-nav-card",
        onclick = "Shiny.setInputValue('nav_target', 'trust', {priority: 'event'})",
        
        div(
          class = "page-header",
          div(
            class = "page-header-content",
            h4(class = "page-card-title", "Confianza Institucional"),
            p(class = "page-card-subtitle", "¿Qué tan confiables son las instituciones públicas?")
          ),
          div(
            class = "page-icon",
            svg_icon("Cap--44.svg", width = "40", height = "40")
          )
        )
      ),
      
      # Accountability Card
      div(
        class = "page-card page-card-gobierno section-nav-card",
        onclick = "Shiny.setInputValue('nav_target', 'accountability', {priority: 'event'})",
        
        div(
          class = "page-header",
          div(
            class = "page-header-content",
            h4(class = "page-card-title", "Rendición de Cuentas"),
            p(class = "page-card-subtitle", "¿Cómo evalúan la transparencia gubernamental?")
          ),
          div(
            class = "page-icon",
            svg_icon("Cap--45.svg", width = "40", height = "40")
          )
        )
      ),
      
      # Political Representation Card
      div(
        class = "page-card page-card-gobierno section-nav-card",
        onclick = "Shiny.setInputValue('nav_target', 'representation', {priority: 'event'})",
        
        div(
          class = "page-header",
          div(
            class = "page-header-content",
            h4(class = "page-card-title", "Representación Política"),
            p(class = "page-card-subtitle", "¿Conocen los ciudadanos a sus representantes?")
          ),
          div(
            class = "page-icon",
            svg_icon("Cap--46.svg", width = "40", height = "40")
          )
        )
      ),
      
      # Government Expectations Card
      div(
        class = "page-card page-card-gobierno section-nav-card",
        onclick = "Shiny.setInputValue('nav_target', 'expectations', {priority: 'event'})",
        
        div(
          class = "page-header",
          div(
            class = "page-header-content",
            h4(class = "page-card-title", "Expectativas de Gobierno"),
            p(class = "page-card-subtitle", "¿Qué esperan los ciudadanos de sus gobiernos?")
          ),
          div(
            class = "page-icon",
            svg_icon("Cap--18.svg", width = "40", height = "40")
          )
        )
      )
    ),
    
    # Content Section 1: Knowledge of Officials (Tabbed Content)
    div(
      class = "plot-tabs-container",
      
      navset_tab(
        id = "officials_knowledge_tabs",
        
        # Regidor Tab
        nav_panel(
          title = "Conocimiento de Regidor/a",
          value = "regidor_knowledge",
          
          div(
            class = "plot-card plot-card-gobierno",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Conocimiento del Regidor/a por Distrito",
                  create_tooltip("<b>ID</b>: PAR Q16.3 <br>
                    <b>Pregunta</b>: ¿Conoce usted el nombre de su regidor/a? <br>
                     <b>Escala</b>: 1=Sí; 2=No")
                )
              )
            ),
            
            div(
              class = "plot-content",
              plotlyOutput("officials_knowledge_regidor_plot", height = "450px")
            )
          )
        ),
        
        # Sindico Tab
        nav_panel(
          title = "Conocimiento de Síndico/a",
          value = "sindico_knowledge",
          
          div(
            class = "plot-card plot-card-gobierno",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Conocimiento del Síndico/a por Distrito",
                  create_tooltip("<b>ID</b>: PAR Q16.4 <br>
                    <b>Pregunta</b>: ¿Conoce usted el nombre de su síndico/a? <br>
                     <b>Escala</b>: 1=Sí; 2=No")
                )
              )
            ),
            
            div(
              class = "plot-content",
              plotlyOutput("officials_knowledge_sindico_plot", height = "450px")
            )
          )
        ),
        
        # Local/State Deputy Tab
        nav_panel(
          title = "Diputado/a Local/Estatal",
          value = "diputado_local_knowledge",
          
          div(
            class = "plot-card plot-card-gobierno",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Conocimiento del Diputado/a Local/Estatal por Distrito",
                  create_tooltip("<b>ID</b>: PAR Q16.2 <br>
                    <b>Pregunta</b>: ¿Conoce usted el nombre de su diputado/a local y/o estatal? <br>
                     <b>Escala</b>: 1=Sí; 2=No")
                )
              )
            ),
            
            div(
              class = "plot-content",
              plotlyOutput("officials_knowledge_dipupadol_plot", height = "450px")
            )
          )
        ),
        
        # Federal Deputy Tab
        nav_panel(
          title = "Diputado/a Federal",
          value = "diputado_federal_knowledge",
          
          div(
            class = "plot-card plot-card-gobierno",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Conocimiento del Diputado/a Federal por Distrito",
                  create_tooltip("<b>ID</b>: PAR Q16.1 <br>
                    <b>Pregunta</b>: ¿Conoce usted el nombre de su diputado/a federal? <br>
                     <b>Escala</b>: 1=Sí; 2=No")
                )
              )
            ),
            
            div(
              class = "plot-content",
              plotlyOutput("officials_knowledge_diputadof_plot", height = "450px")
            )
          )
        )
      )
    ),
    
    # Content Section 2: Main Government Indicators
    div(
      class = "plot-cards-grid two-columns",
      
      # Inequality Perception Plot Card
      div(
        class = "plot-card plot-card-gobierno",
        
        div(
          class = "plot-header",
          div(
            class = "plot-header-content",
            h6(
              class = "plot-title",
              "Percepción de la Desigualdad en Ciudad Juárez",
              create_tooltip("<b>ID</b>: PER Q87 <br>
                <b>Pregunta</b>: Por distintos motivos, no todas las personas que habitan en Juárez pueden acceder en condiciones de igualdad a los bienes y servicios, ni tienen las mismas oportunidades en la vida. ¿Cómo describiría la desigualdad que se vive hoy en día? <br>
                 <b>Escala</b>: 1=Muy alta; 2=Alta; 3=Media; 4=Baja; 5=Muy baja; 6=No sabe/No contestó")
            )
          )
        ),
        
        div(
          class = "plot-content",
          plotlyOutput("inequality_perception_plot", height = "450px")
        )
      ),
      
      # Government Expectations Plot Card
      div(
        class = "plot-card plot-card-gobierno",
        
        div(
          class = "plot-header",
          div(
            class = "plot-header-content",
            h6(
              class = "plot-title",
              "Expectativas Ciudadanas sobre el Gobierno",
              create_tooltip("<b>ID</b>: PAR Q19, Q20, Q21 <br>
                <b>Pregunta</b>: ¿Cómo calificaría la expectativa que tiene en este momento del gobierno Municipal/Estatal/Federal? <br>
                 <b>Escala</b>: 1-10")
            )
          )
        ),
        
        div(
          class = "plot-content",
          plotlyOutput("government_expectations_plot", height = "450px")
        )
      )
    ),
    
    # Content Section 3: Important Problems (Full Width)
    div(
      class = "plot-cards-grid single-column",
      
      div(
        class = "plot-card plot-card-gobierno",
        
        div(
          class = "plot-header",
          div(
            class = "plot-header-content",
            h6(
              class = "plot-title",
              "Principales Problemas de Ciudad Juárez",
              create_tooltip("<b>ID</b>: PER Q81, Q82 <br>
                <b>Pregunta</b>: Para usted, ¿cuáles son los 2 problemas más importantes de Juárez? <br>
                 <b>Escala</b>: 19 categorías")
            )
          )
        ),
        
        div(
          class = "plot-content",
          plotlyOutput("important_problems_plot", height = "550px")
        )
      )
    )
  )
}