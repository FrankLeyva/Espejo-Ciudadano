# participation_ui.R - Updated to match style guide and wellness_ui pattern

participationUI <- function() {
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
    class = "section-participacion",
    useShinyjs(),
    init_tooltips(),

    # Header
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          style = paste0("background-color: var(--participacion-color) !important; 
            color: white !important; 
            font-family: var(--font-display) !important;
            font-weight: bolder !important; 
            text-align: center !important; 
            border-bottom: none !important;"),
          h1("Participación Ciudadana", class = "dashboard-header")
        )
      )
    ),
    
    # Navigation Cards Section - using new responsive grid
    div(
      class = "section-nav-grid mb-4",
      
      # Civic Participation Card
      div(
        class = "page-card page-card-participacion section-nav-card",
        onclick = "Shiny.setInputValue('nav_target', 'civic', {priority: 'event'})",
        
        # Card Header
        div(
          class = "page-header",
          div(
            class = "page-header-content",
            h4(class = "page-card-title", "Participación Cívica"),
            p(class = "page-card-subtitle", "¿Qué tan interesados están los ciudadanos en la política municipal?")
          ),
          div(
            class = "page-icon",
            svg_icon("Cap--42.svg", width = "40", height = "40")
          )
        )
      ),
      
      # Community Participation Card
      div(
        class = "page-card page-card-participacion section-nav-card",
        onclick = "Shiny.setInputValue('nav_target', 'community', {priority: 'event'})",
        
        # Card Header
        div(
          class = "page-header",
          div(
            class = "page-header-content",
            h4(class = "page-card-title", "Participación Comunitaria"),
            p(class = "page-card-subtitle", "¿Cómo participan los ciudadanos en organizaciones y actividades comunitarias?")
          ),
          div(
            class = "page-icon",
            svg_icon("Cap--43.svg", width = "40", height = "40")
          )
        )
      )
    ),
    
    # Value Box Section - following cultural_ui pattern
    layout_columns(
      col_widths = 12,
      
      # Social Movement Support value box with tooltip
      value_box_with_title_tooltip(
        title = "Apoyo activo a movimientos sociales",
        value = textOutput("social_movement_support"),
        showcase = bsicons::bs_icon("megaphone-fill"),
        theme = value_box_theme(bg = "#ffc9de", fg = "white"),
        tooltip_text = "<b>ID</b>: PAR Q134 <br>
          <b>Pregunta</b>: Durante el año: ¿Apoyó activamente a algún movimiento social? <br>
           <b>Escala</b>: 1=Sí; 2=No"
      )
    ),
    
    # PLOT CARDS SECTION - Using responsive grid like wellness_ui
    
    # Content section 1: Voting importance map and political interest
    div(
      class = "plot-cards-grid two-columns",
      
      # Voting Importance Map Plot Card
      div(
        class = "plot-card plot-card-participacion",
        
        # Plot Header with download button
        div(
          class = "plot-header",
          div(
            class = "plot-header-content",
            h6(
              class = "plot-title",
              "Importancia del voto por distrito",
              create_tooltip("<b>ID</b>: PAR Q137 <br>
                <b>Pregunta</b>: Para usted, votar es... <br>
                 <b>Escala</b>: 1=Importante; 2=Poco importante; 3=Nada importante")
            )
          ),
          div(
            class = "plot-actions",
            downloadButton(
              "download_voting_map", 
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
          leafletOutput("voting_map", height = "450px")
        )
      ),
      
      # Political Interest Plot Card
      div(
        class = "plot-card plot-card-participacion",
        
        # Plot Header
        div(
          class = "plot-header",
          div(
            class = "plot-header-content",
            h6(
              class = "plot-title",
              "Interés en participar en política municipal",
              create_tooltip("<b>ID</b>: PAR Q130 <br>
                <b>Pregunta</b>: ¿Qué tanto le interesa participar en la política del municipio, interviniendo en decisiones o realizando acciones de interés público en beneficio de la ciudad? <br>
                 <b>Escala</b>: 1=NADA; 2=POCO; 3=REGULAR; 4=ALGO; 5=MUCHO")
            )
          )
        ),
        
        # Plot Content
        div(
          class = "plot-content",
          plotlyOutput("interest_pie", height = "450px")
        )
      )
    )
  )
}