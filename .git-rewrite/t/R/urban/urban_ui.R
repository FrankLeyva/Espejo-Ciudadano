# urban_ui.R - Updated to follow new styling patterns
urbanUI <- function() {
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
    class = "section-movilidad",
    useShinyjs(),
    init_tooltips(),

    # Header - following wellness_ui pattern
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          style = paste0("background-color: var(--movilidad-color) !important; 
            color: white !important; 
            font-family: var(--font-display) !important;
            font-weight: bolder !important; 
            text-align: center !important; 
            border-bottom: none !important;"),
          h1("Movilidad Urbana", class = "dashboard-header")
        )
      )
    ),
    
    # Navigation Cards Section - following wellness_ui responsive grid pattern
    div(
      class = "section-nav-grid mb-4",
      
      # Mobility Card
      div(
        class = "page-card page-card-movilidad section-nav-card",
        onclick = "Shiny.setInputValue('nav_target', 'mobility', {priority: 'event'})",
        
        # Card Header
        div(
          class = "page-header",
          div(
            class = "page-header-content",
            h4(class = "page-card-title", "Movilidad"),
            p(class = "page-card-subtitle", "¿Cómo se mueven los ciudadanos por la ciudad?")
          ),
          div(
            class = "page-icon",
            svg_icon("Cap--51.svg", width = "40", height = "40")
          )
        )
      ),
      
      # Transportation Card
      div(
        class = "page-card page-card-movilidad section-nav-card",
        onclick = "Shiny.setInputValue('nav_target', 'transportation', {priority: 'event'})",
        
        div(
          class = "page-header",
          div(
            class = "page-header-content",
            h4(class = "page-card-title", "Transporte Público"),
            p(class = "page-card-subtitle", "¿Qué tan satisfechos están con el servicio?")
          ),
          div(
            class = "page-icon",
            svg_icon("Cap--17.svg", width = "40", height = "40")
          )
        )
      )
    ),
    
    # Value boxes section - following cultural_ui pattern with proper grid
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      
      # Public transport usage
      value_box_with_title_tooltip(
        title = "Uso de Transporte Público",
        value = textOutput("public_transport_work"),
        showcase = bsicons::bs_icon("bus-front"),
        theme = value_box_theme(bg = "#FFA058", fg = "white"),
        tooltip_text = "<b>ID</b>: PER Q72.9 <br>
               <b>Pregunta</b>: Camion/Rutera/ Autobus  <br>
                <b>Escala</b>: 1=Sí; 2=No"
      ),
      
      # Private vehicle usage
      value_box_with_title_tooltip(
        title = "Uso de Vehículo Particular",
        value = textOutput("private_vehicle_usage"),
        showcase = bsicons::bs_icon("car-front"),
        theme = value_box_theme(bg = "#FEC08E", fg = "white"),
        tooltip_text = "<b>ID</b>: PER Q172.8 <br>
               <b>Pregunta</b>: Vehículo propio  <br>
                <b>Escala</b>: 1=Sí; 2=No",
        force_icon_color = "rgba(255, 255, 255, 0.8)"
      ),
      
      # Bus satisfaction
      value_box_with_title_tooltip(
        title = "Satisfacción con Camión/Rutera",
        value = textOutput("bus_satisfaction"),
        showcase = bsicons::bs_icon("star-half"),
        theme = value_box_theme(bg = "#FFA058", fg = "white"),
        tooltip_text = "<b>ID</b>: PER Q75 <br>
               <b>Pregunta</b>: En una escala del 1 al 10, que tan satisfecho está con la calidad del servicio del camión/rutera?  <br>
                <b>Escala</b>: 1-10"
      ),
      
      # Juarez Bus satisfaction
      value_box_with_title_tooltip(
        title = "Satisfacción con Juárez Bus",
        value = textOutput("juarez_bus_satisfaction"),
        showcase = bsicons::bs_icon("star-half"),
        theme = value_box_theme(bg = "#FEC08E", fg = "white"),
        tooltip_text = "<b>ID</b>: PER Q78 <br>
               <b>Pregunta</b>: En una escala del 1 al 10, que tan satisfecho está con la calidad del servicio del BravoBus/EcoBus/ViveBus/Juarez Bus?  <br>
                <b>Escala</b>: 1-10",
        force_icon_color = "rgba(255, 255, 255, 0.8)"
      )
    ),
    
    # Main content with plot card - following new plot card pattern
    div(
      class = "plot-cards-grid single-column",
      
      div(
        class = "plot-card plot-card-movilidad",
        
        # Plot Header
        div(
          class = "plot-header",
          div(
            class = "plot-header-content",
            h6(
              class = "plot-title",
              "Satisfacción con Aspectos Ambientales",
              create_tooltip("<b>ID</b>: PER Q89, Q90, Q91, Q92 <br>
                <b>Pregunta</b>: En una escala del 1 al 10, que tan satisfecho está con LA CALIDAD DEL AIRE/ LA CANTIDAD DE ÁRBOLES EN LA CIUDAD /  LA LIMPIEZA EN LAS CALLES (basura en las calles) / LA CALIDAD DEL AGUA? (CONSIDERAR SI ESTÁ SUCIA, CONTAMINADA O TIENE MAL SABOR) ? <br>
                 <b>Escala</b>: 1-10")
            )
          )
        ),
        
        # Plot Content
        div(
          class = "plot-content",
          plotlyOutput("env_quality_plot", height = "500px")
        )
      )
    )
  )
}