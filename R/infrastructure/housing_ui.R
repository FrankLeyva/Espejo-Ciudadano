# UI para Dashboard de Vivienda
housingUI <- function() {
  page_fluid(
    useShinyjs(),
      
    tags$head(
      tags$link(
        rel = "stylesheet", 
        href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.5/font/bootstrap-icons.css"
      ),
      tags$style(HTML("
        /* General styling */
        .nav-tabs .nav-link.active {
          font-weight: bold;
          color: #0d6efd;
          background-color: #f8f9fa;
          border-bottom: 3px solid #0d6efd;
        }
        
        .nav-tabs .nav-link {
          color: #495057;
          transition: all 0.3s ease;
        }
        
        .nav-tabs .nav-link:hover {
          background-color: #f1f1f1;
        }
        
        /* Custom info box styles */
        .info-box {
          padding: 15px;
          border-radius: 5px;
          margin-bottom: 15px;
          display: flex;
          align-items: flex-start;
        }
        
        .info-box-icon {
          margin-right: 15px;
          font-size: 24px;
          padding-top: 3px;
        }
        
        .info-box-content {
          flex-grow: 1;
        }
        
        .info-box-title {
          font-weight: bold;
          margin-bottom: 10px;
          font-size: 16px;
        }
        
        .info-box-value {
          font-size: 18px;
          line-height: 1.4;
        }
        
        .info-box-info {
          background-color: #d1ecf1;
          color: #0c5460;
        }
        
        .housing-intro {
          background-color: #f8f9fa;
          padding: 20px;
          border-radius: 5px;
          margin-bottom: 20px;
        }
      "))
    ),
    
    theme = bs_theme(
      version = 5,
      bootswatch = "litera",
      primary = "#0d6efd"
    ),
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
    # Encabezado
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          h2("Satisfacción con la Vivienda", class = "text-center")
        )
      )
    ),
    
    # Introducción
    card(
      div(
        class = "housing-intro",
        h4("Evaluación de Satisfacción con la Vivienda"),
        p("En la encuesta de Percepción Ciudadana 2024, preguntamos a los ciudadanos sobre su nivel de satisfacción con diferentes aspectos de su vivienda en una escala del 1 al 10, donde 1 representa 'Nada satisfecho' y 10 representa 'Muy satisfecho'."),
        p("Este dashboard permite explorar los resultados por distrito.")
      )
    ),
    
    # Pestañas para los diferentes aspectos de vivienda
    navset_tab(
      id = "housing_tabs",
      
      # Pestaña: Calidad de materiales
      nav_panel(
        title = "Calidad de Materiales",
        icon = bsicons::bs_icon("bricks"),
        
        card(
          div(
            class = "info-box info-box-info",
            div(class = "info-box-icon", bsicons::bs_icon("bricks")),
            div(
              class = "info-box-content",
              div(class = "info-box-title", "Calidad de Materiales"),
              div(class = "info-box-value", "Esta sección muestra la satisfacción de los ciudadanos con la calidad de los materiales de construcción de sus viviendas, como paredes, techos, pisos, ventanas y puertas.")
            )
          )
        ),
        
        card(
          card_header(
            "Satisfacción con la Calidad de los Materiales por Distrito",
            class = "bg-light"
          ),
          leafletOutput("materials_map", height = "500px")
        ),
        
        layout_columns(
          col_widths = c(6, 6),
          value_box(
            title = "Promedio General",
            value = textOutput("materials_avg"),
            showcase = bsicons::bs_icon("bar-chart"),
            theme = value_box_theme(bg = "#2A9D8F", fg = "white")
          ),
          value_box(
            title = "Distrito con Mayor Satisfacción",
            value = textOutput("materials_best_district"),
            showcase = bsicons::bs_icon("trophy"),
            theme = value_box_theme(bg = "#E9C46A", fg = "white")
          )
        )
      ),
      
      # Pestaña: Tamaño y espacios
      nav_panel(
        title = "Tamaño y Espacios",
        icon = bsicons::bs_icon("layout-text-window"),
        
        card(
          div(
            class = "info-box info-box-info",
            div(class = "info-box-icon", bsicons::bs_icon("layout-text-window")),
            div(
              class = "info-box-content",
              div(class = "info-box-title", "Tamaño y Espacios"),
              div(class = "info-box-value", "Esta sección muestra la satisfacción de los ciudadanos con el tamaño y la distribución de espacios en sus viviendas, incluyendo el número de habitaciones y áreas comunes.")
            )
          )
        ),
        
        card(
          card_header(
            "Satisfacción con el Tamaño y Espacios por Distrito",
            class = "bg-light"
          ),
          leafletOutput("spaces_map", height = "500px")
        )
      ),
      
      # Pestaña: Accesibilidad de la ubicación
      nav_panel(
        title = "Ubicación y Accesibilidad",
        icon = bsicons::bs_icon("geo-alt"),
        
        card(
          div(
            class = "info-box info-box-info",
            div(class = "info-box-icon", bsicons::bs_icon("geo-alt")),
            div(
              class = "info-box-content",
              div(class = "info-box-title", "Ubicación y Accesibilidad"),
              div(class = "info-box-value", "Esta sección muestra la satisfacción de los ciudadanos con la ubicación de sus viviendas y la accesibilidad a servicios básicos, transporte y lugares de interés.")
            )
          )
        ),
        
        card(
          card_header(
            "Satisfacción con la Ubicación y Accesibilidad por Distrito",
            class = "bg-light"
          ),
          leafletOutput("location_map", height = "500px")
        )
      ),
      
      # Pestaña: Comparativa
      nav_panel(
        title = "Comparativa",
        icon = bsicons::bs_icon("graph-up"),
        
        card(
          card_header(
            "Comparación de Satisfacción entre Aspectos de Vivienda",
            class = "bg-light"
          ),
          plotlyOutput("comparison_plot", height = "500px")
        ),
        
        card(
          card_header("Resumen de Resultados"),
          p("Esta visualización muestra una comparación de los niveles de satisfacción promedio entre los tres aspectos evaluados: calidad de materiales, tamaño y espacios, y ubicación/accesibilidad. Los resultados están desglosados por distrito para facilitar la identificación de patrones y áreas de oportunidad."),
          p("Un valor más alto (más cercano a 10) indica mayor satisfacción, mientras que un valor más bajo (más cercano a 1) indica menor satisfacción.")
        )
      )
    ),
    
    # Pie de página
    card(
      p("Datos obtenidos de la Encuesta de Percepción Ciudadana 2024", class = "text-center text-muted"),
      p("Última actualización: Marzo 2025", class = "text-center text-muted")
    )
  )
}