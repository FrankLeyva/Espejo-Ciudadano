# UI para Dashboard de Servicios Públicos
publicServicesUI <- function() {
  page_fluid(
    useShinyjs(),
    theme = bs_theme(
      version = 5,
      bootswatch = "litera",
      primary = "#0d6efd"
    ),
    
    # Encabezado
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          h2("Servicios públicos e Infraestructura", class = "text-center")
        ),
        p("Resultados de la encuesta de percepción ciudadana 2024", class = "text-center")
      )
    ),
    
    # Value boxes en la parte superior
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(
        title = "Días con agua a la semana",
        value = textOutput("water_days"),
        showcase = bsicons::bs_icon("droplet-fill"),
        theme = value_box_theme(bg = "#3498db", fg = "white")
      ),
      value_box(
        title = "Cortes de luz (último mes)",
        value = textOutput("power_outages"),
        showcase = bsicons::bs_icon("lightning-fill"),
        theme = value_box_theme(bg = "#f39c12", fg = "white")
      ),
      value_box(
        title = "Frecuencia de recolección de basura",
        value = textOutput("trash_pickup"),
        showcase = bsicons::bs_icon("trash-fill"),
        theme = value_box_theme(bg = "#2ecc71", fg = "white")
      )
    ),
    
    # Diseño principal con barra lateral
    layout_sidebar(
      sidebar = sidebar(
        title = "Seleccionar Servicio",
        radioButtons(
          "selected_service",
          "Servicio a evaluar:",
          choices = c(
            "Agua" = "Q29",
            "Drenaje y Alcantarillado" = "Q30",
            "Comisión Federal de Electricidad" = "Q35",
            "Recolección de Basura" = "Q40",
            "Alumbrado Público" = "Q45",
            "Calles y Pavimentación" = "Q51",
            "Áreas verdes y Espacios públicos" = "Q55",
            "Unidades deportivas" = "Q56",
            "Bibliotecas" = "Q58",
            "Centros comunitarios" = "Q59",
            "Espacios para personas con discapacidad" = "Q60",
            "Parques" = "Q61",
            "Transporte público" = "Q62"
          ),
          selected = "Q29"
        ),
        hr(),
        downloadButton("download_data", "Descargar Datos"),
        hr(),
        h5("Descripción:"),
        textOutput("question_text")
      ),
      
      # Área de contenido principal: solo el mapa
      card(
        card_header("Evaluación de servicios por distrito"),
        leafletOutput("service_map", height = "600px")
      )
    )
  )
}