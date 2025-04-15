# UI para Dashboard de Servicios Públicos
publicServicesUI <- function() {
  page_fluid(
    class = "section-infraestructura",

    useShinyjs(),
      
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
          style="border-top: 4px solid var(--infraestructura-color)",

          h2("Servicios públicos e Infraestructura", class = "text-center")
        )      )
    ),
    
    # Diseño principal con barra lateral
    layout_sidebar(
      sidebar = sidebar(
        title = "Seleccionar Servicio",
        width = 280,  # Slightly wider for better spacing
        
        # Radio buttons for service selection with improved styling
        div(
          class = "service-list",
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
          )
        )
      ),
      
      # Área de contenido principal: solo el mapa
      card(
        class = "border-0",  # Quitar bordes del card
        card_header(
          div(
            class = "d-flex justify-content-between align-items-center",
            textOutput("service_title"),  # Título dinámico
            downloadButton(
              "download_service_map", 
              "", 
              icon = icon("download"), 
              class = "btn-sm"
            )
          )
        ),
        leafletOutput("service_map", height = "600px")
      )
      
    ),
    conditionalPanel(
      condition = "input.selected_service == 'Q55'", # Show when "Áreas verdes y Espacios públicos" is selected
      card(
        card_header(
          "Evaluación de áreas verdes y espacios públicos",
          class = "bg-light"
        ),
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          value_box(
            title = "Equipamiento",
            value = textOutput("green_areas_equipment"),
            showcase = bsicons::bs_icon("tools"),
            theme = value_box_theme(bg = "#2A9D8F", fg = "white")
          ),
          value_box(
            title = "Iluminación",
            value = textOutput("green_areas_lighting"),
            showcase = bsicons::bs_icon("lightbulb-fill"),
            theme = value_box_theme(bg = "#6969B3", fg = "white")
          ),
          value_box(
            title = "Cuidado y Mantenimiento",
            value = textOutput("green_areas_maintenance"),
            showcase = bsicons::bs_icon("brush"),
            theme = value_box_theme(bg = "#F4A261", fg = "white")
          ),
          value_box(
            title = "Seguridad",
            value = textOutput("green_areas_security"),
            showcase = bsicons::bs_icon("shield-lock"),
            theme = value_box_theme(bg = "#E86486", fg = "white")
          )
        )
      )
    ),
    conditionalPanel(
      condition = "input.selected_service == 'Q29'", # Water service
      card(
        div(
          class = "info-box info-box-info",
          div(class = "info-box-icon", bsicons::bs_icon("droplet-fill")),
          div(
            class = "info-box-content",
            div(class = "info-box-title", "Suministro de Agua"),
            div(class = "info-box-value", "En promedio, los ciudadanos reportaron tener acceso al suministro de agua ", textOutput("water_days", inline = TRUE), " días por semana. Este valor refleja la frecuencia promedio del servicio de agua en toda la ciudad.")
          )
        )
      )
    ),
    
    conditionalPanel(
      condition = "input.selected_service == 'Q35'", # Electricity service
      card(
        div(
          class = "info-box info-box-warning",
          style = "background-color: #FFF3CD !important; color: #856404 !important;",
          div(class = "info-box-icon", bsicons::bs_icon("lightning-fill")),
          div(
            class = "info-box-content",
            div(class = "info-box-title", "Servicio Eléctrico"),
            div(class = "info-box-value", "La experiencia más común reportada por los ciudadanos con respecto a los cortes de luz en el último mes fue: ", textOutput("power_outages", inline = TRUE), ". Este dato representa la situación más frecuente reportada por los encuestados.")
          )
        )
      )
    ),
    
    conditionalPanel(
      condition = "input.selected_service == 'Q40'", # Trash service
      card(
        div(
          class = "info-box info-box-success",
          style = "background-color: #D4EDDA !important; color: #155724 !important;",
          div(class = "info-box-icon", bsicons::bs_icon("trash-fill")),
          div(
            class = "info-box-content",
            div(class = "info-box-title", "Recolección de Basura"),
            div(class = "info-box-value", "En promedio, el servicio de recolección de basura pasa ", textOutput("trash_pickup", inline = TRUE), " veces por semana en los hogares de la ciudad.")
          )
        )
      )
    ),
    card(
      card_header("Reportes de Servicios Públicos"),
      plotlyOutput("report_statistics_plot", height = "450px")
    )
  )
}