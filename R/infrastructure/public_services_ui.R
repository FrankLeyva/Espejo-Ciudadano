# UI para Dashboard de Servicios Públicos
publicServicesUI <- function() {
  page_fluid(
    class = "section-infraestructura",

    useShinyjs(),
    init_tooltips(),

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
              "Semaforización" = "Q55",
              "Áreas verdes y Espacios públicos" = "Q56",
              "Unidades deportivas" = "Q58",
              "Bibliotecas" = "Q59",
              "Centros comunitarios" = "Q60",
              "Banquetas" = "Q61",
              "Espacios para personas con discapacidad" = "Q62"
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
            div(
              class = "d-flex align-items-center",
            textOutput("service_title"), 
            create_dynamic_tooltip("utilities_tooltip")
          ),
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
      condition = "input.selected_service == 'Q56'", # Show when "Áreas verdes y Espacios públicos" is selected
      card(
        card_header(
          "Evaluación de áreas verdes y espacios públicos",
          class = "bg-light"
        ),
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          value_box_with_title_tooltip(
            title = "Equipamiento",
            value = textOutput("green_areas_equipment"),
            showcase = bsicons::bs_icon("tools"),
            theme = value_box_theme(bg = "#2A9D8F", fg = "white"),
            tooltip_text = "<b>ID</b>: PER57.1 <br>
                   <b>Pregunta</b>: Equipamiento  <br>
                    <b>Escala</b>: 1-10"
          ),
          value_box_with_title_tooltip(
            title = "Iluminación",
            value = textOutput("green_areas_lighting"),
            showcase = bsicons::bs_icon("lightbulb-fill"),
            theme = value_box_theme(bg = "#6969B3", fg = "white"),
            tooltip_text = "<b>ID</b>: PER57.2 <br>
                   <b>Pregunta</b>: Iluminacion  <br>
                    <b>Escala</b>: 1-10"
          ),
          value_box_with_title_tooltip(
            title = "Cuidado y Mantenimiento",
            value = textOutput("green_areas_maintenance"),
            showcase = bsicons::bs_icon("brush"),
            theme = value_box_theme(bg = "#F4A261", fg = "white"),
            tooltip_text = "<b>ID</b>: PER57.3 <br>
                   <b>Pregunta</b>: 	Cuidado (limpieza y mantenimiento)  <br>
                    <b>Escala</b>: 1-10",
        force_icon_color = "rgba(255, 255, 255, 0.8)"
          ),
          value_box_with_title_tooltip(
            title = "Seguridad",
            value = textOutput("green_areas_security"),
            showcase = bsicons::bs_icon("shield-lock"),
            theme = value_box_theme(bg = "#E86486", fg = "white"),
            tooltip_text = "<b>ID</b>: PER57.4 <br>
                   <b>Pregunta</b>: Seguridad  <br>
                    <b>Escala</b>: 1-10"
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
      card_header(
        div(
          class = "d-flex align-items-center",
        "Reportes de Servicios Públicos",
        create_tooltip("<b>ID</b>: PER Q32 Q33 Q37 Q38 Q42 Q43 Q48 Q49 Q52 Q53 <br>
          <b>Pregunta</b>: Durante este año Interpuso algún reporte? El problema fue atendido por la dependencia? <br>
           <b>Escala</b>: 1=Si;2=No;3=no sabe")
     )
    ),
      uiOutput("report_statistics_plot")  # Changed from plotlyOutput to uiOutput
    )
  )
}