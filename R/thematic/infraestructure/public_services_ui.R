# UI para Dashboard de Servicios Públicos
publicServicesUI <- function() {
  page_fluid(
    useShinyjs(),
      
    tags$head(
      tags$link(
        rel = "stylesheet", 
        href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.5/font/bootstrap-icons.css"
      ),
      tags$style(HTML("
        /* General styling for sidebar options */
        .form-check {
          padding: 10px 15px;
          margin-bottom: 5px;
          border-radius: 4px;
          transition: background-color 0.2s;
        }
        .form-check:hover {
          background-color: #f8f9fa;
        }
        .form-check-input:checked ~ .form-check-label {
          font-weight: bold;
          color: #0d6efd;
        }
        
        /* Service icons using CSS */
        .service-list label[for$=Q29]:before {
          content: '\\F143';
          font-family: 'bootstrap-icons';
          margin-right: 8px;
          color: #3498db;
        }
        .service-list label[for$=Q30]:before {
          content: '\\F168';
          font-family: 'bootstrap-icons';
          margin-right: 8px;
          color: #2980b9;
        }
        .service-list label[for$=Q35]:before {
          content: '\\F148';
          font-family: 'bootstrap-icons';
          margin-right: 8px;
          color: #f39c12;
        }
        .service-list label[for$=Q40]:before {
          content: '\\F151';
          font-family: 'bootstrap-icons';
          margin-right: 8px;
          color: #2ecc71;
        }
        .service-list label[for$=Q45]:before {
          content: '\\F140';
          font-family: 'bootstrap-icons';
          margin-right: 8px;
          color: #f1c40f;
        }
        .service-list label[for$=Q51]:before {
          content: '\\F16F';
          font-family: 'bootstrap-icons';
          margin-right: 8px;
          color: #95a5a6;
        }
        .service-list label[for$=Q55]:before {
          content: '\\F155';
          font-family: 'bootstrap-icons';
          margin-right: 8px;
          color: #27ae60;
        }
        .service-list label[for$=Q56]:before {
          content: '\\F156';
          font-family: 'bootstrap-icons';
          margin-right: 8px;
          color: #e67e22;
        }
        .service-list label[for$=Q58]:before {
          content: '\\F126';
          font-family: 'bootstrap-icons';
          margin-right: 8px;
          color: #9b59b6;
        }
        .service-list label[for$=Q59]:before {
          content: '\\F14E';
          font-family: 'bootstrap-icons';
          margin-right: 8px;
          color: #34495e;
        }
        .service-list label[for$=Q60]:before {
          content: '\\F602';
          font-family: 'bootstrap-icons';
          margin-right: 8px;
          color: #8e44ad;
        }
        .service-list label[for$=Q61]:before {
          content: '\\F3E7';
          font-family: 'bootstrap-icons';
          margin-right: 8px;
          color: #16a085;
        }
        .service-list label[for$=Q62]:before {
          content: '\\F131';
          font-family: 'bootstrap-icons';
          margin-right: 8px;
          color: #e74c3c;
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
        
        .info-box-warning {
          background-color: #fff3cd;
          color: #856404;
        }
        
        .info-box-success {
          background-color: #d4edda;
          color: #155724;
        }
      "))
    ),
    
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
            style = "background-color: transparent; border-bottom: none;",
            textOutput("service_title"),  # Título dinámico
            class = "h5 fw-bold"
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