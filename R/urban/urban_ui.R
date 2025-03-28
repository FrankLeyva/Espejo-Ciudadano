# urbanUI.R
urbanUI <- function() {
  page_fluid(
    useShinyjs(),
    
    tags$head(
      tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.5/font/bootstrap-icons.css"),
      tags$style(HTML("
        .stat-card {
          transition: transform 0.3s, box-shadow 0.3s;
          border-left: 4px solid #0d6efd;
        }
        .nav-card {
          transition: transform 0.3s, box-shadow 0.3s;
          cursor: pointer;
        }
        .nav-card:hover {
          transform: translateY(-5px);
          box-shadow: 0 10px 20px rgba(0,0,0,0.1);
        }
        .nav-card-icon {
          font-size: 2rem;
          margin-bottom: 15px;
          color: #0d6efd;
        }
      "))
    ),
    
    theme = bs_theme(version = 5, bootswatch = "litera", primary = "#0d6efd"),
    
    # Header
    layout_columns(
      fill = FALSE,
      card(card_header(h2("Desarrollo Urbano", class = "text-center")))
    ),
    
    # Navigation cards
    layout_columns(
      col_widths = c(4, 4, 4),
      
      # Mobility Dashboard Card
      div(
        class = "nav-card",
        id = "nav_mobility_card",
        onclick = "Shiny.setInputValue('nav_target', 'mobility', {priority: 'event'})",
        card(
          card_body(
            div(class = "text-center nav-card-icon", bsicons::bs_icon("bicycle")),
            h4(class = "text-center", "Movilidad"),
            p(class = "text-center", "Patrones de transporte y vehículos")
          )
        )
      ),
      
      # Public Transportation Card
      div(
        class = "nav-card",
        id = "nav_public_transport_card",
        onclick = "Shiny.setInputValue('nav_target', 'transportation', {priority: 'event'})",
        card(
          card_body(
            div(class = "text-center nav-card-icon", bsicons::bs_icon("bus-front")),
            h4(class = "text-center", "Transporte Público"),
            p(class = "text-center", "Satisfacción con el servicio")
          )
        )
      ),
      
      # Environment Card
      div(
        class = "nav-card",
        id = "nav_environment_card",
        onclick = "Shiny.setInputValue('nav_target', 'environment', {priority: 'event'})",
        card(
          card_body(
            div(class = "text-center nav-card-icon", bsicons::bs_icon("tree-fill")),
            h4(class = "text-center", "Medio Ambiente"),
            p(class = "text-center", "Calidad ambiental")
          )
        )
      )
    ),
    
    # Key statistics
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      
      # Public transport usage
      value_box(
        title = "Uso de Transporte Público (Trabajo)",
        value = textOutput("public_transport_work"),
        showcase = bsicons::bs_icon("bus-front"),
        theme = value_box_theme(bg = "#3498db", fg = "white")
      ),
      
      # Private vehicle usage
      value_box(
        title = "Uso de Vehículo Particular (General)",
        value = textOutput("private_vehicle_usage"),
        showcase = bsicons::bs_icon("car-front"),
        theme = value_box_theme(bg = "#f39c12", fg = "white")
      ),
      
      # Bus satisfaction
      value_box(
        title = "Satisfacción con Camión/Rutera",
        value = textOutput("bus_satisfaction"),
        showcase = bsicons::bs_icon("star-half"),
        theme = value_box_theme(bg = "#2ecc71", fg = "white")
      ),
      
      # Juarez Bus satisfaction
      value_box(
        title = "Satisfacción con Juárez Bus",
        value = textOutput("juarez_bus_satisfaction"),
        showcase = bsicons::bs_icon("star-half"),
        theme = value_box_theme(bg = "#9b59b6", fg = "white")
      )
    ),
    
    # Environmental quality plot
    card(
      card_header(
        div(
          style = "background-color: transparent; border-bottom: none;",
          "Calidad Ambiental",
          class = "h5 fw-bold"
        )
      ),
      plotlyOutput("env_quality_plot", height = "500px")
    )
  )
}