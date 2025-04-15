# urbanUI.R
urbanUI <- function() {
  page_fluid(
    class = "section-movilidad",
    useShinyjs(),
   
    # Header
    layout_columns(
      fill = FALSE,

      card(card_header(
        style = paste0("background-color: var(--movilidad-color) !important; 
        color: white !important; 
        font-weight: bolder !important; 
        text-align: center !important; 
        border-bottom: none !important;"),
        h2("Desarrollo Urbano", class = "text-center")))
    ),
    
    # Navigation cards
    layout_columns(
      col_widths = c(6,6),
      
      # Mobility Dashboard Card
      div(
        id = "nav_mobility_card ",
        onclick = "Shiny.setInputValue('nav_target', 'mobility', {priority: 'event'})",
        card(
          class = "nav-card-movilidad",
          card_body(
            div(class = "text-center nav-card-icon", bsicons::bs_icon("bicycle")),
            h4(class = "text-center", "Movilidad"),
            p(class = "text-center", "Patrones de transporte y vehículos")
          )
        )
      ),
      
      # Public Transportation Card
      div(
        id = "nav_trasnportation_card ",
        onclick = "Shiny.setInputValue('nav_target', 'transportation', {priority: 'event'})",
        card(
          class = "nav-card-movilidad",
          card_body(
            div(class = "text-center nav-card-icon", bsicons::bs_icon("bus-front")),
            h4(class = "text-center", "Transporte Público"),
            p(class = "text-center", "Satisfacción con el servicio")
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
        theme = value_box_theme(bg = "#2A9D8F", fg = "white")
      ),
      
      # Private vehicle usage
      value_box(
        title = "Uso de Vehículo Particular (General)",
        value = textOutput("private_vehicle_usage"),
        showcase = bsicons::bs_icon("car-front"),
        theme = value_box_theme(bg = "#80CBC4", fg = "black")
      ),
      
      # Bus satisfaction
      value_box(
        title = "Satisfacción con Camión/Rutera",
        value = textOutput("bus_satisfaction"),
        showcase = bsicons::bs_icon("star-half"),
        theme = value_box_theme(bg = "#2A9D8F", fg = "white")
      ),
      
      # Juarez Bus satisfaction
      value_box(
        title = "Satisfacción con Juárez Bus",
        value = textOutput("juarez_bus_satisfaction"),
        showcase = bsicons::bs_icon("star-half"),
        theme = value_box_theme(bg = "#80CBC4", fg = "black")
      )
    ),
    
    # Environmental quality plot
    card(
      card_header(
          "Satisfacción con aspectos ambientales"
      ),
      plotlyOutput("env_quality_plot", height = "500px")
    )
  )
}