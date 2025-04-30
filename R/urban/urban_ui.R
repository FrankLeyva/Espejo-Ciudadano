# urbanUI.R
urbanUI <- function() {
  page_fluid(
    class = "section-movilidad",
    useShinyjs(),
    init_tooltips(),

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
      value_box_with_title_tooltip(
        title = "Uso de Transporte Público (Trabajo)",
        value = textOutput("public_transport_work"),
        showcase = bsicons::bs_icon("bus-front"),
        theme = value_box_theme(bg = "#2A9D8F", fg = "white"),
        tooltip_text = "<b>ID</b>: PER Q72.9 <br>
               <b>Pregunta</b>: Camion/Rutera/ Autobus  <br>
                <b>Escala</b>: 1=Sí; 2=No"
      ),
      # Private vehicle usage
      value_box_with_title_tooltip(
        title = "Uso de Vehículo Particular (General)",
        value = textOutput("private_vehicle_usage"),
        showcase = bsicons::bs_icon("car-front"),
        theme = value_box_theme(bg = "#80CBC4", fg = "white"),
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
        theme = value_box_theme(bg = "#2A9D8F", fg = "white"),
        tooltip_text = "<b>ID</b>: PER Q75 <br>
               <b>Pregunta</b>: En una escala del 1 al 10, que tan satisfecho está con la calidad del servicio del camión/rutera?  <br>
                <b>Escala</b>: 1-10"
      ),
      
      # Juarez Bus satisfaction
      value_box_with_title_tooltip(
        title = "Satisfacción con Juárez Bus",
        value = textOutput("juarez_bus_satisfaction"),
        showcase = bsicons::bs_icon("star-half"),
        theme = value_box_theme(bg = "#80CBC4", fg = "white"),
        tooltip_text = "<b>ID</b>: PER Q78 <br>
               <b>Pregunta</b>: 	En una escala del 1 al 10, que tan satisfecho está con la calidad del servicio del BravoBus/EcoBus/ViveBus/Juarez Bus)?  <br>
                <b>Escala</b>: 1-10",
        force_icon_color = "rgba(255, 255, 255, 0.8)"
      )
    ),
    
    # Environmental quality plot
    card(
      card_header(
      div(
        class = "d-flex align-items-center",
        "Satisfacción con Aspectos Ambientales",
  create_tooltip("<b>ID</b>: PER Q89, Q90, Q91, Q92 <br>
        <b>Pregunta</b>: En una escala del 1 al 10, que tan satisfecho está con LA CALIDAD DEL AIRE/ LA CANTIDAD DE ÁRBOLES EN LA CIUDAD /  LA LIMPIEZA EN LAS CALLES (basura en las calles) / LA CALIDAD DEL AGUA? (CONSIDERAR SI ESTÁ SUCIA, CONTAMINADA O TIENE MAL SABOR) ? <br>
         <b>Escala</b>: 1-10")
   )
  ),
      plotlyOutput("env_quality_plot", height = "500px")
    )
  )
}