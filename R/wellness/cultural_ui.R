# cultural_ui.R

culturalUI <- function() {
  page_fluid(
    class = "section-bienestar",

    useShinyjs(),
    init_tooltips(),

    # Initialize tooltips
    init_tooltips(),

    div(
      class = "mb-4",
      tags$a(
        href = "#",
        class = "text-decoration-none",
        onclick = "Shiny.setInputValue('nav_target', 'wellness', {priority: 'event'}); return false;",
        tags$i(class = "fas fa-arrow-left me-2"),
        "Volver a Bienestar Social y Económico"
      )
    ),
    # Header
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          style="border-top: 4px solid var(--bienestar-color)",
          h2("Participación Cultural", class = "text-center")
        )
      )
    ),
    
    # Info boxes section
    layout_columns(
      col_widths = c(4, 4, 4),
      
      # Home activities - with automatic tooltip icon color
      value_box_with_title_tooltip(
        title = "Actividades dentro del hogar",
        value = textOutput("home_activities_pct"),
        showcase = bsicons::bs_icon("house-fill"),
        theme = value_box_theme(bg = "#1E88E5", fg = "white"),
        tooltip_text = "<b>ID</b>: PER Q16.1 <br>
               <b>Pregunta</b>: Actividades dentro del hogar (internet, tv, radio, lectura, ejercicio, videojuegos, etc.)  <br>
                <b>Escala</b>: 1=Sí; 2=No"
      ),
      
      # Exercise activities - with forced white color on orange background
      value_box_with_title_tooltip(
        title = "Ejercicio o actividad física",
        value = textOutput("exercise_activities_pct"),
        showcase = bsicons::bs_icon("bicycle"),
        theme = value_box_theme(bg = "#FFA000", fg = "white"),
        tooltip_text = "<b>ID</b>: PER Q16.15 <br>
               <b>Pregunta</b>: Realizo ejercicio o actividad fisica (Gym, correr, bicibleta, caminar, etc.) <br>
                <b>Escala</b>: 1=Sí; 2=No",
        force_icon_color = "rgba(255, 255, 255, 0.8)"  # Force white color
      ),
      
      # Park/nature activities - with automatic tooltip icon color
      value_box_with_title_tooltip(
        title = "Contacto con la naturaleza",
        value = textOutput("nature_activities_pct"),
        showcase = bsicons::bs_icon("tree-fill"),
        theme = value_box_theme(bg = "#1E88E5", fg = "white"),
        tooltip_text = "<b>ID</b>: PER Q16.2 <br>
               <b>Pregunta</b>: Fue a un parque o estuvo en contacto con la naturaleza <br>
                <b>Escala</b>: 1=Sí; 2=No"
      )
    ),
    
    # Main content
    layout_columns(
      col_widths = c(6, 6),
      # Cultural activities bar plot
      card(
        card_header(
          div(
            class = "d-flex align-items-center",
            "Asistencia a actividades culturales (últimos 3 meses)",
            create_tooltip("<b>ID</b>: PER Q16.7 Q16.9 Q16.10 Q16.11 Q16.12 Q16.13 Q16.14  <br>
               <b>Preguntas</b>: Asistió a una biblioteca; concierto o espectaculo de musica; feria, fiesta popular o festividad religiosa; museo, galeria o casa de cultura; espectaculo de teatro o danza (artes escenicas); zona historica o arqueologica, monumentos; conferencias, conservatorios, coloquios; evento deportivo <br>
                <b>Escala</b>: 1=Sí; 2=No")
          )
        ),
        plotlyOutput("cultural_activities_plot", height = "500px"),

      ),
      
      # Commercial/entertainment activities pie chart
      card(
        card_header(
          div(
            class = "d-flex align-items-center",
            "Actividades de ocio y entretenimiento (últimos 3 meses)",
            create_tooltip("<b>ID</b>: PER Q16.3 Q16.4 Q16.5 Q16.6 Q16.8  <br>
              <b>Preguntas</b>: Visito un centro y/o plaza comercial; segundas y/o bazares; feria, fiesta popular o festividad religiosa; cantina, bar o antro; cine <br>
               <b>Escala</b>: 1=Sí; 2=No")          )
        ),
        plotlyOutput("entertainment_activities_plot", height = "500px"),
      )
    )
  
  )
}