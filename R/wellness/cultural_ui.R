# cultural_ui.R

culturalUI <- function() {
  page_fluid(
    class = "section-bienestar",

    useShinyjs(),

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
      
      # Home activities
      value_box(
        title = "Actividades dentro del hogar",
        value = textOutput("home_activities_pct"),
        showcase = bsicons::bs_icon("house-fill"),
        theme = value_box_theme(bg = "#1E88E5", fg = "white")
      ),
      
      # Exercise activities
      value_box(
        title = "Ejercicio o actividad física",
        value = textOutput("exercise_activities_pct"),
        showcase = bsicons::bs_icon("bicycle"),
        theme = value_box_theme(bg = "#FFA000", fg = "white")
      ),
      
      # Park/nature activities
      value_box(
        title = "Contacto con la naturaleza",
        value = textOutput("nature_activities_pct"),
        showcase = bsicons::bs_icon("tree-fill"),
        theme = value_box_theme(bg = "#1E88E5", fg = "white")
      )
    ),
    
    # Main content
    layout_columns(
      col_widths = c(6, 6),
      
      # Cultural activities bar plot
      card(
        card_header(
          "Asistencia a actividades culturales (últimos 3 meses)",
        ),
        plotlyOutput("cultural_activities_plot", height = "500px"),

      ),
      
      # Commercial/entertainment activities pie chart
      card(
        card_header(
          "Actividades de ocio y entretenimiento (últimos 3 meses)",
        ),
        plotlyOutput("entertainment_activities_plot", height = "500px"),
      )
    )
  
  )
}
