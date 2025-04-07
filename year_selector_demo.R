library(shiny)
library(bslib)
library(plotly)
library(leaflet)
library(dplyr)

# Sample data for visualizations
set.seed(123)
district_data <- data.frame(
  district = as.factor(1:10),
  satisfaction = round(runif(10, 6.5, 9.2), 1),
  population = round(runif(10, 80000, 200000)),
  service_access = round(runif(10, 30, 95)),
  housing_quality = round(runif(10, 5.5, 8.5), 1),
  participation = round(runif(10, 15, 65))
)

service_data <- data.frame(
  service = c("Agua", "Electricidad", "Internet", "Parques", "Calles", "Transporte"),
  rating = c(7.2, 8.1, 6.5, 5.9, 5.2, 6.8),
  change = c(0.3, -0.2, 0.5, 0.1, -0.3, 0.0)
)

participation_data <- data.frame(
  type = c("Vota en elecciones", "Participa en juntas", "Asiste a eventos comunitarios", 
           "Forma parte de comités", "Realiza reportes ciudadanos"),
  percentage = c(68, 32, 45, 18, 29)
)

# Section color themes
section_themes <- list(
  bienestar = list(
    primary = "#006D77",
    secondary = "#83C5BE",
    accent = "#006D77"
  ),
  movilidad = list(
    primary = "#2A9D8F",
    secondary = "#80CBC4",
    accent = "#2A9D8F"
  ),
  gobierno = list(
    primary = "#6969B3",
    secondary = "#B39DDB",
    accent = "#6969B3"
  ),
  infraestructura = list(
    primary = "#F4A261",
    secondary = "#FFCC80",
    accent = "#F4A261"
  ),
  participacion = list(
    primary = "#E76F51",
    secondary = "#FFAB91",
    accent = "#E76F51"
  )
)

# District colors (stays consistent across all sections)
district_colors <- c("#88BDBC", "#6E9887", "#BECC92", "#FDD692", 
                     "#F1BB87", "#F28A80", "#D1A5C6", "#9CADCE", 
                     "#B6C5D1", "#D3D9E0")

# Helper function for section-specific styling
apply_section_style <- function(element_id, section) {
  theme_color <- section_themes[[section]]$primary
  return(tags$script(sprintf("
    document.getElementById('%s').style.backgroundColor = '%s';
  ", element_id, theme_color)))
}

# Custom CSS for styling
custom_css <- '
/* Base Styles */
body {
  font-family: "Montserrat", "Segoe UI", sans-serif;
  background-color: #F8F9FA;
  color: #333333;
}

/* Header Styling */
.app-header {
  background-color: #333333;
  color: white;
  padding: 15px 20px;
  margin-bottom: 24px;
  border-radius: 8px;
  display: flex;
  align-items: center;
  justify-content: space-between;
}

.app-logo {
  font-weight: 700;
  font-size: 24px;
  letter-spacing: 0.5px;
}

.app-subtitle {
  font-weight: 400;
  font-size: 14px;
  opacity: 0.9;
}

/* Section indicator */
.section-indicator {
  display: flex;
  align-items: center;
  margin-bottom: 16px;
}

.section-color {
  width: 8px;
  height: 30px;
  margin-right: 10px;
  border-radius: 4px;
}

.section-name {
  font-size: 22px;
  font-weight: 600;
  color: #333333;
}

/* Card Styling */
.dashboard-card {
  background-color: #FFFFFF;
  border-radius: 8px;
  box-shadow: 0 2px 8px rgba(0,0,0,0.06);
  padding: 20px;
  margin-bottom: 24px;
  border: none;
}

.card-header {
  font-family: "Montserrat", sans-serif;
  font-weight: 600;
  font-size: 18px;
  color: #333333;
  padding-bottom: 12px;
  border-bottom: 1px solid #E9ECEF;
  margin-bottom: 16px;
  background-color: transparent;
}

.card-header::before {
  content: "";
  display: inline-block;
  width: 4px;
  height: 18px;
  margin-right: 8px;
  vertical-align: text-top;
}

/* Navigation Styling */
.nav-pills .nav-link.active {
  color: white;
  border-radius: 6px;
  font-weight: 500;
}

.nav-pills .nav-link {
  color: #495057;
  padding: 8px 16px;
  border-radius: 6px;
  transition: all 0.2s;
}

.nav-pills .nav-link:hover:not(.active) {
  background-color: #E9ECEF;
}

/* Value Box Styling */
.value-box-container {
  background-color: white;
  border-radius: 8px;
  box-shadow: 0 2px 8px rgba(0,0,0,0.06);
  padding: 20px;
  margin-bottom: 16px;
  display: flex;
  align-items: center;
}

.value-box-icon {
  width: 48px;
  height: 48px;
  border-radius: 8px;
  display: flex;
  align-items: center;
  justify-content: center;
  margin-right: 16px;
  color: white;
  font-size: 24px;
}

.value-box-content {
  flex-grow: 1;
}

.value-box-title {
  font-size: 14px;
  color: #6C757D;
  margin-bottom: 4px;
}

.value-box-value {
  font-size: 24px;
  font-weight: 700;
  color: #333333;
}

/* Table Styling */
.table {
  width: 100%;
  margin-bottom: 1rem;
  color: #333333;
  border-collapse: collapse;
}

.table th {
  padding: 12px 8px;
  vertical-align: bottom;
  background-color: #F8F9FA;
  border-bottom: 2px solid #E9ECEF;
  font-weight: 600;
  text-align: left;
}

.table td {
  padding: 12px 8px;
  vertical-align: top;
  border-top: 1px solid #E9ECEF;
}

.table tbody tr:hover {
  background-color: rgba(0, 109, 119, 0.05);
}

/* Section tabs */
.section-tabs {
  margin-bottom: 20px;
  border-bottom: 1px solid #dee2e6;
  display: flex;
}

.section-tab {
  padding: 10px 20px;
  margin-right: 5px;
  border-radius: 6px 6px 0 0;
  cursor: pointer;
  border: 1px solid #dee2e6;
  border-bottom: none;
  background-color: #f8f9fa;
}

.section-tab.active {
  background-color: white;
  font-weight: 600;
}

/* Visualization containers */
.visualization-container {
  margin-top: 15px;
}
'

# UI
ui <- page_fluid(
  tags$head(
    tags$link(rel = "stylesheet", 
              href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@400;500;600;700&display=swap"),
    tags$style(custom_css),
    tags$script(src = "https://kit.fontawesome.com/a076d05399.js")
  ),
  
  # Header
  div(class = "app-header",
      div(class = "app-logo", "Dashboard Ciudad Juárez"),
      div(class = "app-subtitle", "Plan Estratégico de Juárez | 2024")
  ),
  
  # Section tabs
  div(class = "section-tabs",
      div(class = "section-tab", 
          id = "tab-general", 
          style = "border-top: 3px solid #333333;",
          onclick = "switchTab('general')",
          "Panorama General"),
      div(class = "section-tab active", 
          id = "tab-bienestar", 
          style = "border-top: 3px solid #006D77;",
          onclick = "switchTab('bienestar')",
          "Bienestar"),
      div(class = "section-tab", 
          id = "tab-movilidad", 
          style = "border-top: 3px solid #2A9D8F;",
          onclick = "switchTab('movilidad')",
          "Movilidad"),
      div(class = "section-tab", 
          id = "tab-gobierno", 
          style = "border-top: 3px solid #6969B3;",
          onclick = "switchTab('gobierno')",
          "Gobierno"),
      div(class = "section-tab", 
          id = "tab-infra", 
          style = "border-top: 3px solid #F4A261;",
          onclick = "switchTab('infraestructura')",
          "Infraestructura"),
      div(class = "section-tab", 
          id = "tab-part", 
          style = "border-top: 3px solid #E76F51;",
          onclick = "switchTab('participacion')",
          "Participación")
  ),
  tags$script("
    function switchTab(section) {
      // Remove active class from all tabs
      document.querySelectorAll('.section-tab').forEach(tab => {
        tab.classList.remove('active');
      });
      
      // Add active class to clicked tab
      document.getElementById('tab-' + section).classList.add('active');
      
      // Update section indicator color
      document.querySelector('.section-color').style.backgroundColor = 
        section === 'general' ? '#333333' : 
        section === 'bienestar' ? '#006D77' : 
        section === 'movilidad' ? '#2A9D8F' : 
        section === 'gobierno' ? '#6969B3' : 
        section === 'infraestructura' ? '#F4A261' : '#E76F51';
        
      // Update card headers
      document.querySelectorAll('.card-header').forEach(header => {
        header.style.setProperty('--accent-color', 
          section === 'general' ? '#333333' : 
          section === 'bienestar' ? '#006D77' : 
          section === 'movilidad' ? '#2A9D8F' : 
          section === 'gobierno' ? '#6969B3' : 
          section === 'infraestructura' ? '#F4A261' : '#E76F51');
      });
      
      // This is just a demo, in a real app you would change content here
    }
  "),
  
  # Section indicator (currently showing "Bienestar")
  div(class = "section-indicator",
      div(class = "section-color", id = "section-color-indicator", 
          style = "background-color: #006D77;"),
      div(class = "section-name", "Bienestar")
  ),
  
  # Main content
  layout_columns(
    col_widths = c(4, 8),
    
    # Left column with key metrics
    div(
      # Value boxes
      div(class = "value-box-container",
          div(class = "value-box-icon", id = "population-icon", 
              style = "background-color: #006D77;", 
              bsicons::bs_icon("people")),
          div(class = "value-box-content",
              div(class = "value-box-title", "Satisfacción promedio"),
              div(class = "value-box-value", "7.8/10")
          )
      ),
      
      div(class = "value-box-container",
          div(class = "value-box-icon", id = "trend-icon",
              style = "background-color: #83C5BE;", 
              bsicons::bs_icon("graph-up")),
          div(class = "value-box-content",
              div(class = "value-box-title", "Tendencia anual"),
              div(class = "value-box-value", "+0.3")
          )
      ),
      
      div(class = "value-box-container",
          div(class = "value-box-icon", id = "district-icon", 
              style = "background-color: #006D77;", 
              bsicons::bs_icon("geo-alt")),
          div(class = "value-box-content",
              div(class = "value-box-title", "Distrito con mejor calificación"),
              div(class = "value-box-value", "Distrito 4 (9.1)")
          )
      ),
      
      # District table
      div(class = "dashboard-card",
          div(class = "card-header", style = "--accent-color: #006D77;", 
              "Distritos con mayor satisfacción"),
          tags$table(class = "table",
                    tags$thead(
                      tags$tr(
                        tags$th("Distrito"),
                        tags$th("Calificación"),
                        tags$th("Variación")
                      )
                    ),
                    tags$tbody(
                      tags$tr(
                        tags$td("Distrito 4"),
                        tags$td("9.1"),
                        tags$td(tags$span(style = "color: #28a745", "+0.3"))
                      ),
                      tags$tr(
                        tags$td("Distrito 9"),
                        tags$td("8.7"),
                        tags$td(tags$span(style = "color: #28a745", "+0.1"))
                      ),
                      tags$tr(
                        tags$td("Distrito 2"),
                        tags$td("8.5"),
                        tags$td(tags$span(style = "color: #dc3545", "-0.2"))
                      ),
                      tags$tr(
                        tags$td("Distrito 8"),
                        tags$td("8.4"),
                        tags$td(tags$span(style = "color: #6c757d", "0.0"))
                      )
                    )
          )
      )
    ),
    
    # Right column with visualizations
    div(
      # City map visualization
      div(class = "dashboard-card",
          div(class = "card-header", style = "--accent-color: #006D77;", 
              "Satisfacción por distrito"),
          leafletOutput("district_map", height = "350px")
      ),
      
      # Services bar chart
      div(class = "dashboard-card",
          div(class = "card-header", style = "--accent-color: #006D77;", 
              "Calificación de servicios públicos"),
          plotlyOutput("services_chart", height = "300px")
      )
    )
  ),
  
  # Comparison of different visualization styles
  div(class = "dashboard-card", 
      div(class = "card-header", style = "--accent-color: #006D77;",
          "Ejemplos de visualizaciones con diferentes esquemas de color"),
      
      p("Un buen diseño de tablero mantiene la coherencia en los colores cuando representan", 
        strong("los mismos tipos de datos"), "pero varía los esquemas según el", 
        strong("tipo de visualización"), "y", strong("función del color"), ":"),
      
      layout_columns(
        col_widths = c(6, 6),
        div(
          h5("Esquema categórico", style = "margin-bottom: 10px; font-weight: 600; color: #333;"),
          p("Colores distintos para categorías diferentes (como distritos o tipos de servicios)"),
          div(class = "visualization-container", plotlyOutput("categorical_example", height = "200px"))
        ),
        div(
          h5("Esquema secuencial", style = "margin-bottom: 10px; font-weight: 600; color: #333;"),
          p("Variaciones de un color para mostrar intensidad (como satisfacción o participación)"),
          div(class = "visualization-container", plotlyOutput("sequential_example", height = "200px"))
        )
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        div(
          h5("Esquema divergente", style = "margin-bottom: 10px; font-weight: 600; color: #333;"),
          p("Dos colores opuestos para valores positivos y negativos (como aumentos y disminuciones)"),
          div(class = "visualization-container", plotlyOutput("diverging_example", height = "200px"))
        ),
        div(
          h5("Aplicación consistente", style = "margin-bottom: 10px; font-weight: 600; color: #333;"),
          p("Mismos distritos = mismos colores, en todos los tableros y tipos de gráficos"),
          div(class = "visualization-container", plotlyOutput("consistency_example", height = "200px"))
        )
      )
  ),
  
  # CSS to apply section color to card headers
  tags$style("
    .card-header::before {
      background-color: var(--accent-color, #006D77);
    }
  ")
)

# Server
server <- function(input, output, session) {
  
  # District map
  output$district_map <- renderLeaflet({
    # Demo map with simulated district polygons
    leaflet() %>%
      addTiles() %>%
      setView(lng = -106.42, lat = 31.67, zoom = 10) %>%
      addCircleMarkers(
        lng = -106.42 + rnorm(10, 0, 0.05),
        lat = 31.67 + rnorm(10, 0, 0.05),
        radius = 15,
        color = "white",
        weight = 1.5,
        opacity = 1,
        fillOpacity = 0.8,
        fillColor = district_colors,  # Consistent district colors
        label = lapply(1:10, function(i) {
          HTML(sprintf(
            '<div style="background-color: white; color: #333; padding: 8px 12px; border-radius: 6px; font-weight: 600; 
            text-align: center; box-shadow: 0 2px 4px rgba(0,0,0,0.2); font-family: Montserrat, sans-serif;">
            <span style="font-size: 14px;">Distrito %s</span><br>
            <span style="font-size: 18px;">%.1f</span></div>',
            i, district_data$satisfaction[i]
          ))
        })
      )
  })
  
  # Services bar chart - using section-specific primary color
  output$services_chart <- renderPlotly({
    plot_ly(
      data = service_data,
      x = ~rating,
      y = ~reorder(service, rating),
      type = "bar",
      orientation = "h",
      marker = list(
        color = "#006D77"  # Section-specific color (Bienestar)
      ),
      text = ~paste0(rating, "/10"),
      textposition = "auto"
    ) %>%
      layout(
        title = FALSE,
        xaxis = list(
          title = "Calificación (1-10)",
          range = c(0, 10),
          zeroline = TRUE,
          showgrid = TRUE,
          gridcolor = "#E9ECEF"
        ),
        yaxis = list(
          title = "",
          zeroline = FALSE,
          showgrid = FALSE
        ),
        margin = list(l = 100, r = 20, t = 20, b = 40),
        font = list(family = "Montserrat")
      )
  })
  
  # Categorical example (using distinct colors for categories)
  output$categorical_example <- renderPlotly({
    df <- data.frame(
      categoria = c("Agua", "Electricidad", "Gas", "Internet", "Transporte"),
      porcentaje = c(92, 98, 76, 82, 65)
    )
    
    plot_ly(
      data = df,
      x = ~categoria,
      y = ~porcentaje,
      type = "bar",
      marker = list(
        color = c("#006D77", "#2A9D8F", "#6969B3", "#F4A261", "#E76F51")
      )
    ) %>%
      layout(
        title = FALSE,
        xaxis = list(title = ""),
        yaxis = list(title = "", range = c(0, 100)),
        showlegend = FALSE
      )
  })
  
  # Sequential example (using shades of one color)
  output$sequential_example <- renderPlotly({
    df <- data.frame(
      distrito = factor(1:5),
      satisfaccion = c(8.2, 7.5, 6.8, 8.9, 7.1)
    )
    
    plot_ly(
      data = df,
      x = ~distrito,
      y = ~satisfaccion,
      type = "bar",
      marker = list(
        color = c("#006D77", "#3A8C94", "#74AAB0", "#ADCCD0", "#E6EEEF"),
        line = list(color = "#FFFFFF", width = 1)
      )
    ) %>%
      layout(
        title = FALSE,
        xaxis = list(title = "Distrito"),
        yaxis = list(title = "", range = c(0, 10)),
        showlegend = FALSE
      )
  })
  
  # Diverging example (using two contrasting colors)
  output$diverging_example <- renderPlotly({
    df <- data.frame(
      servicio = c("Agua", "Electricidad", "Internet", "Parques", "Calles", "Transporte"),
      cambio = c(1.2, -0.8, 0.5, -1.3, 2.1, -0.4)
    )
    
    plot_ly(
      data = df,
      x = ~servicio,
      y = ~cambio,
      type = "bar",
      marker = list(
        color = ~ifelse(cambio >= 0, "#006D77", "#E76F51")
      )
    ) %>%
      layout(
        title = FALSE,
        xaxis = list(title = ""),
        yaxis = list(title = "Cambio", zeroline = TRUE),
        showlegend = FALSE
      )
  })
  
  # Consistency example (same districts = same colors in different visualizations)
  output$consistency_example <- renderPlotly({
    # Using the same district colors in a different visualization type
    plot_ly() %>%
      add_pie(
        labels = ~paste("Distrito", 1:5),
        values = ~district_data$participation[1:5],
        marker = list(
          colors = district_colors[1:5]
        ),
        textinfo = "label+percent"
      ) %>%
      layout(
        title = FALSE,
        showlegend = FALSE
      )
  })
}

# Run the app
shinyApp(ui, server)