library(shiny)
library(bslib)
library(plotly)
library(leaflet)
library(dplyr)

# Custom CSS for styling
custom_css <- '
body {
  font-family: "Montserrat", "Segoe UI", sans-serif;
  background-color: #F8F9FA;
  color: #333333;
  padding: 20px;
}

.dashboard-card {
  background-color: #FFFFFF;
  border-radius: 8px;
  box-shadow: 0 2px 8px rgba(0,0,0,0.06);
  padding: 20px;
  margin-bottom: 24px;
}

.card-header {
  font-family: "Montserrat", sans-serif;
  font-weight: 600;
  font-size: 18px;
  color: #333333;
  padding-bottom: 12px;
  border-bottom: 1px solid #E9ECEF;
  margin-bottom: 16px;
}

.card-header::before {
  content: "";
  display: inline-block;
  width: 4px;
  height: 18px;
  background-color: #006D77;
  margin-right: 8px;
  vertical-align: text-top;
}

h2 {
  font-size: 22px;
  font-weight: 600;
  margin-bottom: 16px;
  color: #333333;
}

h3 {
  font-size: 18px;
  font-weight: 600;
  margin-bottom: 12px;
  color: #333333;
}

.section-divider {
  height: 1px;
  background-color: #E9ECEF;
  margin: 24px 0;
}

.comparison-container {
  display: flex;
  flex-wrap: wrap;
}

.comparison-item {
  flex: 1;
  min-width: 300px;
  margin-right: 24px;
  margin-bottom: 24px;
}

.palette-row {
  display: flex;
  margin-bottom: 10px;
}

.color-box {
  width: 50px;
  height: 50px;
  margin-right: 5px;
  border-radius: 4px;
  border: 1px solid #E9ECEF;
}

.palette-label {
  font-size: 14px;
  margin-bottom: 5px;
  font-weight: 500;
}

pre {
  background-color: #F8F9FA;
  padding: 10px;
  border-radius: 4px;
  font-family: "Consolas", monospace;
}
'

# UI
ui <- page_fluid(
  tags$head(
    tags$link(rel = "stylesheet", 
              href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@400;500;600;700&display=swap"),
    tags$style(custom_css)
  ),
  
  h2("Estrategias de Color para Visualizaciones"),
  
  p("Esta guía muestra estrategias de color optimizadas para diferentes tipos de visualizaciones, manteniendo la coherencia visual sin ser repetitivo."),
  
  div(class = "dashboard-card",
      div(class = "card-header", "Paletas Sequenciales Mejoradas para Fondos Claros"),
      
      p("Las paletas sequenciales estándar pueden tener problemas de visibilidad contra fondos blancos, especialmente para gráficos circulares. Estas versiones ajustadas mantienen la coherencia temática pero con mejor visibilidad:"),
      
      layout_columns(
        col_widths = c(6, 6),
        div(
          h3("Problema: Demasiado Claro"),
          plotlyOutput("problem_sequential"),
          div(class = "palette-row", style = "margin-top: 10px",
              div(class = "color-box", style = "background-color: #E6EEEF"),
              div(class = "color-box", style = "background-color: #ADCCD0"),
              div(class = "color-box", style = "background-color: #74AAB0"),
              div(class = "color-box", style = "background-color: #3A8C94"),
              div(class = "color-box", style = "background-color: #006D77")
          ),
          p("Los colores más claros se pierden contra el fondo blanco")
        ),
        div(
          h3("Solución: Rango Ajustado"),
          plotlyOutput("improved_sequential"),
          div(class = "palette-row", style = "margin-top: 10px",
              div(class = "color-box", style = "background-color: #B2D8D8"),
              div(class = "color-box", style = "background-color: #88C1C1"),
              div(class = "color-box", style = "background-color: #5EAAAA"),
              div(class = "color-box", style = "background-color: #349393"),
              div(class = "color-box", style = "background-color: #007C7C")
          ),
          p("Mismo tema de color pero comenzando desde un tono medio")
        )
      )
  ),
  
  div(class = "dashboard-card",
      div(class = "card-header", "Comparación de Estrategias para Gráficos Circulares"),
      
      p("Los gráficos circulares requieren colores claramente distinguibles, pero que todavía muestren una relación temática:"),
      
      layout_columns(
        col_widths = c(6, 6),
        div(
          h3("Problema: Secuencial Puro"),
          plotlyOutput("problem_pie"),
          p("Difícil distinguir entre categorías adyacentes")
        ),
        div(
          h3("Solución: Colores Análogos"),
          plotlyOutput("improved_pie"),
          p("Mantiene cohesión temática pero con mayor diferenciación")
        )
      )
  ),
  
  div(class = "dashboard-card",
      div(class = "card-header", "Estrategia para Múltiples Visualizaciones"),
      
      p("Al tener múltiples visualizaciones en el mismo panel, usar estas estrategias de color:"),
      
      h3("1. Consistencia por Tipo de Entidad"),
      p("Los distritos siempre usan la misma paleta de colores en todas las visualizaciones:"),
      
      layout_columns(
        col_widths = c(6, 6),
        div(
          plotlyOutput("district_bars")
        ),
        div(
          plotlyOutput("district_pie")
        )
      ),
      
      div(class = "section-divider"),
      
      h3("2. Diferentes Esquemas para Diferentes Tipos de Datos"),
      p("Los tipos de servicios, tendencias y métricas usan paletas distintas pero coordinadas:"),
      
      layout_columns(
        col_widths = c(4, 4, 4),
        div(
          plotlyOutput("services_chart"),
          p("Servicios - Paleta Categórica")
        ),
        div(
          plotlyOutput("trend_chart"),
          p("Tendencias - Paleta Divergente")
        ),
        div(
          plotlyOutput("metrics_chart"),
          p("Métricas - Paleta Secuencial Ajustada")
        )
      )
  ),
  
  div(class = "dashboard-card",
      div(class = "card-header", "Implementación en el Código"),
      
      h3("Paletas Recomendadas por Tipo de Visualización"),
      
      tags$pre(
'# Colores de distrito (consistentes entre visualizaciones)
district_colors <- c("#88BDBC", "#6E9887", "#BECC92", "#FDD692", 
                     "#F1BB87", "#F28A80", "#D1A5C6", "#9CADCE", 
                     "#B6C5D1", "#D3D9E0")

# Paleta secuencial para métricas (rango ajustado para todos los fondos)
bienestar_sequential <- c("#B2D8D8", "#88C1C1", "#5EAAAA", "#349393", "#007C7C")
movilidad_sequential <- c("#B7E4D7", "#8FD1BD", "#67BEA2", "#3EAB88", "#16976D")
gobierno_sequential <- c("#BFB8E8", "#9D94D8", "#7A6FC9", "#584BB9", "#3526A9")
infraestructura_sequential <- c("#FFDBB5", "#FFC48A", "#FFAC5F", "#FF9534", "#FF7E09")
participacion_sequential <- c("#FFB8A8", "#FF957D", "#FF7253", "#FF5028", "#FE2E00")

# Paleta para datos divergentes
diverging_palette <- colorRampPalette(c("#E76F51", "#FFFFFF", "#006D77"))(7)

# Uso consistente en funciones de visualización
create_bar_chart <- function(data, color_type = "sequential", section = "bienestar") {
  # Elegir paleta según el contexto
  if (color_type == "categorical") {
    # Para categorías distintas como servicios
    colors <- district_colors[1:nrow(data)]
  } else if (color_type == "sequential") {
    # Para intensidad o niveles
    if (section == "bienestar") {
      colors <- bienestar_sequential
    } else if (section == "movilidad") {
      colors <- movilidad_sequential
    } else {
      colors <- bienestar_sequential  # Default
    }
  } else if (color_type == "diverging") {
    # Para tendencias positivas/negativas
    colors <- diverging_palette
  }
  
  # Crear gráfico con la paleta elegida
  plot_ly(
    data = data,
    type = "bar",
    marker = list(
      color = colors
    )
  )
}'
      )
  )
)

# Server
server <- function(input, output, session) {
  
  # Problem sequential example
  output$problem_sequential <- renderPlotly({
    df <- data.frame(
      categoria = c("Cat1", "Cat2", "Cat3", "Cat4", "Cat5"),
      valor = c(25, 35, 15, 10, 15)
    )
    
    plot_ly(
      labels = ~df$categoria,
      values = ~df$valor,
      type = "pie",
      marker = list(
        colors = c("#E6EEEF", "#ADCCD0", "#74AAB0", "#3A8C94", "#006D77")
      ),
      textinfo = "label+percent"
    ) %>%
      layout(
        showlegend = FALSE
      )
  })
  
  # Improved sequential example
  output$improved_sequential <- renderPlotly({
    df <- data.frame(
      categoria = c("Cat1", "Cat2", "Cat3", "Cat4", "Cat5"),
      valor = c(25, 35, 15, 10, 15)
    )
    
    plot_ly(
      labels = ~df$categoria,
      values = ~df$valor,
      type = "pie",
      marker = list(
        colors = c("#B2D8D8", "#88C1C1", "#5EAAAA", "#349393", "#007C7C")
      ),
      textinfo = "label+percent"
    ) %>%
      layout(
        showlegend = FALSE
      )
  })
  
  # Problem pie chart
  output$problem_pie <- renderPlotly({
    df <- data.frame(
      categoria = c("Servicio 1", "Servicio 2", "Servicio 3", "Servicio 4", "Servicio 5", "Servicio 6"),
      valor = c(22, 18, 12, 15, 19, 14)
    )
    
    plot_ly(
      labels = ~df$categoria,
      values = ~df$valor,
      type = "pie",
      marker = list(
        colors = colorRampPalette(c("#E6EEEF", "#006D77"))(6)
      ),
      textinfo = "label+percent"
    ) %>%
      layout(
        showlegend = FALSE
      )
  })
  
  # Improved pie chart
  output$improved_pie <- renderPlotly({
    df <- data.frame(
      categoria = c("Servicio 1", "Servicio 2", "Servicio 3", "Servicio 4", "Servicio 5", "Servicio 6"),
      valor = c(22, 18, 12, 15, 19, 14)
    )
    
    plot_ly(
      labels = ~df$categoria,
      values = ~df$valor,
      type = "pie",
      marker = list(
        colors = c("#006D77", "#1A8A98", "#339999", "#4DB1AE", "#66C2AD", "#7FCCBF")
      ),
      textinfo = "label+percent"
    ) %>%
      layout(
        showlegend = FALSE
      )
  })
  
  # District bars
  output$district_bars <- renderPlotly({
    df <- data.frame(
      distrito = paste("Distrito", 1:5),
      valor = c(85, 67, 92, 71, 78)
    )
    
    plot_ly(
      data = df,
      x = ~distrito,
      y = ~valor,
      type = "bar",
      marker = list(
        color = c("#88BDBC", "#6E9887", "#BECC92", "#FDD692", "#F1BB87")
      )
    ) %>%
      layout(
        title = FALSE,
        xaxis = list(title = ""),
        yaxis = list(title = "", range = c(0, 100)),
        showlegend = FALSE
      )
  })
  
  # District pie
  output$district_pie <- renderPlotly({
    df <- data.frame(
      distrito = paste("Distrito", 1:5),
      valor = c(85, 67, 92, 71, 78)
    )
    
    plot_ly(
      labels = ~df$distrito,
      values = ~df$valor,
      type = "pie",
      marker = list(
        colors = c("#88BDBC", "#6E9887", "#BECC92", "#FDD692", "#F1BB87")
      ),
      textinfo = "label+percent"
    ) %>%
      layout(
        showlegend = FALSE
      )
  })
  
  # Services chart (categorical)
  output$services_chart <- renderPlotly({
    df <- data.frame(
      servicio = c("Agua", "Luz", "Gas", "Internet"),
      valor = c(65, 82, 71, 55)
    )
    
    plot_ly(
      data = df,
      x = ~servicio,
      y = ~valor,
      type = "bar",
      marker = list(
        color = c("#006D77", "#2A9D8F", "#6969B3", "#F4A261")
      )
    ) %>%
      layout(
        title = FALSE,
        xaxis = list(title = ""),
        yaxis = list(title = "", range = c(0, 100)),
        showlegend = FALSE
      )
  })
  
  # Trend chart (diverging)
  output$trend_chart <- renderPlotly({
    df <- data.frame(
      año = c("2020", "2021", "2022", "2023", "2024"),
      cambio = c(-0.8, -0.3, 0.2, 0.7, 1.5)
    )
    
    plot_ly(
      data = df,
      x = ~año,
      y = ~cambio,
      type = "bar",
      marker = list(
        color = ~ifelse(cambio >= 0, "#006D77", "#E76F51"),
        opacity = ~abs(cambio)/max(abs(cambio))
      )
    ) %>%
      layout(
        title = FALSE,
        xaxis = list(title = ""),
        yaxis = list(title = ""),
        showlegend = FALSE
      )
  })
  
  # Metrics chart (sequential)
  output$metrics_chart <- renderPlotly({
    df <- data.frame(
      metrica = c("Salud", "Vivienda", "Educación", "Empleo", "Servicios"),
      valor = c(7.5, 6.8, 8.1, 5.9, 7.2)
    )
    
    plot_ly(
      data = df,
      x = ~metrica,
      y = ~valor,
      type = "bar",
      marker = list(
        color = c("#B2D8D8", "#88C1C1", "#5EAAAA", "#349393", "#007C7C"),
        line = list(color = "#FFFFFF", width = 1)
      )
    ) %>%
      layout(
        title = FALSE,
        xaxis = list(title = ""),
        yaxis = list(title = "", range = c(0, 10)),
        showlegend = FALSE
      )
  })
}

# Run the app
shinyApp(ui, server)