# Guía: Modificar y Reorganizar Gráficos

## Introducción

Esta guía explica cómo modificar, reorganizar y personalizar los gráficos del dashboard Espejo Ciudadano. Es ideal para principiantes que quieren cambiar la presentación visual de los datos.

## Conceptos Clave

### 1. Arquitectura de Gráficos
```
Datos (.csv) → Procesamiento → Gráficos (.rds) → Dashboard
```

### 2. Tipos de Visualización
- **Plotly**: Gráficos interactivos (barras, pastel, líneas)
- **Leaflet**: Mapas interactivos
- **Value Boxes**: Cajas con porcentajes destacados

### 3. Estructura de Archivos
```
R/
├── *_module.R              # Módulos de tipo de gráfico
├── [tema]/
│   ├── [tema]_ui.R         # Interfaz visual
│   └── [tema]_server.R     # Lógica del servidor
data/plots/
├── [tema]_[año].rds        # Gráficos pregenerados
```

## Ubicar Gráficos Existentes

### 1. Identificar la Sección
Las secciones principales son:
- **government** (Gobierno)
- **wellness** (Bienestar) 
- **infrastructure** (Infraestructura)
- **participation** (Participación)
- **urban** (Urbanismo)

### 2. Encontrar los Archivos Relacionados
Para modificar un gráfico en la sección "Bienestar":
```
R/wellness/
├── wellness_ui.R           # Layout y navegación
├── wellness_server.R       # Lógica de renderizado
├── economy_ui.R           # UI específica de economía
├── economy_server.R       # Servidor de economía
├── healthcare_ui.R        # UI de salud
└── healthcare_server.R    # Servidor de salud
```

### 3. Ver Datos de Gráficos
```r
# Cargar gráficos de bienestar 2024
plots <- readRDS("data/plots/wellness_2024.rds")
names(plots)  # Ver todos los gráficos disponibles
```

## Modificar Gráficos Existentes

### Paso 1: Identificar el Gráfico
En `R/wellness/wellness_server.R`, busca el output:
```r
output$economic_situation_pie <- renderPlotly({
  plots()$economic_situation_pie  # ← Este es el gráfico
})
```

### Paso 2: Localizar en el Archivo .rds
```r
# Ver el gráfico específico
plots <- readRDS("data/plots/wellness_2024.rds")
plots$economic_situation_pie  # ← Aquí está el objeto ggplot
```

### Paso 3: Modificar el Gráfico
```r
# Cargar el archivo
plots <- readRDS("data/plots/wellness_2024.rds")

# Modificar el gráfico (ejemplo: cambiar colores)
plots$economic_situation_pie <- plots$economic_situation_pie + 
  scale_fill_manual(values = c("#FF6B6B", "#4ECDC4", "#45B7D1"))

# Guardar cambios
saveRDS(plots, "data/plots/wellness_2024.rds")
```

### Paso 4: Probar Cambios
```r
# Ejecutar dashboard para ver cambios
shiny::runApp(port = 3838)
```

## Reorganizar el Layout de una Sección

### Modificar la Interfaz (UI)
Archivo: `R/wellness/wellness_ui.R`

#### Cambiar Orden de Tarjetas de Navegación
```r
# ANTES - Orden original
div(class = "section-nav-grid",
  # Economy Card
  div(class = "nav-card", ...),  
  # Healthcare Card  
  div(class = "nav-card", ...),
  # Education Card
  div(class = "nav-card", ...)
)

# DESPUÉS - Nuevo orden
div(class = "section-nav-grid",
  # Education Card (ahora primero)
  div(class = "nav-card", ...),  
  # Economy Card (ahora segundo)
  div(class = "nav-card", ...),
  # Healthcare Card (ahora tercero)
  div(class = "nav-card", ...)
)
```

#### Cambiar Layout de Gráficos
```r
# ANTES - Layout en columnas
layout_columns(
  col_widths = c(6, 6),
  card(...),  # Primer gráfico
  card(...)   # Segundo gráfico
)

# DESPUÉS - Layout en filas
layout_columns(
  col_widths = 12,
  card(...),  # Primer gráfico (fila completa)
  card(...)   # Segundo gráfico (fila completa) 
)
```

## Agregar Nuevos Gráficos

### Paso 1: Agregar al Archivo .rds
```r
# Cargar gráficos existentes
plots <- readRDS("data/plots/wellness_2024.rds")

# Crear nuevo gráfico (ejemplo)
library(ggplot2)
library(dplyr)

# Supongamos que tienes datos para un nuevo gráfico
new_plot <- ggplot(data = mi_data, aes(x = categoria, y = valor)) +
  geom_bar(stat = "identity", fill = "#4ECDC4") +
  theme_minimal() +
  labs(title = "Mi Nuevo Gráfico", 
       x = "Categoría", 
       y = "Valor")

# Agregar a la lista
plots$mi_nuevo_grafico <- new_plot

# Guardar
saveRDS(plots, "data/plots/wellness_2024.rds")
```

### Paso 2: Agregar Output en Server
En `R/wellness/wellness_server.R`:
```r
# Agregar nuevo output
output$mi_nuevo_grafico <- renderPlotly({
  plots()$mi_nuevo_grafico
})
```

### Paso 3: Agregar a la UI
En `R/wellness/wellness_ui.R`:
```r
# Agregar donde quieras que aparezca
card(
  card_header("Mi Nuevo Gráfico"),
  card_body(
    plotlyOutput("mi_nuevo_grafico", height = "400px")
  )
)
```

## Cambiar Tipos de Visualización

### De Gráfico de Barras a Pastel
```r
# ANTES - Gráfico de barras
bar_plot <- ggplot(data, aes(x = categoria, y = valor)) +
  geom_bar(stat = "identity")

# DESPUÉS - Gráfico de pastel
pie_plot <- ggplot(data, aes(x = "", y = valor, fill = categoria)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void()
```

### De Gráfico Estático a Interactivo
```r
library(plotly)

# Crear gráfico ggplot normal
static_plot <- ggplot(data, aes(x = x, y = y)) +
  geom_point()

# Convertir a plotly interactivo  
interactive_plot <- ggplotly(static_plot)
```

## Personalizar Colores y Temas

### Colores por Sección
El dashboard usa colores específicos por tema:
```css
--gobierno-color: #2E8B57;      /* Verde gobierno */
--bienestar-color: #4682B4;     /* Azul bienestar */
--infraestructura-color: #B8860B; /* Oro infraestructura */
--participacion-color: #9370DB;  /* Púrpura participación */
--urbanismo-color: #DC143C;      /* Rojo urbanismo */
```

### Aplicar Colores Consistentes
```r
# Usar colores del tema
wellness_color <- "#4682B4"

my_plot <- ggplot(data, aes(x = x, y = y)) +
  geom_bar(stat = "identity", fill = wellness_color) +
  theme_minimal()
```

### Personalizar Tema Global
En `R/global_theme.R`, puedes modificar:
```r
custom_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    panel.grid = element_line(color = "gray90")
  )
```

## Trabajar con Mapas

### Modificar Mapas Existentes
Los mapas están en archivos como `map_[tema]_[año].rds`:
```r
# Cargar mapas
maps <- readRDS("data/plots/map_wellness_2024.rds")
names(maps)  # Ver mapas disponibles
```

### Cambiar Colores de Mapas
```r
# Ejemplo de modificación de mapa
library(leaflet)

# Crear nuevo mapa con diferentes colores
new_map <- leaflet(geo_data) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~colorNumeric("Blues", values)(values),  # Cambiar paleta
    fillOpacity = 0.7,
    color = "white",
    weight = 2
  )
```

## Manejo de Datos para Nuevos Gráficos

### Cargar Datos de Respuestas
```r
# Cargar datos procesados
responses <- read.csv("data/processed/PER_2024_responses.csv")
metadata <- read.csv("data/processed/PER_2024_metadata_classified.csv")

# Filtrar por pregunta específica
question_data <- responses %>%
  select(Q2, Q5) %>%  # Q2=distrito, Q5=pregunta específica
  filter(!is.na(Q5))
```

### Procesar Datos por Distrito
```r
# Agrupar por distrito
district_summary <- question_data %>%
  group_by(Q2) %>%
  summarise(
    positivos = sum(Q5 == 1, na.rm = TRUE),
    total = n(),
    porcentaje = (positivos / total) * 100
  )
```

## Scripts de Automatización

### Script para Regenerar Todos los Gráficos
```r
# regenerate_plots.R
regenerate_section_plots <- function(section, year) {
  message(paste("Regenerando gráficos para", section, year))
  
  # Cargar datos
  responses <- read.csv(paste0("data/processed/", 
                              get_survey_type(year), "_", year, "_responses.csv"))
  
  # Generar gráficos según sección
  plots_list <- generate_plots_for_section(section, responses)
  
  # Guardar
  saveRDS(plots_list, paste0("data/plots/", section, "_", year, ".rds"))
  
  message("✓ Completado")
}

# Ejecutar para todas las secciones
sections <- c("government", "wellness", "infrastructure", "participation", "urban")
years <- c("2023", "2024")

for(section in sections) {
  for(year in years) {
    regenerate_section_plots(section, year)
  }
}
```

## Errores Comunes y Soluciones

### Error: "Object not found"
```r
✗ Error: object 'mi_grafico' not found
✓ Solución: Verifica que el nombre en plots$mi_grafico coincide con output$mi_grafico
```

### Error: "Plot is empty"
```r
✗ Error: Gráfico vacío o sin datos
✓ Solución: Verifica que los datos no estén vacíos y los filtros sean correctos
```

### Error: "UI element not showing"
```r
✗ Error: Elemento UI no aparece
✓ Solución: Verifica que agregaste tanto el output en server como el elemento en UI
```

## Checklist para Modificaciones

### Modificar Gráfico Existente:
- [ ] Localizar en archivo .rds
- [ ] Hacer modificaciones
- [ ] Guardar archivo .rds
- [ ] Probar en dashboard

### Agregar Nuevo Gráfico:
- [ ] Crear objeto de gráfico
- [ ] Agregar a archivo .rds
- [ ] Agregar output en server
- [ ] Agregar elemento en UI
- [ ] Probar funcionamiento

### Reorganizar Layout:
- [ ] Identificar archivo UI
- [ ] Modificar estructura HTML/Shiny
- [ ] Verificar responsive design
- [ ] Probar en diferentes tamaños de pantalla

## Recursos Adicionales

### Documentación Útil
- **ggplot2**: https://ggplot2.tidyverse.org/
- **plotly**: https://plotly.com/r/
- **leaflet**: https://rstudio.github.io/leaflet/
- **bslib**: https://rstudio.github.io/bslib/

### Herramientas de Desarrollo
```r
# Ver estructura de datos
str(plots)
names(plots)
summary(data)

# Debugging
browser()  # Pausar ejecución para inspeccionar
print(mi_variable)  # Imprimir valores
```

---
*Guía para modificar y reorganizar gráficos - Dashboard Espejo Ciudadano*