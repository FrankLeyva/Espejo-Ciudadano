# Guía: Módulos de Computación de Gráficos

## Introducción

Los módulos de computación de gráficos son el corazón del procesamiento de datos en el dashboard Espejo Ciudadano. Esta guía explica qué hace cada módulo, cómo usarlos, y cuándo aplicar cada uno, dirigida especialmente a principiantes en R.

## ¿Qué son los Módulos de Gráficos?

Los módulos son archivos R que procesan diferentes tipos de preguntas de encuesta y crean visualizaciones apropiadas. Cada módulo está especializado en un tipo específico de dato.

## Módulos Disponibles

### 📊 Tipos de Módulos por Tipo de Dato

```
R/
├── binary_module.R          # Preguntas Sí/No, Verdadero/Falso
├── categorical_module.R     # Categorías sin orden específico  
├── ordinal_module.R         # Escalas ordenadas (Malo-Bueno)
├── interval_module.R        # Datos numéricos con intervalos
├── nominal_module.R         # Texto libre y palabras clave
├── razon_module.R          # Datos de razón/proporción
└── special_module.R        # Casos especiales y reportes
```

## 1. Binary Module (Preguntas Binarias)

### 🎯 **¿Para qué sirve?**
Procesa preguntas que solo tienen dos respuestas posibles: Sí/No, Verdadero/Falso, Seleccionado/No seleccionado.

### 📋 **Ejemplos de uso:**
- "¿Votó en las últimas elecciones?" (Sí/No)
- "¿Conoce a su regidor?" (Sí/No) 
- "¿Reportó este problema?" (Sí/No)
- Preguntas de checkbox (Seleccionado/No seleccionado)

### ⚙️ **Función principal:**
```r
prepare_binary_data(data, question_id, metadata, treat_na_as_negative = NULL)
```

### 🔧 **Parámetros:**
- `data`: Datos de la encuesta
- `question_id`: ID de la pregunta (ej: "Q15")  
- `metadata`: Metadatos con etiquetas de valores
- `treat_na_as_negative`: ¿Tratar valores faltantes como "No"?

### 💡 **Características especiales:**
- **Detección automática** de preguntas tipo checkbox
- **Manejo inteligente** de valores NA (datos faltantes)
- **Soporte para múltiples formatos**: "1/0", "Sí/No", "TRUE/FALSE", "Selected/Not Selected"

### 📈 **Visualizaciones que genera:**
- Gráficos de barras por distrito
- Mapas con porcentajes
- Comparaciones por género y edad
- Tablas de frecuencia

### 🚨 **Casos especiales que maneja:**
```r
# Preguntas checkbox como Q6.1, Q6.2, etc.
if (grepl("^Q(6|17)\\.[0-9]+$", question_id)) {
  # Lógica especial para checkboxes
}
```

## 2. Categorical Module (Categorías)

### 🎯 **¿Para qué sirve?**
Procesa preguntas con múltiples categorías que NO tienen un orden específico.

### 📋 **Ejemplos de uso:**
- "¿En qué medio de transporte se mueve?" (Auto, Camión, Bicicleta, A pie)
- "¿Cuál es su ocupación?" (Empleado, Estudiante, Independiente, etc.)
- "¿Qué tipo de vivienda tiene?" (Casa, Departamento, Otra)

### ⚙️ **Función principal:**
```r
prepare_categorical_data(data, question_id, metadata)
```

### 🔧 **Características:**
- **No asume orden** en las categorías
- **Filtra respuestas NS/NC** (No Sabe/No Contesta)
- **Crea factores** para análisis estadístico

### 📈 **Visualizaciones típicas:**
- Gráficos de barras
- Gráficos circulares (pie charts)
- Tablas de frecuencia
- Distribuciones por distrito

## 3. Ordinal Module (Escalas Ordenadas)

### 🎯 **¿Para qué sirve?**
Procesa preguntas con categorías que SÍ tienen un orden lógico, como escalas de satisfacción.

### 📋 **Ejemplos de uso:**
- "¿Qué tan satisfecho está?" (Muy insatisfecho → Muy satisfecho)
- "¿Cómo califica el servicio?" (Muy malo → Muy bueno)
- "Nivel de confianza" (Nada → Mucho)

### ⚙️ **Función principal:**
```r
prepare_ordinal_data(data, question_id, metadata)
```

### 💡 **Características especiales:**
- **Respeta el orden** de las categorías
- **Maneja casos mixtos** (números + texto como "No hay")
- **Procesa escalas complejas** como 1-10 + "No existe"

### 📊 **Ejemplo de escala compleja:**
```
1 = Muy malo
2 = Malo  
3 = Regular
4 = Bueno
5 = Muy bueno
99 = No sabe/No contesta
```

### 📈 **Visualizaciones que genera:**
- Histogramas ordenados
- Gráficos de barras con orden lógico
- Estadísticas de tendencia central

## 4. Interval Module (Datos Numéricos)

### 🎯 **¿Para qué sirve?**
Procesa datos numéricos donde los intervalos entre valores tienen significado, como escalas 1-10, edades, años, etc.

### 📋 **Ejemplos de uso:**
- Escalas de 1 a 10 de satisfacción
- "¿Cuántos años tiene viviendo aquí?"
- "¿Del 1 al 10, qué tan seguro se siente?"
- Evaluaciones numéricas

### ⚙️ **Función principal:**
```r
prepare_interval_data(data, question_id, metadata)
```

### 💪 **Capacidades avanzadas:**
- **Estadísticas completas**: media, mediana, desviación estándar
- **Detección automática** de escalas (1-5, 1-7, 1-10)
- **Manejo inteligente** de valores atípicos
- **Múltiples visualizaciones**

### 📈 **Visualizaciones disponibles:**
```r
# Tipos de gráficos que puede generar:
"histogram"         # Histograma de distribución
"pie"              # Gráfico circular
"map"              # Mapa por distritos
"age_bars"         # Barras por edad
"gender_dumbbell"  # Comparación por género
"bars"             # Barras por distrito
"ridge_plot"       # Gráfico de crestas (distribuciones)
```

### 🗺️ **Funcionalidades del mapa:**
- **Modo promedio**: Muestra promedios por distrito
- **Modo filtrado**: Filtra respuestas específicas
- **Resaltado automático**: Marca distritos con valores extremos
- **Gradientes de color**: Visualización por intensidad

### 📊 **Estadísticas que calcula:**
```r
# Estadísticas generales
Media, Mediana, Desviación Estándar, Mínimo, Máximo, Moda

# Por distrito, género y grupo de edad
Estadísticas completas para cada subgrupo
```

## 5. Nominal Module (Texto Libre)

### 🎯 **¿Para qué sirve?**
Procesa respuestas de texto libre, palabras clave y comentarios abiertos.

### 📋 **Ejemplos de uso:**
- "¿Cuál es el principal problema de su colonia?"
- "¿Qué sugiere para mejorar el transporte?"
- Comentarios y sugerencias abiertas

### ⚙️ **Función principal:**
```r
prepare_nominal_data(data, question_id, metadata)
```

### 🔍 **Procesamiento de texto:**
- **Normalización**: Convierte a minúsculas, remueve acentos
- **Limpieza**: Elimina puntuación y espacios extra
- **Tokenización**: Separa en palabras individuales
- **Frecuencia de palabras**: Cuenta ocurrencias

### 📝 **Funcionalidades incluidas:**
```r
# Palabras vacías en español
get_spanish_stopwords()  # "a", "de", "el", "la", etc.

# Análisis de frecuencia
word_freq <- as.data.frame(table(tokens))

# Nube de palabras (si está disponible)
create_wordcloud(word_freq)
```

## 6. Razon Module (Datos de Razón)

### 🎯 **¿Para qué sirve?**
Procesa datos numéricos donde el cero absoluto tiene significado, como cantidades, edades reales, ingresos, etc.

### 📋 **Ejemplos de uso:**
- "¿Cuántos años tiene?"
- "¿Cuántas personas viven en su hogar?"
- "¿Cuántas veces al mes usa el transporte público?"

### ⚙️ **Función principal:**
```r
prepare_razon_data(data, question_id, metadata)
```

### 📊 **Análisis que realiza:**
- **Medias por distrito**: Calcula promedios geográficos
- **Distribución por edad**: Análisis demográfico
- **Detección de valores atípicos**: Identifica respuestas inusuales
- **Estadísticas de tendencia**: Media, mediana, moda

## 7. Special Module (Casos Especiales)

### 🎯 **¿Para qué sirve?**
Maneja visualizaciones especializadas que no encajan en los otros módulos, como reportes de problemas urbanos.

### 📋 **Ejemplo principal:**
**Reporte de Problemas de Servicios**:
- Q32-Q33: Agua potable (¿reportó? → ¿fue atendido?)
- Q48-Q49: Alumbrado público
- Q52-Q53: Calles y pavimentación  
- Q37-Q38: Energía eléctrica
- Q42-Q43: Recolección de basura

### ⚙️ **Función principal:**
```r
create_report_statistics(survey_data)
```

### 🎨 **Características visuales:**
- **Gráfico de barras horizontal**: Muestra % que reportaron
- **Cajas de porcentaje**: Muestra % que fue atendido
- **Colores específicos** por servicio
- **Versión responsive**: Adapta a móvil y escritorio

### 🏗️ **Lógica de procesamiento:**
```r
# Para cada servicio:
1. Calcula % que hizo reporte
2. Para los que reportaron → calcula % atendido
3. Crea visualización combinada

# Maneja múltiples formatos de respuesta:
"1", "TRUE", "Yes", "true" = Positivo
```

## Uso Práctico de los Módulos

### 🔍 **¿Cómo determinar qué módulo usar?**

#### 1. **Analiza el tipo de pregunta:**
```r
# Revisa los metadatos
metadata %>% filter(variable == "Q15") %>% select(ResponseType)
```

#### 2. **Tipos de respuesta:**
- **"Binary"** → `binary_module.R`
- **"Ordinal"** → `ordinal_module.R` 
- **"Nominal"** → `categorical_module.R` o `nominal_module.R`
- **"Interval"** → `interval_module.R`
- **Números puros** → `razon_module.R`
- **Reportes especiales** → `special_module.R`

### 🛠️ **Patrón de uso típico:**

```r
# 1. Cargar datos y metadatos
data <- read.csv("data/processed/PER_2024_responses.csv")
metadata <- read.csv("data/processed/PER_2024_metadata_classified.csv")

# 2. Preparar datos según el tipo
if (response_type == "Binary") {
  prepared_data <- prepare_binary_data(data, "Q15", metadata)
} else if (response_type == "Interval") {
  prepared_data <- prepare_interval_data(data, "Q15", metadata)
}
# etc.

# 3. Crear visualizaciones
plot <- create_binary_district_map(prepared_data, geo_data)
```

## Integración con el Dashboard

### 🔗 **¿Cómo se conectan los módulos?**

#### 1. **En los archivos de servidor** (`*_server.R`):
```r
# Los módulos se usan para procesar datos
plots <- reactive({
  data_manager$get_plots("wellness", selectedYear())
})

output$economic_plot <- renderPlotly({
  plots()$economic_situation_pie  # Gráfico pregenerado
})
```

#### 2. **En los archivos de datos** (`data/plots/*.rds`):
```r
# Los módulos generan estos archivos:
wellness_plots <- list(
  "Q25" = binary_plot,      # Gráfico binario
  "Q26" = interval_plot,    # Gráfico de intervalo
  "Q27" = categorical_plot  # Gráfico categórico
)

saveRDS(wellness_plots, "data/plots/wellness_2024.rds")
```

## Personalización y Temas

### 🎨 **Soporte de temas personalizados:**

Todos los módulos aceptan un parámetro `custom_theme`:

```r
# Ejemplo con tema personalizado
my_theme <- list(
  colors = list(
    primary = "#2E8B57",
    secondary = "#4682B4"
  ),
  typography = list(
    font_family = "Roboto"
  )
)

plot <- create_interval_histogram(
  data, 
  custom_theme = my_theme
)
```

## Debugging y Solución de Problemas

### 🐛 **Errores comunes:**

#### 1. **"Question not found"**
```r
✗ Error: Question Q99 not found
✓ Solución: Verifica que la pregunta existe en los datos
names(data)  # Ver columnas disponibles
```

#### 2. **"No valid data"**
```r
✗ Warning: No valid data for question Q15
✓ Solución: Revisa si hay datos válidos
table(data$Q15, useNA = "always")
```

#### 3. **"Missing metadata"**
```r
✗ Error: Missing question metadata
✓ Solución: Verifica el archivo de metadatos
metadata %>% filter(variable == "Q15")
```

### 🔍 **Herramientas de debugging:**

```r
# Ver atributos de los datos procesados
prepared_data <- prepare_binary_data(data, "Q15", metadata)
attributes(prepared_data)

# Inspeccionar conteos
attr(prepared_data, "total_responses")
attr(prepared_data, "missing_count")

# Ver etiquetas de valores
attr(prepared_data, "value_labels")
```

## Extensión y Modificación

### ➕ **¿Cómo agregar nuevas funcionalidades?**

#### 1. **Agregar nueva visualización:**
```r
# En interval_module.R, agregar nueva función:
create_interval_new_plot <- function(data, custom_theme = NULL) {
  # Tu código aquí
}

# Luego actualizar la UI para incluir la opción
```

#### 2. **Modificar procesamiento de datos:**
```r
# En prepare_*_data(), agregar nueva lógica:
if (special_case_condition) {
  # Procesamiento especial
}
```

#### 3. **Agregar nuevo tipo de módulo:**
```r
# Crear nuevo archivo: R/mi_nuevo_module.R
prepare_mi_nuevo_data <- function(data, question_id, metadata) {
  # Lógica de preparación
}

create_mi_nuevo_plot <- function(data, custom_theme = NULL) {
  # Lógica de visualización
}
```

## Checklist de Desarrollo

### ✅ **Para modificar un módulo existente:**
- [ ] Hacer backup del archivo original
- [ ] Entender la estructura actual
- [ ] Probar cambios con datos de muestra
- [ ] Verificar que no rompe funcionalidad existente
- [ ] Documentar cambios realizados

### ✅ **Para crear nuevo módulo:**
- [ ] Definir tipo de datos que procesará
- [ ] Crear función `prepare_*_data()`
- [ ] Crear funciones de visualización
- [ ] Agregar soporte para temas personalizados
- [ ] Incluir manejo de errores
- [ ] Documentar funciones y parámetros
- [ ] Probar con datos reales
- [ ] Integrar con el dashboard principal

---

## Recursos Adicionales

### 📚 **Documentación relacionada:**
- [Guía Principal del Dashboard](GUIA_DASHBOARD.md)
- [Modificar y Reorganizar Gráficos](GUIA_MODIFICAR_GRAFICOS.md)
- [Agregar Nuevas Encuestas](GUIA_NUEVAS_ENCUESTAS.md)

### 🛠️ **Herramientas útiles:**
```r
# Ver estructura de un módulo
str(prepared_data)

# Probar función de módulo
test_data <- prepare_binary_data(sample_data, "Q15", metadata)

# Verificar outputs
names(test_data)
attributes(test_data)
```

---
*Guía de Módulos de Computación de Gráficos - Dashboard Espejo Ciudadano*  
*Dirigida a principiantes en R para entender y modificar el procesamiento de datos*