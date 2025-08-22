# Guía: Ejemplos Prácticos de Módulos de Gráficos

## Introducción

Esta guía proporciona ejemplos paso a paso de cómo usar cada módulo de computación de gráficos en situaciones reales. Perfecto para principiantes que quieren ver código funcionando.

## Ejemplo 1: Pregunta Binaria (Sí/No)

### 🎯 **Pregunta:** "¿Conoce el nombre de su regidor?"

#### **Paso 1: Identificar el tipo**
```r
# Esta es una pregunta binaria: Sí/No
# Usaremos binary_module.R
```

#### **Paso 2: Cargar datos**
```r
# Cargar datos de la encuesta
data <- read.csv("data/processed/PER_2024_responses.csv")
metadata <- read.csv("data/processed/PER_2024_metadata_classified.csv")

# Verificar que la pregunta existe
"Q15" %in% names(data)  # TRUE si existe
```

#### **Paso 3: Procesar datos**
```r
# Usar el módulo binario
source("R/binary_module.R")

# Preparar los datos
prepared_data <- prepare_binary_data(
  data = data,
  question_id = "Q15", 
  metadata = metadata
)

# Ver qué obtuvimos
str(prepared_data)
```

#### **Paso 4: Analizar resultados**
```r
# Ver estadísticas básicas
cat("Total de respuestas:", attr(prepared_data, "total_responses"))
cat("Respuestas válidas:", nrow(prepared_data))

# Ver distribución de respuestas
table(prepared_data$binary_value)
#   FALSE  TRUE 
#    342   158   # Ejemplo: 158 sí conocen, 342 no
```

#### **Paso 5: Crear visualizaciones**
```r
# Gráfico por distritos
district_plot <- create_binary_bars(prepared_data)

# Mapa interactivo
district_map <- create_binary_district_map(prepared_data, geo_data)

# Comparación por género
gender_plot <- create_binary_gender_comparison(prepared_data)
```

## Ejemplo 2: Escala de Satisfacción (Ordinal)

### 🎯 **Pregunta:** "¿Qué tan satisfecho está con el servicio de recolección de basura?"
**Escala:** 1=Muy insatisfecho, 2=Insatisfecho, 3=Regular, 4=Satisfecho, 5=Muy satisfecho

#### **Paso 1: Preparar datos ordinales**
```r
source("R/ordinal_module.R")

# Procesar pregunta ordinal
ordinal_data <- prepare_ordinal_data(
  data = data,
  question_id = "Q35",  # Pregunta de satisfacción
  metadata = metadata
)

# Ver las etiquetas de valores
attr(ordinal_data, "value_labels")
# $1: "Muy insatisfecho"
# $2: "Insatisfecho" 
# $3: "Regular"
# $4: "Satisfecho"
# $5: "Muy satisfecho"
```

#### **Paso 2: Análisis estadístico**
```r
# Ver distribución
table(ordinal_data$value)
#  1  2  3  4  5 
# 45 67 89 123 76   # Ejemplo de distribución

# Calcular estadísticas
mean_satisfaction <- mean(as.numeric(ordinal_data$value), na.rm = TRUE)
cat("Satisfacción promedio:", round(mean_satisfaction, 2))  # 3.2
```

#### **Paso 3: Visualizaciones específicas**
```r
# Histograma ordenado
hist_plot <- create_ordinal_histogram(ordinal_data)

# Gráfico de barras por distrito
district_bars <- create_ordinal_district_bars(ordinal_data)

# Análisis por género y edad
demographic_analysis <- create_ordinal_demographic_breakdown(ordinal_data)
```

## Ejemplo 3: Datos Numéricos (Interval)

### 🎯 **Pregunta:** "Del 1 al 10, ¿qué tan seguro se siente en su colonia por las noches?"

#### **Paso 1: Usar módulo de intervalos**
```r
source("R/interval_module.R")

# Preparar datos de intervalo
interval_data <- prepare_interval_data(
  data = data,
  question_id = "Q28",
  metadata = metadata
)

# Ver rango de valores
range(interval_data$value_num, na.rm = TRUE)
# [1]  1 10   # Escala completa 1-10
```

#### **Paso 2: Estadísticas detalladas**
```r
# Estadísticas descriptivas
summary(interval_data$value_num)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   1.00    4.00    6.00    5.75    8.00   10.00 

# Por distrito
district_stats <- interval_data %>%
  group_by(district) %>%
  summarise(
    promedio = mean(value_num, na.rm = TRUE),
    mediana = median(value_num, na.rm = TRUE),
    n = n()
  )

print(district_stats)
```

#### **Paso 3: Múltiples visualizaciones**
```r
# Histograma de distribución
histogram <- create_interval_histogram(
  interval_data, 
  bins = 10,
  title = "Distribución de Sensación de Seguridad"
)

# Mapa con promedios por distrito
safety_map <- create_interval_district_map(
  interval_data, 
  geo_data,
  use_gradient = TRUE,
  color_scale = "Reds"  # Rojo para seguridad
)

# Gráfico de crestas (distribuciones por distrito)
ridge_plot <- create_interval_ridge_plot(interval_data)

# Comparación por género
gender_comparison <- create_interval_gender_dumbbell(interval_data)
```

## Ejemplo 4: Texto Libre (Nominal)

### 🎯 **Pregunta:** "¿Cuál considera que es el principal problema de su colonia?"

#### **Paso 1: Procesar texto**
```r
source("R/nominal_module.R")

# Preparar datos de texto
text_data <- prepare_nominal_data(
  data = data,
  question_id = "Q45",
  metadata = metadata
)

# Ver algunas respuestas
head(text_data$value, 10)
# [1] "falta de alumbrado publico"
# [2] "baches en las calles" 
# [3] "inseguridad y robos"
# [4] "no hay recoleccion de basura"
# [5] "calles sin pavimentar"
```

#### **Paso 2: Análisis de frecuencia**
```r
# Obtener frecuencia de palabras
word_freq <- attr(text_data, "word_freq")

# Ver las 20 palabras más comunes
head(word_freq, 20)
#      word freq
# 1   calles   89
# 2   falta    67
# 3 alumbrado  54
# 4  baches    45
# 5 basura     42
```

#### **Paso 3: Visualizaciones de texto**
```r
# Gráfico de barras de palabras más frecuentes
word_plot <- create_nominal_word_frequency_chart(
  word_freq, 
  top_n = 15
)

# Nube de palabras (si disponible)
if (requireNamespace("wordcloud", quietly = TRUE)) {
  wordcloud_plot <- create_nominal_wordcloud(word_freq)
}

# Análisis por distrito
district_keywords <- create_nominal_district_analysis(text_data)
```

## Ejemplo 5: Casos Especiales (Special Module)

### 🎯 **Análisis:** Reportes de problemas de servicios públicos

#### **Paso 1: Usar módulo especial**
```r
source("R/special_module.R")

# Este módulo analiza múltiples preguntas relacionadas
report_analysis <- create_report_statistics(data)

# El módulo automáticamente procesa:
# Q32-Q33: Agua potable (¿reportó? → ¿fue atendido?)
# Q48-Q49: Alumbrado público
# Q52-Q53: Calles y pavimentación
# Q37-Q38: Energía eléctrica  
# Q42-Q43: Recolección de basura
```

#### **Paso 2: Interpretar resultados**
```r
# Ver la estructura del análisis
str(report_analysis)

# El resultado incluye:
# - % de personas que reportaron cada problema
# - % de reportes que fueron atendidos
# - Visualización combinada
```

## Ejemplo 6: Integración Completa

### 🎯 **Escenario:** Crear un dashboard completo para una sección

#### **Paso 1: Procesar múltiples preguntas**
```r
# Definir preguntas de la sección "Gobierno"
government_questions <- list(
  trust_municipal = list(id = "Q12", type = "ordinal"),
  know_regidor = list(id = "Q15", type = "binary"), 
  satisfaction_services = list(id = "Q18", type = "interval"),
  main_problem = list(id = "Q45", type = "nominal")
)

# Procesar cada pregunta
government_plots <- list()

for(q_name in names(government_questions)) {
  q_info <- government_questions[[q_name]]
  
  if(q_info$type == "binary") {
    data_prep <- prepare_binary_data(data, q_info$id, metadata)
    government_plots[[q_name]] <- create_binary_bars(data_prep)
    
  } else if(q_info$type == "ordinal") {
    data_prep <- prepare_ordinal_data(data, q_info$id, metadata)
    government_plots[[q_name]] <- create_ordinal_histogram(data_prep)
    
  } else if(q_info$type == "interval") {
    data_prep <- prepare_interval_data(data, q_info$id, metadata)
    government_plots[[q_name]] <- create_interval_bars(data_prep)
    
  } else if(q_info$type == "nominal") {
    data_prep <- prepare_nominal_data(data, q_info$id, metadata)
    word_freq <- attr(data_prep, "word_freq")
    government_plots[[q_name]] <- create_nominal_word_frequency_chart(word_freq)
  }
}
```

#### **Paso 2: Guardar gráficos procesados**
```r
# Guardar todos los gráficos para uso en el dashboard
saveRDS(government_plots, "data/plots/government_2024.rds")

# El dashboard podrá cargar estos gráficos pregenerados
```

## Patrones de Uso Comunes

### 🔄 **Patrón 1: Análisis exploratorio**
```r
# Para explorar una nueva pregunta
explore_question <- function(data, question_id, metadata) {
  
  # Primero, ver qué tipo de datos tenemos
  cat("Pregunta:", question_id, "\n")
  cat("Valores únicos:", length(unique(data[[question_id]])), "\n")
  cat("Tipo de datos:", class(data[[question_id]]), "\n")
  
  # Ver algunos valores
  print(head(table(data[[question_id]]), 10))
  
  # Buscar metadatos
  q_meta <- metadata %>% filter(variable == question_id)
  if(nrow(q_meta) > 0) {
    cat("Tipo clasificado:", q_meta$ResponseType, "\n")
    cat("Tema:", q_meta$Theme, "\n")
  }
}

# Usar función
explore_question(data, "Q25", metadata)
```

### 🔄 **Patrón 2: Comparación temporal**
```r
# Comparar la misma pregunta entre años
compare_years <- function(question_id, years = c("2023", "2024")) {
  
  results <- list()
  
  for(year in years) {
    # Cargar datos del año
    data_file <- paste0("data/processed/PER_", year, "_responses.csv")
    meta_file <- paste0("data/processed/PER_", year, "_metadata_classified.csv")
    
    if(file.exists(data_file) && file.exists(meta_file)) {
      year_data <- read.csv(data_file)
      year_meta <- read.csv(meta_file)
      
      # Procesar según el tipo detectado
      if(question_id %in% names(year_data)) {
        prep_data <- prepare_binary_data(year_data, question_id, year_meta)
        
        # Calcular estadística resumida
        positive_pct <- mean(prep_data$binary_value, na.rm = TRUE) * 100
        results[[year]] <- positive_pct
      }
    }
  }
  
  return(results)
}

# Ejemplo de uso
trust_comparison <- compare_years("Q12")  # Confianza en gobierno
print(trust_comparison)
# $2023: 45.2%
# $2024: 52.8%  # Mejora en confianza
```

### 🔄 **Patrón 3: Análisis por segmentos**
```r
# Analizar diferencias demográficas
analyze_by_demographics <- function(prepared_data, value_column = "binary_value") {
  
  # Por género
  gender_analysis <- prepared_data %>%
    group_by(gender) %>%
    summarise(
      mean_value = mean(!!sym(value_column), na.rm = TRUE),
      n = n(),
      .groups = 'drop'
    )
  
  # Por distrito
  district_analysis <- prepared_data %>%
    group_by(district) %>%
    summarise(
      mean_value = mean(!!sym(value_column), na.rm = TRUE),
      n = n(),
      .groups = 'drop'
    )
  
  # Por edad
  age_analysis <- prepared_data %>%
    group_by(age_group) %>%
    summarise(
      mean_value = mean(!!sym(value_column), na.rm = TRUE),
      n = n(),
      .groups = 'drop'
    )
  
  return(list(
    gender = gender_analysis,
    district = district_analysis,
    age = age_analysis
  ))
}
```

## Troubleshooting Avanzado

### 🐛 **Problema 1: Datos inconsistentes**
```r
# Función para diagnosticar problemas de datos
diagnose_data_issues <- function(data, question_id) {
  
  cat("=== DIAGNÓSTICO DE DATOS ===\n")
  cat("Pregunta:", question_id, "\n\n")
  
  if(!question_id %in% names(data)) {
    cat("❌ ERROR: Pregunta no encontrada en los datos\n")
    cat("Columnas disponibles:", paste(names(data)[1:10], collapse = ", "), "...\n")
    return(NULL)
  }
  
  values <- data[[question_id]]
  
  cat("📊 ESTADÍSTICAS BÁSICAS:\n")
  cat("Total de filas:", length(values), "\n")
  cat("Valores únicos:", length(unique(values)), "\n")
  cat("Valores NA:", sum(is.na(values)), "\n")
  cat("Tipo de datos:", class(values), "\n\n")
  
  cat("🔍 VALORES MÁS COMUNES:\n")
  print(head(sort(table(values, useNA = "always"), decreasing = TRUE), 15))
  
  cat("\n📝 MUESTRA DE VALORES:\n")
  sample_values <- sample(values[!is.na(values)], min(10, sum(!is.na(values))))
  print(sample_values)
  
  # Detectar posible tipo de módulo
  cat("\n🤖 RECOMENDACIÓN DE MÓDULO:\n")
  unique_count <- length(unique(values[!is.na(values)]))
  
  if(unique_count == 2) {
    cat("➡️  Usar binary_module (2 valores únicos)\n")
  } else if(unique_count <= 10 && all(grepl("^[0-9]+$", unique(values[!is.na(values)])))) {
    cat("➡️  Usar ordinal_module o interval_module (valores numéricos limitados)\n")
  } else if(unique_count > 20) {
    cat("➡️  Usar nominal_module (muchos valores únicos, posiblemente texto)\n")
  } else {
    cat("➡️  Usar categorical_module (valores categóricos moderados)\n")
  }
}

# Uso
diagnose_data_issues(data, "Q15")
```

### 🐛 **Problema 2: Visualizaciones que no aparecen**
```r
# Función para verificar pipeline completo
test_full_pipeline <- function(data, question_id, metadata, module_type) {
  
  cat("🔄 PROBANDO PIPELINE COMPLETO\n")
  
  # Paso 1: Preparación de datos
  tryCatch({
    if(module_type == "binary") {
      prepared <- prepare_binary_data(data, question_id, metadata)
    } else if(module_type == "interval") {
      prepared <- prepare_interval_data(data, question_id, metadata)
    }
    # ... otros tipos
    
    cat("✅ Preparación de datos exitosa\n")
    cat("   Filas procesadas:", nrow(prepared), "\n")
    
  }, error = function(e) {
    cat("❌ Error en preparación:", e$message, "\n")
    return(NULL)
  })
  
  # Paso 2: Creación de gráfico
  tryCatch({
    if(module_type == "binary") {
      plot_obj <- create_binary_bars(prepared)
    } else if(module_type == "interval") {
      plot_obj <- create_interval_histogram(prepared)
    }
    
    cat("✅ Creación de gráfico exitosa\n")
    cat("   Tipo de objeto:", class(plot_obj), "\n")
    
    return(plot_obj)
    
  }, error = function(e) {
    cat("❌ Error en visualización:", e$message, "\n")
    return(NULL)
  })
}

# Uso
test_plot <- test_full_pipeline(data, "Q15", metadata, "binary")
if(!is.null(test_plot)) {
  print(test_plot)  # Mostrar el gráfico
}
```

## Scripts de Automatización

### 🤖 **Script para procesar todas las preguntas:**
```r
# generate_all_plots.R
process_all_questions <- function(year = "2024") {
  
  # Cargar datos
  data_file <- paste0("data/processed/PER_", year, "_responses.csv")
  meta_file <- paste0("data/processed/PER_", year, "_metadata_classified.csv")
  
  data <- read.csv(data_file)
  metadata <- read.csv(meta_file)
  
  # Obtener todas las preguntas clasificadas
  questions <- metadata %>%
    filter(!is.na(ResponseType)) %>%
    select(variable, ResponseType, Theme)
  
  all_plots <- list()
  
  # Procesar cada pregunta
  for(i in 1:nrow(questions)) {
    q_id <- questions$variable[i]
    q_type <- questions$ResponseType[i]
    
    cat("Procesando", q_id, "(", q_type, ")... ")
    
    tryCatch({
      if(q_type == "Binary") {
        prep_data <- prepare_binary_data(data, q_id, metadata)
        all_plots[[q_id]] <- create_binary_bars(prep_data)
        
      } else if(q_type == "Interval") {
        prep_data <- prepare_interval_data(data, q_id, metadata)
        all_plots[[q_id]] <- create_interval_histogram(prep_data)
        
      } else if(q_type == "Ordinal") {
        prep_data <- prepare_ordinal_data(data, q_id, metadata)
        all_plots[[q_id]] <- create_ordinal_histogram(prep_data)
        
      } else if(q_type == "Nominal") {
        prep_data <- prepare_nominal_data(data, q_id, metadata)
        word_freq <- attr(prep_data, "word_freq")
        all_plots[[q_id]] <- create_nominal_word_frequency_chart(word_freq)
      }
      
      cat("✅\n")
      
    }, error = function(e) {
      cat("❌ Error:", e$message, "\n")
    })
  }
  
  # Guardar por tema
  themes <- unique(questions$Theme)
  for(theme in themes) {
    theme_questions <- questions$variable[questions$Theme == theme]
    theme_plots <- all_plots[theme_questions]
    theme_plots <- theme_plots[!sapply(theme_plots, is.null)]
    
    if(length(theme_plots) > 0) {
      filename <- paste0("data/plots/", tolower(theme), "_", year, ".rds")
      saveRDS(theme_plots, filename)
      cat("Guardado:", filename, "con", length(theme_plots), "gráficos\n")
    }
  }
}

# Ejecutar
process_all_questions("2024")
```

---

## Conclusión

Los módulos de computación de gráficos son herramientas poderosas que automatizan el procesamiento de datos de encuestas. Con estos ejemplos prácticos, puedes:

1. **Identificar** qué módulo usar para cada tipo de pregunta
2. **Procesar** datos de manera consistente
3. **Crear** visualizaciones apropiadas
4. **Solucionar** problemas comunes
5. **Automatizar** el procesamiento masivo

Recuerda siempre probar con datos pequeños antes de procesar datasets completos, y usar las funciones de diagnóstico cuando encuentres problemas.

---
*Guía de Ejemplos Prácticos - Módulos de Computación de Gráficos*  
*Dashboard Espejo Ciudadano - Para principiantes en R*