# Guía: Agregar Nuevas Encuestas y Datos

## Introducción

Esta guía te explica paso a paso cómo agregar nuevas encuestas al dashboard Espejo Ciudadano. Está dirigida a principiantes en R.

## Flujo General de Datos

```
1. Datos Crudos (.sav, .xlsx) → data/raw/
2. Procesamiento → data/processed/ (.csv)
3. Clasificación → Metadatos clasificados
4. Generación → data/plots/ (.rds)
5. Dashboard → Visualización
```

## Paso 1: Preparar los Datos Crudos

### Ubicación
Coloca tus archivos de encuesta en la carpeta `data/raw/`:
```
data/raw/
├── Nueva_Encuesta_2025.sav     # Archivo SPSS
├── Nueva_Encuesta_2025.xlsx    # O archivo Excel
└── CODING_MANUAL_2025.csv      # Manual de codificación
```

### Formatos Soportados
- **SPSS** (.sav): Formato preferido
- **Excel** (.xlsx): Alternativa aceptable
- **CSV** (.csv): Para datos ya procesados

## Paso 2: Procesar los Datos

### Crear Archivos Procesados
Los datos procesados deben seguir esta nomenclatura:
```
data/processed/
├── TIPO_AÑO_responses.csv           # Respuestas principales
├── TIPO_AÑO_metadata.csv           # Metadatos de preguntas
└── TIPO_AÑO_metadata_classified.csv # Metadatos clasificados
```

**Ejemplo para encuesta de Percepción 2025:**
- `PER_2025_responses.csv`
- `PER_2025_metadata.csv`
- `PER_2025_metadata_classified.csv`

### Estructura de responses.csv
```csv
ResponseId,Q1,Q2,Q3,Q101,Q103,...
1,3,2,4,1,2,...
2,2,1,3,2,3,...
```

**Columnas Importantes:**
- `Q2`: Distrito (SIEMPRE requerida)
- `Q101/Q144`: Género (según tipo de encuesta)
- `Q103/Q146`: Edad (según tipo de encuesta)

### Estructura de metadata.csv
```csv
QuestionCode,QuestionText,ResponseType,Theme
Q1,"¿Cómo califica...?","Ordinal","Government"
Q2,"Distrito","Nominal","Demographics"
Q3,"¿Está de acuerdo...?","Binary","Wellness"
```

**Columnas Requeridas:**
- `QuestionCode`: Código único (Q1, Q2, etc.)
- `QuestionText`: Texto de la pregunta
- `ResponseType`: Tipo de respuesta (Binary, Ordinal, Nominal, etc.)
- `Theme`: Tema principal (Government, Wellness, etc.)

## Paso 3: Configurar la Nueva Encuesta

### Editar survey_config.R
Abre `R/survey_config.R` y agrega tu nueva encuesta:

```r
survey_config <- list(
  # Encuestas existentes...
  PER_2023 = list(...),
  PER_2024 = list(...),
  
  # ¡NUEVA ENCUESTA AQUÍ!
  PER_2025 = list(
    district_col = "Q2",              # Columna del distrito
    gender_col = "Q101",              # Columna del género
    age_col = "Q103",                 # Columna de edad
    gender_mapping = c(               # Mapeo de valores de género
      "0" = "No especificado",
      "1" = "Mujer", 
      "2" = "Hombre",
      "3" = "Otro"
    ),
    # Si hay mapeo de edad específico:
    age_mapping = c(
      "1" = "18 a 29 años",
      "2" = "30 a 44 años", 
      "3" = "45 a 59 años",
      "4" = "60 años o más"
    )
  )
)
```

### Tipos de Encuestas

#### Encuestas PER (Percepción)
- `gender_col = "Q101"`
- `age_col = "Q103"`

#### Encuestas PAR (Participación)  
- `gender_col = "Q144"`
- `age_col = "Q146"`

#### Nueva Encuesta (personalizada)
Revisa tus datos para identificar las columnas correctas:
```r
# Ver estructura de tus datos
data <- read.csv("data/processed/NUEVA_2025_responses.csv")
colnames(data)  # Ver nombres de columnas
```

## Paso 4: Generar Datos de Visualización

### Crear Archivos .rds
Para cada sección temática, necesitas crear archivos `.rds` en `data/plots/`:

```
data/plots/
├── government_2025.rds      # Gráficos de gobierno
├── wellness_2025.rds        # Gráficos de bienestar  
├── infrastructure_2025.rds  # Gráficos de infraestructura
├── participation_2025.rds   # Gráficos de participación
└── urban_2025.rds          # Gráficos de urbanismo
```

### Estructura de un archivo .rds
Cada archivo contiene una lista de gráficos:
```r
# Ejemplo de estructura
plots_list <- list(
  "Q5" = ggplot_object_1,    # Gráfico para pregunta Q5
  "Q7" = ggplot_object_2,    # Gráfico para pregunta Q7
  "Q12" = ggplot_object_3,   # Gráfico para pregunta Q12
  # ... más gráficos
)

# Guardar
saveRDS(plots_list, "data/plots/government_2025.rds")
```

### Script de Generación de Gráficos
Crea un script para automatizar la generación:

```r
# generate_plots_2025.R
library(ggplot2)
library(dplyr)

# Cargar datos
responses <- read.csv("data/processed/PER_2025_responses.csv")
metadata <- read.csv("data/processed/PER_2025_metadata_classified.csv")

# Función para generar gráficos por tema
generate_theme_plots <- function(theme_name) {
  # Filtrar preguntas del tema
  theme_questions <- metadata[metadata$Theme == theme_name, ]
  
  plots_list <- list()
  
  for(question in theme_questions$QuestionCode) {
    # Crear gráfico según el tipo de pregunta
    plot_obj <- create_plot_for_question(question, responses, metadata)
    plots_list[[question]] <- plot_obj
  }
  
  # Guardar
  saveRDS(plots_list, paste0("data/plots/", tolower(theme_name), "_2025.rds"))
}

# Generar para todos los temas
themes <- c("Government", "Wellness", "Infrastructure", "Participation", "Urban")
lapply(themes, generate_theme_plots)
```

## Paso 5: Actualizar Navegación

### Agregar el Año a la UI
Si tu nueva encuesta usa un año no existente, edita `app.R`:

```r
# Buscar esta sección en app.R
radioButtons(
  "selected_year",
  "Año:",
  choices = c("2023" = "2023", 
              "2024" = "2024",
              "2025" = "2025"),  # ¡AGREGAR AQUÍ!
  selected = "2024"
)
```

## Paso 6: Verificar la Integración

### Pruebas Básicas
```r
# 1. Verificar configuración
source("R/survey_config.R")
print(survey_config$PER_2025)  # Reemplaza con tu encuesta

# 2. Verificar archivos de datos
file.exists("data/processed/PER_2025_responses.csv")
file.exists("data/plots/government_2025.rds")

# 3. Cargar un archivo de gráficos
plots <- readRDS("data/plots/government_2025.rds")
names(plots)  # Ver qué gráficos están disponibles
```

### Ejecutar Dashboard
```r
# Ejecutar localmente para probar
shiny::runApp(port = 3838)
```

## Tipos de Datos Comunes

### 1. Preguntas Binarias (Sí/No)
```csv
QuestionCode,ResponseType,Theme
Q5,"Binary","Government"
```
- Valores típicos: 1=Sí, 2=No, 0=No especificado

### 2. Preguntas Ordinales (Escala)
```csv
QuestionCode,ResponseType,Theme  
Q7,"Ordinal","Wellness"
```
- Valores típicos: 1=Muy malo, 2=Malo, 3=Regular, 4=Bueno, 5=Muy bueno

### 3. Preguntas Nominales (Categorías)
```csv
QuestionCode,ResponseType,Theme
Q12,"Nominal","Infrastructure"
```
- Valores: Categorías sin orden específico

## Errores Comunes y Soluciones

### Error: "Archivo no encontrado"
```
✗ Error: data/plots/tema_2025.rds not found
✓ Solución: Verifica que el archivo existe y el nombre es correcto
```

### Error: "Columna no encontrada"
```  
✗ Error: object 'Q101' not found
✓ Solución: Verifica survey_config.R y los nombres de columnas
```

### Error: "Gráfico vacío"
```
✗ Error: Plot is empty
✓ Solución: Verifica que hay datos para esa pregunta
```

## Checklist Final

- [ ] Datos crudos en `data/raw/`
- [ ] Archivos procesados en `data/processed/`
- [ ] Configuración en `survey_config.R`
- [ ] Archivos .rds en `data/plots/`
- [ ] Año agregado a la UI (si es necesario)
- [ ] Pruebas básicas completadas
- [ ] Dashboard ejecuta sin errores

---
*Guía para agregar nuevas encuestas - Dashboard Espejo Ciudadano*