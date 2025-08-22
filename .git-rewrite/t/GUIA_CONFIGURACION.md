# Guía: Configuración y Personalización Avanzada

## Introducción

Esta guía cubre la configuración avanzada y personalización del dashboard Espejo Ciudadano. Incluye temas, estilos, configuraciones de servidor, y optimizaciones para principiantes en R.

## Configuraciones Básicas

### 1. Configuración Principal (`app.R`)

#### Puerto y Host
```r
# En app.R, al final del archivo
if (interactive()) {
  shiny::runApp(host = "0.0.0.0", port = 3838)
}
```

#### Configuración de Memoria
```r
# Aumentar límite de memoria para archivos grandes
options(shiny.maxRequestSize = 100*1024^2)  # 100MB
```

### 2. Configuración de Datos (`R/survey_config.R`)

#### Añadir Nuevos Tipos de Encuesta
```r
survey_config <- list(
  # Encuestas existentes...
  
  # Nueva configuración personalizada
  CUSTOM_2025 = list(
    district_col = "DISTRITO",           # Nombre personalizado
    gender_col = "GENERO",              # Columna de género
    age_col = "EDAD",                   # Columna de edad
    
    # Mapeos personalizados
    gender_mapping = c(
      "M" = "Mujer",
      "H" = "Hombre", 
      "O" = "Otro",
      "NE" = "No especificado"
    ),
    
    # Configuración de respuestas binarias
    binary_config = list(
      positive_responses = c("SI", "1", "TRUE"),
      negative_responses = c("NO", "0", "FALSE")
    )
  )
)
```

#### Configuración de Tratamiento de NA
```r
# En survey_config.R
binary_response_config = list(
  treat_na_as_negative_by_default = TRUE,
  question_exceptions = list(
    "PER:Q15" = FALSE,    # Para pregunta Q15, NA no es negativo
    "PAR:Q88" = FALSE     # Para pregunta Q88, NA no es negativo
  )
)
```

## Personalización de Temas y Estilos

### 1. Colores del Dashboard (`www/styles.css`)

#### Cambiar Colores Principales
```css
:root {
  /* Colores originales */
  --gobierno-color: #2E8B57;      
  --bienestar-color: #4682B4;     
  --infraestructura-color: #B8860B; 
  --participacion-color: #9370DB;  
  --urbanismo-color: #DC143C;
  
  /* Nuevos colores personalizados */
  --gobierno-color: #1B5E20;      /* Verde más oscuro */
  --bienestar-color: #0D47A1;     /* Azul más profundo */
  --infraestructura-color: #E65100; /* Naranja vibrante */
  --participacion-color: #4A148C;  /* Púrpura más oscuro */
  --urbanismo-color: #B71C1C;      /* Rojo más profundo */
}
```

#### Personalizar Fuentes
```css
:root {
  --font-display: 'Roboto', sans-serif;
  --font-body: 'Open Sans', sans-serif;
}

/* Importar nuevas fuentes */
@import url('https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;600;700&display=swap');

:root {
  --font-display: 'Poppins', sans-serif;
  --font-body: 'Poppins', sans-serif;
}
```

### 2. Temas de Gráficos (`R/global_theme.R`)

#### Crear Tema Personalizado
```r
# Tema personalizado para gráficos
custom_dashboard_theme <- theme_minimal() +
  theme(
    # Título
    plot.title = element_text(
      size = 16, 
      face = "bold", 
      hjust = 0.5,
      color = "#2c3e50"
    ),
    
    # Ejes
    axis.title = element_text(size = 12, color = "#34495e"),
    axis.text = element_text(size = 10, color = "#7f8c8d"),
    
    # Panel
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "#ecf0f1", size = 0.5),
    panel.grid.minor = element_blank(),
    
    # Leyenda
    legend.position = "bottom",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10)
  )

# Función para aplicar colores por sección
get_section_colors <- function(section) {
  colors <- list(
    government = c("#2E8B57", "#90EE90", "#006400"),
    wellness = c("#4682B4", "#87CEEB", "#191970"),
    infrastructure = c("#B8860B", "#F0E68C", "#8B6914"),
    participation = c("#9370DB", "#DDA0DD", "#4B0082"),
    urban = c("#DC143C", "#FFB6C1", "#8B0000")
  )
  
  return(colors[[section]] %||% c("#666666", "#CCCCCC", "#333333"))
}
```

### 3. Layout Responsivo

#### Configurar Breakpoints
```css
/* Breakpoints personalizados */
.custom-responsive-grid {
  display: grid;
  gap: 1rem;
  grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
}

/* Para tablets */
@media (max-width: 768px) {
  .nav-card {
    min-height: 120px;
  }
  
  .dashboard-header {
    font-size: 1.5rem;
  }
}

/* Para móviles */
@media (max-width: 480px) {
  .section-nav-grid {
    grid-template-columns: 1fr;
  }
  
  .nav-card {
    min-height: 100px;
  }
}
```

## Configuración del Sistema de Analytics

### 1. Configurar Base de Datos (`R/analytics.R`)

#### Personalizar Tablas de Analytics
```r
# Modificar estructura de tablas
AnalyticsManager <- R6Class("AnalyticsManager",
  private = list(
    create_tables = function() {
      # Tabla de sesiones con campos personalizados
      dbExecute(private$db, "
        CREATE TABLE IF NOT EXISTS sessions (
          session_id TEXT PRIMARY KEY,
          start_time TEXT,
          end_time TEXT,
          duration_seconds INTEGER,
          user_agent TEXT,
          ip_address TEXT,           -- Campo adicional
          country TEXT,              -- Geolocalización
          sections_visited TEXT,
          total_interactions INTEGER,
          custom_field TEXT          -- Campo personalizable
        )
      ")
      
      # Tabla de eventos personalizada
      dbExecute(private$db, "
        CREATE TABLE IF NOT EXISTS custom_events (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          session_id TEXT,
          event_type TEXT,
          event_data TEXT,
          timestamp TEXT,
          section TEXT,
          question_id TEXT,          -- Para rastrear preguntas específicas
          response_time INTEGER,     -- Tiempo de respuesta
          FOREIGN KEY (session_id) REFERENCES sessions (session_id)
        )
      ")
    }
  )
)
```

### 2. Configurar Triggers de Analytics

#### Personalizar Activación
```r
# En app.R, modificar triggers
observe({
  # Trigger personalizado con Ctrl+Alt+A
  if (input$analytics_trigger == "ctrl_alt_a") {
    showAnalytics()
  }
})

# Agregar trigger por URL
observe({
  query <- parseQueryString(session$clientData$url_search)
  if ("analytics" %in% names(query)) {
    showAnalytics()
  }
})
```

## Configuraciones de Rendimiento

### 1. Optimización de Caché (`R/data_manager.R`)

#### Configurar Límites de Caché
```r
DataManager <- R6Class("DataManager",
  public = list(
    initialize = function(max_cache_mb = 500) {  # Aumentar límite
      # Configuración de caché más agresiva
      assign("CACHE_CONFIG", list(
        max_size_mb = max_cache_mb,
        cleanup_threshold = 0.8,      # Limpiar al 80%
        item_expiry_hours = 24,       # Expirar items después de 24h
        preload_enabled = TRUE        # Habilitar precarga
      ), envir = .GlobalEnv$GLOBAL_CACHE)
    }
  )
)
```

### 2. Configuración de Precarga

#### Habilitar Precarga Inteligente
```r
# En R/background_preloader.R
smart_preload_config <- list(
  # Precargar secciones populares primero
  priority_sections = c("government", "wellness"),
  
  # Precargar años recientes primero
  priority_years = c("2024", "2023"),
  
  # Límite de elementos a precargar
  max_preload_items = 50,
  
  # Precargar solo en horarios de bajo tráfico
  preload_schedule = list(
    enabled = TRUE,
    start_hour = 2,   # 2 AM
    end_hour = 6      # 6 AM
  )
)
```

## Configuraciones de Despliegue

### 1. Docker (`docker-compose.yml`)

#### Configuración Personalizada
```yaml
services:
  espejo-ciudadano:
    build: .
    container_name: espejo-ciudadano-custom
    environment:
      - SHINY_PORT=3838
      - SHINY_HOST=0.0.0.0
      - R_MAX_MEMORY=4G              # Aumentar memoria
      - SHINY_LOG_LEVEL=INFO         # Configurar logs
      - CUSTOM_THEME=dark            # Tema personalizado
    volumes:
      - ./data:/srv/shiny-server/data:ro
      - ./logs:/var/log/shiny-server
      - ./custom_config:/etc/shiny-server  # Configuración personalizada
    deploy:
      resources:
        limits:
          memory: 4G
          cpus: '2.0'
        reservations:
          memory: 2G
          cpus: '1.0'
```

### 2. Configuración de Proxy (`traefik`)

#### SSL y Seguridad Personalizada
```yaml
# En docker-compose.yml
traefik:
  labels:
    - "traefik.http.routers.espejo.rule=Host(`mi-dashboard.com`)"
    - "traefik.http.routers.espejo.tls=true"
    - "traefik.http.routers.espejo.tls.certresolver=letsencrypt"
    
    # Headers de seguridad personalizados
    - "traefik.http.middlewares.security-headers.headers.customrequestheaders.X-Custom-Header=MiValor"
    - "traefik.http.middlewares.security-headers.headers.stsSeconds=31536000"
    - "traefik.http.middlewares.security-headers.headers.stsIncludeSubdomains=true"
```

## Configuraciones de Desarrollo

### 1. Debugging (`R/utils.R`)

#### Funciones de Debug Personalizadas
```r
# Función de debug personalizada
debug_dashboard <- function(section = NULL, verbose = TRUE) {
  if (verbose) {
    message("=== DEBUG DASHBOARD ===")
    message("Fecha: ", Sys.time())
    message("Sección: ", section %||% "Todas")
    
    # Información del sistema
    message("Memoria R: ", format(object.size(ls(envir = .GlobalEnv)), units = "MB"))
    message("Memoria caché: ", get_cache_size())
    
    # Estado de archivos de datos
    check_data_integrity()
  }
}

# Verificar integridad de datos
check_data_integrity <- function() {
  data_files <- list.files("data/plots", pattern = "\\.rds$", full.names = TRUE)
  
  for (file in data_files) {
    tryCatch({
      plots <- readRDS(file)
      message("✓ ", basename(file), " - ", length(plots), " gráficos")
    }, error = function(e) {
      warning("✗ Error en ", basename(file), ": ", e$message)
    })
  }
}
```

### 2. Logging Personalizado

#### Sistema de Logs Avanzado
```r
# En R/utils.R
custom_logger <- function(message, level = "INFO", section = NULL) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- sprintf("[%s] %s %s: %s", 
                      timestamp, level, 
                      ifelse(is.null(section), "", paste0("(", section, ")")), 
                      message)
  
  # Escribir a archivo
  cat(log_entry, "\n", file = "logs/dashboard.log", append = TRUE)
  
  # También imprimir en consola en desarrollo
  if (getOption("shiny.development", FALSE)) {
    message(log_entry)
  }
}

# Uso
custom_logger("Usuario accedió a sección gobierno", "INFO", "government")
custom_logger("Error cargando gráfico Q5", "ERROR", "wellness")
```

## Variables de Entorno

### 1. Configuración (.env)
```bash
# Configuraciones del dashboard
DASHBOARD_TITLE="Mi Dashboard Personalizado"
DASHBOARD_VERSION="1.0.0"
DASHBOARD_THEME="custom"

# Configuraciones de rendimiento
SHINY_MAX_MEMORY=4G
CACHE_SIZE_MB=1000
PRELOAD_ENABLED=true

# Configuraciones de analytics
ANALYTICS_ENABLED=true
ANALYTICS_RETENTION_DAYS=90

# Configuraciones de desarrollo
DEBUG_MODE=false
LOG_LEVEL=INFO
```

### 2. Cargar Variables en R
```r
# En app.R
# Cargar configuración desde .env
if (file.exists(".env")) {
  env_vars <- readLines(".env")
  for (var in env_vars) {
    if (grepl("=", var) && !startsWith(var, "#")) {
      parts <- strsplit(var, "=", fixed = TRUE)[[1]]
      if (length(parts) == 2) {
        Sys.setenv(do.call(setNames, list(parts[2], parts[1])))
      }
    }
  }
}

# Usar variables
dashboard_title <- Sys.getenv("DASHBOARD_TITLE", "Espejo Ciudadano")
max_memory <- Sys.getenv("SHINY_MAX_MEMORY", "2G")
```

## Checklist de Configuración

### Configuración Básica:
- [ ] Puerto y host configurados
- [ ] Límites de memoria establecidos
- [ ] Configuración de encuestas actualizada
- [ ] Colores y temas personalizados

### Configuración Avanzada:
- [ ] Sistema de caché optimizado
- [ ] Analytics configurado
- [ ] Logs personalizados
- [ ] Variables de entorno definidas

### Configuración de Producción:
- [ ] Docker configurado
- [ ] SSL/TLS habilitado
- [ ] Backups automáticos
- [ ] Monitoreo habilitado

## Recursos de Configuración

### Archivos Clave de Configuración:
- `app.R` - Configuración principal
- `R/survey_config.R` - Configuración de encuestas
- `www/styles.css` - Estilos personalizados  
- `docker-compose.yml` - Configuración de despliegue
- `.env` - Variables de entorno

### Herramientas de Diagnóstico:
```r
# Verificar configuración actual
debug_dashboard()

# Ver estado del caché
check_cache_status()

# Verificar integridad de datos
check_data_integrity()

# Ver configuración activa
print(survey_config)
```

---
*Guía de configuración y personalización avanzada - Dashboard Espejo Ciudadano*