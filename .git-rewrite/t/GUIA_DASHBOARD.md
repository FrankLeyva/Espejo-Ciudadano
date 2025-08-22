# Guía del Dashboard Espejo Ciudadano

## Introducción

El **Espejo Ciudadano** es un dashboard interactivo desarrollado en R Shiny que presenta los resultados de encuestas sobre percepción y participación ciudadana en Ciudad Juárez. Esta guía está dirigida a principiantes en R que necesitan mantener, actualizar o personalizar el dashboard.

## ¿Qué es este dashboard?

El dashboard visualiza datos de encuestas ciudadanas organizados en secciones temáticas:

- **Gobierno**: Confianza, rendición de cuentas, expectativas
- **Bienestar**: Salud, educación, economía, medio ambiente, identidad, cultura
- **Infraestructura**: Servicios públicos, vivienda, equipamiento urbano
- **Participación**: Participación cívica y comunitaria
- **Urbanismo**: Movilidad y transporte

## Estructura del Proyecto

```
Espejo-Ciudadano/
├── app.R                    # Archivo principal de la aplicación
├── R/                       # Módulos y funciones
│   ├── survey_config.R      # Configuración de encuestas
│   ├── data_manager.R       # Gestión de datos
│   ├── utils.R             # Funciones auxiliares
│   ├── analytics.R         # Sistema de analytics
│   ├── government/         # Módulos de gobierno
│   ├── wellness/           # Módulos de bienestar
│   ├── infrastructure/     # Módulos de infraestructura
│   ├── participation/      # Módulos de participación
│   ├── urban/              # Módulos de urbanismo
│   └── extras/             # Funcionalidades adicionales
├── data/                   # Datos de las encuestas
│   ├── processed/          # Datos procesados (.csv)
│   ├── plots/              # Gráficos pre-generados (.rds)
│   ├── percentages/        # Porcentajes calculados (.rds)
│   ├── geo/               # Datos geográficos (.geojson)
│   └── maps/              # Mapas pre-generados (.png)
└── www/                    # Archivos web (CSS, JS, imágenes)
```

## Conceptos Clave para Principiantes

### 1. ¿Qué es R Shiny?
Shiny es una librería de R que permite crear aplicaciones web interactivas. El dashboard tiene dos partes principales:
- **UI (User Interface)**: Lo que ve el usuario
- **Server**: La lógica que procesa los datos

### 2. Arquitectura Modular
Cada sección temática tiene dos archivos:
- `*_ui.R`: Define la interfaz visual
- `*_server.R`: Contiene la lógica de procesamiento

### 3. Sistema de Datos
Los datos siguen este flujo:
1. **Datos crudos** → `data/raw/` (archivos .sav, .xlsx)
2. **Datos procesados** → `data/processed/` (archivos .csv)
3. **Gráficos** → `data/plots/` (archivos .rds)
4. **Dashboard** → Presenta los gráficos

## Comandos Básicos para Desarrollo

### Ejecutar Localmente
```r
# Instalar dependencias
renv::restore()

# Ejecutar la aplicación
shiny::runApp(port = 3838)
```

### Con Docker
```bash
# Construir la imagen
docker-compose build espejo-ciudadano

# Ejecutar
docker-compose up -d

# Ver logs
docker-compose logs -f espejo-ciudadano
```

### Desplegar en Producción
```bash
# Despliegue completo
./deploy.sh

# Actualización
./deploy.sh --update
```

## Configuración de Encuestas

El archivo `R/survey_config.R` es crucial para integrar nuevas encuestas. Define:

```r
survey_config <- list(
  PER_2024 = list(                    # Nombre de la encuesta
    district_col = "Q2",              # Columna para distrito
    gender_col = "Q101",              # Columna para género
    age_col = "Q103",                 # Columna para edad
    gender_mapping = c(               # Mapeo de valores
      "1" = "Mujer",
      "2" = "Hombre",
      "3" = "Otro"
    )
  )
)
```

### Tipos de Encuestas Actuales
- **PER** (Percepción): Q101 para género, Q103 para edad
- **PAR** (Participación): Q144 para género, Q146 para edad

## Sistema de Analytics

El dashboard incluye un sistema de analytics accesible mediante:
- **Teclado**: `Ctrl+Shift+A`
- **UI**: Triple clic en "Vista Rápida"
- **Consola R**: `showAnalytics()`

Rastrea:
- Duración de sesiones
- Secciones visitadas
- Interacciones del usuario
- Estadísticas de uso diario

## Archivos Importantes

### `app.R`
Archivo principal que:
- Carga todas las librerías
- Define la estructura de navegación
- Conecta todos los módulos

### `R/survey_config.R`
Configuración de encuestas:
- Mapeo de columnas demográficas
- Valores de respuestas categóricas
- Configuraciones específicas por encuesta

### `R/data_manager.R`
Gestión eficiente de datos:
- Sistema de caché en memoria
- Carga optimizada de archivos .rds
- Manejo de errores

### `R/utils.R`
Funciones auxiliares comunes:
- Filtros demográficos
- Utilidades de datos
- Funciones de formateo

## Próximos Pasos

Esta guía continúa con secciones detalladas sobre:
1. **Agregar nuevas encuestas y datos**
2. **Modificar y reorganizar gráficos**
3. **Personalización avanzada**

---
*Documentación creada para principiantes en R - Dashboard Espejo Ciudadano*