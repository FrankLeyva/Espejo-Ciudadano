# Documentación del Dashboard Espejo Ciudadano

## Índice de Guías

Esta documentación está dirigida a **principiantes en R** que necesitan mantener, actualizar o personalizar el Dashboard Espejo Ciudadano de Ciudad Juárez.

### 📖 Guías Disponibles

#### 1. [**Guía Principal del Dashboard**](GUIA_DASHBOARD.md)
- **¿Para quién?** Principiantes que necesitan entender qué es el dashboard
- **Contenido:**
  - Introducción al dashboard y sus secciones
  - Estructura del proyecto explicada paso a paso
  - Conceptos clave de R Shiny para principiantes
  - Comandos básicos de desarrollo
  - Sistema de analytics incluido

#### 2. [**Cómo Agregar Nuevas Encuestas**](GUIA_NUEVAS_ENCUESTAS.md)
- **¿Para quién?** Usuarios que necesitan integrar datos de nuevas encuestas
- **Contenido:**
  - Flujo completo de datos (desde archivos crudos hasta dashboard)
  - Preparación y procesamiento de datos
  - Configuración de nuevas encuestas en `survey_config.R`
  - Generación de archivos de visualización (.rds)
  - Integración con la interfaz del dashboard
  - Solución de errores comunes

#### 3. [**Modificar y Reorganizar Gráficos**](GUIA_MODIFICAR_GRAFICOS.md)
- **¿Para quién?** Usuarios que quieren cambiar la presentación visual
- **Contenido:**
  - Cómo localizar y modificar gráficos existentes
  - Reorganizar el layout de las secciones
  - Agregar nuevos tipos de visualizaciones
  - Cambiar colores y temas
  - Trabajar con mapas interactivos
  - Scripts de automatización

#### 4. [**Configuración y Personalización Avanzada**](GUIA_CONFIGURACION.md)
- **¿Para quién?** Usuarios que quieren personalizar profundamente el dashboard
- **Contenido:**
  - Configuraciones de servidor y rendimiento
  - Personalización de temas y estilos
  - Sistema de analytics avanzado
  - Configuraciones de despliegue con Docker
  - Variables de entorno y logging
  - Optimización de caché y memoria

#### 5. [**Módulos de Computación de Gráficos**](GUIA_MODULOS_GRAFICOS.md)
- **¿Para quién?** Desarrolladores que quieren entender el procesamiento interno de datos
- **Contenido:**
  - Explicación detallada de cada módulo (binary, ordinal, interval, etc.)
  - Tipos de datos que maneja cada módulo
  - Funciones principales y parámetros
  - Visualizaciones que genera cada tipo
  - Personalización de temas
  - Debugging y solución de problemas

#### 6. [**Ejemplos Prácticos de Módulos**](GUIA_EJEMPLOS_MODULOS.md)
- **¿Para quién?** Principiantes que quieren ver código funcionando paso a paso
- **Contenido:**
  - Ejemplos reales de cada tipo de pregunta
  - Código completo para cada escenario
  - Patrones de uso comunes
  - Scripts de automatización
  - Herramientas de diagnóstico
  - Troubleshooting avanzado

#### 7. [**Despliegue en VPS**](GUIA_DESPLIEGUE_VPS.md)
- **¿Para quién?** Administradores que necesitan subir el dashboard a producción
- **Contenido:**
  - Configuración completa del VPS (45.90.208.220)
  - Proceso de export/import de imágenes Docker
  - Configuración de Traefik y SSL automático
  - Scripts de despliegue y monitoreo
  - Backup y restauración
  - Troubleshooting de producción

## 🚀 Inicio Rápido

### Para Desarrolladores Nuevos:
1. Lee la [**Guía Principal**](GUIA_DASHBOARD.md) para entender la estructura
2. Ejecuta el dashboard localmente siguiendo los comandos básicos
3. Explora las diferentes secciones del dashboard

### Para Agregar Datos Nuevos:
1. Sigue la [**Guía de Nuevas Encuestas**](GUIA_NUEVAS_ENCUESTAS.md) paso a paso
2. Prepara tus datos en formato CSV procesado
3. Configura la nueva encuesta en `survey_config.R`
4. Genera los archivos .rds de visualización

### Para Cambios Visuales:
1. Usa la [**Guía de Gráficos**](GUIA_MODIFICAR_GRAFICOS.md) para localizar elementos
2. Modifica archivos .rds para cambios de gráficos
3. Edita archivos UI para cambios de layout

### Para Configuración Avanzada:
1. Consulta la [**Guía de Configuración**](GUIA_CONFIGURACION.md)
2. Modifica variables de entorno según necesidades
3. Personaliza temas y estilos en archivos CSS

## 📁 Estructura de Archivos Clave

```
Espejo-Ciudadano/
│
├── 📖 DOCUMENTACIÓN
│   ├── GUIA_DASHBOARD.md           # Guía principal
│   ├── GUIA_NUEVAS_ENCUESTAS.md    # Agregar encuestas  
│   ├── GUIA_MODIFICAR_GRAFICOS.md  # Modificar visuales
│   ├── GUIA_CONFIGURACION.md       # Configuración avanzada
│   ├── GUIA_MODULOS_GRAFICOS.md    # Módulos de computación
│   ├── GUIA_EJEMPLOS_MODULOS.md    # Ejemplos prácticos
│   ├── GUIA_DESPLIEGUE_VPS.md      # Despliegue en servidor
│   └── README_DOCUMENTACION.md     # Este archivo (índice)
│
├── 🚀 APLICACIÓN
│   ├── app.R                       # Archivo principal
│   ├── R/                          # Módulos y funciones
│   ├── data/                       # Datos de encuestas
│   ├── www/                        # Archivos web (CSS, JS)
│   └── docker-compose.yml          # Configuración de despliegue
│
└── 📋 CLAUDE.md                    # Instrucciones para Claude Code
```

## 🛠️ Comandos Más Usados

### Desarrollo Local:
```r
# Instalar dependencias
renv::restore()

# Ejecutar dashboard
shiny::runApp(port = 3838)

# Ver estructura de datos
str(readRDS("data/plots/wellness_2024.rds"))
```

### Docker:
```bash
# Construir y ejecutar
docker-compose up -d

# Ver logs
docker-compose logs -f espejo-ciudadano

# Parar servicios
docker-compose down
```

### Debugging:
```r
# Verificar integridad de datos
source("debug_analytics.R")

# Ver analytics (en dashboard)
Ctrl+Shift+A
```

## ❓ Preguntas Frecuentes

### **¿Dónde están los datos de las encuestas?**
- Datos crudos: `data/raw/`
- Datos procesados: `data/processed/`
- Gráficos: `data/plots/`

### **¿Cómo agrego un nuevo año de encuesta?**
Sigue la [Guía de Nuevas Encuestas](GUIA_NUEVAS_ENCUESTAS.md) paso a paso.

### **¿Cómo cambio los colores del dashboard?**
Edita `www/styles.css` siguiendo la [Guía de Gráficos](GUIA_MODIFICAR_GRAFICOS.md).

### **¿El dashboard no funciona después de mis cambios?**
1. Revisa los logs en consola R
2. Verifica que todos los archivos .rds existen
3. Confirma que `survey_config.R` está bien configurado

### **¿Cómo accedo al sistema de analytics?**
- Teclado: `Ctrl+Shift+A`
- UI: Triple clic en "Vista Rápida"
- R: `showAnalytics()`

## 🔧 Estructura del Sistema

### Flujo de Datos:
```
Encuesta → Procesamiento → Clasificación → Visualización → Dashboard
(.sav)     (.csv)         (metadatos)     (.rds)         (Shiny)
```

### Arquitectura de Módulos:
```
app.R → Secciones → Subsecciones → Gráficos
        │
        ├── government/
        ├── wellness/
        ├── infrastructure/
        ├── participation/
        └── urban/
```

## 📞 Soporte y Recursos

### Documentación de Tecnologías:
- **R Shiny**: https://shiny.rstudio.com/
- **ggplot2**: https://ggplot2.tidyverse.org/
- **plotly**: https://plotly.com/r/
- **leaflet**: https://rstudio.github.io/leaflet/

### Documentación del Proyecto:
- [Guía Principal del Dashboard](GUIA_DASHBOARD.md)
- [Agregar Nuevas Encuestas](GUIA_NUEVAS_ENCUESTAS.md)
- [Modificar y Reorganizar Gráficos](GUIA_MODIFICAR_GRAFICOS.md)
- [Configuración Avanzada](GUIA_CONFIGURACION.md)
- [Módulos de Computación de Gráficos](GUIA_MODULOS_GRAFICOS.md)
- [Ejemplos Prácticos de Módulos](GUIA_EJEMPLOS_MODULOS.md)
- [Despliegue en VPS](GUIA_DESPLIEGUE_VPS.md)

### Para Resolver Problemas:
1. Consulta la sección de "Errores Comunes" en cada guía
2. Revisa los logs del dashboard
3. Verifica la integridad de los archivos de datos
4. Usa las funciones de debug incluidas

---

## 🎯 Recomendación por Nivel

### **Principiante Total en R:**
1. [Guía Principal](GUIA_DASHBOARD.md) - Conceptos básicos
2. Practica ejecutando el dashboard localmente
3. [Guía de Gráficos](GUIA_MODIFICAR_GRAFICOS.md) - Cambios simples

### **Usuario de R con Experiencia Básica:**
1. [Guía de Nuevas Encuestas](GUIA_NUEVAS_ENCUESTAS.md) - Manejo de datos
2. [Guía de Gráficos](GUIA_MODIFICAR_GRAFICOS.md) - Personalización visual
3. [Guía de Configuración](GUIA_CONFIGURACION.md) - Según necesidades

### **Desarrollador Experimentado:**
1. [Guía de Configuración](GUIA_CONFIGURACION.md) - Configuración completa
2. [Módulos de Computación](GUIA_MODULOS_GRAFICOS.md) - Procesamiento interno
3. Revisa `CLAUDE.md` para detalles técnicos
4. Explora el código fuente directamente

### **Administrador de Sistema:**
1. [Despliegue en VPS](GUIA_DESPLIEGUE_VPS.md) - Subir a producción
2. [Configuración Avanzada](GUIA_CONFIGURACION.md) - Variables de entorno
3. Monitoreo y mantenimiento del servidor

---

*Documentación creada para facilitar el mantenimiento del Dashboard Espejo Ciudadano*  
*Dirigida especialmente a principiantes en R*