# themes_metadata.R
library(dplyr)

# Function to load and process thematic classification files
load_thematic_classifications <- function() {
  # Load the thematic classification files
  per_themes <- read.csv("data/processed/PER_2024_metadata_classified_V3.csv", 
                         encoding = "utf-8", stringsAsFactors = FALSE)
  par_themes <- read.csv("data/processed/PAR_2024_metadata_classified_V3.csv", 
                         encoding = "utf-8", stringsAsFactors = FALSE)
  
  # Add survey identifier
  per_themes$survey_id <- "PER_2024"
  par_themes$survey_id <- "PAR_2024"
  
  # Combine both datasets
  all_themes <- rbind(per_themes, par_themes)
  
  return(all_themes)
}

# Define theme properties (colors, icons, etc.)
theme_properties <- list(
  "Internal" = list(
    description = "Información interna para el funcionamiento del dashboard",
    color = "#adb5bd",
    icon = "info-circle",
    hidden = TRUE,
    subthemes = list(
      "Internal" = list(
        description = "Información interna y metadatos"
      )
    )
  ),
  
  "Social & Economic Wellbeing" = list(
    description = "Indicadores de bienestar social y económico",
    color = "#fd7e14",
    icon = "heart",
    hidden = FALSE,
    subthemes = list(
      "Economic Conditions" = list(
        description = "Condiciones económicas y empleo"
      ),
      "Cultural Participation" = list(
        description = "Participación en actividades culturales"
      ),
      "Demographic Trends" = list(
        description = "Tendencias demográficas"
      )
    )
  ),
  
  "Public Services" = list(
    description = "Evaluación de servicios públicos",
    color = "#ffc107",
    icon = "water",
    hidden = FALSE,
    subthemes = list(
      "Education" = list(
        description = "Servicios educativos"
      ),
      "Healthcare" = list(
        description = "Servicios de salud"
      ),
      "Housing" = list(
        description = "Vivienda"
      ),
      "Utilities & Infrastructure" = list(
        description = "Servicios públicos e infraestructura"
      )
    )
  ),
  
  "Urban Mobility & Environment" = list(
    description = "Movilidad urbana y calidad del medio ambiente",
    color = "#20c997",
    icon = "tree",
    hidden = FALSE,
    subthemes = list(
      "Transportation" = list(
        description = "Transporte y movilidad"
      ),
      "Environmental Quality" = list(
        description = "Calidad ambiental"
      )
    )
  ),
  
  "Governance & Civic Engagement" = list(
    description = "Gobierno y participación ciudadana",
    color = "#6f42c1",
    icon = "landmark",
    hidden = FALSE,
    subthemes = list(
      "Government Operations" = list(
        description = "Operaciones del gobierno"
      ),
      "Political Representation" = list(
        description = "Representación política"
      ),
      "Trust in Institutions" = list(
        description = "Confianza en instituciones"
      ),
      "Public Integrity" = list(
        description = "Integridad pública"
      ),
      "Government Performance" = list(
        description = "Desempeño gubernamental"
      ),
      "Justice & Accountability" = list(
        description = "Justicia y rendición de cuentas"
      )
    )
  ),
  
  "Dashboard Context" = list(
    description = "Información de contexto para el dashboard",
    color = "#6c757d",
    icon = "person-circle",
    hidden = TRUE,
    subthemes = list(
      "Survey Demographics" = list(
        description = "Demografía de las encuestas"
      )
    )
  ),
  
  "Community Participation" = list(
    description = "Niveles y formas de participación comunitaria",
    color = "#198754",
    icon = "people",
    hidden = FALSE,
    subthemes = list(
      "Civic Activities" = list(
        description = "Actividades cívicas"
      ),
      "Community Involvement" = list(
        description = "Participación comunitaria"
      ),
      "Decision-Making Participation" = list(
        description = "Participación en toma de decisiones"
      )
    )
  )
)

# Get all unique main themes (filtering out hidden ones for public display)
get_all_themes <- function(include_hidden = FALSE) {
  themes_data <- load_thematic_classifications()
  unique_themes <- unique(themes_data$MainTheme)
  
  if (!include_hidden) {
    # Filter out hidden themes
    visible_themes <- character(0)
    for (theme in unique_themes) {
      if (!is.null(theme_properties[[theme]]) && 
          !isTRUE(theme_properties[[theme]]$hidden)) {
        visible_themes <- c(visible_themes, theme)
      }
    }
    return(visible_themes)
  }
  
  return(unique_themes)
}

# Get all subthemes for a given main theme
get_subthemes_by_theme <- function(theme_name) {
  themes_data <- load_thematic_classifications()
  subthemes <- themes_data %>%
    filter(MainTheme == theme_name) %>%
    select(Subtheme) %>%
    distinct() %>%
    pull(Subtheme)
  
  return(subthemes)
}

# Get all questions for a given main theme (from both surveys)
get_questions_by_theme <- function(theme_name) {
  themes_data <- load_thematic_classifications()
  questions <- themes_data %>%
    filter(MainTheme == theme_name) %>%
    select(variable, label, scale_type, survey_id, Subtheme)
  
  return(questions)
}

# Get all questions for a given subtheme (from both surveys)
get_questions_by_subtheme <- function(theme_name, subtheme_name) {
  themes_data <- load_thematic_classifications()
  questions <- themes_data %>%
    filter(MainTheme == theme_name, Subtheme == subtheme_name) %>%
    select(variable, label, scale_type, survey_id, MainTheme, Subtheme)
  
  return(questions)
}

# Function to get theme properties
get_theme_property <- function(theme_name) {
  if (theme_name %in% names(theme_properties)) {
    return(theme_properties[[theme_name]])
  } else {
    # Default properties
    return(list(
      description = "Información relacionada con este tema",
      color = "#6C757D",
      icon = "folder",
      hidden = FALSE,
      subthemes = list()
    ))
  }
}

# Function to get subtheme properties
get_subtheme_property <- function(theme_name, subtheme_name) {
  theme_props <- get_theme_property(theme_name)
  
  if (subtheme_name %in% names(theme_props$subthemes)) {
    return(theme_props$subthemes[[subtheme_name]])
  } else {
    # Default properties
    return(list(
      description = "Información relacionada con este subtema"
    ))
  }
}

theme_name_translations <- list(
  "Internal" = "Interno",
  "Social & Economic Wellbeing" = "Bienestar Social y Económico",
  "Public Services" = "Servicios Públicos",
  "Urban Mobility & Environment" = "Movilidad Urbana y Medio Ambiente",
  "Governance & Civic Engagement" = "Gobernanza y Participación Ciudadana",
  "Dashboard Context" = "Contexto del Dashboard",
  "Community Participation" = "Participación Comunitaria"
)

# Mapeo de nombres de subtemas de inglés a español
subtheme_name_translations <- list(
  "Internal" = "Interno",
  "Economic Conditions" = "Condiciones Económicas",
  "Cultural Participation" = "Participación Cultural",
  "Demographic Trends" = "Tendencias Demográficas",
  "Education" = "Educación",
  "Healthcare" = "Salud",
  "Housing" = "Vivienda",
  "Utilities & Infrastructure" = "Servicios e Infraestructura",
  "Transportation" = "Transporte",
  "Environmental Quality" = "Calidad Ambiental",
  "Government Operations" = "Operaciones Gubernamentales",
  "Political Representation" = "Representación Política",
  "Trust in Institutions" = "Confianza en Instituciones",
  "Public Integrity" = "Integridad Pública",
  "Government Performance" = "Desempeño Gubernamental",
  "Justice & Accountability" = "Justicia y Rendición de Cuentas",
  "Survey Demographics" = "Demografía de la Encuesta",
  "Civic Activities" = "Actividades Cívicas",
  "Community Involvement" = "Participación en la Comunidad",
  "Decision-Making Participation" = "Participación en Toma de Decisiones"
)

# Función para traducir nombres de temas
translate_theme_name <- function(english_name) {
  if (english_name %in% names(theme_name_translations)) {
    return(theme_name_translations[[english_name]])
  }
  return(english_name)  # Si no hay traducción, devuelve el nombre original
}

# Función para traducir nombres de subtemas
translate_subtheme_name <- function(english_name) {
  if (english_name %in% names(subtheme_name_translations)) {
    return(subtheme_name_translations[[english_name]])
  }
  return(english_name)  # Si no hay traducción, devuelve el nombre original
}

# Modificar la lista theme_metadata para incluir las nuevas funciones
theme_metadata <- list(
  load_thematic_classifications = load_thematic_classifications,
  get_all_themes = get_all_themes,
  get_subthemes_by_theme = get_subthemes_by_theme,
  get_questions_by_theme = get_questions_by_theme,
  get_questions_by_subtheme = get_questions_by_subtheme,
  get_theme_property = get_theme_property,
  get_subtheme_property = get_subtheme_property,
  translate_theme_name = translate_theme_name,
  translate_subtheme_name = translate_subtheme_name
)