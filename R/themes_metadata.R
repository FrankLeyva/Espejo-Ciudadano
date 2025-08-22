# themes_metadata.R
library(dplyr)

# Function to load and process thematic classification files for all years
load_thematic_classifications <- function() {
  # Initialize empty data frame with the structure we need
  all_themes <- data.frame(
    variable = character(0),
    label = character(0),
    value_labels = character(0),
    scale_type = character(0),
    MainTheme = character(0),
    Subtheme = character(0),
    survey_id = character(0),
    stringsAsFactors = FALSE
  )
  
  # Define the files to try for each year and survey (in order of preference)
  file_config <- list(
    "2023" = list(
      "PER" = c(
        "data/processed/PER_2023_metadata_classified_V3.csv"
      ),
      "PAR" = c(
        "data/processed/PAR_2023_metadata_classified_V2.csv"
      )
    ),
    "2024" = list(
      "PER" = c(
        "data/processed/PER_2024_metadata_classified_V4.csv"
      ),
      "PAR" = c(
        "data/processed/PAR_2024_metadata_classified_V4.csv"
      )
    )
  )
  
  # Define the columns we need
  required_columns <- c("variable", "label", "value_labels", "scale_type", "MainTheme", "Subtheme")
  
  # Load data for each year and survey
  for (year in names(file_config)) {
    for (survey_type in names(file_config[[year]])) {
      survey_id <- paste0(survey_type, "_", year)
      files_to_try <- file_config[[year]][[survey_type]]
      
      # Try each file until we find one that exists
      loaded <- FALSE
      for (file_path in files_to_try) {
        if (file.exists(file_path)) {
          tryCatch({
            # Load the file
            themes_data <- read.csv(file_path, 
                                   encoding = "utf-8", 
                                   stringsAsFactors = FALSE)
            
            message(paste("Raw file", file_path, "has columns:", paste(names(themes_data), collapse = ", ")))
            
            # Check which required columns exist
            available_columns <- intersect(required_columns, names(themes_data))
            missing_columns <- setdiff(required_columns, names(themes_data))
            
            if (length(missing_columns) > 0) {
              message(paste("Missing columns in", file_path, ":", paste(missing_columns, collapse = ", ")))
            }
            
            # Only select the columns we need (and that exist)
            if (length(available_columns) >= 4) {  # Need at least variable, MainTheme, Subtheme, and one other
              # Create a standardized data frame with all required columns
              standardized_data <- data.frame(
                variable = if ("variable" %in% names(themes_data)) themes_data$variable else NA_character_,
                label = if ("label" %in% names(themes_data)) themes_data$label else NA_character_,
                value_labels = if ("value_labels" %in% names(themes_data)) themes_data$value_labels else NA_character_,
                scale_type = if ("scale_type" %in% names(themes_data)) themes_data$scale_type else NA_character_,
                MainTheme = if ("MainTheme" %in% names(themes_data)) themes_data$MainTheme else NA_character_,
                Subtheme = if ("Subtheme" %in% names(themes_data)) themes_data$Subtheme else NA_character_,
                survey_id = survey_id,
                stringsAsFactors = FALSE
              )
              
              # Remove rows with missing essential data
              standardized_data <- standardized_data[
                !is.na(standardized_data$variable) & 
                !is.na(standardized_data$MainTheme) & 
                !is.na(standardized_data$Subtheme), 
              ]
              
              # Combine with existing data
              all_themes <- rbind(all_themes, standardized_data)
              
              message(paste("Successfully loaded", survey_id, "metadata from", basename(file_path), "with", nrow(standardized_data), "valid questions"))
              loaded <- TRUE
              break  # Stop trying other versions once we successfully load one
            } else {
              message(paste("File", file_path, "doesn't have enough required columns"))
            }
            
          }, error = function(e) {
            message(paste("Error loading", file_path, ":", e$message))
          })
        }
      }
      
      if (!loaded) {
        warning(paste("No usable metadata file found for", survey_id, ". Tried:", paste(files_to_try, collapse = ", ")))
      }
    }
  }
  
  message(paste("Total metadata loaded:", nrow(all_themes), "questions from", length(unique(all_themes$survey_id)), "surveys"))
  
  # Show summary by survey
  if (nrow(all_themes) > 0) {
    survey_summary <- all_themes %>%
      group_by(survey_id) %>%
      summarise(
        questions = n(),
        themes = n_distinct(MainTheme),
        .groups = 'drop'
      )
    
    for (i in 1:nrow(survey_summary)) {
      message(paste(survey_summary$survey_id[i], ":", survey_summary$questions[i], "questions,", survey_summary$themes[i], "themes"))
    }
  }
  
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
            "Education" = list(
        description = "Servicios educativos"
      ),
      "Healthcare" = list(
        description = "Servicios de salud"
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
  "Social & Economic Wellbeing" = "Calidad de Vida",
  "Public Services" = "Infraestructura y Equipamiento",
  "Urban Mobility & Environment" = "Movilidad Urbana",
  "Governance & Civic Engagement" = "Instituciones",
  "Dashboard Context" = "Contexto del Dashboard",
  "Community Participation" = "Participación Ciudadana"
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
  "Utilities & Infrastructure" = "Servicios Públicos e Infraestructura",
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