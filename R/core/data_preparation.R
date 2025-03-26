
library(dplyr)
library(readr)

#' Theme definitions with properties
theme_definitions <- list(
  "Participación Comunitaria" = list(
    description = "Involucramiento en actividades comunitarias y procesos democráticos",
    icon = "users",
    color = "#2ECC71"  # Green
  ),
  "Gobernanza y Participación Cívica" = list(
    description = "Percepción sobre instituciones gubernamentales y participación ciudadana",
    icon = "landmark",
    color = "#E74C3C"  # Red
  ),
  "Servicios Públicos" = list(
    description = "Evaluación de la calidad y acceso a servicios públicos como agua, luz, transporte",
    icon = "hands-helping",
    color = "#3498DB"  # Blue
  ),
  "Bienestar Social y Económico" = list(
    description = "Percepción de situación económica, empleo y calidad de vida",
    icon = "coins",
    color = "#F1C40F"  # Yellow
  ),
  "Movilidad Urbana y Medio Ambiente" = list(
    description = "Transporte, infraestructura urbana y cuestiones ambientales",
    icon = "bus",
    color = "#27AE60"  # Dark Green
  ),
  "Seguridad" = list(
    description = "Percepción de seguridad, violencia, y confianza en instituciones de seguridad",
    icon = "shield-alt",
    color = "#C0392B"  # Dark Red
  ),
  "Calidad de Vida" = list(
    description = "Bienestar general, satisfacción con la vida, y factores que afectan la calidad de vida",
    icon = "home",
    color = "#9B59B6"  # Purple
  ),
  "Demografía" = list(
    description = "Información demográfica de los encuestados",
    icon = "id-card",
    color = "#95A5A6"  # Gray
  )
)

#' Load combined themes from both surveys
#' 
#' @param per_path Path to PER survey theme classification file
#' @param par_path Path to PAR survey theme classification file
#' @return Combined themes data frame
load_combined_themes <- function(per_path, par_path) {
  # Load PER survey themes
  per_themes <- tryCatch({
    read_csv(per_path, col_types = cols()) %>%
      mutate(survey_id = "PER_2024")
  }, error = function(e) {
    warning(paste("Error loading PER survey themes:", e$message))
    tibble()
  })
  
  # Load PAR survey themes
  par_themes <- tryCatch({
    read_csv(par_path, col_types = cols()) %>%
      mutate(survey_id = "PAR_2024")
  }, error = function(e) {
    warning(paste("Error loading PAR survey themes:", e$message))
    tibble()
  })
  
  # Combine them
  all_themes <- bind_rows(per_themes, par_themes)
  
  return(all_themes)
}

#' Get all questions for a specific theme
#' 
#' @param all_themes Combined themes data frame
#' @param theme_name Theme name
#' @return Data frame with questions for the theme
get_questions_by_theme <- function(all_themes, theme_name) {
  all_themes %>%
    filter(MainTheme == theme_name) %>%
    select(variable, label, scale_type, Subtheme, survey_id)
}

#' Get all questions for a specific subtheme
#'
#' @param all_themes Combined themes data frame
#' @param theme_name Theme name
#' @param subtheme Subtheme name
#' @return Data frame with questions for the subtheme
get_questions_by_subtheme <- function(all_themes, theme_name, subtheme) {
  all_themes %>%
    filter(MainTheme == theme_name, Subtheme == subtheme) %>%
    select(variable, label, scale_type, survey_id)
}

#' Get all available themes
#'
#' @param all_themes Combined themes data frame
#' @return Vector of theme names
get_all_themes <- function(all_themes) {
  all_themes %>%
    select(MainTheme) %>%
    filter(!is.na(MainTheme) & MainTheme != "") %>%
    distinct() %>%
    pull(MainTheme)
}

#' Get all subthemes for a theme
#'
#' @param all_themes Combined themes data frame
#' @param theme_name Theme name
#' @return Vector of subtheme names
get_subthemes_for_theme <- function(all_themes, theme_name) {
  all_themes %>%
    filter(MainTheme == theme_name) %>%
    select(Subtheme) %>%
    filter(!is.na(Subtheme) & Subtheme != "") %>%
    distinct() %>%
    pull(Subtheme)
}

#' Initialize theme mapping
#'
#' @return List with theme mapping functions and data
init_theme_mapping <- function() {
  # Paths to the theme classification files
  per_path <- "data/processed/PER_2024_Thematic_Classification_V2.csv"
  par_path <- "data/processed/PAR_2024_Thematic_Classification_V2.csv"
  
  # Load and combine themes
  all_themes <- load_combined_themes(per_path, par_path)
  
  # Create theme mapping interface
  theme_mapping <- list(
    all_themes = all_themes,
    get_questions_by_theme = function(theme_name) {
      get_questions_by_theme(all_themes, theme_name)
    },
    get_questions_by_subtheme = function(theme_name, subtheme) {
      get_questions_by_subtheme(all_themes, theme_name, subtheme)
    },
    get_all_themes = function() {
      get_all_themes(all_themes)
    },
    get_subthemes_for_theme = function(theme_name) {
      get_subthemes_for_theme(all_themes, theme_name)
    },
    get_theme_property = function(theme_name) {
      theme_definitions[[theme_name]]
    }
  )
  
  return(theme_mapping)
}