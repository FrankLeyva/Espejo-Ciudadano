# R/extras/reports_metadata.R

# Source the thumbnail generator
source("R/extras/thumbnail_generator.R")

# Function to parse and categorize reports based on filenames
load_reports_metadata <- function(generate_thumbnails_if_missing = TRUE) {
  
  # Define the reports directory path
  reports_dir <- "www/reports"
  
  # Check if directory exists
  if (!dir.exists(reports_dir)) {
    warning("Reports directory not found: ", reports_dir)
    return(data.frame(
      filename = character(0),
      title = character(0),
      category = character(0),
      year = numeric(0),
      type = character(0),
      description = character(0),
      thumbnail_path = character(0),
      has_thumbnail = logical(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Get all PDF files
  pdf_files <- list.files(reports_dir, pattern = "\\.pdf$", ignore.case = TRUE)
  
  if (length(pdf_files) == 0) {
    warning("No PDF files found in reports directory")
    return(data.frame(
      filename = character(0),
      title = character(0),
      category = character(0),
      year = numeric(0),
      type = character(0),
      description = character(0),
      thumbnail_path = character(0),
      has_thumbnail = logical(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Generate thumbnails if requested and dependencies are available
  if (generate_thumbnails_if_missing && check_thumbnail_dependencies()) {
    # Check which thumbnails are missing
    missing_thumbnails <- c()
    for (pdf_file in pdf_files) {
      thumb_path <- get_thumbnail_path(pdf_file)
      if (is.null(thumb_path)) {
        missing_thumbnails <- c(missing_thumbnails, pdf_file)
      }
    }
    
    if (length(missing_thumbnails) > 0) {
      cat("Generating", length(missing_thumbnails), "missing thumbnails...\n")
      # Generate only missing thumbnails
      for (pdf_file in missing_thumbnails) {
        pdf_path <- file.path(reports_dir, pdf_file)
        generate_pdf_thumbnail(pdf_path)
      }
    }
  }
  
  # Process each file
  reports_list <- lapply(pdf_files, function(filename) {
    parse_report_metadata(filename)
  })
  
  # Combine into data frame
  reports_df <- do.call(rbind, reports_list)
  
  # Sort by year (descending), then by category, then by title
  reports_df <- reports_df[order(-reports_df$year, reports_df$category, reports_df$title), ]
  
  return(reports_df)
}

# Function to parse individual report metadata
parse_report_metadata <- function(filename) {
  # Extract base filename without extension
  base_name <- tools::file_path_sans_ext(filename)
  lower_name <- tolower(base_name)
  
  # Extract year from filename
  year_matches <- regmatches(lower_name, gregexpr("20[0-9]{2}", lower_name))[[1]]
  if (length(year_matches) > 0) {
    # Take the last year found (most likely to be the report year)
    year <- as.numeric(year_matches[length(year_matches)])
  } else {
    year <- NA
  }
  
  # Determine document type
  type <- if (grepl("presentacion", lower_name)) {
    "presentacion"
  } else {
    "informe"
  }
  
  # Categorize based on content keywords
  category <- categorize_report(lower_name)
  
  # Generate title
  title <- generate_report_title(base_name, category, year, type)
  
  # Generate description
  description <- generate_report_description(category, year, type, lower_name)
  
  # Get thumbnail information
  thumbnail_path <- get_thumbnail_path(filename)
  has_thumbnail <- !is.null(thumbnail_path)
  
  return(data.frame(
    filename = filename,
    title = title,
    category = category,
    year = ifelse(is.na(year), 2020, year), # Default year if not found
    type = type,
    description = description,
    thumbnail_path = ifelse(has_thumbnail, thumbnail_path, ""),
    has_thumbnail = has_thumbnail,
    stringsAsFactors = FALSE
  ))
}

# Function to categorize reports based on filename content
categorize_report <- function(lower_name) {
  
  # Define category keywords
  categories <- list(
    "Bienestar" = c("aej", "bienestar", "subjetivo", "calidad", "vida"),
    "Servicios Públicos" = c("servicios", "publicos", "agua", "recoleccion", "alumbrado"),
    "Gobierno" = c("corrupcion", "confianza", "instituciones", "gobierno", "buen.gobierno"),
    "Economía" = c("economia", "economica", "pobreza", "empleo"),
    "Salud" = c("salud", "covid", "suicidio"),
    "Educación" = c("educacion", "infancia", "juventud", "estudiantes"),
    "Movilidad" = c("movilidad", "transporte", "calles", "seguras"),
    "Medio Ambiente" = c("medio.ambiente", "ambiente", "ambiental")
  )
  
  # Check each category
  for (cat_name in names(categories)) {
    keywords <- categories[[cat_name]]
    if (any(sapply(keywords, function(kw) grepl(kw, lower_name)))) {
      return(cat_name)
    }
  }
  
  # Default category
  return("General")
}

# Function to generate human-readable titles
generate_report_title <- function(base_name, category, year, type) {
  
  # Clean up the base name
  clean_name <- base_name
  clean_name <- gsub("_", " ", clean_name)
  clean_name <- gsub("-", " ", clean_name)
  clean_name <- gsub("\\.", " ", clean_name)
  
  # Remove redundant year information
  if (!is.na(year)) {
    clean_name <- gsub(as.character(year), "", clean_name)
  }
  
  # Remove common prefixes
  clean_name <- gsub("^informe\\s*", "", clean_name, ignore.case = TRUE)
  clean_name <- gsub("^presentacion\\s*", "", clean_name, ignore.case = TRUE)
  
  # Clean up extra spaces
  clean_name <- gsub("\\s+", " ", clean_name)
  clean_name <- trimws(clean_name)
  
  # Specific title mappings for common patterns
  title_mappings <- list(
    "aej" = "Así Estamos Juárez",
    "economia" = "Informe de Economía",
    "servicios publicos" = "Servicios Públicos",
    "medio ambiente" = "Medio Ambiente",
    "corrupcion" = "Corrupción",
    "salud" = "Salud",
    "movilidad" = "Movilidad",
    "juventud" = "Juventudes",
    "bienestar subjetivo" = "Bienestar Subjetivo",
    "confianza instituciones" = "Confianza en las Instituciones",
    "calles seguras" = "Calles Seguras"
  )
  
  # Apply mappings
  lower_clean <- tolower(clean_name)
  for (pattern in names(title_mappings)) {
    if (grepl(pattern, lower_clean)) {
      return(title_mappings[[pattern]])
    }
  }
  
  # Capitalize first letter of each word
  title <- tools::toTitleCase(clean_name)
  
  # If title is too short or unclear, use category
  if (nchar(title) < 3 || title == "") {
    title <- category
  }
  
  return(title)
}

# Function to generate descriptions
generate_report_description <- function(category, year, type, lower_name) {
  
  base_descriptions <- list(
    "Bienestar" = "Análisis integral de la calidad de vida y bienestar de los ciudadanos de Juárez.",
    "Servicios Públicos" = "Evaluación de la calidad y satisfacción con los servicios públicos municipales.",
    "Gobierno" = "Estudio sobre confianza institucional, corrupción y buen gobierno.",
    "Economía" = "Análisis de las condiciones económicas y oportunidades de desarrollo.",
    "Salud" = "Evaluación del sistema de salud y condiciones sanitarias de la ciudad.",
    "Educación" = "Estudio sobre el sistema educativo y desarrollo de juventudes.",
    "Movilidad" = "Análisis de la movilidad urbana y sistemas de transporte.",
    "Medio Ambiente" = "Evaluación de las condiciones ambientales y sustentabilidad.",
    "General" = "Informe general sobre diversos aspectos del desarrollo urbano."
  )
  
  base_desc <- base_descriptions[[category]] %||% base_descriptions[["General"]]
  
  # Add year context if available
  if (!is.na(year) && year >= 2014) {
    base_desc <- paste0(base_desc, " Datos correspondientes al año ", year, ".")
  }
  
  # Add type-specific information
  if (type == "presentacion") {
    base_desc <- paste0(base_desc, " Presentación ejecutiva de resultados.")
  }
  
  # Add specific details based on filename patterns
  if (grepl("covid", lower_name)) {
    base_desc <- paste0(base_desc, " Incluye análisis del impacto de COVID-19.")
  }
  
  if (grepl("juventud|infancia", lower_name)) {
    base_desc <- paste0(base_desc, " Enfoque especial en población joven.")
  }
  
  if (grepl("rueda.prensa", lower_name)) {
    base_desc <- paste0(base_desc, " Material de rueda de prensa.")
  }
  
  return(base_desc)
}

# Helper null coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x