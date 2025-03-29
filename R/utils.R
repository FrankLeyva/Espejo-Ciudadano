
get_question_label <- function(question_id, metadata) {
  # Find the metadata entry for this question
  question_meta <- metadata %>%
    filter(variable == question_id) %>%
    first()
  
  if (!is.null(question_meta) && !is.na(question_meta$label)) {
    return(question_meta$label)
  } else {
    return(question_id)
  }
}
get_column_from_question <- function(data, question_id) {
  # Get the column mapping from attributes
  col_mapping <- attr(data, "col_mapping")
  
  # Try to find column with exact name first
  if (question_id %in% names(data)) {
    return(question_id)
  }
  
  # Try to find column using the mapping
  if (!is.null(col_mapping) && question_id %in% names(col_mapping)) {
    return(col_mapping[[question_id]])
  }
  
  # Final fallback: look for any column that starts with the question_id
  for (col in names(data)) {
    if (startsWith(col, question_id)) {
      return(col)
    }
  }
  
  # If all else fails, return NULL
  return(NULL)
}

# Function to check if standardized columns exist and handle missing ones
check_standardized_columns <- function(data) {
  required_cols <- c("DISTRICT", "GENDER", "AGE_GROUP")
  missing_cols <- required_cols[!required_cols %in% names(data)]
  
  if (length(missing_cols) > 0) {
    # Add dummy values for missing columns
    for (col in missing_cols) {
      data[[col]] <- NA
      warning(paste("Missing standardized column:", col, "- adding NA values"))
    }
  }
  
  return(data)
}

# Function to safely prepare numeric data
prepare_numeric_data <- function(data, value_col) {
  if (is.null(data) || is.null(value_col) || !value_col %in% names(data)) {
    return(NULL)
  }
  
  # Check and add standardized columns
  data <- check_standardized_columns(data)
  
  # Convert value column to numeric safely
  result <- data %>%
    mutate(
      value = suppressWarnings(as.numeric(as.character(!!sym(value_col)))),
      district = as.factor(DISTRICT),
      gender = as.factor(GENDER),
      age_group = as.factor(AGE_GROUP)
    ) %>%
    # Remove NA values after conversion
    filter(!is.na(value))
  
  return(result)
}

# Utility functions for methodology and data section

# Function to generate survey metadata summary
generate_survey_metadata <- function(survey_id) {
  tryCatch({
    # Load the survey data
    survey_data <- load_survey_data(survey_id)
    
    # Extract responses and metadata
    responses <- survey_data$responses
    metadata <- survey_data$metadata
    
    # Calculate basic statistics
    total_questions <- length(unique(gsub("\\..*$", "", grep("^Q", names(responses), value = TRUE))))
    total_respondents <- nrow(responses)
    
    # Count by district if available
    district_counts <- if ("DISTRICT" %in% names(responses)) {
      table(responses$DISTRICT)
    } else {
      NULL
    }
    
    # Return metadata summary
    return(list(
      survey_id = survey_id,
      total_questions = total_questions,
      total_respondents = total_respondents,
      district_counts = district_counts,
      collection_date = "Febrero 2025"  # Hardcoded from text
    ))
  }, error = function(e) {
    # Return error information
    return(list(
      survey_id = survey_id,
      error = TRUE,
      error_message = e$message
    ))
  })
}

# Function to create a data dictionary from survey metadata
create_data_dictionary <- function(survey_id) {
  tryCatch({
    # Load the survey data
    survey_data <- load_survey_data(survey_id)
    
    # Extract metadata
    metadata <- survey_data$metadata
    
    # Create simplified data dictionary
    if (!is.null(metadata) && nrow(metadata) > 0) {
      dictionary <- metadata %>%
        select(variable, label, type) %>%
        mutate(
          description = label,
          possible_values = NA_character_
        )
      
      return(dictionary)
    } else {
      return(NULL)
    }
  }, error = function(e) {
    return(NULL)
  })
}

# Function to export data dictionary to CSV
export_data_dictionary <- function(survey_id, file) {
  dictionary <- create_data_dictionary(survey_id)
  if (!is.null(dictionary)) {
    write.csv(dictionary, file, row.names = FALSE)
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Function to create a summary of available data
get_survey_summary <- function() {
  # Try to load both surveys
  per_meta <- generate_survey_metadata("PER_2024")
  par_meta <- generate_survey_metadata("PAR_2024")
  
  # Combine into a summary table
  summary_data <- data.frame(
    survey_id = c("PER_2024", "PAR_2024"),
    name = c("Encuesta de Percepción Ciudadana", 
             "Encuesta de Participación Ciudadana y Buen Gobierno"),
    respondents = c(
      if(!is.null(per_meta$total_respondents)) per_meta$total_respondents else NA,
      if(!is.null(par_meta$total_respondents)) par_meta$total_respondents else NA
    ),
    questions = c(
      if(!is.null(per_meta$total_questions)) per_meta$total_questions else NA,
      if(!is.null(par_meta$total_questions)) par_meta$total_questions else NA
    ),
    collection_date = c("Febrero 2025", "Febrero 2025"),
    stringsAsFactors = FALSE
  )
  
  return(summary_data)
}

# Helper function to process downloaded data
prepare_download_data <- function(survey_data) {
  # Extract responses
  export_data <- survey_data$responses
  
  # Sanitize data for export
  # 1. Convert factors to character strings
  for (col in names(export_data)) {
    if (is.factor(export_data[[col]])) {
      export_data[[col]] <- as.character(export_data[[col]])
    }
  }
  
  # 2. Ensure consistent NA representation
  export_data[is.na(export_data)] <- ""
  
  return(export_data)
}

# Function to generate downloadable files in various formats
generate_download_file <- function(survey_id, format = "csv", file) {
  tryCatch({
    # Load survey data
    survey_data <- load_survey_data(survey_id)
    
    # Prepare data
    export_data <- prepare_download_data(survey_data)
    
    # Export based on requested format
    if (format == "csv") {
      write.csv(export_data, file, row.names = FALSE, na = "")
      return(TRUE)
    } else if (format == "xlsx") {
      # Requires the openxlsx package
      if (requireNamespace("openxlsx", quietly = TRUE)) {
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Data")
        openxlsx::writeData(wb, "Data", export_data)
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
        return(TRUE)
      } else {
        return(FALSE)
      }
    } else {
      return(FALSE)
    }
  }, error = function(e) {
    return(FALSE)
  })
}