# Header component function
create_dashboard_header <- function(title, subtitle = NULL) {
  div(
    class = "dashboard-header mb-4",
    div(
      class = "container-fluid",  # Use fluid container for full width
      div(
        class = "row align-items-center",
        div(
          class = "col",
          h1(class = "display-5 fw-bold text-primary", title),
          if (!is.null(subtitle)) {
            p(class = "lead text-muted", subtitle)
          }
        ),
        # Only include logo if you have one
        # div(
        #   class = "col-auto",
        #   img(src = "logo.png", height = "60px", alt = "Logo")
        # )
      )
    )
  )
}

# Footer component function
create_dashboard_footer <- function() {
  div(
    class = "border-top mt-5 pt-4 pb-4 text-center text-muted",
    div(
      class = "container-fluid",
      p("Dashboard creado por Plan Estratégico de Juárez", class = "mb-1"),
      p("Datos actualizados: Marzo 2025", class = "mb-0 small")
    )
  )
}
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

# Update these functions in your global.R file

# Create a dynamic tooltip that updates based on input (with duplicate prevention)
create_dynamic_tooltip <- function(id, css_class = "info-tooltip") {
  tags$span(
    class = paste(css_class, "tooltip-element"),  # Added tooltip-element class for easier selection
    id = id,
    style = "margin-left: 6px; cursor: help;",
    `data-bs-toggle` = "tooltip",
    `data-bs-placement` = "top",
    `data-bs-html` = "true",
    title = "Cargando información...",
    bsicons::bs_icon("question-circle-fill", size = "1em")
  )
}

# Create a static tooltip (with duplicate prevention)
create_tooltip <- function(tooltip_text, placement = "top", css_class = "info-tooltip") {
  tags$span(
    class = paste(css_class, "tooltip-element"),  # Added tooltip-element class for easier selection
    style = "margin-left: 6px; cursor: help;",
    `data-bs-toggle` = "tooltip",
    `data-bs-placement` = placement,
    `data-bs-html` = "true",
    title = tooltip_text,
    bsicons::bs_icon("question-circle-fill", size = "1em")
  )
}

# Updated tooltip update function to properly cleanup existing instances
update_tooltip_content <- function(session, tooltip_id, new_content) {
  # Send an improved jQuery command that properly disposes old tooltips
  session$sendCustomMessage("simple-tooltip-update", 
                           list(id = tooltip_id, content = new_content))
}

# Improved tooltip initialization with duplicate prevention
init_tooltips <- function() {
  tags$script(HTML("
    $(document).ready(function() {
      // Clean up function to make sure we don't have duplicate tooltips
      function initializeTooltips() {
        // First make sure all existing tooltips are disposed
        $('.tooltip-element').each(function() {
          try {
            // Try to dispose any existing tooltip
            var tooltipInstance = bootstrap.Tooltip.getInstance(this);
            if (tooltipInstance) {
              tooltipInstance.dispose();
            }
          } catch (e) {
            console.log('Error disposing tooltip:', e);
          }
        });
        
        // Initialize tooltips with a small delay to ensure dispose is complete
        setTimeout(function() {
          $('.tooltip-element').tooltip({
            html: true,
            container: 'body'  // This helps prevent tooltip positioning issues
          });
        }, 50);
      }
      
      // Initialize tooltips on page load
      initializeTooltips();
      
      // Simple direct update handler with improved cleanup
      Shiny.addCustomMessageHandler('simple-tooltip-update', function(message) {
        // Get the element by ID
        var tooltipEl = $('#' + message.id);
        
        if(tooltipEl.length) {
          try {
            // Try to dispose old tooltip
            var tooltipInstance = bootstrap.Tooltip.getInstance(tooltipEl[0]);
            if (tooltipInstance) {
              tooltipInstance.dispose();
            }
          } catch (e) {
            console.log('Error disposing old tooltip:', e);
          }
          
          // Update the title attribute
          tooltipEl.attr('title', message.content);
          
          // Re-initialize with a small delay
          setTimeout(function() {
            tooltipEl.tooltip({
              html: true,
              container: 'body'
            });
          }, 50);
          
          console.log('Updated tooltip: ' + message.id);
        } else {
          console.error('Tooltip element not found: ' + message.id);
        }
      });
      
      // Re-initialize tooltips after tab changes
      $(document).on('shown.bs.tab', function() {
        initializeTooltips();
      });
      
      // Re-initialize tooltips when Shiny updates UI elements
      $(document).on('shiny:value', function() {
        initializeTooltips();
      });
    });
  "))
}

value_box_with_title_tooltip <- function(title, value, showcase, theme, tooltip_text, force_icon_color = NULL) {
  # Extract background color from theme to determine icon color
  bg_color <- NULL
  if (!is.null(theme) && !is.null(theme$bg)) {
    bg_color <- theme$bg
  }
  
  # Use forced color if provided, otherwise determine automatically
  icon_color <- if (!is.null(force_icon_color)) {
    force_icon_color
  } else {
    # Default to white for dark backgrounds
    icon_color <- "rgba(255, 255, 255, 0.8)" 
    
    # Convert hex to RGB and check brightness if we have a background color
    if (!is.null(bg_color) && substr(bg_color, 1, 1) == "#") {
      # Simple brightness calculation
      # Convert hex to RGB
      r <- strtoi(substr(bg_color, 2, 3), 16)
      g <- strtoi(substr(bg_color, 4, 5), 16)
      b <- strtoi(substr(bg_color, 6, 7), 16)
      
      # Calculate brightness (simplified formula)
      brightness <- (r * 299 + g * 587 + b * 114) / 1000
      
      # If background is light, use dark icon
      if (brightness > 128) {
        icon_color <- "rgba(0, 0, 0, 0.7)"
      }
    }
    
    icon_color
  }
  
  # Create an icon with appropriate styling
  tooltip_icon <- tags$span(
    class = "info-tooltip-icon",
    style = paste0("margin-left: 6px; cursor: help; color: ", icon_color, ";"),
    title = tooltip_text,
    `data-bs-toggle` = "tooltip",
    `data-bs-placement` = "top",
    `data-bs-html` = "true",
    bsicons::bs_icon("question-circle-fill", size = "0.8em")
  )
  
  # Build a title with tooltip icon
  title_with_tooltip <- div(
    class = "d-flex align-items-center",
    title,
    tooltip_icon
  )
  
  # Create the value box with enhanced title
  value_box(
    title = title_with_tooltip,
    value = value,
    showcase = showcase,
    theme = theme
  )
}