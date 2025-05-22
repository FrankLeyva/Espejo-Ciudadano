# R/enhanced_data_manager.R

#' Enhanced DataManager Class for Full Dashboard
#' 
#' An R6 class for efficiently loading, processing, and caching all dashboard data
#' 
#' @export
EnhancedDataManager <- R6::R6Class("EnhancedDataManager",
  public = list(
    #' @field cache A list to store cached data
    cache = list(),
    
    #' @field plot_cache A list to store cached plots
    plot_cache = list(),
    
    #' @field survey_cache A list to store cached survey data
    survey_cache = list(),
    
    #' @description
    #' Create a new EnhancedDataManager object
    #' @return A new EnhancedDataManager object
    initialize = function() {
      # Initialize memoized functions for survey data loading
      private$memoised_load_survey_data <- memoise::memoise(function(survey_id) {
        message(paste("Loading survey data for", survey_id))
        data <- load_survey_data(survey_id)
        
        # Store in survey cache
        self$survey_cache[[survey_id]] <- data
        return(data)
      })
      
      # Initialize memoized functions for different data types
      private$memoised_prepare_binary_data <- memoise::memoise(function(survey_id, question_id) {
        survey_data <- self$get_survey_data(survey_id)
        if (!is.null(survey_data)) {
          return(prepare_binary_data(
            data = survey_data$responses,
            question_id = question_id,
            metadata = survey_data$metadata
          ))
        }
        return(NULL)
      })
      
      private$memoised_prepare_categorical_data <- memoise::memoise(function(survey_id, question_id) {
        survey_data <- self$get_survey_data(survey_id)
        if (!is.null(survey_data)) {
          return(prepare_categorical_data(
            data = survey_data$responses,
            question_id = question_id,
            metadata = survey_data$metadata
          ))
        }
        return(NULL)
      })
      
      private$memoised_prepare_interval_data <- memoise::memoise(function(survey_id, question_id) {
        survey_data <- self$get_survey_data(survey_id)
        if (!is.null(survey_data)) {
          return(prepare_interval_data(
            data = survey_data$responses,
            question_id = question_id,
            metadata = survey_data$metadata
          ))
        }
        return(NULL)
      })
      
      private$memoised_prepare_ordinal_data <- memoise::memoise(function(survey_id, question_id) {
        survey_data <- self$get_survey_data(survey_id)
        if (!is.null(survey_data)) {
          return(prepare_ordinal_data(
            data = survey_data$responses,
            question_id = question_id,
            metadata = survey_data$metadata
          ))
        }
        return(NULL)
      })
      
      private$memoised_prepare_nominal_data <- memoise::memoise(function(survey_id, question_id) {
        survey_data <- self$get_survey_data(survey_id)
        if (!is.null(survey_data)) {
          return(prepare_nominal_data(
            data = survey_data$responses,
            question_id = question_id,
            metadata = survey_data$metadata
          ))
        }
        return(NULL)
      })
      
      private$memoised_prepare_razon_data <- memoise::memoise(function(survey_id, question_id) {
        survey_data <- self$get_survey_data(survey_id)
        if (!is.null(survey_data)) {
          return(prepare_razon_data(
            data = survey_data$responses,
            question_id = question_id,
            metadata = survey_data$metadata
          ))
        }
        return(NULL)
      })
      
      message("Enhanced DataManager initialized")
    },
    
    #' @description
    #' Get survey data with caching
    #' @param survey_id Survey identifier (e.g., "PER_2024", "PAR_2024")
    #' @return Survey data object
    get_survey_data = function(survey_id) {
      if (is.null(self$survey_cache[[survey_id]])) {
        return(private$memoised_load_survey_data(survey_id))
      }
      return(self$survey_cache[[survey_id]])
    },
    
    #' @description
    #' Get processed data for any question type
    #' @param survey_id Survey identifier
    #' @param question_id Question identifier
    #' @param data_type Type of data processing ("binary", "categorical", "interval", "ordinal", "nominal", "razon")
    #' @return Processed data
    get_processed_data = function(survey_id, question_id, data_type = "auto") {
      # Determine data type automatically if not specified
      if (data_type == "auto") {
        survey_data <- self$get_survey_data(survey_id)
        if (!is.null(survey_data)) {
          question_meta <- survey_data$metadata[survey_data$metadata$variable == question_id, ]
          if (nrow(question_meta) > 0) {
            data_type <- classify_question(question_meta)
          } else {
            data_type <- "categorical"  # Default fallback
          }
        }
      }
      
      # Call appropriate memoized function
      switch(data_type,
        "binary" = private$memoised_prepare_binary_data(survey_id, question_id),
        "categorical" = private$memoised_prepare_categorical_data(survey_id, question_id),
        "interval" = private$memoised_prepare_interval_data(survey_id, question_id),
        "ordinal" = private$memoised_prepare_ordinal_data(survey_id, question_id),
        "nominal" = private$memoised_prepare_nominal_data(survey_id, question_id),
        "razon" = private$memoised_prepare_razon_data(survey_id, question_id),
        private$memoised_prepare_categorical_data(survey_id, question_id)  # Default
      )
    },
    
    #' @description
    #' Get or create a plot with caching
    #' @param plot_key Unique key for the plot
    #' @param plot_function Function to create the plot
    #' @param ... Arguments to pass to plot_function
    #' @return Plotly object
    get_or_create_plot = function(plot_key, plot_function, ...) {
      if (is.null(self$plot_cache[[plot_key]])) {
        message(paste("Creating plot:", plot_key))
        self$plot_cache[[plot_key]] <- plot_function(...)
      }
      return(self$plot_cache[[plot_key]])
    },
    
    #' @description
    #' Get section-specific data and plots
    #' @param section Section name
    #' @param year Survey year
    #' @param survey_type Survey type ("PER" or "PAR")
    #' @return List of data and plots for the section
    get_section_data = function(section, year, survey_type = "PER") {
      survey_id <- paste0(survey_type, "_", year)
      section_key <- paste0(section, "_", survey_id)
      
      if (is.null(self$cache[[section_key]])) {
        message(paste("Loading section data for", section, year))
        
        # Get questions for this section from thematic classification
        section_questions <- self$get_section_questions(section, survey_type)
        
        # Process data for each question in the section
        section_data <- list()
        
        for (question in section_questions$variable) {
          processed_data <- self$get_processed_data(survey_id, question)
          if (!is.null(processed_data)) {
            section_data[[question]] <- processed_data
          }
        }
        
        self$cache[[section_key]] <- section_data
      }
      
      return(self$cache[[section_key]])
    },
    
    #' @description
    #' Get questions for a specific section based on thematic classification
    #' @param section Section name
    #' @param survey_type Survey type
    #' @return Data frame of questions for the section
    get_section_questions = function(section, survey_type = "PER") {
      # Map dashboard sections to thematic classifications
      section_mapping <- list(
        "wellness" = "Social & Economic Wellbeing",
        "economic" = "Social & Economic Wellbeing", 
        "cultural" = "Social & Economic Wellbeing",
        "identity" = "Social & Economic Wellbeing",
        "environment" = "Urban Mobility & Environment",
        "urban" = "Urban Mobility & Environment",
        "mobility" = "Urban Mobility & Environment", 
        "transportation" = "Urban Mobility & Environment",
        "government" = "Governance & Civic Engagement",
        "inequality" = "Governance & Civic Engagement",
        "accountability" = "Governance & Civic Engagement",
        "representation" = "Governance & Civic Engagement",
        "expectations" = "Governance & Civic Engagement",
        "trust" = "Governance & Civic Engagement",
        "infrastructure" = "Public Services",
        "public_services" = "Public Services",
        "education" = "Public Services",
        "healthcare" = "Public Services",
        "housing" = "Public Services",
        "participation" = "Community Participation",
        "civic" = "Community Participation",
        "community" = "Community Participation"
      )
      
      main_theme <- section_mapping[[section]]
      
      if (!is.null(main_theme)) {
        # Use thematic classification to get questions
        questions <- get_questions_by_theme(main_theme)
        # Filter by survey type
        questions <- questions[questions$survey_id == paste0(survey_type, "_2024"), ]
        return(questions)
      }
      
      return(data.frame())
    },
    
    #' @description
    #' Preload data for multiple sections and years
    #' @param years Vector of years to preload
    #' @param sections Vector of sections to preload
    #' @param survey_types Vector of survey types to preload
    preload_data = function(years = c(2023, 2024), 
                           sections = c("wellness", "economic", "cultural", "identity", "environment",
                                      "urban", "mobility", "transportation", "government", "inequality",
                                      "accountability", "representation", "expectations", "trust",
                                      "infrastructure", "public_services", "education", "healthcare",
                                      "housing", "participation", "civic", "community"),
                           survey_types = c("PER", "PAR")) {
      
      message("Starting data preloading...")
      
      # First, preload survey data
      for (year in years) {
        for (survey_type in survey_types) {
          survey_id <- paste0(survey_type, "_", year)
          tryCatch({
            self$get_survey_data(survey_id)
          }, error = function(e) {
            warning(paste("Failed to preload survey:", survey_id, "-", e$message))
          })
        }
      }
      
      # Then preload section data
      for (year in years) {
        for (section in sections) {
          for (survey_type in survey_types) {
            tryCatch({
              self$get_section_data(section, year, survey_type)
            }, error = function(e) {
              warning(paste("Failed to preload section:", section, year, survey_type, "-", e$message))
            })
          }
        }
      }
      
      message("Data preloading completed")
    },
    
    #' @description
    #' Load pre-saved plots if they exist
    #' @param section Section name
    #' @param year Survey year
    #' @return List of plots or NULL
    load_saved_plots = function(section, year) {
      plot_path <- paste0("data/plots/", section, "_", year, ".rds")
      
      if (file.exists(plot_path)) {
        tryCatch({
          plots <- readRDS(plot_path)
          message(paste("Loaded saved plots for", section, year))
          return(plots)
        }, error = function(e) {
          warning(paste("Failed to load saved plots:", e$message))
          return(NULL)
        })
      }
      
      return(NULL)
    },
    
    #' @description
    #' Save plots to RDS file
    #' @param plots List of plots
    #' @param section Section name
    #' @param year Survey year
    save_plots = function(plots, section, year) {
      # Create plots directory if it doesn't exist
      plots_dir <- "data/plots"
      if (!dir.exists(plots_dir)) {
        dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
      }
      
      plot_path <- paste0(plots_dir, "/", section, "_", year, ".rds")
      
      tryCatch({
        saveRDS(plots, plot_path)
        message(paste("Saved plots for", section, year))
      }, error = function(e) {
        warning(paste("Failed to save plots:", e$message))
      })
    },
    
    #' @description
    #' Get geographic data with caching
    #' @return SF object with geographic data
    get_geo_data = function() {
      if (is.null(private$geo_data_cache)) {
        tryCatch({
          private$geo_data_cache <- sf::st_read('data/geo/Jrz_Map.geojson', quiet = TRUE)
          message("Geographic data loaded and cached")
        }, error = function(e) {
          warning(paste("Failed to load geographic data:", e$message))
          private$geo_data_cache <- NULL
        })
      }
      return(private$geo_data_cache)
    },
    
    #' @description
    #' Clear all caches
    clear_cache = function() {
      self$cache <- list()
      self$plot_cache <- list()
      self$survey_cache <- list()
      private$geo_data_cache <- NULL
      
      # Clear memoized function caches
      if (!is.null(private$memoised_load_survey_data)) {
        memoise::forget(private$memoised_load_survey_data)
      }
      if (!is.null(private$memoised_prepare_binary_data)) {
        memoise::forget(private$memoised_prepare_binary_data)
      }
      if (!is.null(private$memoised_prepare_categorical_data)) {
        memoise::forget(private$memoised_prepare_categorical_data)
      }
      if (!is.null(private$memoised_prepare_interval_data)) {
        memoise::forget(private$memoised_prepare_interval_data)
      }
      if (!is.null(private$memoised_prepare_ordinal_data)) {
        memoise::forget(private$memoised_prepare_ordinal_data)
      }
      if (!is.null(private$memoised_prepare_nominal_data)) {
        memoise::forget(private$memoised_prepare_nominal_data)
      }
      if (!is.null(private$memoised_prepare_razon_data)) {
        memoise::forget(private$memoised_prepare_razon_data)
      }
      
      message("All caches cleared")
    },
    
    #' @description
    #' Get cache statistics
    #' @return List with cache information
    get_cache_stats = function() {
      return(list(
        survey_cache_items = length(self$survey_cache),
        section_cache_items = length(self$cache),
        plot_cache_items = length(self$plot_cache),
        geo_data_loaded = !is.null(private$geo_data_cache)
      ))
    }
  ),
  
  private = list(
    #' @field memoised_load_survey_data Memoised function for loading survey data
    memoised_load_survey_data = NULL,
    
    #' @field memoised_prepare_binary_data Memoised function for preparing binary data
    memoised_prepare_binary_data = NULL,
    
    #' @field memoised_prepare_categorical_data Memoised function for preparing categorical data
    memoised_prepare_categorical_data = NULL,
    
    #' @field memoised_prepare_interval_data Memoised function for preparing interval data
    memoised_prepare_interval_data = NULL,
    
    #' @field memoised_prepare_ordinal_data Memoised function for preparing ordinal data
    memoised_prepare_ordinal_data = NULL,
    
    #' @field memoised_prepare_nominal_data Memoised function for preparing nominal data
    memoised_prepare_nominal_data = NULL,
    
    #' @field memoised_prepare_razon_data Memoised function for preparing razon data
    memoised_prepare_razon_data = NULL,
    
    #' @field geo_data_cache Cached geographic data
    geo_data_cache = NULL
  )
)