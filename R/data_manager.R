
#' @export
DataManager <- R6::R6Class("DataManager",
  public = list(
    #' @field session_cache A list to store session-specific cached data
    session_cache = list(),
    
    #' @description
    #' Create a new DataManager object and initialize global cache if needed
    #' @return A new DataManager object
    initialize = function() {
      # Initialize the global cache if it doesn't exist yet
      if (!exists("GLOBAL_CACHE", envir = .GlobalEnv)) {
        assign("GLOBAL_CACHE", new.env(), envir = .GlobalEnv)
        # Add a simple stats tracker
        assign("CACHE_STATS", list(
          hits = 0,
          misses = 0,
          created = Sys.time()
        ), envir = .GlobalEnv$GLOBAL_CACHE)
      }
      
      message("DataManager initialized with shared cache")
    },
    
    #' @description
    #' Load plot data from RDS files based on section and year using shared cache
    #' @param section Section name (e.g., "wellness", "economic", "cultural", "identity", "environment")
    #' @param year Survey year (numeric)
    #' @return A list of plots
    get_plots = function(section, year) {
      key <- paste0("plots_", section, "_", year)
      
      # Try to get from global cache first
      if (exists(key, envir = .GlobalEnv$GLOBAL_CACHE)) {
        # Cache hit
        .GlobalEnv$GLOBAL_CACHE$CACHE_STATS$hits <- .GlobalEnv$GLOBAL_CACHE$CACHE_STATS$hits + 1
        return(get(key, envir = .GlobalEnv$GLOBAL_CACHE))
      }
      
      # Cache miss
      .GlobalEnv$GLOBAL_CACHE$CACHE_STATS$misses <- .GlobalEnv$GLOBAL_CACHE$CACHE_STATS$misses + 1
      
      # If not in cache, load it
      message(paste("Loading plots for", section, year, "from RDS"))
      path <- paste0("data/plots/", section, "_", year, ".rds")
      
      result <- tryCatch({
        if (file.exists(path)) {
          readRDS(path)
        } else {
          warning(paste("Plot file not found:", path))
          list()
        }
      }, error = function(e) {
        warning(paste("Failed to load plots for", section, year, ":", e$message))
        return(list())
      })
      
      # Store in global cache
      assign(key, result, envir = .GlobalEnv$GLOBAL_CACHE)
      
      return(result)
    },
    
    #' @description
    #' Load map data from RDS files based on section and year using shared cache
    #' @param section Section name (e.g., "wellness", "economic", "cultural", "identity", "environment")
    #' @param year Survey year (numeric)
    #' @return A list of maps
    get_maps = function(section, year) {
      key <- paste0("maps_", section, "_", year)
      
      # Try to get from global cache first
      if (exists(key, envir = .GlobalEnv$GLOBAL_CACHE)) {
        .GlobalEnv$GLOBAL_CACHE$CACHE_STATS$hits <- .GlobalEnv$GLOBAL_CACHE$CACHE_STATS$hits + 1
        return(get(key, envir = .GlobalEnv$GLOBAL_CACHE))
      }
      
      # Cache miss
      .GlobalEnv$GLOBAL_CACHE$CACHE_STATS$misses <- .GlobalEnv$GLOBAL_CACHE$CACHE_STATS$misses + 1
      
      message(paste("Loading maps for", section, year, "from RDS"))
      path <- paste0("data/plots/map_", section, "_", year, ".rds")
      
      result <- tryCatch({
        if (file.exists(path)) {
          readRDS(path)
        } else {
          warning(paste("Map file not found:", path))
          list()
        }
      }, error = function(e) {
        warning(paste("Failed to load maps for", section, year, ":", e$message))
        return(list())
      })
      
      # Store in global cache
      assign(key, result, envir = .GlobalEnv$GLOBAL_CACHE)
      
      return(result)
    },
    
    #' @description
    #' Get static percentages for a section and year with shared caching
    #' @param section Section name (e.g., "cultural", "wellness", "economic")
    #' @param year Survey year (numeric)
    #' @return A list of percentages
    get_percentages = function(section, year) {
      key <- paste0("pct_", section, "_", year)
      
      # Try to get from global cache
      if (exists(key, envir = .GlobalEnv$GLOBAL_CACHE)) {
        .GlobalEnv$GLOBAL_CACHE$CACHE_STATS$hits <- .GlobalEnv$GLOBAL_CACHE$CACHE_STATS$hits + 1
        return(get(key, envir = .GlobalEnv$GLOBAL_CACHE))
      }
      
      # Cache miss
      .GlobalEnv$GLOBAL_CACHE$CACHE_STATS$misses <- .GlobalEnv$GLOBAL_CACHE$CACHE_STATS$misses + 1
      
      # Load from RDS file if available, otherwise use hardcoded values
      rds_path <- paste0("data/percentages/", section, "_", year, ".rds")
      
      result <- tryCatch({
        if (file.exists(rds_path)) {
          readRDS(rds_path)
        } else {
          # Fallback to hardcoded percentages
          private$get_hardcoded_percentages(section, year)
        }
      }, error = function(e) {
        warning(paste("Failed to load percentages for", section, year, ":", e$message))
        private$get_hardcoded_percentages(section, year)
      })
      
      # Store in global cache
      assign(key, result, envir = .GlobalEnv$GLOBAL_CACHE)
      
      return(result)
    },
    
    #' @description
    #' Get the path to a map PNG file
    #' @param map_name Name of the map file (without extension)
    #' @param year Survey year
    #' @return File path to the PNG
    get_map_path = function(map_name, year) {
      paste0("data/maps/", map_name, "_", year, ".png")
    },
    
    #' @description
    #' Preload common data to improve responsiveness with shared caching
    #' @param years Vector of years to preload
    #' @param sections Vector of sections to preload
    preload_data = function(years = c(2023, 2024), 
                           sections = c("wellness", "economic", "cultural", "identity", "environment")) {
      start_time <- Sys.time()
      
      message("Starting data preloading...")
      
      total_items <- 0
      loaded_items <- 0
      
      for (year in years) {
        for (section in sections) {
          total_items <- total_items + 1
          
          # Trigger loading by calling the methods
          plots <- self$get_plots(section, year)
          if (length(plots) > 0) loaded_items <- loaded_items + 1
          
          # Load maps for sections that have them
          if (section %in% c("wellness", "economic", "identity", "environment")) {
            maps <- self$get_maps(section, year)
            if (length(maps) > 0) loaded_items <- loaded_items + 1
            total_items <- total_items + 1
          }
          
          # Load percentages for sections that have them
          if (section %in% c("cultural", "wellness", "economic")) {
            percentages <- self$get_percentages(section, year)
            if (length(percentages) > 0) loaded_items <- loaded_items + 1
            total_items <- total_items + 1
          }
        }
      }
      
      end_time <- Sys.time()
      duration <- difftime(end_time, start_time, units = "secs")
      message(sprintf("Data preloading completed in %.2f seconds", as.numeric(duration)))
      message(sprintf("Loaded %d/%d items successfully", loaded_items, total_items))
      
      # Return the object for method chaining
      return(invisible(self))
    },
    
    #' @description
    #' Get cache statistics
    #' @return List with cache stats
    get_cache_stats = function() {
      if (exists("GLOBAL_CACHE", envir = .GlobalEnv) && 
          exists("CACHE_STATS", envir = .GlobalEnv$GLOBAL_CACHE)) {
        stats <- .GlobalEnv$GLOBAL_CACHE$CACHE_STATS
        total <- stats$hits + stats$misses
        hit_rate <- if(total > 0) stats$hits/total*100 else 0
        
        return(list(
          hits = stats$hits,
          misses = stats$misses,
          hit_rate = round(hit_rate, 2),
          total_requests = total,
          cache_size = length(ls(.GlobalEnv$GLOBAL_CACHE)) - 1, # -1 for CACHE_STATS
          created = stats$created
        ))
      }
      return(list())
    },
    
    #' @description
    #' Clear the cache (both session and global)
    #' @param global Boolean, whether to clear global cache
    clear_cache = function(global = FALSE) {
      self$session_cache <- list()
      
      if (global && exists("GLOBAL_CACHE", envir = .GlobalEnv)) {
        # Save stats before clearing
        stats <- .GlobalEnv$GLOBAL_CACHE$CACHE_STATS
        
        # Clear everything but stats
        rm(list = setdiff(ls(.GlobalEnv$GLOBAL_CACHE), "CACHE_STATS"), envir = .GlobalEnv$GLOBAL_CACHE)
        
        # Reset stats
        .GlobalEnv$GLOBAL_CACHE$CACHE_STATS <- list(
          hits = 0,
          misses = 0,
          created = Sys.time()
        )
        
        message("Global cache cleared")
      } else {
        message("Session cache cleared")
      }
      
      return(invisible(self))
    }
  ),
  
  private = list(
    #' @description
    #' Get hardcoded percentages as fallback
    #' @param section Section name
    #' @param year Survey year
    #' @return List of percentages
    get_hardcoded_percentages = function(section, year) {
      if (section == "cultural") {
        if (year == 2024) {
          return(list(
            home_activities_pct = "82.7%", 
            exercise_activities_pct = "57.9%",
            nature_activities_pct = "62.6%"
          ))
        } else if (year == 2023) {
          return(list(
            home_activities_pct = "95.2%", 
            exercise_activities_pct = "25.9%",
            nature_activities_pct = "74.8%"
          ))
        }
      } else if (section == "wellness") {
        if (year == 2024) {
          return(list(
            migration_intention_pct = "45.2%"
          ))
        } else if (year == 2023) {
          return(list(
            migration_intention_pct = "48.1%"
          ))
        }
      } else if (section == "identity") {
        if (year == 2024) {
          return(list(
            neighborhood_connection_pct = "67.3%",
            neighbors_connection_pct = "58.9%",
            city_pride_pct = "74.1%"
          ))
        } else if (year == 2023) {
          return(list(
            neighborhood_connection_pct = "65.8%",
            neighbors_connection_pct = "56.2%",
            city_pride_pct = "71.9%"
          ))
        }
      }
      
      return(list())
    }
  )
)