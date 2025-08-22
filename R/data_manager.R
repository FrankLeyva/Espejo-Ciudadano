# R/data_manager.R - FIXED VERSION

DataManager <- R6::R6Class("DataManager",
  public = list(
    session_cache = list(),
    
    initialize = function(max_cache_mb = 200) {
      # Initialize global cache environment
      if (!exists("GLOBAL_CACHE", envir = .GlobalEnv)) {
        assign("GLOBAL_CACHE", new.env(), envir = .GlobalEnv)
        assign("CACHE_STATS", list(
          hits = 0,
          misses = 0,
          created = Sys.time(),
          max_size_mb = max_cache_mb
        ), envir = .GlobalEnv$GLOBAL_CACHE)
      }
      
      message(sprintf("DataManager initialized with %d MB cache limit", max_cache_mb))
    },
    
    # Enhanced get_plots with cache management
    get_plots = function(section, year) {
      key <- paste0("plots_", section, "_", year)
      
      # Try cache first
      if (exists(key, envir = .GlobalEnv$GLOBAL_CACHE)) {
        .GlobalEnv$GLOBAL_CACHE$CACHE_STATS$hits <- .GlobalEnv$GLOBAL_CACHE$CACHE_STATS$hits + 1
        return(get(key, envir = .GlobalEnv$GLOBAL_CACHE))
      }
      
      # Cache miss
      .GlobalEnv$GLOBAL_CACHE$CACHE_STATS$misses <- .GlobalEnv$GLOBAL_CACHE$CACHE_STATS$misses + 1
      
      # Load data
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
      
      # Store in cache and manage size
      assign(key, result, envir = .GlobalEnv$GLOBAL_CACHE)
      private$manage_cache_size()
      
      return(result)
    },
    
    # Enhanced get_maps with cache management  
    get_maps = function(section, year) {
      key <- paste0("maps_", section, "_", year)
      
      if (exists(key, envir = .GlobalEnv$GLOBAL_CACHE)) {
        .GlobalEnv$GLOBAL_CACHE$CACHE_STATS$hits <- .GlobalEnv$GLOBAL_CACHE$CACHE_STATS$hits + 1
        return(get(key, envir = .GlobalEnv$GLOBAL_CACHE))
      }
      
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
      
      assign(key, result, envir = .GlobalEnv$GLOBAL_CACHE)
      private$manage_cache_size()
      
      return(result)
    },
    
    # Enhanced get_percentages with cache management
    get_percentages = function(section, year) {
      key <- paste0("pct_", section, "_", year)
      
      if (exists(key, envir = .GlobalEnv$GLOBAL_CACHE)) {
        .GlobalEnv$GLOBAL_CACHE$CACHE_STATS$hits <- .GlobalEnv$GLOBAL_CACHE$CACHE_STATS$hits + 1
        return(get(key, envir = .GlobalEnv$GLOBAL_CACHE))
      }
      
      .GlobalEnv$GLOBAL_CACHE$CACHE_STATS$misses <- .GlobalEnv$GLOBAL_CACHE$CACHE_STATS$misses + 1
      
      rds_path <- paste0("data/percentages/", section, "_", year, ".rds")
      
      result <- tryCatch({
        if (file.exists(rds_path)) {
          readRDS(rds_path)
        } else {
          private$get_hardcoded_percentages(section, year)
        }
      }, error = function(e) {
        warning(paste("Failed to load percentages for", section, year, ":", e$message))
        private$get_hardcoded_percentages(section, year)
      })
      
      assign(key, result, envir = .GlobalEnv$GLOBAL_CACHE)
      private$manage_cache_size()
      
      return(result)
    },
    
    # Smart preloading - only load related sections
    smart_preload = function(current_section, year) {
      # Define section groups that users typically navigate between
      section_groups <- list(
        "wellness" = c("wellness", "economic", "cultural", "identity", "environment"),
        "economic" = c("wellness", "economic", "cultural", "identity"),
        "cultural" = c("wellness", "economic", "cultural", "identity"),
        "identity" = c("wellness", "economic", "cultural", "identity", "environment"),
        "environment" = c("wellness", "environment", "identity"),
        
        "government" = c("government", "inequality", "accountability", "representation", "trust"),
        "inequality" = c("government", "inequality", "accountability"),
        "accountability" = c("government", "inequality", "accountability", "representation"),
        "representation" = c("government", "representation", "trust"),
        "trust" = c("government", "representation", "trust"),
        
        "infrastructure" = c("infrastructure", "public_services", "education", "healthcare", "housing"),
        "public_services" = c("infrastructure", "public_services", "housing"),
        "education" = c("infrastructure", "education", "healthcare"),
        "healthcare" = c("infrastructure", "education", "healthcare"),
        "housing" = c("infrastructure", "public_services", "housing"),
        
        "participation" = c("participation", "civic", "community"),
        "civic" = c("participation", "civic", "community"),
        "community" = c("participation", "civic", "community"),
        
        "urban" = c("urban", "mobility", "transportation"),
        "mobility" = c("urban", "mobility", "transportation"),
        "transportation" = c("urban", "mobility", "transportation")
      )
      
      # Get sections to preload
      sections_to_load <- section_groups[[current_section]]
      if(is.null(sections_to_load)) {
        sections_to_load <- c(current_section)
      }
      
      message(sprintf("Smart preloading for %s: %s", current_section, paste(sections_to_load, collapse = ", ")))
      
      # Load data for related sections
      for(section in sections_to_load) {
        tryCatch({
          # Always load plots and percentages
          self$get_plots(section, year)
          self$get_percentages(section, year)
          
          # Only load maps for sections that have them (based on your mapping table)
          map_sections <- c("identity", "environment", "economic", "cultural", "infrastructure", 
                          "expectations", "government", "inequality", "representation", 
                          "civic", "participation", "transportation", "urban")
          
          if(section %in% map_sections) {
            self$get_maps(section, year)
          }
        }, error = function(e) {
          warning(sprintf("Failed to preload %s: %s", section, e$message))
        })
      }
      
      return(invisible(self))
    },
    
    # Enhanced cache statistics
    get_cache_stats = function() {
      if (exists("GLOBAL_CACHE", envir = .GlobalEnv) && 
          exists("CACHE_STATS", envir = .GlobalEnv$GLOBAL_CACHE)) {
        stats <- .GlobalEnv$GLOBAL_CACHE$CACHE_STATS
        total <- stats$hits + stats$misses
        hit_rate <- if(total > 0) stats$hits/total*100 else 0
        
        # Calculate current cache size
        cache_size_mb <- private$get_cache_size_mb()
        
        return(list(
          hits = stats$hits,
          misses = stats$misses,
          hit_rate = round(hit_rate, 2),
          total_requests = total,
          cache_size_mb = cache_size_mb,
          max_cache_mb = stats$max_size_mb,
          cache_objects = length(ls(.GlobalEnv$GLOBAL_CACHE)) - 1,
          created = stats$created
        ))
      }
      return(list(cache_size_mb = 0, hit_rate = 0, cache_objects = 0))
    },
    
    # Add method for map path resolution (needed by cultural_server.R)
    get_map_path = function(map_name, year) {
      # Return the path to the PNG map file
      map_path <- paste0("data/maps/", map_name, "_", year, ".png")
      return(map_path)
    }
  ),
  
  private = list(
    # Calculate current cache size in MB
    get_cache_size_mb = function() {
      if (!exists("GLOBAL_CACHE", envir = .GlobalEnv)) {
        return(0)
      }
      
      cache_env <- .GlobalEnv$GLOBAL_CACHE
      cache_objects <- ls(cache_env, all.names = TRUE)
      cache_objects <- cache_objects[cache_objects != "CACHE_STATS"]
      
      if(length(cache_objects) > 0) {
        total_size_bytes <- sum(sapply(cache_objects, function(obj) {
          as.numeric(object.size(get(obj, envir = cache_env)))
        }))
        return(round(total_size_bytes / 1024^2, 2))
      }
      return(0)
    },
    
    # Manage cache size using LRU strategy
    manage_cache_size = function() {
      if (!exists("GLOBAL_CACHE", envir = .GlobalEnv)) return()
      
      cache_env <- .GlobalEnv$GLOBAL_CACHE
      if (!exists("CACHE_STATS", envir = cache_env)) return()
      
      stats <- cache_env$CACHE_STATS
      max_size <- stats$max_size_mb
      
      current_size <- private$get_cache_size_mb()
      
      # Only manage cache if it exceeds the limit
      if(current_size > max_size) {
        cache_objects <- ls(cache_env, all.names = TRUE)
        cache_objects <- cache_objects[cache_objects != "CACHE_STATS"]
        
        if(length(cache_objects) > 0) {
          # Remove oldest 30% of objects (simple LRU approximation)
          num_to_remove <- ceiling(length(cache_objects) * 0.3)
          objects_to_remove <- cache_objects[1:num_to_remove]
          
          rm(list = objects_to_remove, envir = cache_env)
          
          new_size <- private$get_cache_size_mb()
          message(sprintf("Cache management: Reduced from %.1f MB to %.1f MB (removed %d objects)", 
                         current_size, new_size, num_to_remove))
        }
      }
    },
    
    # Get hardcoded percentages as fallback
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

# CRITICAL: Explicitly assign to global environment
assign("DataManager", DataManager, envir = .GlobalEnv)

message("DataManager class loaded and assigned to global environment")