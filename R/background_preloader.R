# R/background_preloader.R - FIXED VERSION WITHOUT REACTIVE CONTEXT ISSUES

library(future)
library(promises)

BackgroundPreloader <- R6::R6Class("BackgroundPreloader",
  public = list(
    status = "idle",
    progress = list(
      total_tasks = 0,
      completed_tasks = 0,
      current_task = "Not started",
      start_time = NULL,
      data_tasks = 0,
      ui_tasks = 0,
      server_tasks = 0,
      completed_data = 0,
      completed_ui = 0,
      completed_server = 0
    ),
    
    initialize = function(data_manager, max_concurrent = 1, session = NULL) {
      private$data_manager <- data_manager
      private$max_concurrent <- max_concurrent
      private$session <- session  # Store session reference
      
      # Define sections with their module information
      private$priority_sections <- list(
        # Priority 0: Explorer (highest priority)
        list(section = "explorer", priority = 0, folder = "extras", has_server = TRUE, has_ui = TRUE),
        
        # Priority 1: Main sections (load first)
        list(section = "wellness", priority = 1, folder = "wellness", has_server = TRUE, has_ui = TRUE),
        list(section = "government", priority = 1, folder = "government", has_server = TRUE, has_ui = TRUE),
        list(section = "infrastructure", priority = 1, folder = "infrastructure", has_server = TRUE, has_ui = TRUE),
        list(section = "urban", priority = 1, folder = "urban", has_server = TRUE, has_ui = TRUE),
        list(section = "participation", priority = 1, folder = "participation", has_server = TRUE, has_ui = TRUE),
        
        # Priority 2: Subsections
        list(section = "economic", priority = 2, folder = "wellness", module_name = "economy", has_server = TRUE, has_ui = TRUE),
        list(section = "cultural", priority = 2, folder = "wellness", has_server = TRUE, has_ui = TRUE),
        list(section = "identity", priority = 2, folder = "wellness", has_server = TRUE, has_ui = TRUE),
        list(section = "environment", priority = 2, folder = "wellness", has_server = TRUE, has_ui = TRUE),
        list(section = "education", priority = 2, folder = "wellness", has_server = TRUE, has_ui = TRUE),
        list(section = "healthcare", priority = 2, folder = "wellness", has_server = TRUE, has_ui = TRUE),
        list(section = "housing", priority = 2, folder = "infrastructure", has_server = TRUE, has_ui = TRUE),
        
        list(section = "inequality", priority = 2, folder = "government", has_server = TRUE, has_ui = TRUE),
        list(section = "accountability", priority = 2, folder = "government", has_server = TRUE, has_ui = TRUE),
        list(section = "representation", priority = 2, folder = "government", has_server = TRUE, has_ui = TRUE),
        list(section = "expectations", priority = 2, folder = "government", has_server = TRUE, has_ui = TRUE),
        list(section = "trust", priority = 2, folder = "government", has_server = TRUE, has_ui = TRUE),
        
        list(section = "public_services", priority = 2, folder = "infrastructure", has_server = TRUE, has_ui = TRUE),
        list(section = "equipment", priority = 2, folder = "infrastructure", has_server = TRUE, has_ui = TRUE),
        
        list(section = "mobility", priority = 2, folder = "urban", has_server = TRUE, has_ui = TRUE),
        list(section = "transportation", priority = 2, folder = "urban", has_server = TRUE, has_ui = TRUE),
        
        list(section = "civic", priority = 2, folder = "participation", has_server = TRUE, has_ui = TRUE),
        list(section = "community", priority = 2, folder = "participation", has_server = TRUE, has_ui = TRUE),
        
        # Priority 3: Extra sections
        list(section = "reports", priority = 3, folder = "extras", has_server = TRUE, has_ui = TRUE),
        list(section = "methodology", priority = 3, folder = "extras", has_server = TRUE, has_ui = TRUE),
        list(section = "about", priority = 3, folder = "extras", has_server = TRUE, has_ui = TRUE),
        list(section = "dashboard_map", priority = 3, folder = "extras", has_server = TRUE, has_ui = TRUE)
      )
      
      # Calculate total tasks
      data_sections <- length(private$priority_sections) - 4  # Exclude some extras from data loading
      self$progress$data_tasks <- data_sections * 2 + 2  # 2 years + explorer surveys
      self$progress$ui_tasks <- length(private$priority_sections)
      self$progress$server_tasks <- length(private$priority_sections)
      self$progress$total_tasks <- self$progress$data_tasks + self$progress$ui_tasks + self$progress$server_tasks
      
      # CRITICAL: Initialize progress tracking in global environment (not reactive)
      if (!exists("PRELOADER_PROGRESS", envir = .GlobalEnv)) {
        assign("PRELOADER_PROGRESS", self$progress, envir = .GlobalEnv)
      }
      
      message(sprintf("BackgroundPreloader initialized (non-reactive): %d total tasks (%d data, %d UI, %d server)", 
                     self$progress$total_tasks, self$progress$data_tasks, 
                     self$progress$ui_tasks, self$progress$server_tasks))
    },
    
    # Start the background preloading process
    start_preloading = function() {
      if (self$status != "idle") {
        message("Preloading already in progress")
        return(invisible(self))
      }
      
      message("Starting non-interfering background preloading...")
      self$status <- "running"
      self$progress$start_time <- Sys.time()
      self$progress$current_task <- "Starting background tasks"
      
      # Update global progress (NOT reactive)
      assign("PRELOADER_PROGRESS", self$progress, envir = .GlobalEnv)
      
      # PRIORITY ORDER (non-interfering):
      # 1. Explorer UI/Server (immediate - users likely to click)
      private$priority_load_explorer()
      
      # 2. Main section UI modules (quick loading)
      private$schedule_ui_loading()
      
      # 3. Explorer data (can take time)
      private$schedule_explorer_data_loading()
      
      # 4. Section data loading (background)
      private$schedule_section_loading()
      
      # 5. Server modules (after UI is ready)
      private$schedule_server_loading()
      
      return(invisible(self))
    },
    
    # Stop preloading
    stop_preloading = function() {
      self$status <- "stopping"
      message("Stopping background preloading...")
      return(invisible(self))
    },
    
    # Get current status with detailed breakdown
    get_status = function() {
      list(
        status = self$status,
        progress = self$progress,
        cache_stats = if(!is.null(private$data_manager)) private$data_manager$get_cache_stats() else NULL,
        breakdown = list(
          data_progress = sprintf("%d/%d", self$progress$completed_data, self$progress$data_tasks),
          ui_progress = sprintf("%d/%d", self$progress$completed_ui, self$progress$ui_tasks),
          server_progress = sprintf("%d/%d", self$progress$completed_server, self$progress$server_tasks)
        )
      )
    },
    
    # SILENT priority load (immediate, blocking) - Enhanced for non-interference
    priority_load = function(section, year = 2024) {
      message(sprintf("Silent priority loading: %s %s", section, year))
      
      # Load UI and Server first (fast) - SILENTLY
      private$load_ui_module_silent(section)
      private$load_server_module_silent(section)
      
      # Then load data SILENTLY
      if (section == "explorer") {
        private$load_explorer_data_silent(year)
      } else {
        private$silent_data_preload(section, year)
      }
      
      return(invisible(self))
    }
  ),
  
  private = list(
    data_manager = NULL,
    max_concurrent = 1,
    session = NULL,  # Store session reference
    priority_sections = list(),
    loaded_ui_modules = character(0),
    loaded_server_modules = character(0),
    
    # NON-REACTIVE progress update that schedules UI updates safely
    update_progress_safe = function() {
      # Update global progress (safe from any context)
      assign("PRELOADER_PROGRESS", self$progress, envir = .GlobalEnv)
      
      # Schedule UI update in main thread ONLY if session exists
      if (!is.null(private$session)) {
        later::later(function() {
          tryCatch({
            # Only send update if not during transition
            if (!exists("transitionInProgress", envir = .GlobalEnv) || 
                !get("transitionInProgress", envir = .GlobalEnv)) {
              
              private$session$sendCustomMessage("preloader-status", list(
                status = self$status,
                progress = self$progress
              ))
            }
          }, error = function(e) {
            # Silently ignore session errors
            message("Session unavailable for progress update")
          })
        }, delay = 0.1)
      }
    },
    
    # SILENT loading methods to prevent interference
    
    # Silent data preload
    silent_data_preload = function(section, year) {
      tryCatch({
        if (!is.null(private$data_manager)) {
          private$data_manager$smart_preload(section, year)
        }
      }, error = function(e) {
        message(sprintf("Silent data preload failed for %s %s: %s", section, year, e$message))
      })
    },
    
    # Silent explorer data loading
    load_explorer_data_silent = function(year) {
      tryCatch({
        message(sprintf("Silent loading explorer data for %s...", year))
        
        # Load without any progress indicators
        per_data <- load_survey_data(paste0("PER_", year))
        par_data <- load_survey_data(paste0("PAR_", year))
        
        if (!is.null(per_data)) {
          assign(paste0("survey_PER_", year), per_data, envir = .GlobalEnv$GLOBAL_CACHE)
        }
        if (!is.null(par_data)) {
          assign(paste0("survey_PAR_", year), par_data, envir = .GlobalEnv$GLOBAL_CACHE)
        }
        
        message(sprintf("Silent explorer data loaded for %s", year))
        return(TRUE)
      }, error = function(e) {
        message(sprintf("Silent explorer loading failed for %s: %s", year, e$message))
        return(FALSE)
      })
    },
    
    # PRIORITY: Load explorer UI/Server immediately (users likely to click)
    priority_load_explorer = function() {
      message("Priority: Loading explorer UI and server modules...")
      
      # Load explorer modules immediately
      private$load_ui_module_silent("explorer")
      private$load_server_module_silent("explorer")
      
      # Schedule basic visualization modules if not loaded
      basic_modules <- c("binary", "categorical", "interval", "ordinal", "nominal", "razon", "special")
      for (module in basic_modules) {
        if (!exists(paste0(module, "UI"), envir = .GlobalEnv)) {
          tryCatch({
            source(paste0("R/", module, "_module.R"))
            message(sprintf("Loaded basic module: %s", module))
          }, error = function(e) {
            message(sprintf("Failed to load basic module %s: %s", module, e$message))
          })
        }
      }
    },
    
    # Schedule UI loading with priority order
    schedule_ui_loading = function() {
      # Sort sections by priority (0 = highest priority)
      sorted_sections <- private$priority_sections[order(sapply(private$priority_sections, function(x) x$priority))]
      
      delay_counter <- 0
      for (section_info in sorted_sections) {
        if (!section_info$has_ui) next
        
        section <- section_info$section
        priority <- section_info$priority
        
        # Skip explorer (already loaded)
        if (section == "explorer") next
        
        # Stagger by priority: priority 1 = 2-4 seconds, priority 2 = 6-16 seconds, etc.
        delay_time <- priority * 3 + delay_counter * 0.5
        delay_counter <- delay_counter + 1
        
        private$schedule_single_ui_load(section, delay_time)
      }
    },
    
    # Schedule server loading (after UI modules)
    schedule_server_loading = function() {
      sorted_sections <- private$priority_sections[order(sapply(private$priority_sections, function(x) x$priority))]
      
      delay_counter <- 0
      for (section_info in sorted_sections) {
        if (!section_info$has_server) next
        
        section <- section_info$section
        priority <- section_info$priority
        
        # Skip explorer (already loaded)
        if (section == "explorer") next
        
        # Start server loading after UI loading begins (add 15 seconds base delay)
        delay_time <- 15 + priority * 3 + delay_counter * 0.5
        delay_counter <- delay_counter + 1
        
        private$schedule_single_server_load(section, delay_time)
      }
    },
    
    # Load UI module silently (blocking)
    load_ui_module_silent = function(section) {
      section_info <- private$get_section_info(section)
      if (is.null(section_info) || !section_info$has_ui) return(FALSE)
      
      module_name <- if(!is.null(section_info$module_name)) section_info$module_name else section
      folder <- section_info$folder
      ui_function_name <- paste0(module_name, "UI")
      
      # Skip if already loaded
      if (ui_function_name %in% private$loaded_ui_modules) {
        return(TRUE)
      }
      
      # Check if function already exists
      if (exists(ui_function_name, envir = .GlobalEnv)) {
        private$loaded_ui_modules <- c(private$loaded_ui_modules, ui_function_name)
        return(TRUE)
      }
      
      ui_path <- file.path("R", folder, paste0(module_name, "_ui.R"))
      
      tryCatch({
        if (file.exists(ui_path)) {
          source(ui_path)
          private$loaded_ui_modules <- c(private$loaded_ui_modules, ui_function_name)
          message(sprintf("Silent loaded UI module: %s", ui_function_name))
          return(TRUE)
        } else {
          message(sprintf("UI file not found: %s", ui_path))
          return(FALSE)
        }
      }, error = function(e) {
        message(sprintf("Failed to load UI module %s: %s", ui_function_name, e$message))
        return(FALSE)
      })
    },
    
    # Load server module silently (blocking)
    load_server_module_silent = function(section) {
      section_info <- private$get_section_info(section)
      if (is.null(section_info) || !section_info$has_server) return(FALSE)
      
      module_name <- if(!is.null(section_info$module_name)) section_info$module_name else section
      folder <- section_info$folder
      server_function_name <- paste0(module_name, "Server")
      
      # Skip if already loaded
      if (server_function_name %in% private$loaded_server_modules) {
        return(TRUE)
      }
      
      # Check if function already exists
      if (exists(server_function_name, envir = .GlobalEnv)) {
        private$loaded_server_modules <- c(private$loaded_server_modules, server_function_name)
        return(TRUE)
      }
      
      server_path <- file.path("R", folder, paste0(module_name, "_server.R"))
      
      tryCatch({
        if (file.exists(server_path)) {
          source(server_path)
          private$loaded_server_modules <- c(private$loaded_server_modules, server_function_name)
          message(sprintf("Silent loaded server module: %s", server_function_name))
          return(TRUE)
        } else {
          message(sprintf("Server file not found: %s", server_path))
          return(FALSE)
        }
      }, error = function(e) {
        message(sprintf("Failed to load server module %s: %s", server_function_name, e$message))
        return(FALSE)
      })
    },
    
    # Helper to get section info
    get_section_info = function(section) {
      for (section_info in private$priority_sections) {
        if (section_info$section == section) {
          return(section_info)
        }
      }
      return(NULL)
    },
    
    # Schedule single UI load (non-blocking)
    schedule_single_ui_load = function(section, delay_time) {
      force(section)
      force(delay_time)
      
      later::later(function() {
        if (self$status != "running") return()
        
        private$load_ui_background(section)
      }, delay = delay_time)
    },
    
    # Schedule single server load (non-blocking)
    schedule_single_server_load = function(section, delay_time) {
      force(section)
      force(delay_time)
      
      later::later(function() {
        if (self$status != "running") return()
        
        private$load_server_background(section)
      }, delay = delay_time)
    },
    
    # Load UI in background - FIXED to avoid reactive context
    load_ui_background = function(section) {
      force(section)
      
      # DON'T use future for simple UI loading - just do it directly
      tryCatch({
        success <- private$load_ui_module_silent(section)
        if (success) {
          # Schedule the callback in main thread
          later::later(function() {
            private$on_ui_completed(section)
          }, delay = 0.1)
        } else {
          later::later(function() {
            private$on_ui_failed(section)
          }, delay = 0.1)
        }
      }, error = function(e) {
        message(sprintf("Background UI loading failed for %s: %s", section, e$message))
        later::later(function() {
          private$on_ui_failed(section)
        }, delay = 0.1)
      })
    },
    
    # Load server in background - FIXED to avoid reactive context
    load_server_background = function(section) {
      force(section)
      
      # DON'T use future for simple server loading - just do it directly
      tryCatch({
        success <- private$load_server_module_silent(section)
        if (success) {
          # Schedule the callback in main thread
          later::later(function() {
            private$on_server_completed(section)
          }, delay = 0.1)
        } else {
          later::later(function() {
            private$on_server_failed(section)
          }, delay = 0.1)
        }
      }, error = function(e) {
        message(sprintf("Background server loading failed for %s: %s", section, e$message))
        later::later(function() {
          private$on_server_failed(section)
        }, delay = 0.1)
      })
    },
    
    # Schedule explorer data loading - FIXED to avoid reactive context
    schedule_explorer_data_loading = function() {
      # Load 2024 data first (higher priority)
      later::later(function() {
        if (self$status != "running") return()
        
        # Use future for data loading (which is heavy) but handle completion carefully
        future({
          tryCatch({
            message("Background: Silently loading explorer data for 2024...")
            per_data <- load_survey_data("PER_2024")
            par_data <- load_survey_data("PAR_2024")
            
            # Store directly in global cache (not reactive)
            if (!is.null(per_data)) {
              assign("survey_PER_2024", per_data, envir = .GlobalEnv$GLOBAL_CACHE)
            }
            if (!is.null(par_data)) {
              assign("survey_PAR_2024", par_data, envir = .GlobalEnv$GLOBAL_CACHE)
            }
            
            message("Background: Completed explorer data for 2024")
            return("explorer_2024_completed")
          }, error = function(e) {
            message(sprintf("Background explorer 2024 failed: %s", e$message))
            return("explorer_2024_failed")
          })
        }, seed = TRUE) %...>% (function(result) {
          # THIS runs in main thread, so it's safe
          later::later(function() {
            private$on_data_completed("explorer_2024")
          }, delay = 0.1)
        })
      }, delay = 5)  # Start after 5 seconds
      
      # Load 2023 data later
      later::later(function() {
        if (self$status != "running") return()
        
        future({
          tryCatch({
            message("Background: Silently loading explorer data for 2023...")
            per_data <- load_survey_data("PER_2023")
            par_data <- load_survey_data("PAR_2023")
            
            if (!is.null(per_data)) {
              assign("survey_PER_2023", per_data, envir = .GlobalEnv$GLOBAL_CACHE)
            }
            if (!is.null(par_data)) {
              assign("survey_PAR_2023", par_data, envir = .GlobalEnv$GLOBAL_CACHE)
            }
            
            message("Background: Completed explorer data for 2023")
            return("explorer_2023_completed")
          }, error = function(e) {
            message(sprintf("Background explorer 2023 failed: %s", e$message))
            return("explorer_2023_failed")
          })
        }, seed = TRUE) %...>% (function(result) {
          # THIS runs in main thread, so it's safe
          later::later(function() {
            private$on_data_completed("explorer_2023")
          }, delay = 0.1)
        })
      }, delay = 20)  # Wait 20 seconds before starting 2023 data
    },
    
    # Silent section loading methods
    schedule_section_loading = function() {
      delay_counter <- 0
      
      for (i in seq_along(private$priority_sections)) {
        section_info <- private$priority_sections[[i]]
        section <- section_info$section
        priority <- section_info$priority
        
        # Skip extras sections (no data files)
        if (section_info$folder == "extras") next
        
        # Stagger loading based on priority (start data loading later)
        delay_time <- 30 + (priority - 1) * 15 + delay_counter * 3
        delay_counter <- delay_counter + 1
        
        private$schedule_single_section(section, 2024, delay_time)
        private$schedule_single_section(section, 2023, delay_time + 45)
      }
    },
    
    # Schedule single section loading
    schedule_single_section = function(section, year, delay_time) {
      force(section)
      force(year)
      force(delay_time)
      
      later::later(function() {
        if (self$status != "running") return()
        
        private$load_section_silent(section, year)
      }, delay = delay_time)
    },
    
    # Silent section loading - FIXED to avoid reactive context
    load_section_silent = function(section, year) {
      force(section)
      force(year)
      
      skip_sections <- c("trust", "equipment")
      if (section %in% skip_sections) {
        message(sprintf("Skipping %s %s (uses data from other sections)", section, year))
        later::later(function() {
          private$on_data_completed(paste0(section, "_", year))
        }, delay = 0.1)
        return(invisible(NULL))
      }
      
      future({
        tryCatch({
          plots_path <- paste0("data/plots/", section, "_", year, ".rds")
          maps_path <- paste0("data/plots/map_", section, "_", year, ".rds")
          pct_path <- paste0("data/percentages/", section, "_", year, ".rds")
          
          loaded_items <- 0
          
          # Load silently without progress updates
          if (file.exists(plots_path)) {
            plots_data <- readRDS(plots_path)
            plots_key <- paste0("plots_", section, "_", year)
            assign(plots_key, plots_data, envir = .GlobalEnv$GLOBAL_CACHE)
            loaded_items <- loaded_items + 1
          }
          
          if (file.exists(maps_path)) {
            maps_data <- readRDS(maps_path)
            maps_key <- paste0("maps_", section, "_", year)
            assign(maps_key, maps_data, envir = .GlobalEnv$GLOBAL_CACHE)
            loaded_items <- loaded_items + 1
          }
          
          if (file.exists(pct_path)) {
            pct_data <- readRDS(pct_path)
            pct_key <- paste0("pct_", section, "_", year)
            assign(pct_key, pct_data, envir = .GlobalEnv$GLOBAL_CACHE)
            loaded_items <- loaded_items + 1
          }
          
          if (loaded_items > 0) {
            message(sprintf("Background: Silently loaded %s %s (%d items)", section, year, loaded_items))
          }
          
          return(paste0(section, "_", year, "_completed"))
        }, error = function(e) {
          message(sprintf("Background section loading failed for %s %s: %s", section, year, e$message))
          return(paste0(section, "_", year, "_failed"))
        })
      }, seed = TRUE) %...>% (function(result) {
        # THIS runs in main thread, so it's safe
        later::later(function() {
          private$on_data_completed(paste0(section, "_", year))
        }, delay = 0.1)
      })
    },
    
    # FIXED callback handlers - completely non-reactive
    on_ui_completed = function(section) {
      self$progress$completed_ui <- self$progress$completed_ui + 1
      self$progress$completed_tasks <- self$progress$completed_tasks + 1
      self$progress$current_task <- sprintf("Loaded UI: %s", section)
      
      private$update_progress_safe()
      private$check_completion()
    },
    
    on_ui_failed = function(section) {
      # Still count as completed to avoid getting stuck
      self$progress$completed_ui <- self$progress$completed_ui + 1
      self$progress$completed_tasks <- self$progress$completed_tasks + 1
      self$progress$current_task <- sprintf("Failed UI: %s", section)
      
      private$update_progress_safe()
      private$check_completion()
    },
    
    on_server_completed = function(section) {
      self$progress$completed_server <- self$progress$completed_server + 1
      self$progress$completed_tasks <- self$progress$completed_tasks + 1
      self$progress$current_task <- sprintf("Loaded server: %s", section)
      
      private$update_progress_safe()
      private$check_completion()
    },
    
    on_server_failed = function(section) {
      # Still count as completed to avoid getting stuck
      self$progress$completed_server <- self$progress$completed_server + 1
      self$progress$completed_tasks <- self$progress$completed_tasks + 1
      self$progress$current_task <- sprintf("Failed server: %s", section)
      
      private$update_progress_safe()
      private$check_completion()
    },
    
    on_data_completed = function(task_id) {
      self$progress$completed_data <- self$progress$completed_data + 1
      self$progress$completed_tasks <- self$progress$completed_tasks + 1
      self$progress$current_task <- sprintf("Loaded data: %s", task_id)
      
      private$update_progress_safe()
      private$check_completion()
    },
    
    # Check if all tasks completed
    check_completion = function() {
      if (self$progress$completed_tasks >= self$progress$total_tasks) {
        private$on_all_completed()
      }
    },
    
    # Handle completion of all tasks
    on_all_completed = function() {
      self$status <- "completed"
      total_duration <- as.numeric(difftime(Sys.time(), self$progress$start_time, units = "mins"))
      
      cache_stats <- tryCatch({
        if (!is.null(private$data_manager)) {
          private$data_manager$get_cache_stats()
        } else {
          list(cache_size_mb = 0, hit_rate = 0, cache_objects = 0)
        }
      }, error = function(e) {
        list(cache_size_mb = 0, hit_rate = 0, cache_objects = 0)
      })
      
      message(sprintf("Background preloading completed! Duration: %.1f minutes", total_duration))
      message(sprintf("Summary: %d/%d UI, %d/%d Server, %d/%d Data tasks completed", 
                     self$progress$completed_ui, self$progress$ui_tasks,
                     self$progress$completed_server, self$progress$server_tasks,
                     self$progress$completed_data, self$progress$data_tasks))
      message(sprintf("Final cache: %.1f MB, %.2f%% hit rate, %d objects", 
                     cache_stats$cache_size_mb, cache_stats$hit_rate, cache_stats$cache_objects))
      
      self$progress$current_task <- "All preloading completed"
      private$update_progress_safe()
    }
  )
)

# ===== EXPORT FOR GLOBAL ENVIRONMENT =====

# Make sure the class is available globally
assign("BackgroundPreloader", BackgroundPreloader, envir = .GlobalEnv)

message("Fixed background preloader loaded without reactive context dependencies")