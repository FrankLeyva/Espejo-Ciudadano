# analytics.R - Final fixed version
library(DBI)
library(RMySQL)
library(jsonlite)
library(httr)
library(pool)

# CRITICAL FIX: Create ONE global pool that's shared across all instances
if (!exists("GLOBAL_ANALYTICS_POOL", envir = .GlobalEnv)) {
  message("🔧 Creating GLOBAL analytics pool...")
  assign("GLOBAL_ANALYTICS_POOL", pool::dbPool(
    drv = RMySQL::MySQL(),
    dbname = Sys.getenv("MYSQL_DB", "u550512989_asivemosjuarez"),
    host = Sys.getenv("MYSQL_HOST", "srv960.hstgr.io"),
    username = Sys.getenv("MYSQL_USER", "u550512989_Frank"),
    password = Sys.getenv("MYSQL_PASSWORD", "o^habiSrSQ7"),
    port = 3306,
    # ULTRA CONSERVATIVE: Use only 2 connections max
    minSize = 1,      
    maxSize = 2,      # Reduced from 4 to 2
    idleTimeout = 180,  # Reduced from 300 to 180 seconds
    validationInterval = 60  # Increased from 30 to 60 seconds
  ), envir = .GlobalEnv)
  message("✅ Global analytics pool created successfully")
}

# Analytics Manager Class with FIXED pool management
AnalyticsManager <- R6::R6Class("AnalyticsManager",
  private = list(
    session_data = list(),
    connection_retries = 0,
    max_retries = 3,
    retry_delay = 5,
    cooldown_period = 60,
    last_disable_time = NULL,
    .is_enabled = TRUE,
    last_error = NULL,
    error_log_file = "analytics_error.log",
    
    # Remove individual connection parameters - use global pool only
    db_host = Sys.getenv("MYSQL_HOST", "srv960.hstgr.io"),
    db_user = Sys.getenv("MYSQL_USER", "u550512989_Frank"),
    db_password = Sys.getenv("MYSQL_PASSWORD", "o^habiSrSQ7"),
    db_name = Sys.getenv("MYSQL_DB", "u550512989_asivemosjuarez"),
    db_port = 3306,
    
    validate_config = function() {
      # Just log the config, don't create connections here
      message(sprintf("Analytics config: %s@%s/%s", private$db_user, private$db_host, private$db_name))
    },
    
    log_error = function(msg) {
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      cat(sprintf("[%s] %s\n", timestamp, msg), file = private$error_log_file, append = TRUE)
    },
    
    # FIXED: Test connection using global pool
    test_connection = function() {
      tryCatch({
        if (!exists("GLOBAL_ANALYTICS_POOL", envir = .GlobalEnv)) {
          stop("Global analytics pool not available")
        }
        
        pool_obj <- get("GLOBAL_ANALYTICS_POOL", envir = .GlobalEnv)
        conn <- pool::poolCheckout(pool_obj)
        
        # Test query
        result <- DBI::dbGetQuery(conn, "SELECT 1 as test")
        
        # CRITICAL: Always return connection to pool
        pool::poolReturn(conn)
        
        if (nrow(result) == 1 && result$test[1] == 1) {
          message("✅ Database connection test successful")
          return(TRUE)
        } else {
          stop("Database test query failed")
        }
      }, error = function(e) {
        # Make sure connection is returned even on error
        if (exists("conn") && !is.null(conn)) {
          tryCatch(pool::poolReturn(conn), error = function(e2) {})
        }
        message(paste("❌ Database connection test failed:", e$message))
        return(FALSE)
      })
    },
    
    # FIXED: Initialize database using global pool
    init_database = function() {
      for (attempt in 1:private$max_retries) {
        tryCatch({
          message(sprintf("Initializing database (attempt %d/%d)...", attempt, private$max_retries))
          
          if (!exists("GLOBAL_ANALYTICS_POOL", envir = .GlobalEnv)) {
            stop("Global analytics pool not available")
          }
          
          pool_obj <- get("GLOBAL_ANALYTICS_POOL", envir = .GlobalEnv)
          conn <- pool::poolCheckout(pool_obj)
          
          # Create tables
          DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS sessions (session_id VARCHAR(64) PRIMARY KEY, start_time DATETIME, end_time DATETIME, ip_address VARCHAR(64), user_agent TEXT, total_sections_visited INT DEFAULT 0, total_downloads INT DEFAULT 0, last_activity DATETIME)")
          DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS page_views (id INT AUTO_INCREMENT PRIMARY KEY, session_id VARCHAR(64), section VARCHAR(128), timestamp DATETIME, time_spent_seconds INT, year_selected VARCHAR(16), FOREIGN KEY (session_id) REFERENCES sessions(session_id))")
          DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS interactions (id INT AUTO_INCREMENT PRIMARY KEY, session_id VARCHAR(64), interaction_type VARCHAR(64), section VARCHAR(128), details TEXT, timestamp DATETIME, FOREIGN KEY (session_id) REFERENCES sessions(session_id))")
          DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS daily_stats (date DATE PRIMARY KEY, unique_sessions INT, total_page_views INT, most_popular_section VARCHAR(128), avg_session_duration DOUBLE, total_downloads INT)")
          
          # CRITICAL: Always return connection
          pool::poolReturn(conn)
          
          message("✅ Database initialized successfully")
          private$connection_retries <- 0
          return(TRUE)
          
        }, error = function(e) {
          # Make sure connection is returned even on error
          if (exists("conn") && !is.null(conn)) {
            tryCatch(pool::poolReturn(conn), error = function(e2) {})
          }
          
          private$last_error <- e$message
          message(sprintf("❌ Database initialization attempt %d failed: %s", attempt, e$message))
          
          if (attempt < private$max_retries) {
            message(sprintf("Retrying in %d seconds...", private$retry_delay))
            Sys.sleep(private$retry_delay)
          } else {
            message("❌ All database initialization attempts failed")
            private$.is_enabled <- FALSE
            private$last_disable_time <- Sys.time()
            stop(paste("Failed to initialize database after", private$max_retries, "attempts. Last error:", e$message))
          }
        })
      }
    },
    
    # FIXED: Get connection from global pool
    get_connection = function() {
      if (!private$.is_enabled) {
        stop("Analytics is disabled due to previous errors")
      }
      
      if (!exists("GLOBAL_ANALYTICS_POOL", envir = .GlobalEnv)) {
        stop("Global analytics pool not available")
      }
      
      pool_obj <- get("GLOBAL_ANALYTICS_POOL", envir = .GlobalEnv)
      conn <- pool::poolCheckout(pool_obj)
      return(conn)
    },
    
    # FIXED: Safe database operation with proper connection management
    safe_db_operation = function(operation_func, operation_name = "database operation") {
      if (!private$.is_enabled) {
        if (!is.null(private$last_disable_time) &&
            as.numeric(difftime(Sys.time(), private$last_disable_time, units = "secs")) > private$cooldown_period) {
          message("Cooldown expired, attempting to re-enable analytics...")
          private$.is_enabled <- TRUE
          private$connection_retries <- 0
        } else {
          message(paste("Skipping", operation_name, "- analytics disabled (cooldown)"))
          return(FALSE)
        }
      }
      
      conn <- NULL
      tryCatch({
        # Get connection
        conn <- private$get_connection()
        
        # Execute operation (this should include all DB operations)
        result <- operation_func(conn)
        
        # Return connection
        pool::poolReturn(conn)
        conn <- NULL  # Mark as returned
        
        private$connection_retries <- 0 # Reset on success
        return(result)
        
      }, error = function(e) {
        # CRITICAL: Always return connection on error
        if (!is.null(conn)) {
          tryCatch(pool::poolReturn(conn), error = function(e2) {
            message(paste("Error returning connection:", e2$message))
          })
        }
        
        msg <- sprintf("❌ %s failed: %s", operation_name, e$message)
        message(msg)
        private$log_error(msg)
        private$last_error <- e$message
        private$connection_retries <- private$connection_retries + 1
        
        if (private$connection_retries >= private$max_retries) {
          message("❌ Too many consecutive errors - disabling analytics (cooldown)")
          private$.is_enabled <- FALSE
          private$last_disable_time <- Sys.time()
        }
        return(FALSE)
      })
    }
  ),
  
  public = list(
    initialize = function() {
      message("🔧 Initializing Enhanced Analytics Manager...")
      # Wait a bit before trying to connect to avoid startup race conditions
      Sys.sleep(2)
      tryCatch({
        private$validate_config()
        if (!private$test_connection()) {
          stop("Initial connection test failed")
        }
        private$init_database()
        message("✅ Enhanced Analytics Manager initialized successfully")
      }, error = function(e) {
        msg <- sprintf("❌ Analytics initialization failed: %s", e$message)
        message(msg)
        private$log_error(msg)
        private$.is_enabled <- FALSE
        private$last_error <- e$message
        private$last_disable_time <- Sys.time()
        message("Analytics will be disabled for this session (cooldown)")
      })
    },
    
    is_enabled = function() {
      return(private$.is_enabled)
    },
    
    get_last_error = function() {
      return(private$last_error)
    },
    
    get_status = function() {
      list(
        enabled = private$.is_enabled,
        retries = private$connection_retries,
        last_error = private$last_error,
        db_config = list(
          host = private$db_host,
          user = private$db_user,
          database = private$db_name
        )
      )
    },
    
    retry_initialization = function() {
      message("🔄 Attempting to re-enable analytics...")
      private$.is_enabled <- TRUE
      private$connection_retries <- 0
      private$last_error <- NULL
      private$last_disable_time <- NULL
      tryCatch({
        private$validate_config()
        private$init_database()
        message("✅ Analytics re-enabled successfully")
        return(TRUE)
      }, error = function(e) {
        msg <- sprintf("❌ Re-initialization failed: %s", e$message)
        message(msg)
        private$log_error(msg)
        private$.is_enabled <- FALSE
        private$last_error <- e$message
        private$last_disable_time <- Sys.time()
        return(FALSE)
      })
    },
    
    # FIXED: All database operations now use the safe wrapper
    start_session = function(session_id, ip_address = "unknown", user_agent = "unknown") {
      private$safe_db_operation(function(conn) {
        private$session_data[[session_id]] <- list(
          start_time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          current_section = NULL,
          section_start_time = NULL,
          sections_visited = character(0),
          downloads = 0
        )
        
        sql <- sprintf(
          "REPLACE INTO sessions (session_id, start_time, ip_address, user_agent, last_activity) VALUES ('%s', '%s', '%s', '%s', '%s')",
          session_id, format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ip_address, user_agent, format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )
        DBI::dbExecute(conn, sql)
        message(sprintf("Session started: %s", session_id))
        return(TRUE)
      }, "start_session")
    },
    
    track_navigation = function(session_id, section, year_selected = NULL) {
      private$safe_db_operation(function(conn) {
        if (!session_id %in% names(private$session_data)) {
          self$start_session(session_id)
        }
        
        session_info <- private$session_data[[session_id]]
        time_spent <- 0
        
        if (!is.null(session_info$section_start_time)) {
          time_spent <- as.numeric(difftime(Sys.time(), session_info$section_start_time, units = "secs"))
        }
        
        if (!is.null(session_info$current_section) && time_spent > 0) {
          sql_pv <- sprintf(
            "INSERT INTO page_views (session_id, section, timestamp, time_spent_seconds, year_selected) VALUES ('%s', '%s', '%s', %d, '%s')",
            session_id, session_info$current_section, session_info$section_start_time, round(time_spent), year_selected
          )
          DBI::dbExecute(conn, sql_pv)
        }
        
        private$session_data[[session_id]]$current_section <- section
        private$session_data[[session_id]]$section_start_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        private$session_data[[session_id]]$sections_visited <- unique(c(
          private$session_data[[session_id]]$sections_visited, section
        ))
        
        sql_update <- sprintf(
          "UPDATE sessions SET total_sections_visited = %d, last_activity = '%s' WHERE session_id = '%s'",
          length(private$session_data[[session_id]]$sections_visited), format(Sys.time(), "%Y-%m-%d %H:%M:%S"), session_id
        )
        DBI::dbExecute(conn, sql_update)
        message(sprintf("Navigation tracked: %s -> %s", session_id, section))
        return(TRUE)
      }, "track_navigation")
    },
    
    track_interaction = function(session_id, type, section = NULL, details = NULL) {
      private$safe_db_operation(function(conn) {
        if (!session_id %in% names(private$session_data)) {
          self$start_session(session_id)
        }
        
        if (type == "download") {
          private$session_data[[session_id]]$downloads <- private$session_data[[session_id]]$downloads + 1
          sql_update <- sprintf(
            "UPDATE sessions SET total_downloads = %d, last_activity = '%s' WHERE session_id = '%s'",
            private$session_data[[session_id]]$downloads, format(Sys.time(), "%Y-%m-%d %H:%M:%S"), session_id
          )
          DBI::dbExecute(conn, sql_update)
        }
        
        sql_int <- sprintf(
          "INSERT INTO interactions (session_id, interaction_type, section, details, timestamp) VALUES ('%s', '%s', '%s', '%s', '%s')",
          session_id, type, section, ifelse(is.null(details), '', details), format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )
        DBI::dbExecute(conn, sql_int)
        message(sprintf("Interaction tracked: %s - %s in %s", session_id, type, section))
        return(TRUE)
      }, "track_interaction")
    },
    
    end_session = function(session_id) {
      private$safe_db_operation(function(conn) {
        if (session_id %in% names(private$session_data)) {
          session_info <- private$session_data[[session_id]]
          
          if (!is.null(session_info$section_start_time)) {
            time_spent <- as.numeric(difftime(Sys.time(), session_info$section_start_time, units = "secs"))
            if (!is.null(session_info$current_section) && time_spent > 0) {
              sql_pv <- sprintf(
                "INSERT INTO page_views (session_id, section, timestamp, time_spent_seconds) VALUES ('%s', '%s', '%s', %d)",
                session_id, session_info$current_section, session_info$section_start_time, round(time_spent)
              )
              DBI::dbExecute(conn, sql_pv)
            }
          }
          
          sql_update <- sprintf(
            "UPDATE sessions SET end_time = '%s', last_activity = '%s' WHERE session_id = '%s'",
            format(Sys.time(), "%Y-%m-%d %H:%M:%S"), format(Sys.time(), "%Y-%m-%d %H:%M:%S"), session_id
          )
          DBI::dbExecute(conn, sql_update)
          private$session_data[[session_id]] <- NULL
          message(sprintf("Session ended: %s", session_id))
          return(TRUE)
        }
      }, "end_session")
    },
    
    # FIXED: Get connection for external use (with return responsibility)
get_db_connection = function() {
  warning("get_db_connection() is deprecated - use execute_safe_query() instead")
  return(NULL)  # Force users to use safe methods
},
    execute_safe_query = function(query_func, operation_name = "external_query") {
  return(private$safe_db_operation(query_func, operation_name))
},
    health_check = function() {
      if (!private$.is_enabled) {
        return(list(
          status = "disabled",
          message = "Analytics disabled due to previous errors",
          last_error = private$last_error
        ))
      }
      
      # Use safe_db_operation for health check
      result <- private$safe_db_operation(function(conn) {
        result <- DBI::dbGetQuery(conn, "SELECT COUNT(*) as session_count FROM sessions LIMIT 1")
        return(result$session_count[1])
      }, "health_check")
      
      if (result != FALSE) {  # FIXED: Changed !== to !=
        return(list(
          status = "healthy",
          message = "Analytics working normally",
          session_count = result,
          retries = private$connection_retries
        ))
      } else {
        return(list(
          status = "error",
          message = "Health check failed",
          last_error = private$last_error
        ))
      }
    }
  )
)

# CLEANUP FUNCTION for app shutdown
onStop(function() {
  if (exists("GLOBAL_ANALYTICS_POOL", envir = .GlobalEnv)) {
    pool_obj <- get("GLOBAL_ANALYTICS_POOL", envir = .GlobalEnv)
    pool::poolClose(pool_obj)
    rm("GLOBAL_ANALYTICS_POOL", envir = .GlobalEnv)
    message("✅ Global analytics pool closed")
  }
})

# ADD: Connection monitoring function
monitor_pool_connections <- function() {
  if (exists("GLOBAL_ANALYTICS_POOL", envir = .GlobalEnv)) {
    pool_obj <- get("GLOBAL_ANALYTICS_POOL", envir = .GlobalEnv)
    # Get pool info
    tryCatch({
      info <- pool::dbGetInfo(pool_obj)
      message(sprintf("📊 Pool status: %d active connections", 
                     length(pool_obj$counters$free) + length(pool_obj$counters$taken)))
    }, error = function(e) {
      message("Could not get pool info: ", e$message)
    })
  }
}

# ADD: Emergency pool reset function
reset_analytics_pool <- function() {
  if (exists("GLOBAL_ANALYTICS_POOL", envir = .GlobalEnv)) {
    tryCatch({
      pool_obj <- get("GLOBAL_ANALYTICS_POOL", envir = .GlobalEnv)
      pool::poolClose(pool_obj)
      rm("GLOBAL_ANALYTICS_POOL", envir = .GlobalEnv)
      message("🔄 Analytics pool closed and removed")
      # Recreate with conservative settings
      assign("GLOBAL_ANALYTICS_POOL", pool::dbPool(
        drv = RMySQL::MySQL(),
        dbname = Sys.getenv("MYSQL_DB", "u550512989_asivemosjuarez"),
        host = Sys.getenv("MYSQL_HOST", "srv960.hstgr.io"),
        username = Sys.getenv("MYSQL_USER", "u550512989_Frank"),
        password = Sys.getenv("MYSQL_PASSWORD", "o^habiSrSQ7"),
        port = 3306,
        minSize = 1,
        maxSize = 2,  # Very conservative
        idleTimeout = 120,
        validationInterval = 120
      ), envir = .GlobalEnv)
      message("✅ Analytics pool recreated with conservative settings")
    }, error = function(e) {
      message("❌ Error resetting pool: ", e$message)
    })
  }
}
get_pool_status = function() {
  if (exists("GLOBAL_ANALYTICS_POOL", envir = .GlobalEnv)) {
    tryCatch({
      return(list(
        pool_exists = TRUE,
        max_size = 2,
        status = "active"
      ))
    }, error = function(e) {
      return(list(
        pool_exists = TRUE,
        error = e$message,
        status = "error"
      ))
    })
  } else {
    return(list(
      pool_exists = FALSE,
      status = "not_created"
    ))
  }
}