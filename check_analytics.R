# Check Analytics Database
# Run this in R console or RStudio

# Load required libraries
if (!require(DBI)) install.packages("DBI")
if (!require(RSQLite)) install.packages("RSQLite")

library(DBI)
library(RSQLite)

cat("🔍 Checking Analytics Database...\n\n")

# Check database file
db_path <- "data/analytics.db"
if (file.exists(db_path)) {
  cat("✅ Database file exists:", db_path, "\n")
  cat("📊 File size:", round(file.size(db_path) / 1024, 2), "KB\n")
  cat("📅 Last modified:", file.info(db_path)$mtime, "\n\n")
} else {
  cat("❌ Database file not found:", db_path, "\n")
  stop("Database not found")
}

# Connect to database
tryCatch({
  conn <- dbConnect(SQLite(), db_path)
  cat("✅ Database connection successful\n\n")
  
  # List tables
  tables <- dbGetQuery(conn, "SELECT name FROM sqlite_master WHERE type='table'")
  cat("📋 Tables found:", nrow(tables), "\n")
  if (nrow(tables) > 0) {
    for (i in 1:nrow(tables)) {
      cat("  -", tables$name[i], "\n")
    }
  }
  cat("\n")
  
  # Check sessions
  if ("sessions" %in% tables$name) {
    session_count <- dbGetQuery(conn, "SELECT COUNT(*) as count FROM sessions")
    cat("👥 Total sessions:", session_count$count[1], "\n")
    
    if (session_count$count[1] > 0) {
      # Recent sessions
      recent_sessions <- dbGetQuery(conn, "
        SELECT session_id, start_time, total_sections_visited, total_downloads
        FROM sessions 
        ORDER BY start_time DESC 
        LIMIT 3
      ")
      cat("📅 Recent sessions:\n")
      print(recent_sessions)
    }
  }
  
  # Check page views
  if ("page_views" %in% tables$name) {
    page_views_count <- dbGetQuery(conn, "SELECT COUNT(*) as count FROM page_views")
    cat("\n📄 Total page views:", page_views_count$count[1], "\n")
    
    if (page_views_count$count[1] > 0) {
      # Popular sections
      popular_sections <- dbGetQuery(conn, "
        SELECT section, COUNT(*) as visits
        FROM page_views 
        GROUP BY section 
        ORDER BY visits DESC 
        LIMIT 5
      ")
      cat("🔥 Popular sections:\n")
      print(popular_sections)
    }
  }
  
  # Check interactions
  if ("interactions" %in% tables$name) {
    interactions_count <- dbGetQuery(conn, "SELECT COUNT(*) as count FROM interactions")
    cat("\n🔄 Total interactions:", interactions_count$count[1], "\n")
  }
  
  dbDisconnect(conn)
  
}, error = function(e) {
  cat("❌ Error:", e$message, "\n")
})

cat("\n✅ Check complete!\n") 