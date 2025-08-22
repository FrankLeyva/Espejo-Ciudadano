# Debug script for MySQL/MariaDB analytics database
# Run this to check if analytics database is working and parameterized queries are supported

library(DBI)
library(RMySQL)

cat("🔍 Debugging MySQL/MariaDB Analytics Database...\n\n")

# Connection parameters (edit if needed)
db_host <- Sys.getenv("MYSQL_HOST", "srv960.hstgr.io")
db_user <- Sys.getenv("MYSQL_USER", "u550512989_Frank")
db_password <- Sys.getenv("MYSQL_PASSWORD", "o^habiSrSQ7")
db_name <- Sys.getenv("MYSQL_DB", "u550512989_asivemosjuarez")
db_port <- 3306

cat(sprintf("Connecting to %s@%s/%s...\n", db_user, db_host, db_name))

# Try to connect to MySQL/MariaDB
test_conn <- NULL
tryCatch({
  test_conn <- dbConnect(RMySQL::MySQL(),
    dbname = db_name,
    host = db_host,
    user = db_user,
    password = db_password,
    port = db_port
  )
  cat("✅ MySQL/MariaDB connection successful\n\n")
}, error = function(e) {
  cat("❌ Connection failed:", e$message, "\n")
  quit(save = "no")
})

# List tables
cat("📋 Tables in database:\n")
print(dbListTables(test_conn))
cat("\n")

# Check sessions table schema
if ("sessions" %in% dbListTables(test_conn)) {
  cat("👥 'sessions' table schema:\n")
  print(dbGetQuery(test_conn, "DESCRIBE sessions"))
  cat("\n")
} else {
  cat("❌ 'sessions' table not found!\n")
  quit(save = "no")
}

# Test 1: Parameterized INSERT
cat("Test 1: Parameterized INSERT...\n")
tryCatch({
  dbExecute(test_conn, "INSERT INTO sessions (session_id, start_time, ip_address, user_agent, last_activity) VALUES (?, ?, ?, ?, ?)",
    list("debug_test_1", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "127.0.0.1", "debug_agent", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  cat("✅ Parameterized INSERT succeeded\n")
}, error = function(e) {
  cat("❌ Parameterized INSERT failed:", e$message, "\n")
})

# Test 2: Parameterized REPLACE INTO
cat("Test 2: Parameterized REPLACE INTO...\n")
tryCatch({
  dbExecute(test_conn, "REPLACE INTO sessions (session_id, start_time, ip_address, user_agent, last_activity) VALUES (?, ?, ?, ?, ?)",
    list("debug_test_1", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "127.0.0.1", "debug_agent", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  cat("✅ Parameterized REPLACE INTO succeeded\n")
}, error = function(e) {
  cat("❌ Parameterized REPLACE INTO failed:", e$message, "\n")
})

# Test 3: Direct SQL REPLACE INTO (string interpolation)
cat("Test 3: Direct SQL REPLACE INTO (no parameters)...\n")
sql <- sprintf(
  "REPLACE INTO sessions (session_id, start_time, ip_address, user_agent, last_activity) VALUES ('%s', '%s', '%s', '%s', '%s')",
  "debug_test_2", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "127.0.0.1", "debug_agent", format(Sys.time(), "%Y-%m-%d %H:%M:%S")
)
tryCatch({
  dbExecute(test_conn, sql)
  cat("✅ Direct SQL REPLACE INTO succeeded\n")
}, error = function(e) {
  cat("❌ Direct SQL REPLACE INTO failed:", e$message, "\n")
})

# Show recent sessions
cat("\nRecent sessions in table:\n")
print(dbGetQuery(test_conn, "SELECT session_id, start_time, ip_address, user_agent, last_activity FROM sessions ORDER BY start_time DESC LIMIT 5"))

# Clean up test rows
cat("\nCleaning up test rows...\n")
tryCatch({
  dbExecute(test_conn, "DELETE FROM sessions WHERE session_id LIKE 'debug_test_%'")
  cat("✅ Test rows deleted\n")
}, error = function(e) {
  cat("❌ Failed to delete test rows:", e$message, "\n")
})

dbDisconnect(test_conn)
cat("\n🔍 Debug complete!\n") 