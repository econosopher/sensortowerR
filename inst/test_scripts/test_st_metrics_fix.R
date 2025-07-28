# Test st_metrics fix
library(sensortowerR)

cat("=== Testing st_metrics Fix ===\n\n")

# Test with platform-specific IDs
cat("Test 1: Platform-specific IDs\n")
result <- st_metrics(
  ios_app_id = "1195621598",
  android_app_id = "com.playrix.homescapes",
  start_date = Sys.Date() - 7,
  end_date = Sys.Date() - 1,
  verbose = TRUE
)

cat(sprintf("\nResult: %d rows returned\n", nrow(result)))
cat("Columns:", paste(names(result), collapse = ", "), "\n")
print(head(result))

# Test with unified app_id
cat("\n\nTest 2: Unified app_id with daily data\n")
result2 <- st_metrics(
  app_id = "553834731",  # Candy Crush
  start_date = Sys.Date() - 7,
  end_date = Sys.Date() - 1,
  date_granularity = "daily",
  verbose = TRUE
)

cat(sprintf("\nResult: %d rows returned\n", nrow(result2)))
print(head(result2))