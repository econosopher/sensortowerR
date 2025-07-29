# Debug st_batch_metrics
library(sensortowerR)

# Test with verbose output
cat("Testing st_batch_metrics with single app...\n\n")

# First test st_metrics directly
cat("First testing st_metrics directly:\n")
direct_result <- st_metrics(
  os = "ios", 
  ios_app_id = "553834731",
  countries = "US",
  date_granularity = "monthly",
  start_date = "2024-01-01",
  end_date = "2024-01-31"
)
cat("Direct st_metrics result: ", nrow(direct_result), "rows\n")
cat("Column names:", paste(names(direct_result), collapse = ", "), "\n")
print(direct_result)

cat("\n\nNow testing st_batch_metrics:\n")
result <- st_batch_metrics(
  os = "ios",
  app_list = "553834731",  # Just Candy Crush
  metrics = c("revenue", "downloads"),
  date_range = list(start_date = "2024-01-01", end_date = "2024-01-31"),
  countries = "US",
  granularity = "monthly",
  verbose = TRUE
)

cat("\n\nResult:\n")
if (!is.null(result)) {
  cat("Rows:", nrow(result), "\n")
  cat("Columns:", paste(names(result), collapse = ", "), "\n")
  print(result)
} else {
  cat("Result is NULL\n")
}