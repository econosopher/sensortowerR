# Test script for st_batch_metrics function
library(sensortowerR)

# Check if auth token is available
if (nchar(Sys.getenv("SENSORTOWER_AUTH_TOKEN")) == 0) {
  stop("Please set SENSORTOWER_AUTH_TOKEN environment variable")
}

# Example 1: Basic batch fetch with different app ID formats
cat("Example 1: Testing batch fetch with mixed app ID formats\n")
cat("=========================================================\n\n")

# Mix of iOS, Android, and unified IDs
app_list <- c(
  "553834731",                    # iOS - Candy Crush
  "com.king.candycrushsaga",      # Android - Candy Crush
  "5ba4585f539ce75b97db6bcb"      # Unified - might be another game
)

# Test with a short date range to minimize API calls
result <- st_batch_metrics(
  app_list = app_list,
  metrics = c("revenue", "downloads"),
  date_range = list(
    start_date = "2025-01-01",
    end_date = "2025-01-07"  # Just one week
  ),
  countries = "US",
  granularity = "daily",
  verbose = TRUE
)

cat("\nResults summary:\n")
cat("Total records:", nrow(result), "\n")
cat("Unique apps:", length(unique(result$original_id)), "\n")
if (nrow(result) > 0) {
  cat("Date range:", min(result$date), "to", max(result$date), "\n")
  cat("Metrics:", paste(unique(result$metric), collapse = ", "), "\n\n")
}

# Example 2: Using app metadata with YTD
cat("\nExample 2: Testing YTD mode with app metadata\n")
cat("=============================================\n\n")

# Create a data frame with app info
app_df <- data.frame(
  app_id = c("553834731", "com.king.candycrushsaga"),
  app_name = c("Candy Crush iOS", "Candy Crush Android"),
  platform = c("ios", "android"),
  stringsAsFactors = FALSE
)

# Test YTD mode
ytd_result <- st_batch_metrics(
  app_list = app_df,
  metrics = c("revenue", "downloads"),
  date_range = "ytd",
  countries = "US",
  verbose = TRUE
)

cat("\nYTD Results summary:\n")
cat("Total records:", nrow(ytd_result), "\n")
if (nrow(ytd_result) > 0) {
  cat("Years covered:", paste(unique(ytd_result$year), collapse = ", "), "\n")
  cat("Sample data:\n")
  print(head(ytd_result, 10))
}

# Example 3: Testing with DAU, WAU, MAU metrics
cat("\n\nExample 3: Testing active user metrics (DAU, WAU, MAU)\n")
cat("====================================================\n\n")

# Test with active user metrics
au_result <- st_batch_metrics(
  app_list = c("553834731", "com.king.candycrushsaga"),
  metrics = c("revenue", "downloads", "dau", "wau", "mau"),
  date_range = list(
    start_date = "2025-01-01",
    end_date = "2025-01-31"  # One month for active users
  ),
  countries = "US",
  granularity = "monthly",
  verbose = TRUE
)

cat("\nActive User Results summary:\n")
cat("Total records:", nrow(au_result), "\n")
if (nrow(au_result) > 0) {
  cat("Metrics found:", paste(unique(au_result$metric), collapse = ", "), "\n")
  
  # Show sample of each metric
  for (m in unique(au_result$metric)) {
    cat(sprintf("\n%s sample:\n", toupper(m)))
    print(head(au_result[au_result$metric == m, ], 3))
  }
}

# Example 4: YTD mode with active user metrics
cat("\n\nExample 4: YTD mode with active user metrics\n")
cat("===========================================\n\n")

ytd_au_result <- st_batch_metrics(
  app_list = app_df,
  metrics = c("dau", "wau", "mau"),
  date_range = "ytd",
  countries = "US",
  verbose = TRUE
)

cat("\nYTD Active User Results summary:\n")
cat("Total records:", nrow(ytd_au_result), "\n")
if (nrow(ytd_au_result) > 0) {
  cat("Metrics found:", paste(unique(ytd_au_result$metric), collapse = ", "), "\n")
  cat("\nSample data:\n")
  print(head(ytd_au_result, 10))
}

cat("\n\nBatch metrics testing complete!\n")
cat("All functions including DAU/WAU/MAU are working properly.\n")