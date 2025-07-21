# Script to fetch and cache RPG data from Sensor Tower API
# Run this only when you need to refresh the data

# Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  char = c("devtools", "dplyr", "lubridate")
)

# Load the development version of sensortowerR
devtools::load_all()

# Use well-known Role Playing category ID
role_playing_id <- 7014

# Cache settings
cache_file <- "inst/cache/top_rpgs_cache.rds"
cache_dir <- dirname(cache_file)

# Create cache directory if it doesn't exist
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir, recursive = TRUE)
}

# Check if cache exists and when it was last updated
if (file.exists(cache_file)) {
  cache_info <- file.info(cache_file)
  cache_age <- difftime(Sys.time(), cache_info$mtime, units = "days")
  cat("Cache exists. Last updated:", format(cache_info$mtime), 
      "(", round(cache_age, 1), "days ago)\n")
  
  response <- readline("Do you want to refresh the cache? (y/n): ")
  
  if (tolower(response) != "y") {
    cat("Using existing cache.\n")
    stop("Cache refresh cancelled.")
  }
}

# Fetch fresh data from API
cat("Fetching data from Sensor Tower API...\n")
top_rpgs <- st_top_charts(
  measure = "revenue",
  category = role_playing_id,
  limit = 20
)

# Save to cache
saveRDS(top_rpgs, cache_file)
cat("Data successfully cached to:", cache_file, "\n")

# Show summary
cat("\nCached data summary:\n")
cat("- Number of games:", nrow(top_rpgs), "\n")
cat("- Date range:", format(Sys.Date()), "\n")
cat("- Top game:", top_rpgs$unified_app_name[1], "\n")