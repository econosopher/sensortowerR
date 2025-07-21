# Regenerate cache with corrected retention values
library(devtools)
load_all()

# Fetch fresh data from API
cat("Fetching fresh data from API with corrected retention handling...\n")
top_rpgs <- st_top_charts(
  measure = "revenue",
  category = 7014,  # Role Playing
  limit = 20
)

# Save to cache
cache_file <- "inst/cache/top_rpgs_cache.rds"
cache_dir <- dirname(cache_file)

if (!dir.exists(cache_dir)) {
  dir.create(cache_dir, recursive = TRUE)
}

saveRDS(top_rpgs, cache_file)
cat("Data cached to:", cache_file, "\n")

# Check retention values
cat("\nSample retention values (should be 0-100 range):\n")
retention_cols <- grep("retention", names(top_rpgs), value = TRUE)
if (length(retention_cols) > 0) {
  print(head(top_rpgs[, c("unified_app_name", retention_cols[1:min(3, length(retention_cols))])]))
} else {
  cat("No retention columns found in the data.\n")
}