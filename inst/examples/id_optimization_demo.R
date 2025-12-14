# Demonstration of ID Optimization Features in sensortowerR
# This shows how the package minimizes API calls through intelligent caching

library(sensortowerR)

# Enable verbose mode to see the optimization in action
options(sensortowerR.verbose = TRUE)

cat("=== sensortowerR ID Optimization Demo ===\n\n")

# 1. First, let's clear any existing cache to start fresh
st_clear_id_cache()

# 2. Show cache info (should be empty)
cat("\nInitial cache status:\n")
st_cache_info()

# 3. First request - will make API calls
cat("\n\nFIRST REQUEST (Cold Cache):\n")
cat("==========================\n")

apps <- c(
  "553834731",                    # Candy Crush iOS
  "com.king.candycrushsaga",      # Candy Crush Android
  "5ba4585f539ce75b97db6bcb",     # Star Trek unified
  "1195621598",                   # Homescapes iOS
  "com.playrix.homescapes"        # Homescapes Android
)

# This will make API calls to resolve IDs
metrics1 <- st_smart_metrics(
  app_ids = apps,
  metrics = c("revenue", "downloads"),
  start_date = "2024-01-01",
  end_date = "2024-01-07",
  countries = "US"
)

# 4. Check cache after first request
cat("\n\nCache after first request:\n")
st_cache_info()

# 5. Second request with same apps - should use cache
cat("\n\nSECOND REQUEST (Warm Cache):\n")
cat("============================\n")

# This should NOT make any ID lookup API calls
metrics2 <- st_smart_metrics(
  app_ids = apps,
  metrics = c("revenue", "downloads", "dau"),
  start_date = "2024-02-01",
  end_date = "2024-02-07",
  countries = "US"
)

cat("\nNotice: No ID lookup API calls needed - all IDs resolved from cache!\n")

# 6. Request with partially cached apps
cat("\n\nTHIRD REQUEST (Mixed Cache):\n")
cat("===========================\n")

mixed_apps <- c(
  "553834731",                    # Cached
  "1094591345",                   # New - Pokemon GO iOS
  "com.nianticlabs.pokemongo"     # New - Pokemon GO Android
)

metrics3 <- st_smart_metrics(
  app_ids = mixed_apps,
  metrics = c("revenue"),
  start_date = "2024-03-01",
  end_date = "2024-03-07",
  countries = "WW"
)

# 7. Show final cache statistics
cat("\n\nFinal cache statistics:\n")
st_cache_info()

# 8. Demonstrate batch resolution efficiency
cat("\n\nBATCH RESOLUTION DEMO:\n")
cat("=====================\n")

# Many apps at once - uses batch API endpoint when possible
many_apps <- c(
  "529479190",                    # Clash of Clans iOS
  "com.supercell.clashofclans",   # Clash of Clans Android
  "1053012308",                   # MONOPOLY GO! iOS
  "com.scopely.monopolygo",       # MONOPOLY GO! Android
  "1354664240",                   # Royal Match iOS
  "com.dreamgames.royalmatch"     # Royal Match Android
)

cat("Resolving", length(many_apps), "app IDs...\n")
batch_metrics <- st_smart_metrics(
  app_ids = many_apps,
  metrics = c("revenue"),
  start_date = "2024-01-01",
  end_date = "2024-01-01",
  countries = "US"
)

# 9. Show how the cache persists between sessions
cat("\n\nCACHE PERSISTENCE:\n")
cat("==================\n")

# Save current cache stats
cache_stats_before <- st_cache_info()

# To persist cache between sessions, explicitly call save_id_cache()
cat("\nTo save cache for future sessions, call save_id_cache()\n")
cat("Cache location (CRAN-compliant): ", tools::R_user_dir("sensortowerR", "cache"), "\n")
cat("To load cached data in a new session, call load_id_cache()\n")

# 10. ID Type Detection Demo
cat("\n\nID TYPE DETECTION:\n")
cat("==================\n")

test_ids <- c(
  "553834731",                    # iOS numeric
  "com.king.candycrushsaga",      # Android package
  "5ba4585f539ce75b97db6bcb",     # Unified hex
  "pub123456",                    # Publisher ID
  "unknown_format_123"            # Unknown
)

for (id in test_ids) {
  cat(sprintf("%-30s -> %s\n", id, detect_id_type(id)))
}

# 11. Configuration options
cat("\n\nCONFIGURATION OPTIONS:\n")
cat("=====================\n")

cat("Current settings:\n")
cat("  Cache enabled:", getOption("sensortowerR.use_cache"), "\n")
cat("  Cache max age:", getOption("sensortowerR.cache_max_age_days"), "days\n")
cat("  Auto resolve:", getOption("sensortowerR.auto_resolve"), "\n")
cat("  Verbose mode:", getOption("sensortowerR.verbose"), "\n")

# Summary
cat("\n\n=== SUMMARY ===\n")
cat("The ID optimization features provide:\n")
cat("1. Automatic caching of ID lookups (30-day expiry)\n")
cat("2. Batch resolution to minimize API calls\n")
cat("3. Smart ID type detection\n")
cat("4. Persistent cache between R sessions\n")
cat("5. Configurable behavior via options()\n")
cat("\nResult: Dramatically reduced API calls and faster performance!\n")