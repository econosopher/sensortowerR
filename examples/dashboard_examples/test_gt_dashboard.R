# Test the new st_gt_dashboard function
library(devtools)
load_all()

# Load cached data
cache_file <- "inst/cache/top_rpgs_cache.rds"
if (!file.exists(cache_file)) {
  stop("No cached data found. Please run squad_rpg_analysis_538_themed_improved.R first.")
}

top_rpgs <- readRDS(cache_file)

# Check retention values before processing
cat("Sample retention values from raw data:\n")
print(head(top_rpgs[, c("unified_app_name", "retention_1d_us", "retention_7d_us", "retention_30d_us")]))

# Test 1: Basic usage with default settings
cat("\n\nTest 1: Creating dashboard with default settings...\n")
gt1 <- st_gt_dashboard(
  top_rpgs,
  title = "Top Role-Playing Games on Mobile",
  save_path = "inst/images/test_dashboard_default.png"
)

# Test 2: Raw mode (minimal styling)
cat("\nTest 2: Creating raw dashboard...\n")
gt2 <- st_gt_dashboard(
  top_rpgs,
  title = "Top Role-Playing Games - Raw Mode",
  raw = TRUE,
  save_path = "inst/images/test_dashboard_raw.png"
)

# Test 3: Custom configuration
cat("\nTest 3: Creating customized dashboard...\n")
gt3 <- st_gt_dashboard(
  top_rpgs,
  title = "Top RPGs by 30-Day Revenue",
  ranking_metric = "revenue_30d_ww",
  show_retention = TRUE,
  show_engagement = TRUE,
  bar_charts = TRUE,
  color_scheme = list(
    revenue = "#E74C3C",
    downloads = "#3498DB",
    engagement = "#9B59B6",
    rpd = "#27AE60",
    retention_low = "#FFE5E5",
    retention_mid = "#E5F5E5", 
    retention_high = "#27AE60"
  ),
  save_path = "inst/images/test_dashboard_custom.png"
)

# Test 4: Minimal columns
cat("\nTest 4: Creating minimal dashboard...\n")
gt4 <- st_gt_dashboard(
  top_rpgs,
  title = "Top RPGs - Revenue Focus",
  show_demographics = FALSE,
  show_engagement = FALSE,
  show_retention = FALSE,
  show_rpd = FALSE,
  save_path = "inst/images/test_dashboard_minimal.png"
)

cat("\nAll tests completed! Check inst/images/ for the generated dashboards.\n")