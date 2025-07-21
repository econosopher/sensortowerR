# Diagnostic script to help understand column availability
# This helps debug issues with region-specific columns

library(sensortowerR)
library(dplyr)

# Function to diagnose column structure
diagnose_data_structure <- function(data, context = "Data") {
  cat("\n", rep("=", 60), "\n", sep = "")
  cat("DIAGNOSTIC REPORT:", context, "\n")
  cat(rep("=", 60), "\n\n", sep = "")
  
  # Basic info
  cat("Dimensions:", nrow(data), "rows x", ncol(data), "columns\n\n")
  
  # App name columns
  cat("APP NAME COLUMNS:\n")
  app_cols <- grep("(app_)?name|unified", names(data), value = TRUE, ignore.case = TRUE)
  if (length(app_cols) > 0) {
    cat("  Found:", paste(app_cols, collapse = ", "), "\n")
  } else {
    cat("  WARNING: No app name columns found!\n")
  }
  
  # Revenue columns
  cat("\nREVENUE COLUMNS:\n")
  revenue_cols <- grep("revenue", names(data), value = TRUE, ignore.case = TRUE)
  if (length(revenue_cols) > 0) {
    for (col in revenue_cols) {
      region <- ifelse(grepl("_us$", col), "US", 
                      ifelse(grepl("_ww$", col), "WW", "Unknown"))
      cat("  -", col, "(Region:", region, ")\n")
    }
  } else {
    cat("  None found\n")
  }
  
  # Download columns
  cat("\nDOWNLOAD COLUMNS:\n")
  download_cols <- grep("download|units", names(data), value = TRUE, ignore.case = TRUE)
  if (length(download_cols) > 0) {
    for (col in download_cols) {
      region <- ifelse(grepl("_us$", col), "US", 
                      ifelse(grepl("_ww$", col), "WW", "Unknown"))
      cat("  -", col, "(Region:", region, ")\n")
    }
  } else {
    cat("  None found\n")
  }
  
  # User metric columns
  cat("\nUSER METRIC COLUMNS:\n")
  user_cols <- grep("dau|mau|wau", names(data), value = TRUE, ignore.case = TRUE)
  if (length(user_cols) > 0) {
    for (col in user_cols) {
      region <- ifelse(grepl("_us$", col), "US", 
                      ifelse(grepl("_ww$", col), "WW", "Unknown"))
      cat("  -", col, "(Region:", region, ")\n")
    }
  } else {
    cat("  None found\n")
  }
  
  # Retention columns
  cat("\nRETENTION COLUMNS:\n")
  retention_cols <- grep("retention", names(data), value = TRUE, ignore.case = TRUE)
  if (length(retention_cols) > 0) {
    for (col in retention_cols) {
      region <- ifelse(grepl("_us$", col), "US", 
                      ifelse(grepl("_ww$", col), "WW", "Unknown"))
      cat("  -", col, "(Region:", region, ")\n")
    }
  } else {
    cat("  None found\n")
  }
  
  # Monetization columns
  cat("\nMONETIZATION COLUMNS:\n")
  monetization_cols <- grep("rpd|arpu", names(data), value = TRUE, ignore.case = TRUE)
  if (length(monetization_cols) > 0) {
    for (col in monetization_cols) {
      region <- ifelse(grepl("_us$", col), "US", 
                      ifelse(grepl("_ww$", col), "WW", "Unknown"))
      cat("  -", col, "(Region:", region, ")\n")
    }
  } else {
    cat("  None found\n")
  }
  
  # Region summary
  cat("\nREGION AVAILABILITY SUMMARY:\n")
  all_metric_cols <- c(revenue_cols, download_cols, user_cols, retention_cols, monetization_cols)
  us_cols <- sum(grepl("_us$", all_metric_cols))
  ww_cols <- sum(grepl("_ww$", all_metric_cols))
  cat("  US columns:", us_cols, "\n")
  cat("  WW columns:", ww_cols, "\n")
  
  # All columns (first 20)
  cat("\nALL COLUMNS (first 20):\n")
  all_cols <- names(data)
  for (i in 1:min(20, length(all_cols))) {
    cat("  ", i, ". ", all_cols[i], "\n", sep = "")
  }
  if (length(all_cols) > 20) {
    cat("  ... and", length(all_cols) - 20, "more columns\n")
  }
  
  cat("\n", rep("=", 60), "\n\n", sep = "")
}

# Example usage
cat("Column Structure Diagnostic Tool\n")
cat("================================\n\n")

# Test 1: Fetch with US region
cat("TEST 1: Fetching top games with regions='US'\n")
data_us <- st_top_charts(
  measure = "revenue",
  category = 6000,  # iOS Games
  regions = "US",
  limit = 5
)
diagnose_data_structure(data_us, "US Region Request")

# Test 2: Fetch with WW region
cat("\nTEST 2: Fetching top games with regions='WW'\n")
data_ww <- st_top_charts(
  measure = "revenue",
  category = 6000,  # iOS Games
  regions = "WW",
  limit = 5
)
diagnose_data_structure(data_ww, "WW Region Request")

# Compare results
cat("\nCOMPARISON SUMMARY:\n")
cat("===================\n")
cat("When requesting US region:\n")
cat("  - Revenue/Download columns returned:", 
    ifelse(any(grepl("revenue.*_us$", names(data_us))), "US", "WW"), "\n")
cat("  - Retention columns returned:", 
    ifelse(any(grepl("retention.*_us$", names(data_us))), "US", "WW"), "\n")
cat("  - User metrics returned:", 
    ifelse(any(grepl("(dau|mau|wau).*_us$", names(data_us))), "US", "WW"), "\n")

cat("\nRECOMMENDATION:\n")
cat("If you need US-specific data but only get WW columns for revenue/downloads,\n")
cat("you may need to:\n")
cat("1. Use the WW columns as they represent the best available data\n")
cat("2. Filter by regions='US' to ensure US-focused results\n")
cat("3. Use retention and user metrics which typically have US variants\n")