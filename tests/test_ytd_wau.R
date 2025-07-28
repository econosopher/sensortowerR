# Test WAU functionality in st_ytd_metrics
library(sensortowerR)
library(dplyr)
library(tidyr)

# Setup
AUTH_TOKEN <- Sys.getenv("SENSORTOWER_AUTH_TOKEN")
if (AUTH_TOKEN == "") stop("Set SENSORTOWER_AUTH_TOKEN")

cat("=== TESTING WAU IN ST_YTD_METRICS ===\n")
cat(sprintf("Date: %s\n\n", Sys.Date()))

# Test 1: Single app WAU for current year
cat("1. Testing single app WAU\n")
cat("=" , strrep("=", 50), "\n")

wau_single <- st_ytd_metrics(
  unified_app_id = "553834731",  # Candy Crush
  years = 2025,
  metrics = "wau",
  verbose = TRUE
)

if (nrow(wau_single) > 0) {
  cat("\nResults:\n")
  print(wau_single)
  cat(sprintf("\nAverage WAU: %s\n", format(round(wau_single$value[1]), big.mark = ",")))
}

# Test 2: Multiple apps WAU
cat("\n\n2. Testing multiple apps WAU\n")
cat("=" , strrep("=", 50), "\n")

wau_multi <- st_ytd_metrics(
  ios_app_id = c("553834731", "1195621598"),
  android_app_id = c("com.king.candycrushsaga", "com.playrix.homescapes"),
  years = 2025,
  metrics = "wau",
  verbose = TRUE
)

if (nrow(wau_multi) > 0) {
  cat("\nResults by app:\n")
  wau_multi %>%
    arrange(entity_id) %>%
    print()
}

# Test 3: WAU with other metrics
cat("\n\n3. Testing WAU combined with other metrics\n")
cat("=" , strrep("=", 50), "\n")

all_metrics <- st_ytd_metrics(
  unified_app_id = "1053012308",  # MONOPOLY GO!
  years = 2025,
  metrics = c("revenue", "downloads", "dau", "wau"),
  verbose = TRUE
)

if (nrow(all_metrics) > 0) {
  cat("\nAll metrics summary:\n")
  all_metrics %>%
    pivot_wider(names_from = metric, values_from = value) %>%
    print()
  
  # Calculate DAU/WAU ratio
  dau_val <- all_metrics %>% filter(metric == "dau") %>% pull(value)
  wau_val <- all_metrics %>% filter(metric == "wau") %>% pull(value)
  
  if (length(dau_val) > 0 && length(wau_val) > 0) {
    ratio <- dau_val / wau_val
    cat(sprintf("\nDAU/WAU ratio: %.2f (%.1f%% daily engagement)\n", 
                ratio, ratio * 100))
  }
}

# Test 4: Multi-year WAU comparison
cat("\n\n4. Testing multi-year WAU comparison\n")
cat("=" , strrep("=", 50), "\n")

wau_years <- st_ytd_metrics(
  unified_app_id = "553834731",
  years = c(2024, 2025),
  metrics = "wau",
  period_start = "01-01",
  period_end = "07-21",  # Same period for both years
  verbose = TRUE
)

if (nrow(wau_years) > 0) {
  cat("\nYear-over-year WAU comparison:\n")
  wau_comparison <- wau_years %>%
    pivot_wider(names_from = year, values_from = value)
  
  print(wau_comparison)
  
  if (ncol(wau_comparison) >= 5) {  # Has both years
    yoy_change <- (wau_comparison$`2025` - wau_comparison$`2024`) / wau_comparison$`2024` * 100
    cat(sprintf("\nYoY WAU change: %.1f%%\n", yoy_change))
  }
}

# Test 5: Custom period WAU
cat("\n\n5. Testing custom period WAU (Q2)\n")
cat("=" , strrep("=", 50), "\n")

q2_wau <- st_ytd_metrics(
  unified_app_id = "553834731",
  years = 2025,
  period_start = "04-01",
  period_end = "06-30",
  metrics = "wau",
  verbose = TRUE
)

if (nrow(q2_wau) > 0) {
  cat("\nQ2 2025 WAU:\n")
  print(q2_wau)
}

cat("\n\n=== WAU IMPLEMENTATION SUMMARY ===\n")
cat("1. WAU is fetched using time_period='week' from Active Users endpoint\n")
cat("2. Values are averaged across all weeks in the period\n")
cat("3. iOS WAU = iPhone users + iPad users\n")
cat("4. Android WAU = users field\n")
cat("5. Multiple platforms are automatically combined\n")
cat("6. Caching minimizes redundant API calls\n")
cat("7. WAU enables week-over-week engagement analysis\n")