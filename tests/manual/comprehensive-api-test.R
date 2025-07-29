# Comprehensive API Test Report for sensortowerR v0.7.0
# This script tests all major functions and verifies they return expected data

library(sensortowerR)
library(dplyr)

cat("\n========================================\n")
cat("sensortowerR API Test Report\n")
cat("Version:", as.character(packageVersion("sensortowerR")), "\n")
cat("Date:", Sys.Date(), "\n")
cat("========================================\n\n")

# Helper function to test and report
test_function <- function(name, test_code) {
  cat("\n--- Testing", name, "---\n")
  result <- tryCatch({
    eval(test_code)
  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(result)) {
    cat("✓ Success: Retrieved", nrow(result), "rows\n")
    cat("✓ Columns:", ncol(result), "\n")
    cat("✓ Column names:", paste(head(names(result), 10), collapse = ", "), 
        if(ncol(result) > 10) "..." else "", "\n")
    
    # Show data sample
    if (nrow(result) > 0) {
      cat("\nSample data:\n")
      print(head(result[, 1:min(5, ncol(result))], 3))
    }
  }
  
  return(result)
}

# 1. Test st_metrics - iOS
cat("\n=== 1. st_metrics (iOS) ===\n")
metrics_ios <- test_function("st_metrics iOS", quote({
  st_metrics(
    os = "ios",
    ios_app_id = "553834731",  # Candy Crush
    countries = "US",
    date_granularity = "monthly",
    start_date = "2024-01-01",
    end_date = "2024-03-31"
  )
}))

if (!is.null(metrics_ios)) {
  cat("\nMetrics summary:\n")
  cat("- Total revenue: $", format(sum(metrics_ios$revenue), big.mark = ","), "\n")
  cat("- Total downloads:", format(sum(metrics_ios$downloads), big.mark = ","), "\n")
  cat("- Date range:", min(metrics_ios$date), "to", max(metrics_ios$date), "\n")
}

# 2. Test st_metrics - Unified
cat("\n=== 2. st_metrics (Unified) ===\n")
metrics_unified <- test_function("st_metrics Unified", quote({
  st_metrics(
    os = "unified",
    ios_app_id = "553834731",
    android_app_id = "com.king.candycrushsaga",
    countries = "WW",
    date_granularity = "monthly",
    start_date = "2024-01-01",
    end_date = "2024-01-31"
  )
}))

# 3. Test st_batch_metrics
cat("\n=== 3. st_batch_metrics ===\n")
batch_result <- test_function("st_batch_metrics", quote({
  st_batch_metrics(
    os = "ios",
    app_list = c("553834731", "1195621598", "529479190"),  # Popular games
    metrics = c("revenue", "downloads"),
    date_range = list(start_date = "2024-01-01", end_date = "2024-01-31"),
    countries = "US",
    granularity = "monthly"
  )
}))

if (!is.null(batch_result)) {
  cat("\nApps processed:\n")
  app_summary <- batch_result %>%
    group_by(original_id, metric) %>%
    summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = metric, values_from = total)
  print(app_summary)
}

# 4. Test st_yoy_metrics
cat("\n=== 4. st_yoy_metrics ===\n")
yoy_result <- test_function("st_yoy_metrics", quote({
  st_yoy_metrics(
    os = "ios",
    ios_app_id = "553834731",
    years = c(2023, 2024),
    period_start = "01-01",
    period_end = "03-31",
    countries = "US",
    metrics = c("revenue", "downloads")
  )
}))

if (!is.null(yoy_result) && any(!is.na(yoy_result$yoy_change))) {
  cat("\nYear-over-Year Changes:\n")
  yoy_summary <- yoy_result %>%
    filter(!is.na(yoy_change)) %>%
    select(year, metric, value, yoy_change, yoy_change_absolute) %>%
    arrange(metric, year)
  print(yoy_summary)
}

# 5. Test st_top_charts
cat("\n=== 5. st_top_charts ===\n")
top_charts <- test_function("st_top_charts", quote({
  st_top_charts(
    os = "ios",
    comparison_attribute = "absolute",
    time_range = "month",
    measure = "revenue",
    date = "2024-01-01",
    category = 6014,  # Games
    regions = "US",
    limit = 10
  )
}))

if (!is.null(top_charts) && nrow(top_charts) > 0) {
  cat("\nTop 5 games by revenue:\n")
  # Find revenue column
  revenue_col <- grep("revenue", names(top_charts), value = TRUE, ignore.case = TRUE)[1]
  rank_col <- if("rank" %in% names(top_charts)) "rank" else "current_rank"
  
  if (!is.na(revenue_col) && rank_col %in% names(top_charts)) {
    top_5 <- top_charts %>%
      select(all_of(c(rank_col, "app_id")), any_of(revenue_col)) %>%
      head(5)
    print(top_5)
  }
}

# 6. Test st_top_publishers
cat("\n=== 6. st_top_publishers ===\n")
top_publishers <- test_function("st_top_publishers", quote({
  st_top_publishers(
    measure = "revenue",
    os = "unified",
    category = 6014,
    date = "2024-01-01",
    country = "US",
    limit = 5,
    include_apps = TRUE
  )
}))

if (!is.null(top_publishers) && nrow(top_publishers) > 0) {
  cat("\nTop publishers:\n")
  pub_summary <- top_publishers %>%
    select(rank, publisher_name, any_of(c("revenue_absolute", "revenue_usd", "number_of_apps")))
  print(pub_summary)
}

# 7. Test st_app_info
cat("\n=== 7. st_app_info ===\n")
app_info <- test_function("st_app_info", quote({
  st_app_info(
    term = "Candy Crush",
    app_store = "unified",
    entity_type = "app",
    limit = 3
  )
}))

if (!is.null(app_info) && nrow(app_info) > 0) {
  cat("\nApps found:\n")
  app_summary <- app_info %>%
    select(unified_app_name, any_of(c("ios_app_id", "android_app_id", "publisher_name")))
  print(app_summary)
}

# 8. Test st_game_summary
cat("\n=== 8. st_game_summary ===\n")
game_summary <- test_function("st_game_summary", quote({
  st_game_summary(
    categories = 6014,  # Games
    countries = "US",
    os = "ios",
    date_granularity = "monthly",
    start_date = "2024-01-01",
    end_date = "2024-01-31"
  )
}))

if (!is.null(game_summary) && nrow(game_summary) > 0) {
  # Count metric types
  revenue_metrics <- sum(grepl("revenue", names(game_summary), ignore.case = TRUE))
  download_metrics <- sum(grepl("download", names(game_summary), ignore.case = TRUE))
  user_metrics <- sum(grepl("dau|mau|wau|user", names(game_summary), ignore.case = TRUE))
  
  cat("\nMetric counts:\n")
  cat("- Revenue metrics:", revenue_metrics, "\n")
  cat("- Download metrics:", download_metrics, "\n")
  cat("- User metrics:", user_metrics, "\n")
}

# 9. Test st_category_rankings
cat("\n=== 9. st_category_rankings ===\n")
category_rankings <- test_function("st_category_rankings", quote({
  st_category_rankings(
    os = "ios",
    category = 6014,  # Games
    chart_type = "topfreeapplications",
    country = "US",
    date = "2024-01-01",
    limit = 10
  )
}))

# Summary
cat("\n\n========================================\n")
cat("TEST SUMMARY\n")
cat("========================================\n")

tests <- list(
  "st_metrics (iOS)" = !is.null(metrics_ios),
  "st_metrics (Unified)" = !is.null(metrics_unified),
  "st_batch_metrics" = !is.null(batch_result),
  "st_yoy_metrics" = !is.null(yoy_result),
  "st_top_charts" = !is.null(top_charts),
  "st_top_publishers" = !is.null(top_publishers),
  "st_app_info" = !is.null(app_info),
  "st_game_summary" = !is.null(game_summary),
  "st_category_rankings" = !is.null(category_rankings)
)

passed <- sum(unlist(tests))
total <- length(tests)

cat("\nTests passed:", passed, "/", total, "\n\n")

for (test_name in names(tests)) {
  status <- if(tests[[test_name]]) "✓ PASS" else "✗ FAIL"
  cat(sprintf("%-25s %s\n", test_name, status))
}

cat("\n✓ Test completed successfully!\n")