# Comprehensive Analysis of st_top_charts API Response
# This example demonstrates the rich data structure returned by the Sensor Tower API
# when using st_top_charts with custom filters

library(sensortowerR)
suppressPackageStartupMessages(library(dplyr))

# Example URL from Sensor Tower web interface
# This URL includes a custom filter and requests DAU (Daily Active Users) data
test_url <- "https://app.sensortower.com/market-analysis/top-apps?os=unified&edit=1&granularity=weekly&start_date=2025-06-29&end_date=2025-07-28&duration=P30D&measure=DAU&comparison_attribute=absolute&category=0&device=iphone&device=ipad&device=android&metric=revenue&page=1&page_size=25&custom_fields_filter_mode=include_unified_apps&period=day&custom_fields_filter_id=60746340241bc16eb8a65d76&country=US&country=CA&country=AU&country=NZ&country=GB&country=CH&country=SE&country=NO&country=NL&country=LU&country=IE&country=DE&country=FR&country=FI&country=DK&country=BE&country=AT&country=ES&country=IT&country=PT"

# Parse URL and make API call
cat("=== SENSOR TOWER API RESPONSE ANALYSIS ===\n\n")
params <- st_parse_web_url(test_url, verbose = FALSE)
data <- suppressMessages(do.call(st_top_charts, params))

# 1. RESPONSE OVERVIEW
cat("1. RESPONSE OVERVIEW\n")
cat(paste(rep("-", 50), collapse = ""), "\n")
cat("Total apps returned:", nrow(data), "\n")
cat("Total data columns:", ncol(data), "\n")
cat("Primary metric:", params$measure, "(Daily Active Users)\n")
cat("Date range:", params$date, "to", params$end_date, "\n")
cat("Countries:", length(strsplit(params$regions, ",")[[1]]), "countries\n")
cat("Custom filter applied:", if(!is.null(params$custom_fields_filter_id)) "Yes" else "No", "\n\n")

# 2. TOP APPS BY PRIMARY METRIC (DAU)
cat("2. TOP 10 APPS BY DAILY ACTIVE USERS\n")
cat(paste(rep("-", 50), collapse = ""), "\n")
top_apps <- data %>% 
  select(
    Rank = rank,
    App = unified_app_name,
    Publisher = publisher_name,
    DAU = entities.users_absolute
  ) %>%
  arrange(desc(DAU)) %>%
  mutate(DAU = format(round(DAU), big.mark = ",")) %>%
  head(10)
print(top_apps, row.names = FALSE)

# 3. DATA CATEGORIES BREAKDOWN
cat("\n3. DATA CATEGORIES (", ncol(data), " columns total)\n", sep = "")
cat(paste(rep("-", 50), collapse = ""), "\n")

# Count columns by category
categories <- list(
  "Core App Info" = length(grep("^(unified_app_|publisher_|rank)", names(data))),
  "Entity Metrics" = length(grep("^entities\\.", names(data))),
  "Custom Tags" = length(grep("custom_tags", names(data))),
  "Revenue Data" = length(grep("revenue", names(data), ignore.case = TRUE)),
  "Download Data" = length(grep("download|units", names(data), ignore.case = TRUE)),
  "User Metrics" = length(grep("dau|wau|mau|users", names(data), ignore.case = TRUE)),
  "Retention Rates" = length(grep("retention", names(data), ignore.case = TRUE)),
  "Monetization" = length(grep("rpd|arpu|arpdau", names(data), ignore.case = TRUE)),
  "Demographics" = length(grep("age|gender|male|female", names(data), ignore.case = TRUE)),
  "Ratings" = length(grep("rating", names(data), ignore.case = TRUE)),
  "Dates" = length(grep("date|release", names(data), ignore.case = TRUE))
)

for (cat_name in names(categories)) {
  cat(sprintf("%-20s: %3d columns\n", cat_name, categories[[cat_name]]))
}

# 4. RICH METRICS FOR TOP APP
cat("\n4. SAMPLE METRICS FOR TOP APP\n")
cat(paste(rep("-", 50), collapse = ""), "\n")
top_app <- data[which.max(data$entities.users_absolute), ]
cat("App:", top_app$unified_app_name, "\n")
cat("Publisher:", top_app$publisher_name, "\n")
cat("App ID:", top_app$unified_app_id, "\n\n")

# Display key metrics
metrics <- list(
  "Daily Active Users (DAU)" = list(value = top_app$entities.users_absolute, format = "number"),
  "30-Day Revenue (WW)" = list(value = top_app$revenue_30d_ww, format = "currency"),
  "30-Day Downloads (WW)" = list(value = top_app$downloads_30d_ww, format = "number"),
  "All-Time Revenue (WW)" = list(value = top_app$revenue_alltime_ww, format = "currency"),
  "All-Time Downloads (WW)" = list(value = top_app$downloads_alltime_ww, format = "number"),
  "7-Day Retention (US)" = list(value = top_app$retention_7d_us, format = "percent"),
  "30-Day Retention (US)" = list(value = top_app$retention_30d_us, format = "percent"),
  "Revenue Per Download (US)" = list(value = top_app$rpd_alltime_us, format = "currency"),
  "Monthly Active Users (WW)" = list(value = top_app$mau_month_ww, format = "number"),
  "Average Age (US)" = list(value = top_app$age_us, format = "decimal")
)

cat("Key Performance Metrics:\n")
for (metric_name in names(metrics)) {
  metric <- metrics[[metric_name]]
  if (!is.null(metric$value) && !is.na(metric$value)) {
    formatted_value <- switch(metric$format,
      "currency" = paste0("$", format(round(metric$value), big.mark = ",")),
      "number" = format(round(metric$value), big.mark = ","),
      "percent" = sprintf("%.1f%%", metric$value * 100),
      "decimal" = sprintf("%.1f", metric$value)
    )
    cat(sprintf("  %-30s: %s\n", metric_name, formatted_value))
  }
}

# 5. CUSTOM FILTER ANALYSIS
cat("\n5. CUSTOM FILTER ANALYSIS\n")
cat(paste(rep("-", 50), collapse = ""), "\n")
if (!is.null(params$custom_fields_filter_id)) {
  cat("Filter ID:", params$custom_fields_filter_id, "\n")
  cat("Filter Mode:", params$custom_tags_mode, "\n")
  cat("Apps returned:", nrow(data), "(filtered from thousands)\n\n")
  
  # Publisher distribution
  cat("Publisher Distribution (Top 5):\n")
  pub_count <- data %>% 
    count(publisher_name) %>% 
    arrange(desc(n)) %>%
    head(5)
  
  for (i in 1:nrow(pub_count)) {
    cat(sprintf("  %-30s: %d apps\n", pub_count$publisher_name[i], pub_count$n[i]))
  }
} else {
  cat("No custom filter applied\n")
}

# 6. GEOGRAPHIC COVERAGE
cat("\n6. GEOGRAPHIC COVERAGE\n")
cat(paste(rep("-", 50), collapse = ""), "\n")
countries <- strsplit(params$regions, ",")[[1]]
cat("Countries included (", length(countries), "):\n", sep = "")
cat(paste(strwrap(paste(countries, collapse = ", "), width = 70), collapse = "\n"), "\n")

# 7. EXAMPLE COLUMN STRUCTURE
cat("\n7. COLUMN NAMING STRUCTURE\n")
cat(paste(rep("-", 50), collapse = ""), "\n")
cat("The API returns nested JSON that gets flattened using dot notation:\n\n")
example_cols <- c(
  "unified_app_name" = "App name (unified across platforms)",
  "entities.users_absolute" = "Primary metric value (DAU in this case)",
  "entities.delta" = "Change from previous period",
  "entities.custom_tags.Revenue" = "Custom account-specific metrics",
  "revenue_30d_ww" = "30-day revenue worldwide",
  "retention_7d_us" = "7-day retention rate in US"
)

for (col in names(example_cols)) {
  cat(sprintf("  %-35s: %s\n", col, example_cols[col]))
}

# 8. DAU STATISTICS
cat("\n8. DAILY ACTIVE USERS STATISTICS\n")
cat(paste(rep("-", 50), collapse = ""), "\n")
dau_stats <- data %>%
  summarise(
    Min = min(entities.users_absolute, na.rm = TRUE),
    Q1 = quantile(entities.users_absolute, 0.25, na.rm = TRUE),
    Median = median(entities.users_absolute, na.rm = TRUE),
    Mean = mean(entities.users_absolute, na.rm = TRUE),
    Q3 = quantile(entities.users_absolute, 0.75, na.rm = TRUE),
    Max = max(entities.users_absolute, na.rm = TRUE)
  )

cat("DAU Distribution:\n")
cat(sprintf("  Minimum:    %s\n", format(round(dau_stats$Min), big.mark = ",")))
cat(sprintf("  Q1:         %s\n", format(round(dau_stats$Q1), big.mark = ",")))
cat(sprintf("  Median:     %s\n", format(round(dau_stats$Median), big.mark = ",")))
cat(sprintf("  Mean:       %s\n", format(round(dau_stats$Mean), big.mark = ",")))
cat(sprintf("  Q3:         %s\n", format(round(dau_stats$Q3), big.mark = ",")))
cat(sprintf("  Maximum:    %s\n", format(round(dau_stats$Max), big.mark = ",")))

# 9. DATA EXPORT OPTIONS
cat("\n9. DATA EXPORT\n")
cat(paste(rep("-", 50), collapse = ""), "\n")
cat("Saving data for further analysis:\n")

# Save full dataset
saveRDS(data, "top_charts_api_response.rds")
cat("  - Full dataset saved to: top_charts_api_response.rds\n")

# Save column names
writeLines(names(data), "api_column_names.txt")
cat("  - Column names saved to: api_column_names.txt\n")

# Save top apps summary
write.csv(top_apps, "top_apps_by_dau.csv", row.names = FALSE)
cat("  - Top apps summary saved to: top_apps_by_dau.csv\n")

# 10. SUMMARY
cat("\n10. SUMMARY\n")
cat(paste(rep("=", 50), collapse = ""), "\n")
cat("This API response demonstrates the comprehensive data available:\n\n")
cat("✓", nrow(data), "apps matching the custom filter\n")
cat("✓", ncol(data), "data points per app\n")
cat("✓ Primary metric: Daily Active Users (DAU)\n")
cat("✓ Coverage: Revenue, downloads, retention, demographics\n")
cat("✓ Time periods: 30-day, 180-day, all-time metrics\n")
cat("✓ Geographic scope:", length(countries), "countries\n")
cat("✓ Custom metrics specific to your account/filter\n")

cat("\nThe flattened structure makes it easy to analyze in R using\n")
cat("standard data manipulation tools like dplyr and ggplot2.\n")