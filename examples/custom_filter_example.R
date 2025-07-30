# Example: Using custom_fields_filter_id with Sensor Tower API
# Based on information from Sensor Tower support

library(sensortowerR)
library(dplyr)
library(ggplot2)

# This example demonstrates how to use the custom_fields_filter_id parameter
# to pull "Top Apps by Downloads and Revenue" using a pre-configured filter
# from the Sensor Tower web interface.

# The custom_fields_filter_id allows you to:
# 1. Configure complex filters in the Sensor Tower web interface
# 2. Save those filters and get a unique ID
# 3. Use that ID in API calls to apply the same filters

# Example custom filter ID provided by Sensor Tower support
# NOTE: This is a test ID that may not work with your account
# You need to create your own filter in the Sensor Tower web interface
custom_filter_id <- "687df26ac5a19ebcfe817d7f"

# Using the custom filter with st_top_charts
# Note: When using custom_fields_filter_id with os = "unified", 
# you must also provide custom_tags_mode
tryCatch({
  
  # Example 1: Using custom filter with unified OS
  cat("=== Example 1: Custom Filter with Unified OS ===\n")
  top_apps_unified <- st_top_charts(
    measure = "revenue",
    os = "unified",
    custom_fields_filter_id = custom_filter_id,
    custom_tags_mode = "include",  # Required when os = "unified"
    regions = "US",
    time_range = "month",
    limit = 10
  )
  
  if (nrow(top_apps_unified) > 0) {
    cat("\nTop apps by revenue (using custom filter):\n")
    print(top_apps_unified %>% 
            select(unified_app_name, revenue, downloads_30d_ww) %>%
            head())
  }
  
}, error = function(e) {
  cat("Error with unified OS example:", e$message, "\n")
})

# Example 2: Using custom filter with platform-specific OS
# When using iOS or Android specifically, custom_tags_mode is not required
tryCatch({
  
  cat("\n=== Example 2: Custom Filter with iOS ===\n")
  top_apps_ios <- st_top_charts(
    measure = "revenue",
    os = "ios",
    custom_fields_filter_id = custom_filter_id,
    regions = "US",
    time_range = "month",
    limit = 10
  )
  
  if (nrow(top_apps_ios) > 0) {
    cat("\nTop iOS apps by revenue (using custom filter):\n")
    print(top_apps_ios %>% 
            select(unified_app_name, revenue, retention_7d_us) %>%
            head())
  }
  
}, error = function(e) {
  cat("Error with iOS example:", e$message, "\n")
})

# Example 3: Using custom filter for downloads instead of revenue
tryCatch({
  
  cat("\n=== Example 3: Custom Filter for Downloads ===\n")
  top_apps_downloads <- st_top_charts(
    measure = "units",  # Downloads
    os = "android",
    custom_fields_filter_id = custom_filter_id,
    regions = "WW",  # Worldwide
    time_range = "week",
    limit = 20
  )
  
  if (nrow(top_apps_downloads) > 0) {
    cat("\nTop Android apps by downloads (using custom filter):\n")
    print(top_apps_downloads %>% 
            select(unified_app_name, downloads_absolute, dau_30d_ww) %>%
            head(10))
  }
  
}, error = function(e) {
  cat("Error with Android downloads example:", e$message, "\n")
})

# Example 4: Combining custom filter with date ranges
tryCatch({
  
  cat("\n=== Example 4: Custom Filter with Date Range ===\n")
  
  # Get data for a specific date
  specific_date <- Sys.Date() - 7  # 7 days ago
  
  historical_data <- st_top_charts(
    measure = "revenue",
    os = "ios",
    custom_fields_filter_id = custom_filter_id,
    regions = "US",
    time_range = "day",
    date = specific_date,
    limit = 15
  )
  
  if (nrow(historical_data) > 0) {
    cat("\nTop apps on", format(specific_date, "%Y-%m-%d"), "(using custom filter):\n")
    print(historical_data %>% 
            select(unified_app_name, revenue, rank) %>%
            head())
  }
  
}, error = function(e) {
  cat("Error with date range example:", e$message, "\n")
})

# Example 5: Visualizing data from custom filter
tryCatch({
  
  cat("\n=== Example 5: Visualizing Custom Filter Results ===\n")
  
  # Get top games using custom filter
  viz_data <- st_top_charts(
    measure = "revenue",
    os = "unified",
    custom_fields_filter_id = custom_filter_id,
    custom_tags_mode = "include",
    regions = "US",
    time_range = "month",
    limit = 10
  )
  
  if (nrow(viz_data) > 0 && "revenue" %in% names(viz_data)) {
    # Create a bar chart of top apps
    p <- ggplot(viz_data %>% head(10), 
                aes(x = reorder(unified_app_name, revenue), y = revenue/1e6)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = "Top Apps by Revenue (Custom Filter)",
           subtitle = paste("Filter ID:", custom_filter_id),
           x = "",
           y = "Revenue (Millions USD)") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 14),
            plot.subtitle = element_text(size = 10, color = "gray50"))
    
    print(p)
    
    # Save the plot
    ggsave("custom_filter_revenue_chart.png", p, width = 10, height = 6, dpi = 300)
    cat("\nChart saved as 'custom_filter_revenue_chart.png'\n")
  }
  
}, error = function(e) {
  cat("Error with visualization example:", e$message, "\n")
})

# Important notes about custom_fields_filter_id:
cat("\n=== Important Notes ===\n")
cat("1. The custom_fields_filter_id corresponds to filters you create in the Sensor Tower web interface\n")
cat("2. The filter ID in the URL after configuring filters can be used in the API\n")
cat("3. When os='unified', you must also provide custom_tags_mode (e.g., 'include' or 'exclude')\n")
cat("4. The filter ID provided (687df26ac5a19ebcfe817d7f) is a test example from Sensor Tower\n")
cat("5. Create your own filters at: https://app.sensortower.com/ and copy the ID from the URL\n")

# Example 6: Alternative - Using regular category filter (always works)
# This example shows how st_top_charts works without custom_fields_filter_id
cat("\n=== Example 6: Standard Usage Without Custom Filter ===\n")
tryCatch({
  
  # This will work with any valid Sensor Tower account
  standard_top_apps <- st_top_charts(
    measure = "revenue",
    os = "ios",
    category = 6014,  # Games category
    regions = "US",
    time_range = "month",
    limit = 5
  )
  
  if (nrow(standard_top_apps) > 0) {
    cat("\nTop iOS games by revenue (standard category filter):\n")
    print(standard_top_apps %>% 
            select(unified_app_name, revenue, retention_7d_us) %>%
            head())
    
    cat("\nThis demonstrates that st_top_charts works normally with category parameter.\n")
    cat("When you have a valid custom_fields_filter_id, you can use it instead of category.\n")
  }
  
}, error = function(e) {
  cat("Error with standard category example:", e$message, "\n")
})

# How to get your own custom filter ID:
cat("\n=== How to Get Your Own Filter ID ===\n")
cat("1. Log into Sensor Tower web interface\n")
cat("2. Navigate to Top Apps section\n")
cat("3. Configure your desired filters (categories, publishers, keywords, etc.)\n")
cat("4. Look at the URL - it will contain 'custom_fields_filter_id=YOUR_ID'\n")
cat("5. Copy that ID and use it in your API calls\n")