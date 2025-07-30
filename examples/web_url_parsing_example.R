# Example: Parsing Sensor Tower Web URLs for API Use
# This example shows how to convert web interface URLs to API parameters

library(sensortowerR)
library(dplyr)

# Example 1: Parse the complex URL provided
cat("=== Example 1: Complex Web URL with Multiple Countries ===\n")

# This is a real URL from the Sensor Tower web interface
complex_url <- "https://app.sensortower.com/market-analysis/top-apps?os=unified&edit=1&granularity=weekly&start_date=2025-06-29&end_date=2025-07-28&duration=P30D&measure=DAU&comparison_attribute=absolute&country=US&country=AU&country=CA&country=CN&country=FR&country=DE&country=GB&country=IT&country=JP&country=RU&country=KR&category=0&device=iphone&device=ipad&device=android&metric=downloads&page=1&page_size=25&custom_fields_filter_mode=include_unified_apps&period=day"

# Parse the URL
params <- st_parse_web_url(complex_url)

# Show the resulting parameters
cat("\nAPI Parameters extracted:\n")
str(params)

# Example 2: Use the parsed parameters in an API call
cat("\n=== Example 2: Using Parsed Parameters ===\n")

tryCatch({
  # Make the API call with parsed parameters
  data <- do.call(st_top_charts, params)
  
  if (nrow(data) > 0) {
    cat("\nSuccessfully retrieved", nrow(data), "apps\n")
    cat("Top 5 apps by", params$measure, ":\n")
    print(data %>% 
            select(unified_app_name, all_of(params$measure)) %>%
            head(5))
  }
}, error = function(e) {
  cat("Error making API call:", e$message, "\n")
  cat("This might be due to:\n")
  cat("- Invalid authentication\n")
  cat("- Rate limiting\n")
  cat("- Invalid parameter combinations\n")
})

# Example 3: Extract and analyze URL parameters
cat("\n=== Example 3: Detailed Parameter Analysis ===\n")

params_df <- st_extract_url_params(complex_url)
print(params_df)

# Show country analysis
countries_in_url <- strsplit(params_df[params_df$parameter == "country", "value"], ", ")[[1]]
cat("\nCountry Analysis:\n")
cat("- Total countries specified:", length(countries_in_url), "\n")
cat("- First 10 countries:", paste(head(countries_in_url, 10), collapse = ", "), "\n")
cat("- Recommendation: Use 'WW' for worldwide instead of listing all countries\n")

# Example 4: Simpler URL parsing
cat("\n=== Example 4: Simple URL Example ===\n")

simple_url <- "https://app.sensortower.com/market-analysis/top-apps?os=ios&measure=revenue&category=6014&country=US&granularity=monthly"

simple_params <- st_parse_web_url(simple_url, verbose = FALSE)
cat("\nSimple URL parsed to:\n")
print(unlist(simple_params))

# Example 5: Build a web URL from API parameters
cat("\n=== Example 5: Build Web URL from Parameters ===\n")

# Create a URL for viewing top games by revenue in US and UK
web_url <- st_build_web_url(
  os = "unified",
  measure = "revenue",
  category = 6014,  # Games
  regions = "US,GB",
  start_date = "2025-01-01",
  end_date = "2025-01-31"
)

cat("\nYou can visit this URL in your browser to see the same data in the web interface\n")

# Example 6: Handle special cases
cat("\n=== Example 6: Special Cases ===\n")

# URL with custom filter
filter_url <- "https://app.sensortower.com/market-analysis/top-apps?os=unified&custom_fields_filter_id=687df26ac5a19ebcfe817d7f&custom_fields_filter_mode=include_unified_apps"

filter_params <- st_parse_web_url(filter_url, verbose = FALSE)
cat("\nCustom filter parameters:\n")
cat("- Filter ID:", filter_params$custom_fields_filter_id, "\n")
cat("- Tags Mode:", filter_params$custom_tags_mode, "\n")

# Example 7: Parameter differences between web and API
cat("\n=== Example 7: Web vs API Parameter Differences ===\n")

cat("\nKey differences to remember:\n")
cat("1. Web 'page_size' -> API 'limit'\n")
cat("2. Web 'country' (multiple) -> API 'regions' (comma-separated)\n")
cat("3. Web 'granularity' -> API 'time_range'\n")
cat("4. Web 'custom_fields_filter_mode' -> API 'custom_tags_mode'\n")
cat("5. Web category='0' (all) -> API category=NULL\n")
cat("6. Web has UI parameters (edit, duration, period) ignored by API\n")

# Save parsed parameters for reuse
cat("\n=== Saving Parameters for Reuse ===\n")

# Save to file
saveRDS(params, "parsed_web_params.rds")
cat("Parameters saved to 'parsed_web_params.rds'\n")

# Load and reuse later
# saved_params <- readRDS("parsed_web_params.rds")
# data <- do.call(st_top_charts, saved_params)