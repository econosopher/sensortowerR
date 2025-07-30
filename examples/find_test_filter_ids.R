# Script to Help Find or Create Test Filter IDs
# This script provides methods to obtain valid filter IDs for testing

library(sensortowerR)

cat("=== Sensor Tower Custom Filter ID Helper ===\n\n")

# Method 1: Using the example filter ID from support
cat("## Method 1: Example Filter ID from Sensor Tower Support\n")
example_filter <- "687df26ac5a19ebcfe817d7f"
cat("Filter ID:", example_filter, "\n")
cat("Note: This may not work with your account\n\n")

# Test the example filter
cat("Testing example filter...\n")
result <- st_test_filter(example_filter, verbose = TRUE)

# Method 2: Generate URLs to create your own filters
cat("\n## Method 2: Create Your Own Filters\n")
cat("Generate URLs for creating filters in the Sensor Tower web interface:\n\n")

# Games filter URL
games_url <- st_build_filter_url(
  os = "ios",
  category = 6014,  # Games
  countries = c("US", "GB", "JP")
)

# Social apps filter URL  
social_url <- st_build_filter_url(
  os = "android",
  category = "SOCIAL",
  countries = "US"
)

# Method 3: Extract filter ID from existing URL
cat("\n## Method 3: Extract Filter ID from URL\n")
cat("If you have a Sensor Tower URL with filters, extract the ID:\n\n")

example_url <- "https://app.sensortower.com/top-charts?os=ios&category=6014&custom_fields_filter_id=687df26ac5a19ebcfe817d7f"
extracted_id <- st_extract_filter_id(example_url)

# Method 4: Generate example IDs for format testing
cat("\n## Method 4: Example IDs for Format Testing\n")
cat("These are randomly generated and won't work with the API:\n\n")

example_ids <- st_generate_example_filter_ids(5, seed = 42)
for (id in example_ids) {
  cat("  ", id, "\n")
}

# Method 5: Interactive filter discovery
cat("\n## Method 5: Interactive Filter Discovery\n")
cat("Try different categories to find working filters:\n\n")

# Common iOS categories to try
ios_categories <- list(
  Games = 6000,
  Entertainment = 6016,
  Social_Networking = 6005,
  Photo_Video = 6008,
  Education = 6017
)

# Function to test if any default filters work
test_category_filters <- function() {
  working_filters <- list()
  
  for (name in names(ios_categories)) {
    cat("Testing", name, "category...\n")
    
    result <- tryCatch(
      st_top_charts(
        os = "ios",
        category = ios_categories[[name]],
        regions = "US",
        limit = 1
      ),
      error = function(e) NULL
    )
    
    if (!is.null(result)) {
      cat("  ✅ Category", ios_categories[[name]], "works\n")
      working_filters[[name]] <- ios_categories[[name]]
    } else {
      cat("  ❌ Category", ios_categories[[name]], "failed\n")
    }
  }
  
  working_filters
}

# Run category test
cat("\nTesting standard categories (these should work):\n")
working_categories <- test_category_filters()

# Provide instructions for getting real filter IDs
cat("\n## Instructions for Getting Real Filter IDs\n")
cat("1. Log into your Sensor Tower account at app.sensortower.com\n")
cat("2. Navigate to Top Apps or Market Intelligence section\n")
cat("3. Apply filters using the web interface:\n")
cat("   - Select specific publishers\n")
cat("   - Choose custom tags\n")
cat("   - Set revenue/download thresholds\n")
cat("   - Configure any other criteria\n")
cat("4. Look at the browser URL after applying filters\n")
cat("5. Find 'custom_fields_filter_id=' in the URL\n")
cat("6. Copy the 24-character ID that follows\n")
cat("7. Use it in your R code with st_top_charts()\n")

# Save a template configuration file
cat("\n## Creating Filter Configuration File\n")

filter_config <- list(
  example_filter = list(
    id = example_filter,
    description = "Example filter from Sensor Tower support",
    created = Sys.Date()
  ),
  # Add your own filters here
  my_custom_filter = list(
    id = "YOUR_FILTER_ID_HERE",
    description = "Description of what this filter does",
    created = Sys.Date()
  )
)

# Save as YAML
if (requireNamespace("yaml", quietly = TRUE)) {
  yaml::write_yaml(filter_config, "sensor_tower_filters.yml")
  cat("Created template configuration file: sensor_tower_filters.yml\n")
  cat("Edit this file to add your own filter IDs\n")
}

# Summary
cat("\n=== Summary ===\n")
cat("1. The example filter ID may not work with your account\n")
cat("2. Create your own filters using the generated URLs\n")
cat("3. Standard category filters (without custom_fields_filter_id) should always work\n")
cat("4. Save your filter IDs in a configuration file for easy reuse\n")
cat("5. Use st_test_filter() to validate any filter ID before using it\n")