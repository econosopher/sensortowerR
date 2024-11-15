# sensortowerR 📱📊

## Overview
`sensortowerR` is a comprehensive R package that provides a seamless interface to the Sensor Tower API, enabling data scientists and analysts to access mobile app market intelligence programmatically.

## Installation 🔧

```R
# Install from GitHub
devtools::install_github("peterparkerspicklepatch/sensortowerR")

# Load the package
library(sensortowerR)
```

## Authentication 🔑

Before using the package, set up your Sensor Tower API authentication:

```R
# Set environment variable
Sys.setenv(SENSORTOWER_AUTH="your_token_here")

# Or use in individual functions
search_entities(term = "Spotify", auth_token = "your_token_here")
```

## Core Functions 🛠️

### search_entities()

Search for apps or publishers across app stores.

#### Parameters:
- `term`: Search term (min 3 Latin chars or 2 non-Latin chars)
- `os`: Platform ("unified", "ios", "android")
- `entity_type`: Type of entity ("app" or "publisher")
- `limit`: Number of results (max 250)
- `auth_token`: Optional if set in environment

#### Examples:

```R
# Basic search
spotify_apps <- search_entities(term = "Spotify")

# Advanced search with parameters
netflix_apps <- search_entities(
  term = "Netflix",
  os = "ios",
  entity_type = "app",
  limit = 10
)

# Batch processing multiple terms
app_terms <- c("Spotify", "Netflix", "TikTok")
results <- lapply(app_terms, function(x) {
  search_entities(term = x, limit = 5)
})

# Process results
app_names <- lapply(results, function(x) {
  sapply(x, function(app) app$name)
})
```

### fetch_sensor_tower_metrics()

Retrieve detailed metrics for apps.

#### Parameters:
- `app_id`: Unified app ID
- `metrics`: Vector of desired metrics
- `date_range`: Time period for data

#### Examples:

```R
# Get basic metrics
metrics <- fetch_sensor_tower_metrics(
  app_id = "com.spotify.music",
  metrics = c("downloads", "revenue")
)

# Advanced metrics with date range
detailed_metrics <- fetch_sensor_tower_metrics(
  app_id = "com.netflix.mediaclient",
  metrics = c("downloads", "revenue", "dau"),
  date_range = "last_30_days"
)
```

### get_unified_app_id()

Convert store-specific app IDs to unified Sensor Tower IDs.

#### Parameters:
- `app_id`: Store-specific app ID
- `store`: App store ("ios" or "android")

#### Examples:

```R
# Get unified ID for Spotify
spotify_id <- get_unified_app_id(
  app_id = "com.spotify.music",
  store = "android"
)
```

## Data Processing Tips 💡

### Working with Results

```R
# Extract specific fields from search results
extract_app_info <- function(results) {
  lapply(results, function(x) {
    data.frame(
      name = x$name,
      publisher = x$publisher_name,
      downloads = x$global_downloads,
      stringsAsFactors = FALSE
    )
  })
}

# Combine multiple searches
combined_results <- do.call(rbind, 
  lapply(results, extract_app_info)
)
```

### Error Handling

```R
# Safe search wrapper
safe_search <- function(term, ...) {
  tryCatch({
    search_entities(term = term, ...)
  }, error = function(e) {
    warning(sprintf("Error searching for %s: %s", term, e$message))
    return(NULL)
  })
}
```

## Best Practices 🎯

1. **Rate Limiting**: Implement delays between requests for batch processing
2. **Error Handling**: Always wrap API calls in error handling code
3. **Data Validation**: Verify returned data structure before processing
4. **Authentication**: Store API tokens securely using environment variables

## Contributing 🤝

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit changes (`git commit -m "Add amazing feature"`)
4. Push to branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## Troubleshooting 🔍

Common issues and solutions:

1. **Authentication Errors**
   - Verify token is correctly set
   - Check token permissions
   - Ensure token is not expired

2. **Rate Limiting**
   - Implement delays between requests
   - Use batch processing wisely
   - Monitor API usage

3. **Data Issues**
   - Validate input parameters
   - Check return structures
   - Handle missing data appropriately

## License 📄

MIT License - see LICENSE file for details

## Support 💬

- Documentation: [Package Documentation](https://github.com/peterparkerspicklepatch/sensortowerR/wiki)
- Issues: [GitHub Issues](https://github.com/peterparkerspicklepatch/sensortowerR/issues)
- Email: team@julius.ai

---
Made with ❤️ by [Julius AI](https://julius.ai)
