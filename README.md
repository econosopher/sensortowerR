# sensortowerR

R package for accessing Sensor Tower API data

## Installation

```R
devtools::install_github("peterparkerspicklepatch/sensortowerR")
```

## Available Functions

### search_entities
Search for apps or publishers in Sensor Tower. Supports both single searches and batch processing of multiple terms.

```R
search_entities(term = "Spotify")
# or with multiple terms
terms <- c("Spotify", "Netflix")
results <- lapply(terms, function(x) search_entities(term = x))
```

### fetch_sensor_tower_metrics
Fetch metrics from Sensor Tower API.

### get_unified_app_id
Get unified app ID for use with other Sensor Tower API functions.

## Authentication

Set your Sensor Tower API token as an environment variable:
```R
Sys.setenv(SENSORTOWER_AUTH="your_token_here")
```

## Examples

See function documentation for detailed examples and usage.
