# sensortowerR

## Overview
R package providing interface to Sensor Tower API with flexible time granularity options.

## Installation
\`\`\`r
devtools::install_github("peterparkerspicklepatch/sensortowerR")
\`\`\`

## Usage
\`\`\`r
library(sensortowerR)

# Fetch metrics using unified app ID
data <- fetch_sensor_tower_metrics(
  auth_token = "your_token",
  unified_app_id = "123",
  start_date = "2024-01-01",
  end_date = "2024-01-31",
  grain = "daily"
)
\`\`\`

## Functions

### fetch_sensor_tower_metrics

Fetch Sensor Tower metrics using unified app ID.

#### Parameters
- \`auth_token\`: Sensor Tower API authentication token
- \`unified_app_id\`: Unified App ID from Sensor Tower
- \`start_date\`: Start date for data collection (YYYY-MM-DD)
- \`end_date\`: End date for data collection (YYYY-MM-DD)
- \`grain\`: Granularity of data (daily, weekly, or monthly)

#### Returns
A data frame containing the metrics
