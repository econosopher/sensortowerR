# sensortowerR

## Overview
R package providing interface to Sensor Tower API with flexible time granularity options.

## Installation
```r
devtools::install_github("peterparkerspicklepatch/sensortowerR")
```

## Usage
```r
library(sensortowerR)

# Fetch daily metrics
data <- fetch_sensor_tower_metrics(
  auth_token = "your_token",
  app_id = "123",
  start_date = "2024-01-01",
  end_date = "2024-01-31",
  app_name = "GameName",
  grain = "daily"
)
```
