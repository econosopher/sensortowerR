# Verification Script for SensorTowerR

library(sensortowerR)

# Check if auth token is present
if (Sys.getenv("SENSORTOWER_AUTH_TOKEN") == "") {
    stop("SENSORTOWER_AUTH_TOKEN is not set. Please set it to run verification.")
}

message("Starting verification...")

# 1. Fetch metrics for a known app (Candy Crush iOS)
message("\n1. Fetching metrics for Candy Crush (iOS)...")
tryCatch(
    {
        metrics <- st_metrics(
            os = "ios",
            ios_app_id = "553834731",
            countries = "US",
            date_granularity = "monthly",
            start_date = "2024-01-01",
            end_date = "2024-01-31"
        )
        print(head(metrics))
        if (nrow(metrics) > 0) message("SUCCESS: Metrics fetched.") else message("WARNING: No data returned.")
    },
    error = function(e) {
        message("FAILURE: ", e$message)
    }
)

# 2. Test Top Charts
message("\n2. Fetching Top Charts (Revenue, US, Games)...")
tryCatch(
    {
        charts <- st_top_charts(
            os = "ios",
            measure = "revenue",
            category = 6014,
            regions = "US",
            limit = 5
        )
        print(head(charts))
        if (nrow(charts) > 0) message("SUCCESS: Top charts fetched.") else message("WARNING: No data returned.")
    },
    error = function(e) {
        message("FAILURE: ", e$message)
    }
)

message("\nVerification complete.")
