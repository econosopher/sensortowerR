# Example showing the importance of date_start and date_end columns
# This demonstrates how different time_range values affect the actual period covered

library(sensortowerR)
library(dplyr)

# Example 1: Monthly data
cat("=== MONTHLY DATA ===\n")
monthly <- st_top_publishers(
  time_range = "month",
  date = "2025-06-15",  # Mid-month date
  limit = 1
)

cat("Input date: 2025-06-15\n")
cat("Actual period covered: ", monthly$date_start[1], " to ", monthly$date_end[1], "\n")
cat("That's a full month even though we specified mid-month!\n\n")

# Example 2: Weekly data  
cat("=== WEEKLY DATA ===\n")
weekly <- st_top_publishers(
  time_range = "week",
  date = "2025-06-15",  # Sunday
  limit = 1
)

cat("Input date: 2025-06-15 (Sunday)\n")
cat("Actual period covered: ", weekly$date_start[1], " to ", weekly$date_end[1], "\n")
cat("Covers Monday to Sunday of that week\n\n")

# Example 3: Quarterly data
cat("=== QUARTERLY DATA ===\n")
quarterly <- st_top_publishers(
  time_range = "quarter",
  date = "2025-04-01",  # Start of Q2
  limit = 1
)

cat("Input date: 2025-04-01\n")
cat("Actual period covered: ", quarterly$date_start[1], " to ", quarterly$date_end[1], "\n")
cat("That's the full Q2 2025!\n\n")

# Example 4: Why this matters for revenue calculations
cat("=== WHY THIS MATTERS ===\n")
cat("Without date_start and date_end columns:\n")
cat("- You might think monthly data for '2025-06-15' is just for that day\n")
cat("- You might not realize it's actually for June 1-30\n")
cat("- Revenue/download totals could be misinterpreted\n")
cat("\nWith date_start and date_end columns:\n")
cat("- Crystal clear what period the data covers\n")
cat("- No ambiguity about what's included\n")
cat("- Accurate comparisons and calculations\n")