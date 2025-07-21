# Simple example: Create a dashboard with one line of code after st_top_charts()

library(sensortowerR)

# Step 1: Get top charts data (using cached data for this example)
top_rpgs <- readRDS("inst/cache/top_rpgs_cache.rds")

# Step 2: Create dashboard with ONE line of code!
st_gt_dashboard(top_rpgs)

# That's it! The function automatically:
# - Ranks games by 180-day revenue
# - Adds bar charts to numeric columns
# - Applies heatmaps to retention metrics
# - Formats billions with 2 decimal places
# - Adds country flags
# - Styles with FiveThirtyEight theme

# Want a simpler version without all the styling?
st_gt_dashboard(top_rpgs, raw = TRUE)

# Want to customize it?
st_gt_dashboard(
  top_rpgs,
  title = "Top Mobile RPGs Q4 2024",
  ranking_metric = "revenue_30d_ww",
  color_scheme = list(
    revenue = "#E74C3C",     # Red
    downloads = "#3498DB",   # Blue
    engagement = "#9B59B6"   # Purple
  ),
  save_path = "my_custom_dashboard.png"
)