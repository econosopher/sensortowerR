# Global variable declarations to avoid R CMD check warnings
# This file is loaded last (zzz is conventional name for this purpose)

# Declare global variables used in dplyr pipelines and other non-standard evaluation contexts
utils::globalVariables(c(
  # st_ytd_metrics variables
  "country", "daily_users", "total_user_days", "n_days", "dau",
  "weekly_users", "total_user_weeks", "n_weeks", "wau",
  "monthly_users", "total_user_months", "n_months", "mau",
  "entity_id", "entity_name", "date_start", "date_end", "metric", "value",
  
  # st_metrics variables  
  "total_revenue", "total_downloads", "revenue", "downloads", "platform",
  
  # st_top_publishers variables
  "revenue_absolute", "apps"
))