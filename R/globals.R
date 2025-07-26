# Global variables used in the package
utils::globalVariables(c(
  # Common tidyverse variables
  ".",
  
  # Variables from API responses
  "category_id",
  "name",
  "user_rating",
  "user_rating_count",
  "score",
  "developer",
  "developer_id",
  "developer_email",
  "developer_address",
  "iphone_revenue_cents",
  "ipad_revenue_cents",
  "iphone_downloads",
  "ipad_downloads",
  "iphone_revenue",
  "ipad_revenue",
  "revenue_cents",
  
  # Variables from custom tags
  "entities.custom_tags.Genders (Last Quarter, WW)",
  "revenue_30d_ww",
  "revenue_alltime_ww",
  "genders_ww",
  "unified_app_name",
  
  # gt function names that are used without namespace prefix in some contexts
  "cell_text",
  "cells_body",
  "px",
  "text_transform"
))