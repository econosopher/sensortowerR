# Global variables used in the package
# These are variables used in NSE contexts (dplyr, tidyselect) that
# R CMD check would otherwise flag as undefined global variables
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

  # Variables used in dplyr/tidyr operations
  "android_id",
  "android_only",
  "app_id",
  "both_id",
  "custom_fields",
  "ios_id",
  "ios_only",
  "original_id",
  "prev_value",
  "true_unified_id",
  "unified_app_id",
  "unified_id",
  "unified_revenue",
  "users_absolute",
  "values",

  # gt function names that are used without namespace prefix in some contexts
  "cell_text",
  "cells_body",
  "px",
  "text_transform",

  # Variables used in st_publisher_portfolio
  "aggregate_tags.Game Sub-genre",
  "global_rank",
  "subgenre_rank",
  "Month",
  "Year",
  "subgenre",
  "revenue",
  "downloads",
  "mau",
  "value"
))

# Import na.omit from stats
#' @importFrom stats na.omit
NULL