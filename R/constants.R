# --- Constants for sensortowerR ---

#' Metric Mapping for Custom Tags
#' 
#' Maps raw API field names (from entities.custom_tags or aggregate_tags)
#' to cleaner, user-friendly column names.
#' 
#' @keywords internal
METRIC_MAPPING <- c(
  # Performance metrics
  "aggregate_tags.Last 180 Days Downloads (WW)" = "downloads_180d_ww",
  "aggregate_tags.Last 180 Days Revenue (WW)" = "revenue_180d_ww", 
  "aggregate_tags.Last 30 Days Average DAU (US)" = "dau_30d_us",
  "aggregate_tags.Last 30 Days Average DAU (WW)" = "dau_30d_ww",
  "aggregate_tags.Last 30 Days Downloads (WW)" = "downloads_30d_ww",
  "aggregate_tags.Last 30 Days Revenue (WW)" = "revenue_30d_ww",
  "aggregate_tags.Last 4 Weeks Average WAU (US)" = "wau_4w_us",
  "aggregate_tags.Last 4 Weeks Average WAU (WW)" = "wau_4w_ww",
  "aggregate_tags.Last Month Average MAU (US)" = "mau_month_us",
  "aggregate_tags.Last Month Average MAU (WW)" = "mau_month_ww",
  
  # Historical metrics
  "aggregate_tags.All Time Downloads (US)" = "downloads_alltime_us",
  "aggregate_tags.All Time Downloads (WW)" = "downloads_alltime_ww",
  "aggregate_tags.All Time Revenue (US)" = "revenue_alltime_us", 
  "aggregate_tags.All Time Revenue (WW)" = "revenue_alltime_ww",
  "aggregate_tags.All Time Publisher Downloads (WW)" = "publisher_downloads_alltime_ww",
  "aggregate_tags.All Time Publisher Revenue (WW)" = "publisher_revenue_alltime_ww",
  
  # RPD metrics
  "aggregate_tags.RPD (All Time, US)" = "rpd_alltime_us",
  "aggregate_tags.RPD (All Time, WW)" = "rpd_alltime_ww",
  
  # Launch metrics
  "aggregate_tags.Release Date (US)" = "release_date_us",
  "aggregate_tags.Release Date (WW)" = "release_date_ww",
  "aggregate_tags.Release Date (JP)" = "release_date_jp",
  "aggregate_tags.Earliest Release Date" = "earliest_release_date",
  "aggregate_tags.Revenue First 30 Days (WW)" = "revenue_first30d_ww",
  "aggregate_tags.Downloads First 30 Days (WW)" = "downloads_first30d_ww",
  
  # Retention metrics
  "aggregate_tags.Day 1 Retention (Last Quarter, US)" = "retention_1d_us",
  "aggregate_tags.Day 1 Retention (Last Quarter, WW)" = "retention_1d_ww",
  "aggregate_tags.Day 7 Retention (Last Quarter, US)" = "retention_7d_us",
  "aggregate_tags.Day 7 Retention (Last Quarter, WW)" = "retention_7d_ww",
  "aggregate_tags.Day 14 Retention (Last Quarter, US)" = "retention_14d_us",
  "aggregate_tags.Day 14 Retention (Last Quarter, WW)" = "retention_14d_ww",
  "aggregate_tags.Day 30 Retention (Last Quarter, US)" = "retention_30d_us",
  "aggregate_tags.Day 30 Retention (Last Quarter, WW)" = "retention_30d_ww",
  "aggregate_tags.Day 60 Retention (Last Quarter, US)" = "retention_60d_us",
  "aggregate_tags.Day 60 Retention (Last Quarter, WW)" = "retention_60d_ww",
  
  # Demographics
  "aggregate_tags.Age (Last Quarter, US)" = "age_us",
  "aggregate_tags.Age (Last Quarter, WW)" = "age_ww",
  "aggregate_tags.Genders (Last Quarter, US)" = "genders_us",
  "aggregate_tags.Genders (Last Quarter, WW)" = "genders_ww",
  "aggregate_tags.Gender (Last Quarter, US)" = "gender_us",
  "aggregate_tags.Gender (Last Quarter, WW)" = "gender_ww",
  
  # Additional revenue metrics
  "aggregate_tags.Most Popular Country by Revenue" = "most_popular_country_revenue",
  "aggregate_tags.Last 90 Days Downloads (US)" = "downloads_90d_us",
  "aggregate_tags.Last 90 Days Downloads (WW)" = "downloads_90d_ww",
  "aggregate_tags.Last 90 Days Revenue (US)" = "revenue_90d_us",
  "aggregate_tags.Last 90 Days Revenue (WW)" = "revenue_90d_ww",
  "aggregate_tags.ARPU (90 Days, US)" = "arpu_90d_us",
  "aggregate_tags.ARPU (90 Days, WW)" = "arpu_90d_ww",
  
  # Platform shares
  "aggregate_tags.Android Share (WW)" = "android_share_ww",
  "aggregate_tags.iOS Share (WW)" = "ios_share_ww",
  "aggregate_tags.Female Share (US)" = "female_share_us",
  "aggregate_tags.Male Share (US)" = "male_share_us",
  
  # Additional useful metrics from entities.custom_tags (when available)
  "entities.custom_tags.Game Sub-genre" = "game_subgenre",
  "entities.custom_tags.Game Genre" = "game_genre",
  "entities.custom_tags.Game Art Style" = "game_art_style",
  "entities.custom_tags.Primary Category" = "primary_category",
  "entities.custom_tags.Overall US Rating" = "us_rating",
  "entities.custom_tags.Current US Rating" = "current_us_rating",
  "entities.custom_tags.Free" = "is_free",
  "entities.custom_tags.In-App Purchases" = "has_iap",
  "entities.custom_tags.Contains Ads" = "has_ads"
)

#' Numeric Metric Patterns
#' 
#' Patterns to identify columns that should be converted to numeric.
#' 
#' @keywords internal
NUMERIC_METRIC_PATTERNS <- c(
  "downloads", "revenue", "users", "dau", "wau", "mau", "rpd", 
  "retention", "age", "rating", "count", "days", "size", "absolute",
  "delta", "share", "percent", "time", "last", "average", "total",
  "first", "all time", "180", "30", "90", "7d", "14d", "month"
)
