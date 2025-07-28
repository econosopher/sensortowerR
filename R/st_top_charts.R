#' Fetch Top Apps by Various Metrics
#'
#' Retrieves top apps from Sensor Tower based on revenue, downloads ("units"), 
#' or active user metrics (DAU, WAU, MAU). This unified function automatically 
#' selects the appropriate API endpoint based on the measure specified.
#'
#' @param measure Character string. Metric to measure. Must be one of:
#'   - **Revenue/Downloads**: "revenue" (default), "units" 
#'   - **Active Users**: "DAU", "WAU", "MAU"
#' @param os Character string. Operating System. Must be one of
#'   "ios", "android", or "unified". Required.
#' @param comparison_attribute Character string. Comparison attribute
#'   type. Must be one of "absolute", "delta", or "transformed_delta". 
#'   Defaults to "absolute".
#' @param time_range Character string. Time granularity. Must be one of
#'   "day", "week", "month", or "quarter". Defaults to "month".
#'   Note: "week" is not available when `measure` is "MAU".
#' @param date Character string or Date object. Start date for the
#'   query in "YYYY-MM-DD" format. Defaults to the start of the current month.
#' @param category Character string or numeric. The ID of the category to 
#'   filter by. **Required for revenue/downloads and active users**.
#'   Use `st_categories()` to find valid IDs.
#' @param regions Character vector or comma-separated string. Region
#'   codes (e.g., `"US"`, `c("US", "GB")`, `"WW"` for worldwide) to filter results. 
#'   Required.
#' @param end_date Optional. Character string or Date object. End date for the
#'   query in "YYYY-MM-DD" format, inclusive. Only used for revenue/downloads.
#' @param limit Optional. Integer. Maximum number of apps to return per call.
#'   Defaults to 20.
#' @param offset Optional. Integer. Number of apps to skip for pagination.
#' @param device_type Optional. Character string. For `os = "ios"` or
#'   `os = "unified"`: "iphone", "ipad", or "total". Defaults to `"total"`.
#' @param custom_fields_filter_id Optional. Character string. ID of a Sensor
#'   Tower custom field filter to apply.
#' @param custom_tags_mode Optional. Character string. Required if `os` is
#'   'unified' and `custom_fields_filter_id` is provided.
#' @param data_model Optional. Character string. The data model to use.
#'   Defaults to "DM_2025_Q2". Only used for active user metrics.
#' @param auth_token Optional. Character string. Your Sensor Tower API token.
#' @param base_url Optional. Character string. The base URL for the API.
#' @param enrich_response Optional. Logical. If `TRUE` (default), enriches
#'   the response with app metadata and custom metrics.
#' @param deduplicate_apps Optional. Logical. If `TRUE` (default), consolidates
#'   apps with the same name but different platform/regional SKUs into single rows
#'   with aggregated metrics. If `FALSE`, returns separate rows for each SKU.
#'
#' @return A [tibble][tibble::tibble] with top app data including enhanced
#'   custom metrics like downloads, revenue, retention rates, and more.
#'   For sales data (revenue/downloads), app names are automatically looked up
#'   using the app IDs since the sales endpoint doesn't provide app names natively.
#'   
#'   **Revenue Units**: Revenue values are standardized to base currency units (dollars,
#'   euros, etc.) for consistency across all sensortowerR functions. The function provides
#'   a `revenue` column in base units alongside the original `revenue_absolute` (in cents).
#'   
#'   **Data Cleaning**: Numeric metric values are automatically cleaned of special 
#'   characters (%, $, commas) and converted to proper numeric format for analysis.
#'   
#'   **App Deduplication**: By default, apps with the same name but different 
#'   platform/regional SKUs are consolidated into single rows with aggregated metrics
#'   (downloads/revenue summed, rates/percentages averaged).
#'
#' @section API Endpoints Used:
#'   - **Revenue/Downloads**: `GET /v1/\{os\}/sales_report_estimates_comparison_attributes`
#'   - **Active Users**: `GET /v1/\{os\}/top_and_trending/active_users`
#'
#' @section Enhanced Custom Metrics:
#'   The function extracts comprehensive custom metrics including:
#'   - Downloads: `downloads_180d_ww`, `downloads_90d_us`
#'   - Revenue: `revenue_180d_ww`, `revenue_90d_us`
#'   - Retention: `retention_1d_us`, `retention_7d_us`, `retention_30d_us`
#'   - Monetization: `rpd_alltime_us`, `arpu_90d_us`
#'   - Demographics: `male_share_us`, `female_share_us`
#'   - Platform: `ios_share_ww`, `android_share_ww`
#'
#' @examples
#' \dontrun{
#' # Top apps by revenue (default)
#' top_revenue <- st_top_charts(category = 6000)  # iOS Games
#' 
#' # Top apps by downloads
#' top_downloads <- st_top_charts(measure = "units", category = 6000)
#' 
#' # Top apps by Monthly Active Users
#' top_mau <- st_top_charts(measure = "MAU", category = 7014)  # Role Playing
#' 
#' # Custom time range and region
#' top_quarter <- st_top_charts(
#'   measure = "revenue",
#'   time_range = "quarter", 
#'   regions = "US",
#'   category = 6000
#' )
#' }
#'
#' @export
st_top_charts <- function(measure = "revenue",
                          os,
                          comparison_attribute = "absolute",
                          time_range = "month",
                          date = NULL,
                          category = NULL,
                          regions,
                          end_date = NULL,
                          limit = 20,
                          offset = NULL,
                          device_type = NULL,
                          custom_fields_filter_id = NULL,
                          custom_tags_mode = NULL,
                          data_model = "DM_2025_Q2",
                          auth_token = NULL,
                          base_url = "https://api.sensortower.com",
                          enrich_response = TRUE,
                          deduplicate_apps = TRUE) {
  
  # Validate required parameters
  if (missing(os) || is.null(os)) {
    stop("'os' parameter is required. Specify one of: 'ios', 'android', 'unified'.")
  }
  
  if (missing(regions) || is.null(regions) || length(regions) == 0) {
    stop("'regions' parameter is required. Specify country codes (e.g., 'US', 'GB', 'JP', or 'WW' for worldwide).")
  }
  
  # --- Input Validation ---
  measure <- match.arg(measure, c("revenue", "units", "DAU", "WAU", "MAU"))
  
  # Determine which API to use based on measure
  is_active_users <- measure %in% c("DAU", "WAU", "MAU")
  is_sales <- measure %in% c("revenue", "units")
  
  # Validate category requirement
  if (is.null(category)) {
    rlang::abort("The 'category' parameter is required for all measures. Use st_categories() to find valid IDs.")
  }
  
  # --- Handle Default Date ---
  if (is.null(date)) {
    date <- lubridate::floor_date(Sys.Date(), "month")
  }
  
  # --- Authentication ---
  auth_token_val <- auth_token %||% Sys.getenv("SENSORTOWER_AUTH_TOKEN")
  if (auth_token_val == "") {
    rlang::abort(
      "Authentication token not found. Please set it as an environment variable."
    )
  }
  
  # Handle default device_type
  if (is.null(device_type) && os %in% c("ios", "unified")) {
    device_type <- "total"
    if (os == "unified") {
      message("`device_type` is not specified for `os = 'unified'`. Defaulting to 'total'.")
    }
  }
  
  # Route to appropriate API endpoint based on measure type
  if (is_active_users) {
    # --- Input Validation for Active Users ---
    validate_inputs(
      os = os,
      comparison_attribute = comparison_attribute,
      time_range = time_range,
      measure = measure,
      date = date,
      category = category,
      regions = regions,
      limit = limit,
      offset = offset,
      device_type = device_type,
      custom_fields_filter_id = custom_fields_filter_id,
      custom_tags_mode = custom_tags_mode,
      data_model = data_model
    )

    # --- Prepare Query Parameters for Active Users ---
    query_params <- prepare_query_params_active_users(
      auth_token = auth_token_val,
      comparison_attribute = comparison_attribute,
      time_range = time_range,
      measure = measure,
      date = date,
      category = category,
      regions = regions,
      limit = limit,
      offset = offset,
      device_type = device_type,
      custom_fields_filter_id = custom_fields_filter_id,
      custom_tags_mode = custom_tags_mode,
      data_model = data_model,
      os = os
    )

    # --- Build and Perform Request for Active Users ---
    path <- c("v1", os, "top_and_trending", "active_users")
    
  } else {
    # --- Input Validation for Sales ---
    validate_inputs(
      os = os,
      comparison_attribute = comparison_attribute,
      time_range = time_range,
      measure = measure,
      date = date,
      category = category,
      regions = regions,
      end_date = end_date,
      limit = limit,
      offset = offset,
      device_type = device_type,
      custom_fields_filter_id = custom_fields_filter_id,
      custom_tags_mode = custom_tags_mode
    )

    # --- Prepare Query Parameters for Sales ---
    query_params <- prepare_query_params_sales(
      auth_token = auth_token_val,
      comparison_attribute = comparison_attribute,
      time_range = time_range,
      measure = measure,
      date = date,
      category = category,
      end_date = end_date,
      regions = regions,
      limit = limit,
      offset = offset,
      device_type = device_type,
      custom_fields_filter_id = custom_fields_filter_id,
      custom_tags_mode = custom_tags_mode,
      os = os
    )

    # --- Build and Perform Request for Sales ---
    path <- c("v1", os, "sales_report_estimates_comparison_attributes")
  }
  
  # --- Common Request Building and Processing ---
  req <- build_request(base_url, path, query_params)
  resp <- perform_request(req)
  
  # --- Process Response ---
  result <- process_response(resp, enrich_response)
  
  # --- Validate Data ---
  if (nrow(result) > 0) {
    result <- validate_top_charts_data(result, measure, regions)
  }
  
  # --- Deduplicate Apps (if requested) ---
  if (deduplicate_apps && "unified_app_name" %in% names(result)) {
    result <- deduplicate_apps_by_name(result)
  }
  
  # --- Standardize Revenue Units ---
  result <- standardize_revenue_units(result, source = "top_charts")
  
  return(result)
} 