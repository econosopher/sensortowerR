#' Fetch Top Apps by Downloads and Revenue Estimates
#'
#' Retrieves top apps from Sensor Tower based on download ("units") or revenue
#' estimates for a specific OS, category, region, and time period. Allows
#' comparison using absolute values, delta, or transformed delta. Targets the
#' `/v1/{os}/sales_report_estimates_comparison_attributes` endpoint.
#'
#' @param os Required. Character string. Operating System. Must be one of
#'   "ios", "android", or "unified".
#' @param comparison_attribute Required. Character string. Comparison attribute
#'   type. Must be one of "absolute", "delta", or "transformed_delta".
#' @param time_range Required. Character string. Time granularity. Must be one
#'   of "day", "week", "month", or "quarter".
#' @param measure Required. Character string. Metric to measure. Must be one of
#'   "units" (downloads) or "revenue".
#' @param date Required. Character string or Date object. Start date for the
#'   query in "YYYY-MM-DD" format. The API automatically adjusts this to the
#'   beginning of the specified `time_range`.
#' @param category Required. Character string or numeric. The ID of the
#'   category to filter by (e.g., iOS Games is 6000 or "6000", Android Finance
#'   might be "FINANCE"). Use `st_categories()` to find valid IDs.
#' @param regions Required. Character vector or comma-separated string. Region
#'   codes (e.g., `"US"`, `c("US", "GB")`, `"WW"`) to filter results. This
#'   parameter is typically mandatory for this endpoint. Use `get_countries()`
#'   to find valid codes.
#' @param end_date Optional. Character string or Date object. End date for the
#'   query in "YYYY-MM-DD" format, inclusive. If provided, allows aggregation
#'   over multiple periods defined by `time_range`. Auto-adjusts to the end of
#'   the period. Defaults to NULL.
#' @param limit Optional. Integer. Maximum number of apps to return per call.
#'   Defaults to 25. Maximum allowed by API is 2000.
#' @param offset Optional. Integer. Number of apps to skip for pagination.
#'   Useful for retrieving results beyond the `limit`. Defaults to NULL.
#' @param device_type Optional. Character string. For `os = "ios"` or
#'   `os = "unified"`: "iphone", "ipad", or "total". Defaults to `"total"` if
#'   `os` is "ios" or "unified" and this argument is not provided. Leave
#'   blank/NULL for `os = "android"`.
#' @param custom_fields_filter_id Optional. Character string. ID of a Sensor
#'   Tower custom field filter to apply. Requires `custom_tags_mode` if `os`
#'   is 'unified'. Defaults to NULL.
#' @param custom_tags_mode Optional. Character string. Required if `os` is
#'   'unified' and `custom_fields_filter_id` is provided. Typically set to
#'   "include_unified_apps". Defaults to NULL.
#' @param auth_token Optional. Character string. Your Sensor Tower API
#'   authentication token. It is strongly recommended to set the
#'   `SENSORTOWER_AUTH_TOKEN` environment variable instead of passing this
#'   argument directly for security.
#' @param base_url Optional. Character string. The base URL for the Sensor
#'   Tower API. Defaults to `"https://api.sensortower.com"`.
#' @param enrich_response Optional. Logical. If `TRUE` (default), the function
#'   will automatically enrich the response with app metadata like names by
#'   unnesting the `entities` column from the API response.
#'
#' @return A [tibble][tibble::tibble] (data frame) containing the requested top
#'   app estimates. If `enrich_response` is `TRUE`, it includes app metadata
#'   like `app.name`. Returns an empty tibble if the API call is successful
#'   but returns no data or if an error occurs.
#'
#' @section API Endpoint Used:
#'   `GET /v1/{os}/sales_report_estimates_comparison_attributes`
#'
#' @section Common Issues (HTTP 422 Error):
#'   An HTTP 422 "Unprocessable Entity" error often indicates invalid
#'   parameters or combinations (e.g., invalid `category` ID, `regions` code,
#'   missing `device_type` for iOS/Unified, missing `custom_tags_mode` when
#'   using filters with Unified OS). Consult API docs and check console
#'   warnings for the response body.
#'
#' @importFrom httr2 request req_user_agent req_url_path_append req_url_query
#'   req_error req_perform resp_status resp_body_raw resp_check_status
#'   resp_body_string
#' @importFrom jsonlite fromJSON
#' @importFrom rlang abort warn `%||%`
#' @importFrom dplyr bind_rows tibble
#' @importFrom tibble as_tibble
#' @importFrom utils URLencode
#' @export
#'
#' @examples
#' \dontrun{
#' # Ensure SENSORTOWER_AUTH_TOKEN environment variable is set
#' # Sys.setenv(SENSORTOWER_AUTH_TOKEN = "YOUR_TOKEN_HERE")
#'
#' # Get top 10 iOS Games by absolute downloads for the month of Oct 2023 in US
#' top_ios_games_dl <- st_top_sales(
#'   os = "ios",
#'   comparison_attribute = "absolute",
#'   time_range = "month",
#'   measure = "units",
#'   date = "2023-10-01",
#'   category = 6000,      # iOS Games category ID
#'   regions = "US",       # Region is required
#'   limit = 10
#' )
#'
#' print(top_ios_games_dl)
#'
#' # Get top 5 Android Finance apps by revenue delta for Q4 2023 worldwide
#' top_android_finance_rev <- st_top_sales(
#'   os = "android",
#'   comparison_attribute = "delta",
#'   time_range = "quarter",
#'   measure = "revenue",
#'   date = "2023-10-01",
#'   category = "FINANCE",    # Android Category ID might be a string
#'   regions = "WW",          # Worldwide region code is required
#'   limit = 5
#' )
#' print(top_android_finance_rev)
#' }
st_top_sales <- function(os,
                         comparison_attribute,
                         time_range,
                         measure,
                         date,
                         category,
                         regions,
                         end_date = NULL,
                         limit = 25,
                         offset = NULL,
                         device_type = NULL,
                         custom_fields_filter_id = NULL,
                         custom_tags_mode = NULL,
                         auth_token = NULL,
                         base_url = "https://api.sensortower.com",
                         enrich_response = TRUE) {
  # --- Input Validation ---
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

  # --- Authentication ---
  auth_token_val <- auth_token %||% Sys.getenv("SENSORTOWER_AUTH_TOKEN")
  if (auth_token_val == "") {
    rlang::abort(
      "Authentication token not found. Please set it as an environment variable."
    )
  }

  # --- Prepare Query Parameters ---
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

  # --- Build and Perform Request ---
  path <- c("v1", os, "sales_report_estimates_comparison_attributes")
  req <- build_request(base_url, path, query_params)
  resp <- perform_request(req)

  # --- Process Response ---
  process_response(resp, enrich_response = enrich_response)
}