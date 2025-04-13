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
#' @param category Required. Character string or numeric. The ID of the category
#'   to filter by (e.g., iOS Games is 6000 or "6000", Android Finance might be
#'   "FINANCE"). Use `get_categories()` to find valid IDs.
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
#'   Useful for retrieving results beyond the `limit`. Defaults to NULL (meaning 0).
#' @param device_type Optional. Character string. For `os = "ios"` or `os = "unified"`:
#'   "iphone", "ipad", or "total". Defaults to `"total"` if `os` is "ios" or
#'   "unified" and this argument is not provided. Leave blank/NULL for
#'   `os = "android"`.
#' @param custom_fields_filter_id Optional. Character string. ID of a Sensor Tower
#'   custom field filter to apply. Requires `custom_tags_mode` if `os` is 'unified'.
#'   Defaults to NULL.
#' @param custom_tags_mode Optional. Character string. Required if `os` is
#'   'unified' and `custom_fields_filter_id` is provided. Typically set to
#'   "include_unified_apps". Defaults to NULL.
#' @param auth_token Optional. Character string. Your Sensor Tower API
#'   authentication token. It is strongly recommended to set the
#'   `SENSORTOWER_AUTH_TOKEN` environment variable instead of passing this
#'   argument directly for security. Defaults to `Sys.getenv("SENSORTOWER_AUTH_TOKEN")`.
#' @param base_url Optional. Character string. The base URL for the Sensor Tower
#'   API. Defaults to `"https://api.sensortower.com"`.
#'
#' @return A [tibble][tibble::tibble] (data frame) containing the requested top
#'   app estimates. Columns correspond to the fields in the API JSON response
#'   (e.g., `app_id`, `date`, `country`, `current_units_value`, etc.). Returns
#'   an empty tibble if the API call is successful but returns no data or if an
#'   error occurs.
#'
#' @section API Endpoint Used:
#'   `GET /v1/{os}/sales_report_estimates_comparison_attributes`
#'
#' @section Common Issues (HTTP 422 Error):
#'   An HTTP 422 "Unprocessable Entity" error often indicates invalid parameters
#'   or combinations (e.g., invalid `category` ID, `regions` code, missing
#'   `device_type` for iOS/Unified, missing `custom_tags_mode` when using filters
#'   with Unified OS). Consult API docs and check console warnings for the
#'   response body.
#'
#' @importFrom httr2 request req_user_agent req_url_path_append req_url_query
#'   req_error req_perform resp_status resp_body_raw resp_check_status resp_body_string
#' @importFrom jsonlite fromJSON
#' @importFrom rlang abort warn %||%
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
#' # Get top 10 iOS Games by absolute downloads for month starting 2023-10-01 in US
#' top_ios_games_dl <- get_top_apps_by_revenue_and_downloads(
#'   os = "ios",
#'   comparison_attribute = "absolute",
#'   time_range = "month",
#'   measure = "units",
#'   date = "2023-10-01",
#'   category = 6000,      # iOS Games category ID
#'   regions = "US",       # Region is required
#'   limit = 10
#'   # device_type defaults to "total"
#' )
#'
#' print(top_ios_games_dl)
#'
#' # Get top 5 Android Finance apps by revenue delta for Q4 2023 worldwide
#' top_android_finance_rev <- get_top_apps_by_revenue_and_downloads(
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
#'
#' # Get top Unified apps using a filter (if supported by API)
#' # top_unified_filtered <- get_top_apps_by_revenue_and_downloads(
#' #   os = "unified",
#' #   comparison_attribute = "absolute",
#' #   time_range = "month",
#' #   measure = "revenue",
#' #   date = "2023-10-01",
#' #   category = "OVERALL", # Example category
#' #   regions = "WW",
#' #   limit = 10,
#' #   custom_fields_filter_id = "YOUR_FILTER_ID",
#' #   custom_tags_mode = "include_unified_apps" # Required
#' # )
#' # print(top_unified_filtered)
#' }
get_top_apps_by_revenue_and_downloads <- function(os,
                                                  comparison_attribute,
                                                  time_range,
                                                  measure,
                                                  date,
                                                  category,
                                                  regions, # Made mandatory
                                                  end_date = NULL,
                                                  limit = 25,
                                                  offset = NULL,
                                                  device_type = NULL,
                                                  custom_fields_filter_id = NULL,
                                                  custom_tags_mode = NULL, # Added argument
                                                  auth_token = NULL,
                                                  base_url = "https://api.sensortower.com") {

  # --- Input Validation ---
   stopifnot(
    "`os` must be 'ios', 'android', or 'unified'" =
        is.character(os) && length(os) == 1 && os %in% c("ios", "android", "unified"),
    "`comparison_attribute` must be 'absolute', 'delta', or 'transformed_delta'" =
        is.character(comparison_attribute) && length(comparison_attribute) == 1 && comparison_attribute %in% c("absolute", "delta", "transformed_delta"),
    "`time_range` must be 'day', 'week', 'month', or 'quarter'" =
        is.character(time_range) && length(time_range) == 1 && time_range %in% c("day", "week", "month", "quarter"),
    "`measure` must be 'units' or 'revenue'" =
        is.character(measure) && length(measure) == 1 && measure %in% c("units", "revenue"),
    "`date` must be provided" = !missing(date),
    "`category` must be provided" = !missing(category),
    "`regions` must be provided and non-empty" =
        !missing(regions) && !is.null(regions) && length(regions) > 0 && nzchar(paste(regions, collapse="")),
    "`limit` must be a positive integer <= 2000" =
        is.numeric(limit) && length(limit) == 1 && limit > 0 && limit <= 2000 && floor(limit) == limit,
    "`offset` must be NULL or a non-negative integer" =
        is.null(offset) || (is.numeric(offset) && length(offset) == 1 && offset >= 0 && floor(offset) == offset),
    "`end_date` can be NULL, Date, or YYYY-MM-DD string" =
        is.null(end_date) || inherits(end_date, "Date") || (is.character(end_date) && grepl("^\\d{4}-\\d{2}-\\d{2}$", end_date)),
    "`device_type` must be NULL or one of 'iphone', 'ipad', 'total'" =
        is.null(device_type) || (is.character(device_type) && length(device_type) == 1 && device_type %in% c("iphone", "ipad", "total")),
    "`custom_fields_filter_id` must be NULL or a character string" =
        is.null(custom_fields_filter_id) || (is.character(custom_fields_filter_id) && length(custom_fields_filter_id) == 1),
    "`custom_tags_mode` must be NULL or a character string" =
        is.null(custom_tags_mode) || (is.character(custom_tags_mode) && length(custom_tags_mode) == 1)
  )

  # Validate date formats
  start_date_str <- tryCatch({ format(as.Date(date), "%Y-%m-%d") }, error = function(e) rlang::abort("Invalid format for 'date'. Please use Date object or 'YYYY-MM-DD' string."))
  end_date_str   <- if (!is.null(end_date)) tryCatch({ format(as.Date(end_date), "%Y-%m-%d") }, error = function(e) rlang::abort("Invalid format for 'end_date'. Please use Date object or 'YYYY-MM-DD' string.")) else NULL

  # Specific logical checks
  if (os == "unified" && !is.null(custom_fields_filter_id) && is.null(custom_tags_mode)) {
      rlang::abort("When 'os' is 'unified' and 'custom_fields_filter_id' is provided, 'custom_tags_mode' must also be specified (e.g., 'include_unified_apps').")
  }
   if (os != "unified" && !is.null(custom_tags_mode) && is.null(custom_fields_filter_id)) {
      rlang::warn("'custom_tags_mode' provided without 'custom_fields_filter_id' or when os is not 'unified'. It might be ignored.")
  }

  # --- Authentication ---
  auth_token_val <- auth_token %||% Sys.getenv("SENSORTOWER_AUTH_TOKEN")
  if (auth_token_val == "") {
    rlang::abort("Sensor Tower authentication token not found. Set SENSORTOWER_AUTH_TOKEN environment variable or pass via the auth_token argument.")
  }

  # --- Prepare Query Parameters ---
  # Handle device_type: default to 'total' for ios/unified if NULL, else NULL for android
  effective_device_type <- if (os %in% c("ios", "unified")) {
                               device_type %||% "total"
                           } else {
                               NULL # Not applicable for android
                           }

  query_params <- list(
    auth_token = auth_token_val,
    comparison_attribute = comparison_attribute,
    time_range = time_range,
    measure = measure,
    date = start_date_str,
    category = as.character(category), # Ensure category is character
    end_date = end_date_str,
    regions = paste(regions, collapse = ","), # Collapse regions vector/list
    limit = limit,
    offset = offset,
    device_type = effective_device_type, # Use calculated value
    custom_fields_filter_id = custom_fields_filter_id,
    custom_tags_mode = custom_tags_mode # Add the new parameter
  )

  # Remove parameters that are NULL
  query_params <- Filter(Negate(is.null), query_params)

  # --- Build Request ---
  api_path <- file.path("v1", os, "sales_report_estimates_comparison_attributes")

  req <- httr2::request(base_url) |>
    httr2::req_user_agent("sensortowerR R Package (https://github.com/peterparkerspicklepatch/sensortowerR)") |>
    httr2::req_url_path_append(api_path) |>
    httr2::req_url_query(!!!query_params) |>
    httr2::req_error(is_error = function(resp) FALSE) # Manual error checking

  # --- Perform Request ---
  resp <- tryCatch({
      httr2::req_perform(req)
  }, error = function(e) {
      rlang::abort(paste("HTTP request failed:", e$message), parent = e, class = "sensortower_http_error")
  })

  # --- Process Response ---
  status_code <- httr2::resp_status(resp)
  body_raw <- httr2::resp_body_raw(resp)
  body_text <- rawToChar(body_raw) # Need utils::rawToChar if not base

  if (length(body_raw) == 0) {
     if (status_code >= 200 && status_code < 300) {
       rlang::warn(paste("API returned status", status_code, "with an empty response body. Returning an empty tibble."))
       return(dplyr::tibble())
     } else {
       httr2::resp_check_status(resp, info = "API returned an error status with an empty response body.")
       rlang::abort(paste("API Error: Status", status_code, "with empty response."), class = "sensortower_api_error")
     }
  }

  parsed_body <- tryCatch({
    jsonlite::fromJSON(body_text, flatten = TRUE)
  }, error = function(e) {
    snippet <- substr(body_text, 1, 200)
    rlang::abort(
      message = "Failed to parse API response as JSON.",
      body = c("*" = paste("Status code:", status_code), "*" = paste("Parsing Error:", e$message), "*" = paste("Response body snippet:", snippet, if(nchar(body_text)>200) "...")),
      parent = e, class = "sensortower_json_error"
      )
  })

  if (status_code >= 400) {
     api_error_message <- "Unknown API error reason."
     if(is.list(parsed_body) && !is.null(parsed_body$error)) {
       api_error_message <- paste("Reason:", parsed_body$error)
     } else if (is.list(parsed_body) && !is.null(parsed_body$errors) && is.list(parsed_body$errors) && length(parsed_body$errors) > 0 && !is.null(parsed_body$errors[[1]]$title)) {
       api_error_message <- paste("Reason:", parsed_body$errors[[1]]$title)
     } else if (is.character(parsed_body) && length(parsed_body) == 1) {
       api_error_message <- paste("Response:", parsed_body)
     }

     if (status_code == 422) {
         rlang::warn(paste("Received HTTP 422 (Unprocessable Entity). Check parameters/combinations (esp. device_type for iOS/Unified, custom_tags_mode). Response body:", body_text))
     }

     httr2::resp_check_status(resp, info = api_error_message)
     rlang::abort(paste("API Error:", status_code, "-", api_error_message), class = "sensortower_api_error")
  }

  # --- Format Output ---
  if (is.data.frame(parsed_body)) {
    result_df <- parsed_body
  } else if (is.list(parsed_body) && length(parsed_body) > 0 && all(sapply(parsed_body, is.list))) {
    result_df <- tryCatch({ dplyr::bind_rows(parsed_body) }, error = function(e) {
          rlang::warn(paste("Could not automatically bind rows from parsed JSON. Check API response structure. Error:", e$message))
          return(dplyr::tibble())
      })
  } else {
    rlang::warn("API response was parsed but not in the expected data frame or list-of-records format. Returning empty tibble.")
    return(dplyr::tibble())
  }

  return(tibble::as_tibble(result_df))
}