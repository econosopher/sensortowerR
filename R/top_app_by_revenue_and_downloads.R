#' Fetch Top Apps by Downloads and Revenue Estimates
#'
#' Retrieves top apps from Sensor Tower based on download and revenue estimates,
#' allowing comparison using absolute values, delta, or transformed delta.
#'
#' @param os Required. Operating System. Either "ios" or "android".
#' @param comparison_attribute Required. Comparison attribute type. One of
#'   "absolute", "delta", or "transformed_delta".
#' @param time_range Required. Time granularity. One of "day", "week",
#'   "month", or "quarter".
#' @param measure Required. Metric to measure. One of "units" (downloads) or
#'   "revenue".
#' @param date Required. Start date for the query in "YYYY-MM-DD" format.
#'   The API automatically adjusts this to the beginning of the specified
#'   `time_range` (e.g., Monday for week, 1st for month/quarter).
#' @param category Required. The ID of the category to filter by. See Sensor
#'   Tower documentation or use helper functions to find Category IDs.
#'   Example: 6000 (iOS Games).
#' @param end_date Optional. End date for the query in "YYYY-MM-DD" format,
#'   inclusive. If provided, allows aggregation over multiple periods defined
#'   by `time_range`. Auto-adjusts to the end of the period.
#' @param regions Optional. A character vector of region codes (e.g., "US", "GB")
#'   to filter results. If NULL (default), fetches global data where applicable.
#'   Regions parameter *must* be specified according to API docs. Often "WW"
#'   (Worldwide) or specific country codes are needed. Check API requirements.
#' @param limit Optional. Integer. Maximum number of apps to return per call.
#'   Defaults to 25. Maximum is 2000.
#' @param offset Optional. Integer. Number of apps to skip for pagination.
#'   Defaults to 0.
#' @param device_type Optional. Character string. For "ios" only: "iphone",
#'   "ipad", or "total". Use "total" for unified results. Leave blank/NULL for
#'   "android".
#' @param custom_fields_filter_id Optional. Character string. ID of a custom
#'   field filter to apply. Requires `custom_tags_mode` if `os` is 'unified'.
#' @param custom_tags_mode Optional. Character string. Required if `os` is
#'   'unified' and `custom_fields_filter_id` is used. Typically
#'   "include_unified_apps".
#' @param auth_token Optional. Your Sensor Tower API authentication token.
#'   It's recommended to set the `SENSORTOWER_AUTH_TOKEN` environment
#'   variable instead of passing this argument directly.
#' @param base_url Optional. The base URL for the Sensor Tower API. Defaults to
#'   `https://api.sensortower.com`.
#'
#' @return A tibble (data frame) containing the requested top app estimates.
#'   Columns correspond to the fields in the API JSON response (app_id,
#'   current_units_value, comparison_units_value, units_absolute, etc.).
#'
#' @importFrom httr2 request req_url_path_append req_url_query req_perform
#'   resp_body_json req_user_agent req_error resp_check_status
#' @importFrom jsonlite fromJSON
#' @importFrom rlang abort set_names %||%
#' @importFrom dplyr bind_rows
#' @importFrom utils URLencode
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Ensure SENSORTOWER_AUTH_TOKEN environment variable is set
#'
#' # Get top 10 iOS Games by absolute downloads for the week starting 2023-01-02 in the US
#' top_games_dl <- st_get_sales_estimates(
#'   os = "ios",
#'   comparison_attribute = "absolute",
#'   time_range = "week",
#'   measure = "units",
#'   date = "2023-01-02",
#'   category = 6000, # Example: iOS Games category ID
#'   regions = "US",
#'   limit = 10
#' )
#'
#' print(top_games_dl)
#'
#' # Get top 5 Android Finance apps by revenue delta for Q1 2023 worldwide
#' top_finance_rev <- st_get_sales_estimates(
#'   os = "android",
#'   comparison_attribute = "delta",
#'   time_range = "quarter",
#'   measure = "revenue",
#'   date = "2023-01-01",
#'   end_date = "2023-03-31", # Explicit end date for quarter
#'   category = "FINANCE", # Example: Android Finance category ID might be a string
#'   regions = "WW", # Example: Worldwide region code
#'   limit = 5
#' )
#' print(top_finance_rev)
#' }
st_get_sales_estimates <- function(os,
                                   comparison_attribute,
                                   time_range,
                                   measure,
                                   date,
                                   category,
                                   end_date = NULL,
                                   regions = NULL, # Changed default, API implies it's needed
                                   limit = 25,
                                   offset = NULL,
                                   device_type = NULL,
                                   custom_fields_filter_id = NULL,
                                   custom_tags_mode = NULL,
                                   auth_token = NULL,
                                   base_url = "https://api.sensortower.com") {

  # --- Input Validation ---
  stopifnot(
    is.character(os) && length(os) == 1 && os %in% c("ios", "android", "unified"), # Added unified
    is.character(comparison_attribute) && length(comparison_attribute) == 1 && comparison_attribute %in% c("absolute", "delta", "transformed_delta"),
    is.character(time_range) && length(time_range) == 1 && time_range %in% c("day", "week", "month", "quarter"),
    is.character(measure) && length(measure) == 1 && measure %in% c("units", "revenue"),
    is.character(date) && length(date) == 1 && grepl("^\\d{4}-\\d{2}-\\d{2}$", date),
    !is.null(category), # Category is required
    is.null(regions) || is.character(regions), # Regions should be specified
    is.numeric(limit) && length(limit) == 1 && limit > 0 && limit <= 2000,
    is.null(offset) || (is.numeric(offset) && length(offset) == 1 && offset >= 0),
    is.null(end_date) || (is.character(end_date) && length(end_date) == 1 && grepl("^\\d{4}-\\d{2}-\\d{2}$", end_date)),
    is.null(device_type) || (is.character(device_type) && length(device_type) == 1),
    is.null(custom_fields_filter_id) || (is.character(custom_fields_filter_id) && length(custom_fields_filter_id) == 1),
    is.null(custom_tags_mode) || (is.character(custom_tags_mode) && length(custom_tags_mode) == 1)
  )
 if (is.null(regions)) {
    warning("The 'regions' parameter is often required by the Sensor Tower API (e.g., use 'WW' for Worldwide or specific country codes like 'US'). Ensure you provide a valid region specification if needed.", call. = FALSE)
    # Or you could make it mandatory:
    # if (is.null(regions)) rlang::abort("The 'regions' parameter must be specified.")
  }

  # --- Authentication ---
  auth_token <- auth_token %||% Sys.getenv("SENSORTOWER_AUTH_TOKEN")
  if (auth_token == "") {
    rlang::abort("Sensor Tower authentication token not found. Set the SENSORTOWER_AUTH_TOKEN environment variable or pass via the auth_token argument.")
  }

  # --- Prepare Query Parameters ---
  query_params <- list(
    auth_token = auth_token,
    comparison_attribute = comparison_attribute,
    time_range = time_range,
    measure = measure,
    date = date,
    category = category,
    end_date = end_date,
    regions = if (!is.null(regions)) paste(regions, collapse = ","), # Collapse vector into comma-separated string
    limit = limit,
    offset = offset,
    device_type = if (os == "ios") device_type else NULL, # Only relevant for iOS
    custom_fields_filter_id = custom_fields_filter_id,
    custom_tags_mode = custom_tags_mode
  )

  # Remove NULL parameters
  query_params <- Filter(Negate(is.null), query_params)

  # --- Build Request ---
  req <- httr2::request(base_url) |>
    httr2::req_user_agent("sensortowerR (https://github.com/peterparkerspicklepatch/sensortowerR)") |> # Good practice
    httr2::req_url_path_append("v1") |>
    httr2::req_url_path_append(os) |>
    httr2::req_url_path_append("sales_report_estimates_comparison_attributes") |>
    httr2::req_url_query(!!!query_params) |>
    httr2::req_error(is_error = function(resp) FALSE) # Handle errors manually

  # --- Perform Request ---
  resp <- httr2::req_perform(req)

  # --- Process Response ---
  status_code <- httr2::resp_status(resp)
  body_raw <- httr2::resp_body_raw(resp) # Get raw body first

  # Handle potential empty body before parsing JSON
  if (length(body_raw) == 0) {
     if (status_code >= 200 && status_code < 300) {
       # Successful request but empty response, return empty tibble
       warning("API returned a successful status code but an empty response body.", call. = FALSE)
       return(dplyr::tibble())
     } else {
       # Error status code and empty body
       httr2::resp_check_status(resp) # Trigger default httr2 error message
     }
  }

  # Try parsing JSON
  parsed_body <- tryCatch({
    jsonlite::fromJSON(rawToChar(body_raw), flatten = TRUE)
  }, error = function(e) {
    # If JSON parsing fails, raise an error with more context
    raw_content_snippet <- substr(rawToChar(body_raw), 1, 100)
    msg <- paste(
        "Failed to parse JSON response.",
        paste("Status code:", status_code),
        paste("Error:", e$message),
        paste("Response snippet:", raw_content_snippet),
        sep = "\n"
    )
    rlang::abort(msg, parent = e)
  })

  # Check for API-level errors after potentially parsing
  if (status_code >= 400) {
     api_error_message <- "Unknown API error"
     if(is.list(parsed_body) && "error" %in% names(parsed_body)) {
       api_error_message <- parsed_body$error
     } else if (is.list(parsed_body) && "errors" %in% names(parsed_body) && is.list(parsed_body$errors) && length(parsed_body$errors) > 0 && "title" %in% names(parsed_body$errors[[1]])) {
       api_error_message <- parsed_body$errors[[1]]$title
     }
     # Use httr2's default error mechanism but add API message
     httr2::resp_check_status(resp, info = paste("API Error:", api_error_message))
  }

  # --- Format Output ---
  # The response is typically a list of records. Convert to tibble.
  # Check if the main response is the list or nested (e.g., inside a 'data' field - adjust if needed)
  if (is.list(parsed_body) && length(parsed_body) > 0) {
    # If it's a list of lists/records
      result_df <- tryCatch({
          dplyr::bind_rows(parsed_body)
      }, error = function(e) {
          # If bind_rows fails, maybe it's structured differently
          warning("Could not directly bind rows from parsed JSON. Returning raw parsed list.", call. = FALSE)
          return(parsed_body) # Return the raw parsed list as a fallback
      })
      return(tibble::as_tibble(result_df))

  } else if (is.data.frame(parsed_body)) {
    # If fromJSON already returned a data frame
    return(tibble::as_tibble(parsed_body))
  } else {
    # Handle unexpected structure
    warning("API response was not in the expected list-of-records format. Returning raw parsed response.", call. = FALSE)
    return(parsed_body)
  }
}