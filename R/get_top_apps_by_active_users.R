#' Fetch Top Apps by Active User Estimates
#'
#' Retrieves top apps from Sensor Tower based on Daily Active Users (DAU),
#' Weekly Active Users (WAU), or Monthly Active Users (MAU). Allows comparison
#' using absolute values, delta, or transformed delta. Targets the
#' `/v1/{os}/top_and_trending/active_users` endpoint.
#'
#' @param os Required. Character string. Operating System. Must be one of
#'   "ios" or "android". Note: "unified" does not appear supported by this
#'   specific endpoint based on provided documentation.
#' @param comparison_attribute Required. Character string. Comparison attribute
#'   type. Must be one of "absolute", "delta", or "transformed_delta".
#' @param time_range Required. Character string. Time granularity (e.g., "month",
#'   "quarter"). Note: API docs state "week" is *not* available when `measure`
#'   is "MAU". Verify allowed values for DAU/WAU if needed.
#' @param measure Required. Character string. Metric to measure. Must be one of
#'   "DAU", "WAU", or "MAU".
#' @param date Required. Character string or Date object. Start date for the
#'   query in "YYYY-MM-DD" format. Should typically match the beginning of the
#'   `time_range`.
#' @param regions Required. Character vector or comma-separated string. Region
#'   codes (e.g., `"US"`, `c("US", "GB")`, `"WW"`) to filter results. This
#'   parameter is typically mandatory for this endpoint.
#' @param category Optional. Character string or numeric. The ID of the category
#'   to filter by (e.g., 6016 for iOS Social Networking). If NULL (default),
#'   results for all categories are typically returned.
#' @param limit Optional. Integer. Maximum number of apps to return per call.
#'   Defaults to 25.
#' @param offset Optional. Integer. Number of apps to skip for pagination.
#'   Useful for retrieving results beyond the `limit`. Defaults to NULL (meaning 0).
#' @param device_type Optional. Character string. For `os = "ios"` only:
#'   "iphone", "ipad", or "total". Defaults to `"total"` if `os` is "ios" and
#'   this argument is not provided. Leave blank/NULL for `os = "android"`.
#' @param custom_fields_filter_id Optional. Character string. ID of a Sensor Tower
#'   custom field filter to apply. Defaults to NULL.
#' @param auth_token Optional. Character string. Your Sensor Tower API
#'   authentication token. It is strongly recommended to set the
#'   `SENSORTOWER_AUTH_TOKEN` environment variable instead of passing this
#'   argument directly for security. Defaults to `Sys.getenv("SENSORTOWER_AUTH_TOKEN")`.
#' @param base_url Optional. Character string. The base URL for the Sensor Tower
#'   API. Defaults to `"https://api.sensortower.com"`.
#'
#' @return A [tibble][tibble::tibble] (data frame) where each row represents an
#'   app and columns correspond to the fields returned by the API JSON response
#'   (e.g., `app_id`, `date`, `country`, `users_absolute`, `users_delta`,
#'   `users_transformed_delta`, `users_market_share`, custom tags, etc.). Returns
#'   an empty tibble if the API call is successful but returns no data or if an
#'   error occurs.
#'
#' @section API Endpoint Used:
#'   `GET /v1/{os}/top_and_trending/active_users`
#'
#' @section Common Issues (HTTP 422 Error):
#'   An HTTP 422 "Unprocessable Entity" error often indicates a problem with the
#'   combination or format of parameters provided (e.g., invalid `category` ID
#'   for the specified `os`, invalid `regions` code, unsupported combination of
#'   `time_range` and `measure`, missing required `device_type` for iOS).
#'   Consult the Sensor Tower API documentation for valid parameter values and
#'   combinations. Check the R console for the raw API response body if a 422
#'   error occurs.
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
#' # Example 1: Get top 10 iOS Social apps by absolute MAU for Jan 2023 in US & GB
#' top_ios_social_mau <- get_top_apps_by_active_users(
#'   os = "ios",
#'   comparison_attribute = "absolute",
#'   time_range = "month",
#'   measure = "MAU",
#'   date = "2023-01-01",
#'   category = 6016, # Example: iOS Social Networking ID
#'   regions = c("US", "GB"), # Region is required
#'   limit = 10
#'   # device_type defaults to "total" for iOS
#' )
#'
#' print(top_ios_social_mau)
#'
#' # Example 2: Get top 5 Android apps (all categories) by DAU delta
#' # for month starting 2023-03-01 worldwide.
#' top_android_dau_delta <- get_top_apps_by_active_users(
#'   os = "android",
#'   comparison_attribute = "delta",
#'   time_range = "month", # Verify if 'day' is allowed by API for DAU
#'   measure = "DAU",
#'   date = "2023-03-01",
#'   # category = NULL, # Default: All categories
#'   regions = "WW",    # Worldwide is required
#'   limit = 5
#' )
#' print(top_android_dau_delta)
#' }
get_top_apps_by_active_users <- function(os,
                                         comparison_attribute,
                                         time_range,
                                         measure,
                                         date,
                                         regions, # Made mandatory
                                         category = NULL,
                                         limit = 25,
                                         offset = NULL,
                                         device_type = NULL,
                                         custom_fields_filter_id = NULL,
                                         auth_token = NULL,
                                         base_url = "https://api.sensortower.com") {

  # --- Input Validation ---
   stopifnot(
    "`os` must be 'ios' or 'android'" =
        is.character(os) && length(os) == 1 && os %in% c("ios", "android"),
    "`comparison_attribute` must be 'absolute', 'delta', or 'transformed_delta'" =
        is.character(comparison_attribute) && length(comparison_attribute) == 1 && comparison_attribute %in% c("absolute", "delta", "transformed_delta"),
    "`time_range` must be a single character string" =
        is.character(time_range) && length(time_range) == 1, # Specific validation below
    "`measure` must be 'DAU', 'WAU', or 'MAU'" =
        is.character(measure) && length(measure) == 1 && measure %in% c("DAU", "WAU", "MAU"),
    "`date` must be provided" = !missing(date),
    "`regions` must be provided" = !missing(regions) && !is.null(regions) && length(regions) > 0 && nzchar(regions[[1]]),
    "`category` must be NULL or a single string/number" =
        is.null(category) || ((is.character(category) || is.numeric(category)) && length(category) == 1),
    "`limit` must be a positive integer" =
        is.numeric(limit) && length(limit) == 1 && limit > 0 && floor(limit) == limit,
    "`offset` must be NULL or a non-negative integer" =
        is.null(offset) || (is.numeric(offset) && length(offset) == 1 && offset >= 0 && floor(offset) == offset),
    "`device_type` must be NULL or one of 'iphone', 'ipad', 'total'" =
        is.null(device_type) || (is.character(device_type) && length(device_type) == 1 && device_type %in% c("iphone", "ipad", "total")),
    "`custom_fields_filter_id` must be NULL or a character string" =
        is.null(custom_fields_filter_id) || (is.character(custom_fields_filter_id) && length(custom_fields_filter_id) == 1)
  )

  # Validate date format
  start_date_str <- tryCatch({ format(as.Date(date), "%Y-%m-%d") }, error = function(e) rlang::abort("Invalid format for 'date'. Please use Date object or 'YYYY-MM-DD' string."))

  # Specific logical checks
  if (measure == "MAU" && time_range == "week") {
      rlang::abort("Sensor Tower API documentation indicates time_range='week' is not supported when measure='MAU'.")
  }
  # Optional warnings based on observed API behavior/docs
  # if (!time_range %in% c("month", "quarter")) {
  #      rlang::warn(paste0("Using time_range='", time_range, "'. API examples only showed 'month'/'quarter'. Ensure validity."))
  # }

  # --- Authentication ---
  auth_token_val <- auth_token %||% Sys.getenv("SENSORTOWER_AUTH_TOKEN")
  if (auth_token_val == "") {
    rlang::abort("Sensor Tower authentication token not found. Set SENSORTOWER_AUTH_TOKEN environment variable or pass via the auth_token argument.")
  }

  # --- Prepare Query Parameters ---
  query_params <- list(
    auth_token = auth_token_val,
    comparison_attribute = comparison_attribute,
    time_range = time_range,
    measure = measure,
    date = start_date_str,
    category = if (!is.null(category)) as.character(category) else NULL,
    regions = paste(regions, collapse = ","),
    limit = limit,
    offset = offset,
    # Handle device_type: default to 'total' for ios if NULL, else NULL for android
    device_type = if (os == "ios") { device_type %||% "total" } else { NULL },
    custom_fields_filter_id = custom_fields_filter_id
  )

  # Remove parameters that are NULL
  query_params <- Filter(Negate(is.null), query_params)

  # --- Build Request ---
  api_path <- file.path("v1", os, "top_and_trending", "active_users")

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
         rlang::warn(paste("Received HTTP 422 (Unprocessable Entity). Check parameters/combinations (esp. device_type for iOS). Response body:", body_text))
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