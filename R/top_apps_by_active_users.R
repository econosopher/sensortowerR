#' Fetch Top Apps by Active User Estimates
#'
#' Retrieves top apps from Sensor Tower based on Daily Active Users (DAU),
#' Weekly Active Users (WAU), or Monthly Active Users (MAU). Allows comparison
#' using absolute values, delta, or transformed delta. This function targets the
#' `/v1/{os}/top_and_trending/active_users` endpoint.
#'
#' @param os Required. Character string. Operating System. Must be one of
#'   "ios" or "android". Note: "unified" does not appear supported by this
#'   specific endpoint based on provided documentation.
#' @param comparison_attribute Required. Character string. Comparison attribute
#'   type. Must be one of "absolute", "delta", or "transformed_delta".
#' @param time_range Required. Character string. Time granularity. Based on API
#'   documentation examples, likely "month" or "quarter". Note: The API docs
#'   state "week" is *not* available when `measure` is "MAU". Verify allowed
#'   values if using DAU/WAU, as "day" or "week" might be valid but weren't
#'   explicitly shown in the provided examples for this endpoint.
#' @param measure Required. Character string. Metric to measure. Must be one of
#'   "DAU", "WAU", or "MAU".
#' @param date Required. Character string. Start date for the query in
#'   "YYYY-MM-DD" format. Should typically match the beginning of the
#'   `time_range` (e.g., "2023-01-01" for a month or quarter).
#' @param category Optional. Character string or numeric. The ID of the category
#'   to filter by (e.g., 6016 for iOS Social Networking). If NULL (default),
#'   results for all categories are typically returned.
#' @param regions Optional, but often required by API. Character vector of
#'   region codes (e.g., `c("US", "GB")`) to filter results. If NULL (default),
#'   global data might be fetched *if* the API allows, but often specific codes
#'   or "WW" (Worldwide) are necessary. Consult API documentation.
#' @param limit Optional. Integer. Maximum number of apps to return per call.
#'   Defaults to 25.
#' @param offset Optional. Integer. Number of apps to skip for pagination.
#'   Useful for retrieving results beyond the `limit`. Defaults to NULL (meaning 0).
#' @param device_type Optional. Character string. For `os = "ios"` only:
#'   "iphone", "ipad", or "total". API documentation notes this is iOS-only
#'   for this endpoint. Defaults to NULL.
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
#'   an empty tibble if the API call is successful but returns no data.
#'
#' @section Authentication:
#' It's best practice to store your Sensor Tower API token as an environment
#' variable named `SENSORTOWER_AUTH_TOKEN`. You can set this in your `.Renviron`
#' file. The function will automatically use this environment variable if the
#' `auth_token` argument is not provided.
#'
#' @section API Endpoint:
#' `GET /v1/{os}/top_and_trending/active_users`
#'
#' @importFrom httr2 request req_user_agent req_url_path_append req_url_query
#'   req_error req_perform resp_status resp_body_raw resp_check_status
#' @importFrom jsonlite fromJSON
#' @importFrom rlang abort %||% warn
#' @importFrom dplyr bind_rows tibble
#' @importFrom tibble as_tibble
#' @importFrom utils URLencode
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Ensure SENSORTOWER_AUTH_TOKEN environment variable is set before running
#' # Sys.setenv(SENSORTOWER_AUTH_TOKEN = "YOUR_TOKEN_HERE")
#'
#' # Example 1: Get top 10 iOS Social apps by absolute MAU for Jan 2023 in US & GB
#' top_ios_social_mau <- st_get_active_users(
#'   os = "ios",
#'   comparison_attribute = "absolute",
#'   time_range = "month",
#'   measure = "MAU",
#'   date = "2023-01-01",
#'   category = 6016, # Example: iOS Social Networking ID
#'   regions = c("US", "GB"),
#'   limit = 10,
#'   device_type = "total" # Optional for iOS
#' )
#'
#' print(top_ios_social_mau)
#'
#' # Example 2: Get top 5 Android apps (all categories) by DAU delta
#' # for month starting 2023-03-01 worldwide.
#' # Note: API docs example used 'month' time_range even for DAU measure.
#' top_android_dau_delta <- st_get_active_users(
#'   os = "android",
#'   comparison_attribute = "delta",
#'   time_range = "month", # Verify if 'day' is allowed by API for DAU
#'   measure = "DAU",
#'   date = "2023-03-01",
#'   # category = NULL, # Default: All categories
#'   regions = "WW",    # Worldwide
#'   limit = 5
#' )
#' print(top_android_dau_delta)
#'
#' }
st_get_active_users <- function(os,
                                comparison_attribute,
                                time_range,
                                measure,
                                date,
                                category = NULL,
                                regions = NULL,
                                limit = 25,
                                offset = NULL,
                                device_type = NULL,
                                custom_fields_filter_id = NULL,
                                auth_token = NULL,
                                base_url = "https://api.sensortower.com") {

  # --- Input Validation ---
  # Basic type/value checks
  stopifnot(
    is.character(os) && length(os) == 1 && os %in% c("ios", "android"), # Unified not shown in docs for this endpoint
    is.character(comparison_attribute) && length(comparison_attribute) == 1 && comparison_attribute %in% c("absolute", "delta", "transformed_delta"),
    is.character(time_range) && length(time_range) == 1, # Keep flexible, add specific checks below
    is.character(measure) && length(measure) == 1 && measure %in% c("DAU", "WAU", "MAU"),
    is.character(date) && length(date) == 1 && grepl("^\\d{4}-\\d{2}-\\d{2}$", date),
    is.null(category) || ((is.character(category) || is.numeric(category)) && length(category) == 1),
    is.null(regions) || is.character(regions),
    is.numeric(limit) && length(limit) == 1 && limit > 0 && floor(limit) == limit,
    is.null(offset) || (is.numeric(offset) && length(offset) == 1 && offset >= 0 && floor(offset) == offset),
    is.null(device_type) || (is.character(device_type) && length(device_type) == 1 && device_type %in% c("iphone", "ipad", "total")),
    is.null(custom_fields_filter_id) || (is.character(custom_fields_filter_id) && length(custom_fields_filter_id) == 1)
  )

  # Logical checks and warnings based on API documentation/common sense
  if (is.null(regions)) {
    rlang::warn("The 'regions' parameter was not specified. The Sensor Tower API often requires this (e.g., use 'WW' for Worldwide or specific country codes like 'US'). The query may fail or return unexpected results if regions are required by the API endpoint.")
  }

  if (measure == "MAU" && time_range == "week") {
      rlang::abort("Sensor Tower API documentation indicates time_range='week' is not supported when measure='MAU'.")
  }
  # Add warning if time_range seems unusual for measure, but allow API to decide
  if (measure %in% c("DAU", "WAU") && !time_range %in% c("day", "week", "month", "quarter")) {
      rlang::warn(paste0("Using time_range='", time_range, "' with measure='", measure, "'. Ensure this combination is supported by the API endpoint."))
  }
  if (!time_range %in% c("month", "quarter")) {
       rlang::warn(paste0("Using time_range='", time_range, "'. The provided API documentation examples only explicitly showed 'month' and 'quarter' for this endpoint. Ensure your chosen time_range is valid."))
  }


  if (os == "android" && !is.null(device_type)) {
     rlang::warn("The 'device_type' parameter is only applicable for os='ios' according to API docs for this endpoint. It will be ignored for Android. Setting device_type to NULL.")
     device_type <- NULL
   }

  # --- Authentication ---
  auth_token_val <- auth_token %||% Sys.getenv("SENSORTOWER_AUTH_TOKEN")
  if (auth_token_val == "") {
    rlang::abort(
      message = "Sensor Tower authentication token not found.",
      body = c(
        "i" = "Please provide your token via the `auth_token` argument.",
        "i" = "Or, set the `SENSORTOWER_AUTH_TOKEN` environment variable (recommended).",
        "i" = "You can set it temporarily using `Sys.setenv(SENSORTOWER_AUTH_TOKEN = 'YOUR_TOKEN')`.",
        "i" = "For persistent storage, add it to your .Renviron file."
      ),
      class = "sensortower_auth_error"
    )
  }

  # --- Prepare Query Parameters ---
  query_params <- list(
    auth_token = auth_token_val,
    comparison_attribute = comparison_attribute,
    time_range = time_range,
    measure = measure,
    date = date,
    category = if (!is.null(category)) as.character(category) else NULL, # Ensure category is character if provided
    # Collapse regions vector into comma-separated string if not NULL
    regions = if (!is.null(regions)) paste(regions, collapse = ",") else NULL,
    limit = limit,
    offset = offset,
    device_type = device_type, # Already nulled for Android above
    custom_fields_filter_id = custom_fields_filter_id
  )

  # Remove parameters that are NULL
  query_params <- Filter(Negate(is.null), query_params)

  # --- Build Request ---
  # Construct the dynamic path part
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
      # Handle low-level connection errors
      rlang::abort(paste("HTTP request failed:", e$message), parent = e, class = "sensortower_http_error")
  })

  # --- Process Response ---
  # (Reusing the robust response handling logic from st_get_sales_estimates)
  status_code <- httr2::resp_status(resp)
  body_raw <- httr2::resp_body_raw(resp)

  if (length(body_raw) == 0) {
     if (status_code >= 200 && status_code < 300) {
       rlang::warn(paste("API returned status", status_code, "with an empty response body. Returning an empty tibble."))
       return(dplyr::tibble())
     } else {
       httr2::resp_check_status(resp, info = "API returned an error status with an empty response body.")
       rlang::abort(paste("API Error: Status", status_code, "with empty response."), class = "sensortower_api_error")
     }
  }

  body_text <- rawToChar(body_raw)
  parsed_body <- tryCatch({
    jsonlite::fromJSON(body_text, flatten = TRUE, simplifyVector = TRUE)
  }, error = function(e) {
    snippet <- substr(body_text, 1, 200)
    rlang::abort(
      message = "Failed to parse API response as JSON.",
      body = c(
        "*" = paste("Status code:", status_code),
        "*" = paste("Parsing Error:", e$message),
        "*" = paste("Response body snippet:", snippet, if(nchar(body_text)>200) "...")
      ),
      parent = e,
      class = "sensortower_json_error"
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
     httr2::resp_check_status(resp, info = api_error_message)
     rlang::abort(paste("API Error:", status_code, "-", api_error_message), class = "sensortower_api_error")
  }

  # --- Format Output ---
  if (is.data.frame(parsed_body)) {
    result_df <- parsed_body
  } else if (is.list(parsed_body) && length(parsed_body) > 0 && all(sapply(parsed_body, is.list))) {
    result_df <- tryCatch({
        dplyr::bind_rows(parsed_body)
      }, error = function(e) {
          rlang::warn(paste("Could not automatically bind rows from the parsed API response list due to incompatible structures. Returning the raw parsed list. Details:", e$message))
          return(parsed_body)
      })
  } else if (is.list(parsed_body) && length(parsed_body) == 0) {
      result_df <- dplyr::tibble()
  } else {
    rlang::warn("API response was parsed but not in the expected data frame or list-of-records format. Returning the raw parsed response.")
    return(parsed_body)
  }

  return(tibble::as_tibble(result_df))
}