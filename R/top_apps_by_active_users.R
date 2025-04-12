#' Fetch Top Apps by Active User Estimates
#'
#' Retrieves top apps from Sensor Tower based on Daily Active Users (DAU),
#' Weekly Active Users (WAU), or Monthly Active Users (MAU).
#' Allows comparison using absolute values, delta, or transformed delta.
#'
#' @param os Required. Operating System. Either "ios" or "android".
#' @param comparison_attribute Required. Comparison attribute type. One of
#'   "absolute", "delta", or "transformed_delta".
#' @param time_range Required. Time granularity. One of "month", or "quarter".
#'   Note: "week" is *not* available when `measure` is "MAU". Sensor Tower
#'   documentation for this endpoint in the PDF doesn't explicitly show "week"
#'   or "day", verify allowed values if needed. Assuming month/quarter based on MAU.
#' @param measure Required. Metric to measure. One of "DAU", "WAU", or "MAU".
#' @param date Required. Start date for the query in "YYYY-MM-DD" format.
#'   Should match the beginning of the range (e.g., "2023-01-01" for a month/quarter).
#' @param category Optional. The ID of the category to filter by. See Sensor
#'   Tower documentation. Example: 6016 (iOS Social Networking).
#' @param regions Optional. A character vector of region codes (e.g., "US", "GB")
#'   to filter results. If NULL (default), fetches global data where applicable.
#'   Regions parameter *must* be specified according to API docs. Often "WW"
#'   (Worldwide) or specific country codes are needed. Check API requirements.
#' @param limit Optional. Integer. Maximum number of apps to return per call.
#'   Defaults to 25.
#' @param offset Optional. Integer. Number of apps to skip for pagination.
#'   Defaults to 0.
#' @param device_type Optional. Character string. For "ios" only: "iphone",
#'   "ipad", or "total". Note says iOS only for this endpoint.
#' @param custom_fields_filter_id Optional. Character string. ID of a custom
#'   field filter to apply.
#' @param auth_token Optional. Your Sensor Tower API authentication token.
#'   It's recommended to set the `SENSORTOWER_AUTH_TOKEN` environment
#'   variable instead of passing this argument directly.
#' @param base_url Optional. The base URL for the Sensor Tower API. Defaults to
#'   `https://api.sensortower.com`.
#'
#' @return A tibble (data frame) containing the requested top app active user
#'   estimates. Columns correspond to fields like app_id, date, users_absolute, etc.
#'
#' @inheritParams st_get_sales_estimates
#' @importFrom httr2 request req_url_path_append req_url_query req_perform
#'   resp_body_json req_user_agent req_error resp_check_status
#' @importFrom jsonlite fromJSON
#' @importFrom rlang abort set_names %||%
#' @importFrom dplyr bind_rows
#' @importFrom utils URLencode
#' @importFrom tibble as_tibble tibble
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Ensure SENSORTOWER_AUTH_TOKEN environment variable is set
#'
#' # Get top 10 iOS Social apps by absolute MAU for Jan 2023 in US & GB
#' top_social_mau <- st_get_active_users(
#'   os = "ios",
#'   comparison_attribute = "absolute",
#'   time_range = "month",
#'   measure = "MAU",
#'   date = "2023-01-01",
#'   category = 6016, # Example: iOS Social Networking ID
#'   regions = c("US", "GB"),
#'   limit = 10,
#'   device_type = "total"
#' )
#'
#' print(top_social_mau)
#'
#' # Get top 5 Android apps (all categories) by DAU delta for 2023-03-15 worldwide
#' # Note: DAU might imply time_range='day', but docs show month/quarter. Verify API.
#' # Assuming 'month' is required even for DAU measure as per screenshot example
#' top_android_dau <- st_get_active_users(
#'   os = "android",
#'   comparison_attribute = "delta",
#'   time_range = "month", # Adjust if API allows 'day' for DAU/WAU
#'   measure = "DAU",
#'   date = "2023-03-01", # Start of month for the day? API is quirky here.
#'   # category = NULL, # Optional, leave NULL for all categories
#'   regions = "WW",
#'   limit = 5
#' )
#' print(top_android_dau)
#' }
st_get_active_users <- function(os,
                                comparison_attribute,
                                time_range, # Check allowed values based on measure
                                measure,
                                date,
                                category = NULL,
                                regions = NULL, # API implies required
                                limit = 25,
                                offset = NULL,
                                device_type = NULL,
                                custom_fields_filter_id = NULL,
                                auth_token = NULL,
                                base_url = "https://api.sensortower.com") {

  # --- Input Validation ---
  stopifnot(
    is.character(os) && length(os) == 1 && os %in% c("ios", "android"), # Doc doesn't show 'unified' here
    is.character(comparison_attribute) && length(comparison_attribute) == 1 && comparison_attribute %in% c("absolute", "delta", "transformed_delta"),
    is.character(time_range) && length(time_range) == 1, # Allow flexibility, maybe add specific checks later
    is.character(measure) && length(measure) == 1 && measure %in% c("DAU", "WAU", "MAU"),
    is.character(date) && length(date) == 1 && grepl("^\\d{4}-\\d{2}-\\d{2}$", date),
    is.null(category) || is.character(category) || is.numeric(category), # Allow string or number for category ID
    is.null(regions) || is.character(regions), # Should be specified
    is.numeric(limit) && length(limit) == 1 && limit > 0,
    is.null(offset) || (is.numeric(offset) && length(offset) == 1 && offset >= 0),
    is.null(device_type) || (is.character(device_type) && length(device_type) == 1),
    is.null(custom_fields_filter_id) || (is.character(custom_fields_filter_id) && length(custom_fields_filter_id) == 1)
  )

  # Add specific validation based on API constraints if known
  if (measure == "MAU" && time_range == "week") {
      rlang::abort("Sensor Tower API does not support time_range='week' when measure='MAU'.")
  }
   if (os == "android" && !is.null(device_type) && device_type != "") {
     warning("The 'device_type' parameter is only applicable for os='ios'. It will be ignored for Android.", call. = FALSE)
     device_type <- NULL
   }
  if (is.null(regions)) {
    warning("The 'regions' parameter is often required by the Sensor Tower API (e.g., use 'WW' for Worldwide or specific country codes like 'US'). Ensure you provide a valid region specification if needed.", call. = FALSE)
     # Or make it mandatory:
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
    regions = if (!is.null(regions)) paste(regions, collapse = ","),
    limit = limit,
    offset = offset,
    device_type = if (os == "ios") device_type else NULL,
    custom_fields_filter_id = custom_fields_filter_id
  )

  # Remove NULL parameters
  query_params <- Filter(Negate(is.null), query_params)

  # --- Build Request ---
  req <- httr2::request(base_url) |>
    httr2::req_user_agent("sensortowerR (https://github.com/peterparkerspicklepatch/sensortowerR)") |>
    httr2::req_url_path_append("v1") |>
    httr2::req_url_path_append(os) |>
    httr2::req_url_path_append("top_and_trending") |>
    httr2::req_url_path_append("active_users") |>
    httr2::req_url_query(!!!query_params) |>
    httr2::req_error(is_error = function(resp) FALSE) # Handle errors manually

  # --- Perform Request & Process Response (Reusing logic from previous function) ---
  resp <- httr2::req_perform(req)
  status_code <- httr2::resp_status(resp)
  body_raw <- httr2::resp_body_raw(resp)

  if (length(body_raw) == 0) {
     if (status_code >= 200 && status_code < 300) {
       warning("API returned a successful status code but an empty response body.", call. = FALSE)
       return(dplyr::tibble())
     } else {
       httr2::resp_check_status(resp)
     }
  }

   parsed_body <- tryCatch({
    jsonlite::fromJSON(rawToChar(body_raw), flatten = TRUE)
  }, error = function(e) {
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


  if (status_code >= 400) {
     api_error_message <- "Unknown API error"
     if(is.list(parsed_body) && "error" %in% names(parsed_body)) {
       api_error_message <- parsed_body$error
     } else if (is.list(parsed_body) && "errors" %in% names(parsed_body) && is.list(parsed_body$errors) && length(parsed_body$errors) > 0 && "title" %in% names(parsed_body$errors[[1]])) {
       api_error_message <- parsed_body$errors[[1]]$title
     }
     httr2::resp_check_status(resp, info = paste("API Error:", api_error_message))
  }

  # --- Format Output ---
    if (is.list(parsed_body) && length(parsed_body) > 0 && !is.data.frame(parsed_body)) {
        # Check if it's a list containing a single data frame (sometimes happens with fromJSON)
        if (length(parsed_body) == 1 && is.data.frame(parsed_body[[1]])) {
             result_df <- parsed_body[[1]]
        } else {
             # Attempt to bind rows if it's a list of records
             result_df <- tryCatch({
                 dplyr::bind_rows(parsed_body)
             }, error = function(e) {
                 warning("Could not directly bind rows from parsed JSON. Returning raw parsed list.", call. = FALSE)
                 return(parsed_body)
             })
        }
         return(tibble::as_tibble(result_df))

    } else if (is.data.frame(parsed_body)) {
       return(tibble::as_tibble(parsed_body))
    } else {
      warning("API response was not in the expected list-of-records or data frame format. Returning raw parsed response.", call. = FALSE)
      return(parsed_body)
    }
}