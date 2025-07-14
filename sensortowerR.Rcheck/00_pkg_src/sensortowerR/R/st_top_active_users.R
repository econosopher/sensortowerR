#' Fetch Top Apps by Active User Estimates
#'
#' Retrieves top apps from Sensor Tower based on Daily Active Users (DAU),
#' Weekly Active Users (WAU), or Monthly Active Users (MAU). Allows comparison
#' using absolute values, delta, or transformed delta. Targets the
#' `/v1/{os}/top_and_trending/active_users` endpoint.
#'
#' @param os Required. Character string. Operating System. Must be one of
#'   "ios", "android", or "unified".
#' @param comparison_attribute Required. Character string. Comparison attribute
#'   type. Must be one of "absolute", "delta", or "transformed_delta".
#' @param time_range Required. Character string. Time granularity (e.g.,
#'   "month", "quarter"). Note: API docs state "week" is *not* available when
#'   `measure` is "MAU".
#' @param measure Required. Character string. Metric to measure. Must be one of
#'   "DAU", "WAU", or "MAU".
#' @param date Required. Character string or Date object. Start date for the
#'   query in "YYYY-MM-DD" format.
#' @param regions Required. Character vector or comma-separated string. Region
#'   codes (e.g., `"US"`, `c("US", "GB")`, `"WW"`) to filter results.
#' @param category Optional. Character string or numeric. The ID of the
#'   category to filter by. Use `st_categories()` to find valid IDs.
#' @param limit Optional. Integer. Maximum number of apps to return per call.
#'   Defaults to 25.
#' @param offset Optional. Integer. Number of apps to skip for pagination.
#'   Useful for retrieving results beyond the `limit`. Defaults to NULL.
#' @param device_type Optional. Character string. For `os = "ios"` or
#'   `os = "unified"`: "iphone", "ipad", or "total". Defaults to `"total"` if
#'   `os` is "ios" or "unified". Leave blank for `os = "android"`.
#' @param custom_fields_filter_id Optional. Character string. ID of a Sensor
#'   Tower custom field filter to apply.
#' @param custom_tags_mode Optional. Character string. Required if `os` is
#'   'unified' and `custom_fields_filter_id` is provided.
#' @param data_model Optional. Character string. The data model to use for the
#'   query. Defaults to "DM_2025_Q2".
#' @param auth_token Optional. Character string. Your Sensor Tower API
#'   authentication token.
#' @param base_url Optional. Character string. The base URL for the Sensor Tower
#'   API. Defaults to `"https://api.sensortower.com"`.
#' @param enrich_response Optional. Logical. If `TRUE` (default), the function
#'   will automatically enrich the response with app metadata like names by
#'   unnesting the `entities` column from the API response.
#'
#' @return A [tibble][tibble::tibble] (data frame) with top app estimates. If
#'   `enrich_response` is `TRUE`, it includes app metadata like `app.name`.
#'
#' @section API Endpoint Used:
#'   `GET /v1/{os}/top_and_trending/active_users`
#'
#' @section Common Issues (HTTP 422 Error):
#'   An HTTP 422 "Unprocessable Entity" error often indicates invalid
#'   parameters or combinations.
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
#' # Example 1: Top iOS Social apps by MAU
#' top_ios_social_mau <- st_top_active_users(
#'   os = "ios",
#'   comparison_attribute = "absolute",
#'   time_range = "month",
#'   measure = "MAU",
#'   date = "2023-10-01",
#'   category = 6016,
#'   regions = c("US", "GB"),
#'   limit = 10
#' )
#' print(top_ios_social_mau)
#' }
st_top_active_users <- function(os,
                                comparison_attribute,
                                time_range,
                                measure,
                                date,
                                regions,
                                category = NULL,
                                limit = 25,
                                offset = NULL,
                                device_type = NULL,
                                custom_fields_filter_id = NULL,
                                custom_tags_mode = NULL,
                                data_model = "DM_2025_Q2",
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
    limit = limit,
    offset = offset,
    device_type = device_type,
    custom_fields_filter_id = custom_fields_filter_id,
    custom_tags_mode = custom_tags_mode,
    data_model = data_model
  )

  # --- Authentication ---
  auth_token_val <- auth_token %||% Sys.getenv("SENSORTOWER_AUTH_TOKEN")
  if (auth_token_val == "") {
    rlang::abort(
      "Authentication token not found. Please set it as an environment variable."
    )
  }

  # --- Prepare Query Parameters ---
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

  # --- Build and Perform Request ---
  path <- c("v1", os, "top_and_trending", "active_users")
  req <- build_request(base_url, path, query_params)
  resp <- perform_request(req)

  # --- Process Response ---
  process_response(resp, enrich_response = enrich_response)
}