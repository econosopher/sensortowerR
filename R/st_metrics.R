#' Fetch Sensor Tower Metrics for a Unified App
#'
#' Retrieves daily sales estimates and active user metrics for a specified
#' unified application ID from the Sensor Tower API over a given date range.
#' It combines these metrics into a single tibble.
#'
#' @param unified_app_id Character string. The unified app ID for which to fetch
#'   metrics (required).
#' @param start_date Date object or character string (YYYY-MM-DD). The start date
#'   for data collection (required).
#' @param end_date Date object or character string (YYYY-MM-DD). The end date for
#'   data collection (required).
#' @param auth_token Character string. Your Sensor Tower API authentication token.
#'   Defaults to the value stored in the `SENSORTOWER_AUTH_TOKEN` environment
#'   variable.
#'
#' @return A [tibble][tibble::tibble] containing combined sales and usage metrics.
#'   Columns are: `unified_app_id`, `date` (Date object), `country`
#'   (character), `revenue` (numeric, from unified_revenue), `downloads`
#'   (numeric, from unified_units), `active_users` (numeric, sum of platform users).
#'   Returns an empty tibble with the correct structure if no data is found or an
#'   unrecoverable error occurs.
#'
#' @section API Endpoints Used:
#'   - `GET /v1/unified/sales_report_estimates`
#'   - `GET /v1/unified/usage/active_users`
#'   *(Verification needed for exact parameters used by these endpoints)*
#'
#' @examples
#' \dontrun{
#' # Ensure SENSORTOWER_AUTH_TOKEN is set in your environment
#' # Sys.setenv(SENSORTOWER_AUTH_TOKEN = "your_secure_auth_token_here")
#'
#' # Define the unified app ID (use a real, valid ID)
#' unified_app_id <- "YOUR_VALID_UNIFIED_APP_ID" # e.g., "55c5025102ac64f9c0001f96"
#'
#' # Define the date range
#' start_date <- Sys.Date() - 30
#' end_date <- Sys.Date() - 1
#'
#' # Fetch the metrics
#' metrics <- st_metrics(
#'   unified_app_id = unified_app_id,
#'   start_date = start_date,
#'   end_date = end_date
#' )
#'
#' # View the metrics
#' print(metrics)
#' head(metrics)
#' }
#'
#' @importFrom httr GET http_error status_code content http_status
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble as_tibble
#' @importFrom rlang abort warn %||%
#' @importFrom dplyr %>% rename mutate select full_join bind_rows filter all_of rowwise ungroup
#' @importFrom utils str
#' @export
st_metrics <- function(unified_app_id,
                                       start_date,
                                       end_date,
                                       auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN")) {

  # --- Input Validation and Setup ---
  auth_token_val <- auth_token %||% ""
  if (!is.character(auth_token_val) || nchar(auth_token_val) == 0) {
    rlang::abort("Authentication token is required. Set SENSORTOWER_AUTH_TOKEN environment variable or pass via auth_token argument.")
  }
  stopifnot(
    "`unified_app_id` must be a single non-empty character string" =
      is.character(unified_app_id) && length(unified_app_id) == 1 && nzchar(unified_app_id),
    "`start_date` must be provided" = !missing(start_date),
    "`end_date` must be provided" = !missing(end_date)
  )
  start_date_obj <- tryCatch({ as.Date(start_date) }, error = function(e) rlang::abort("Invalid format for 'start_date'. Please use Date object or 'YYYY-MM-DD' string."))
  end_date_obj   <- tryCatch({ as.Date(end_date) }, error = function(e) rlang::abort("Invalid format for 'end_date'. Please use Date object or 'YYYY-MM-DD' string."))
  if (start_date_obj > end_date_obj) {
    rlang::abort("'start_date' must be earlier than or equal to 'end_date'.")
  }

  sales_endpoint <- "https://api.sensortower.com/v1/unified/sales_report_estimates"
  usage_endpoint <- "https://api.sensortower.com/v1/unified/usage/active_users"

  empty_result_structure <- tibble::tibble(
    unified_app_id = character(),
    date = as.Date(character()),
    country = character(),
    revenue = numeric(),
    downloads = numeric(),
    active_users = numeric()
  )

  # --- Internal Helper Function ---
  fetch_api_data <- function(endpoint, params, data_type = "data") {
    response <- tryCatch({ httr::GET(url = endpoint, query = params) }, error = function(e) {
        rlang::warn(paste("HTTP request failed while fetching", data_type, ":", e$message)); return(tibble::tibble()) })
    if (httr::http_error(response)) {
      status <- httr::status_code(response); reason <- httr::http_status(response)$reason
      error_content <- httr::content(response, "text", encoding = "UTF-8")
      error_message <- tryCatch({ jsonlite::fromJSON(error_content)$error %||% error_content }, error = function(e) error_content)
      rlang::warn(paste("HTTP error", status, reason, "while fetching", data_type, ":", error_message)); return(tibble::tibble()) }
    content_text <- httr::content(response, as = "text", encoding = "UTF-8")
    if(content_text == "" || is.null(content_text)) { message("API returned empty body for ", data_type); return(tibble::tibble()) }
    parsed_data <- tryCatch({ jsonlite::fromJSON(content_text, flatten = TRUE) }, error = function(e) {
      rlang::warn(paste("Failed to parse JSON for", data_type, ":", e$message)); return(tibble::tibble()) })

    # --- START DEBUG BLOCK (Optional: Keep or remove after debugging) ---
    # if (!is.null(parsed_data) && (is.data.frame(parsed_data) || (is.list(parsed_data) && length(parsed_data)>0) )) {
    #     message("--- DEBUG: Parsed Data Structure for ", data_type, " ---")
    #     print(utils::str(parsed_data, max.level = 2, list.len = 5))
    #     message("--- DEBUG: Parsed Data Names for ", data_type, " ---")
    #     if(is.data.frame(parsed_data)) { print(names(parsed_data))
    #     } else if (is.list(parsed_data) && length(parsed_data) > 0 && is.list(parsed_data[[1]])) {
    #          message("(Showing names from first list element)"); print(names(parsed_data[[1]]))
    #     } else { message("(Data is not a data frame or list of lists)"); print(names(parsed_data)) }
    #     message("--- END DEBUG ---")
    # } else { message("--- DEBUG: Parsed data for ", data_type, " was NULL or empty list/vector. ---") }
    # --- END DEBUG BLOCK ---


    if (is.list(parsed_data) && length(parsed_data) > 0 && !is.data.frame(parsed_data)) {
        result_df <- tryCatch({ dplyr::bind_rows(parsed_data) }, error = function(e) {
             rlang::warn(paste("Could not bind rows for", data_type, ". Check API response structure.")); return(tibble::tibble()) })
        return(tibble::as_tibble(result_df))
    } else if (is.data.frame(parsed_data)) { return(tibble::as_tibble(parsed_data))
    } else { message("No data structure found after parsing JSON for ", data_type); return(tibble::tibble()) }
  }

  # --- Fetch Data ---
  message("Fetching sales data...")
  sales_params <- list(app_ids = unified_app_id, date_granularity = "daily", start_date = as.character(start_date_obj), end_date = as.character(end_date_obj), auth_token = auth_token_val)
  sales_raw <- fetch_api_data(sales_endpoint, sales_params, data_type = "sales data")

  message("Fetching usage data...")
  usage_params <- list(app_ids = unified_app_id, time_period = "day", start_date = as.character(start_date_obj), end_date = as.character(end_date_obj), auth_token = auth_token_val)
  usage_raw <- fetch_api_data(usage_endpoint, usage_params, data_type = "usage data")

  # --- Process and Combine Data ---
  message("Processing and combining data...")

  # Define ACTUAL columns returned by API needed for processing
  id_col_name <- "app_id" # Confirmed by debug output
  sales_cols_api <- c(id_col_name, "date", "country", "unified_revenue", "unified_units")
  usage_cols_api <- c(id_col_name, "date", "country", "android_users", "ipad_users", "iphone_users")

  # Process Sales Data
  sales_processed <- if (nrow(sales_raw) > 0 && all(sales_cols_api %in% names(sales_raw))) {
      sales_raw %>%
        dplyr::select(dplyr::all_of(sales_cols_api)) %>%
        # Rename to desired output names
        dplyr::rename(
            unified_app_id = !!id_col_name,
            revenue = .data$unified_revenue,
            downloads = .data$unified_units
         ) %>%
        dplyr::mutate(date = as.Date(.data$date)) # Ensure date type
  } else {
      message("Sales data missing expected API columns (", paste(sales_cols_api, collapse=", "), ") or is empty.")
      # Return empty tibble matching the *final* desired structure for sales part
      empty_result_structure %>% dplyr::select(.data$unified_app_id, .data$date, .data$country, .data$revenue, .data$downloads)
  }

  # Process Usage Data
  usage_processed <- if (nrow(usage_raw) > 0 && all(usage_cols_api %in% names(usage_raw))) {
      usage_raw %>%
        dplyr::select(dplyr::all_of(usage_cols_api)) %>%
        # Sum platform users to create active_users, handle potential NAs
        dplyr::rowwise() %>% # Process row by row for summing
        dplyr::mutate(
            active_users = sum(c(.data$android_users, .data$ipad_users, .data$iphone_users), na.rm = TRUE)
         ) %>%
        dplyr::ungroup() %>% # Important to ungroup after rowwise
        # Rename ID and select final columns
        dplyr::rename(unified_app_id = !!id_col_name) %>%
        dplyr::select(.data$unified_app_id, .data$date, .data$country, .data$active_users) %>%
        dplyr::mutate(date = as.Date(.data$date)) # Ensure date type
  } else {
      message("Usage data missing expected API columns (", paste(usage_cols_api, collapse=", "), ") or is empty.")
      # Return empty tibble matching the *final* desired structure for usage part
      empty_result_structure %>% dplyr::select(.data$unified_app_id, .data$date, .data$country, .data$active_users)
  }

  # Combine using full_join
  combined <- tryCatch({
      # Ensure joining columns exist even if one side is totally empty
      dplyr::full_join(sales_processed, usage_processed, by = c("unified_app_id", "date", "country"))
  }, error = function(e) {
      rlang::warn(paste("Failed to join sales and usage data:", e$message))
      return(empty_result_structure) # Return defined empty structure on join error
  })


  # Final check if combined is empty or lacks structure
  if (!inherits(combined, "data.frame") || nrow(combined) == 0) {
    rlang::warn("No data was successfully fetched and combined for the specified unified app ID and date range.")
     if (!identical(lapply(combined, class), lapply(empty_result_structure, class))) {
         rlang::warn("Resulting combined data frame has unexpected structure/types when empty. Returning defined empty structure.")
         combined <- empty_result_structure
     } else if (nrow(combined) == 0) {
         # If it's already an empty df with right structure, ensure it's a tibble
         combined <- tibble::as_tibble(combined)
     }
  } else {
      # Ensure final result is a tibble if not empty
      combined <- tibble::as_tibble(combined)
  }


  message("Finished fetching metrics.")
  return(combined)
}