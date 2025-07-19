#' Fetch Sensor Tower Metrics for a Unified App
#'
#' Retrieves daily sales estimates and active user metrics for a specified
#' unified application ID from the Sensor Tower API over a given date range.
#' It combines these metrics into a single tibble.
#'
#' @param unified_app_id Character string. The unified app ID for which to fetch
#'   metrics (required).
#' @param start_date Date object or character string (YYYY-MM-DD). The start
#'   date for data collection. Defaults to the start of the current month.
#' @param end_date Date object or character string (YYYY-MM-DD). The end date
#'   for data collection. Defaults to the current date.
#' @param auth_token Character string. Your Sensor Tower API authentication
#'   token. Defaults to the value stored in the `SENSORTOWER_AUTH_TOKEN`
#'   environment variable.
#'
#' @return A [tibble][tibble::tibble] containing combined sales and usage
#'   metrics. Columns are: `unified_app_id`, `date` (Date object), `country`
#'   (character), `revenue` (numeric, from unified_revenue), `downloads`
#'   (numeric, from unified_units), `active_users` (numeric, sum of platform
#'   users). Returns an empty tibble with the correct structure if no data is

#'   found or an unrecoverable error occurs.
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
#' unified_app_id <- "YOUR_VALID_UNIFIED_APP_ID"
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
#' @importFrom rlang abort warn `%||%`
#' @importFrom dplyr %>% rename mutate select full_join bind_rows filter all_of
#'   rowwise ungroup
#' @importFrom utils str
#' @importFrom lubridate floor_date
#' @export
st_metrics <- function(unified_app_id,
                       start_date = NULL,
                       end_date = NULL,
                       auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN")) {
  # --- Handle Default Dates ---
  if (is.null(start_date)) {
    start_date <- lubridate::floor_date(Sys.Date(), "month")
  }
  if (is.null(end_date)) {
    end_date <- Sys.Date()
  }
  
  # --- Input Validation and Setup ---
  auth_token_val <- auth_token %||% ""
  if (!is.character(auth_token_val) || nchar(auth_token_val) == 0) {
    rlang::abort(
      paste(
        "Authentication token is required.",
        "Set SENSORTOWER_AUTH_TOKEN environment variable",
        "or pass via auth_token argument."
      )
    )
  }
  stopifnot(
    "`unified_app_id` must be a single non-empty character string" =
      is.character(unified_app_id) &&
        length(unified_app_id) == 1 && nzchar(unified_app_id),
    "`start_date` must be provided" = !missing(start_date),
    "`end_date` must be provided" = !missing(end_date)
  )

  try_as_date <- function(d, var) {
    tryCatch(as.Date(d),
      error = function(e) {
        rlang::abort(
          paste0("Invalid format for '", var, "'. ",
                 "Please use Date object or 'YYYY-MM-DD' string.")
        )
      }
    )
  }
  start_date_obj <- try_as_date(start_date, "start_date")
  end_date_obj <- try_as_date(end_date, "end_date")

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
    response <- tryCatch(
      httr::GET(url = endpoint, query = params),
      error = function(e) {
        rlang::warn(paste("HTTP request failed:", e$message))
        return(NULL)
      }
    )

    if (is.null(response) || httr::http_error(response)) {
      status <- if (!is.null(response)) httr::status_code(response) else "N/A"
      rlang::warn(paste("HTTP error", status, "while fetching", data_type))
      return(tibble::tibble())
    }

    content_text <- httr::content(response, as = "text", encoding = "UTF-8")
    if (content_text == "") return(tibble::tibble())

    parsed_data <- tryCatch(
      jsonlite::fromJSON(content_text, flatten = TRUE),
      error = function(e) {
        rlang::warn(paste("Failed to parse JSON for", data_type))
        return(NULL)
      }
    )

    if (is.null(parsed_data)) return(tibble::tibble())

    if (is.list(parsed_data) && !is.data.frame(parsed_data)) {
      return(tibble::as_tibble(dplyr::bind_rows(parsed_data)))
    }
    tibble::as_tibble(parsed_data)
  }

  # --- Fetch Data ---
  message("Fetching sales data...")
  sales_params <- list(
    app_ids = unified_app_id,
    date_granularity = "daily",
    start_date = as.character(start_date_obj),
    end_date = as.character(end_date_obj),
    auth_token = auth_token_val
  )
  sales_raw <- fetch_api_data(sales_endpoint, sales_params, "sales data")

  message("Fetching usage data...")
  usage_params <- list(
    app_ids = unified_app_id,
    time_period = "day",
    start_date = as.character(start_date_obj),
    end_date = as.character(end_date_obj),
    auth_token = auth_token_val
  )
  usage_raw <- fetch_api_data(usage_endpoint, usage_params, "usage data")

  # --- Process and Combine Data ---
  message("Processing and combining data...")

  id_col_name <- "app_id"
  sales_cols_api <- c(id_col_name, "date", "country",
                      "unified_revenue", "unified_units")
  usage_cols_api <- c(id_col_name, "date", "country",
                      "android_users", "ipad_users", "iphone_users")

  # Process Sales Data
  sales_processed <- if (nrow(sales_raw) > 0) {
    sales_raw %>%
      dplyr::select(dplyr::all_of(sales_cols_api)) %>%
      dplyr::rename(
        unified_app_id = !!id_col_name,
        revenue = .data$unified_revenue,
        downloads = .data$unified_units
      ) %>%
      dplyr::mutate(date = as.Date(.data$date))
  } else {
    empty_result_structure %>%
      dplyr::select(.data$unified_app_id, .data$date, .data$country,
                    .data$revenue, .data$downloads)
  }

  # Process Usage Data
  usage_processed <- if (nrow(usage_raw) > 0) {
    usage_raw %>%
      dplyr::select(dplyr::all_of(usage_cols_api)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        active_users = sum(c(
          .data$android_users, .data$ipad_users, .data$iphone_users
        ), na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::rename(unified_app_id = !!id_col_name) %>%
      dplyr::select(
        .data$unified_app_id, .data$date, .data$country, .data$active_users
      ) %>%
      dplyr::mutate(date = as.Date(.data$date))
  } else {
    empty_result_structure %>%
      dplyr::select(
        .data$unified_app_id, .data$date, .data$country, .data$active_users
      )
  }

  # Combine using full_join
  combined <- dplyr::full_join(
    sales_processed,
    usage_processed,
    by = c("unified_app_id", "date", "country")
  )

  if (nrow(combined) == 0) {
    rlang::warn(
      paste(
        "No data was successfully fetched and combined for the",
        "specified unified app ID and date range."
      )
    )
  }

  message("Finished fetching metrics.")
  return(combined)
}