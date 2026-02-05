#' Fetch Unified Sales Report Estimates (All Regional SKUs Aggregated)
#'
#' Retrieves download and revenue estimates using the unified API endpoint,
#' which properly aggregates ALL regional SKUs (app variants from different
#' publishers/regions) within a unified_app_id.
#'
#' This function solves the problem where apps like "Watcher of Realms" have
#' multiple regional versions (e.g., Moonton, Shanghai Moonton, Vizta Games,
#' Skystone Games publishers) that need to be combined for accurate totals.
#'
#' @param unified_app_id Character string or vector. Sensor Tower unified app ID(s)
#'   (24-character hex format, e.g., "67ec0bf3e540b65904256cc4").
#' @param countries Character vector. Country codes (e.g., c("US", "GB", "JP"),
#'   or "WW" for worldwide). Required.
#' @param start_date Date or character string. Start date in "YYYY-MM-DD" format. Required.
#' @param end_date Date or character string. End date in "YYYY-MM-DD" format. Required.
#' @param date_granularity Character string. One of "daily", "weekly", "monthly", "quarterly". Required.
#' @param auth_token Optional. Character string. Your Sensor Tower API token.
#' @param verbose Logical. If TRUE, prints progress messages.
#'
#' @return A tibble with columns:
#'   - `date`: Date of the data point
#'   - `country`: Country code
#'   - `unified_app_id`: The unified app ID
#'   - `revenue`: Revenue in dollars (converted from cents)
#'   - `downloads`: Number of downloads
#'
#' @details
#' **Why use this instead of st_sales_report()?**
#'
#' When a game has multiple regional SKUs (same game published under different
#' publishers or app IDs in different regions), the standard `st_sales_report()`
#' function with ID resolution only fetches data for the FIRST iOS and Android
#' app ID. This can result in significantly undercounted revenue/downloads.
#'
#' Example: "Watcher of Realms" has 4 iOS apps and 3 Android apps across
#' different publishers (Moonton, Vizta Games, Skystone Games, etc.). Using
#' `st_sales_report()` with the unified_app_id might only fetch data for
#' 2 of these 7 apps.
#'
#' This function uses the `/v1/unified/sales_report_estimates` endpoint which
#' automatically aggregates ALL app IDs within the unified entity.
#'
#' **API Response Fields:**
#' The unified API returns `unified_revenue` and `unified_units` which are
#' automatically converted to `revenue` (dollars) and `downloads`.
#'
#' @examples
#' \dontrun{
#' # Get unified sales data for Watcher of Realms
#' sales <- st_unified_sales_report(
#'   unified_app_id = "67ec0bf3e540b65904256cc4",
#'   countries = "WW",
#'   start_date = "2024-01-01",
#'   end_date = "2024-12-31",
#'   date_granularity = "monthly"
#' )
#'
#' # Multiple apps at once
#' sales <- st_unified_sales_report(
#'   unified_app_id = c("67ec0bf3e540b65904256cc4", "5ba4585f539ce75b97db6bcb"),
#'   countries = c("US", "GB", "JP"),
#'   start_date = "2024-01-01",
#'   end_date = "2024-12-31",
#'   date_granularity = "monthly"
#' )
#' }
#'
#' @export
st_unified_sales_report <- function(unified_app_id,
                                    countries,
                                    start_date,
                                    end_date,
                                    date_granularity,
                                    auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN"),
                                    verbose = TRUE) {

  # Validate required parameters
  if (missing(unified_app_id) || is.null(unified_app_id) || length(unified_app_id) == 0) {
    rlang::abort("'unified_app_id' parameter is required. Provide one or more 24-character hex IDs.")
  }

  # Validate unified_app_id format
  invalid_ids <- unified_app_id[!grepl("^[a-f0-9]{24}$", unified_app_id)]
  if (length(invalid_ids) > 0) {
    rlang::abort(paste0(
      "Invalid unified_app_id format. Expected 24-character hex IDs.\n",
      "Invalid IDs: ", paste(invalid_ids, collapse = ", "), "\n",
      "Use st_app_info() to search for apps and get their unified_app_id."
    ))
  }

  if (missing(countries) || is.null(countries) || length(countries) == 0) {
    rlang::abort("'countries' parameter is required. Specify country codes (e.g., 'US', 'GB', 'JP', or 'WW' for worldwide).")
  }

  if (missing(start_date) || is.null(start_date)) {
    rlang::abort("'start_date' parameter is required. Specify in YYYY-MM-DD format.")
  }

  if (missing(end_date) || is.null(end_date)) {
    rlang::abort("'end_date' parameter is required. Specify in YYYY-MM-DD format.")
  }

  if (missing(date_granularity) || is.null(date_granularity)) {
    rlang::abort("'date_granularity' parameter is required. Specify one of: 'daily', 'weekly', 'monthly', 'quarterly'.")
  }

  date_granularity <- match.arg(date_granularity, c("daily", "weekly", "monthly", "quarterly"))

  # Convert dates to Date objects
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  # Authentication
  auth_token_val <- resolve_auth_token(
    auth_token,
    error_message = "Authentication token not found. Please set SENSORTOWER_AUTH_TOKEN environment variable."
  )

  if (verbose) {
    message("=== Unified Sales Report (All Regional SKUs) ===")
    message("  Apps: ", length(unified_app_id))
    message("  Countries: ", paste(countries, collapse = ", "))
    message("  Date range: ", start_date, " to ", end_date)
    message("  Granularity: ", date_granularity)
    message("================================================")
  }

  # Build query parameters
  query_params <- list(
    auth_token = auth_token_val,
    app_ids = paste(unified_app_id, collapse = ","),
    date_granularity = date_granularity,
    start_date = format(start_date, "%Y-%m-%d"),
    end_date = format(end_date, "%Y-%m-%d"),
    countries = paste(countries, collapse = ",")
  )

  # Build and perform request using the unified endpoint
  endpoint <- st_endpoint_relative_path("unified_sales_report_estimates")

  result <- fetch_data_core(
    endpoint = endpoint,
    params = query_params,
    auth_token = auth_token_val,
    verbose = verbose,
    enrich_response = FALSE,
    processor = process_unified_sales_response
  )

  if (is.null(result) || nrow(result) == 0) {
    if (verbose) message("No data returned from unified API.")
    return(tibble::tibble(
      date = as.Date(character()),
      country = character(),
      unified_app_id = character(),
      revenue = numeric(),
      downloads = numeric()
    ))
  }

  if (verbose) {
    message(sprintf("Retrieved %d records from unified API.", nrow(result)))
  }

  return(result)
}

#' Process unified sales report response
#' @noRd
process_unified_sales_response <- function(resp) {
  body_raw <- httr2::resp_body_raw(resp)
  if (length(body_raw) == 0) {
    return(NULL)
  }

  body_text <- rawToChar(body_raw)
  result <- jsonlite::fromJSON(body_text, flatten = TRUE)

  if (length(result) == 0 || nrow(result) == 0) {
    return(NULL)
  }

  # Convert to tibble
  result_tbl <- tibble::as_tibble(result)

  # Rename and process unified API columns
  # The unified API returns: app_id, date, country, unified_revenue (cents), unified_units
  result_tbl <- result_tbl %>%
    dplyr::rename(
      unified_app_id = "app_id",
      downloads = "unified_units"
    ) %>%
    dplyr::mutate(
      date = as.Date(date),
      # Convert revenue from cents to dollars
      revenue = unified_revenue / 100
    ) %>%
    dplyr::select(date, country, unified_app_id, revenue, downloads)

  return(result_tbl)
}
