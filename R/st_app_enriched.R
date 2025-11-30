#' Fetch Enriched Metrics for Specific Apps
#'
#' Retrieves comprehensive metrics including retention, MAU, DAU, demographics,
#' and other aggregate tags for specific apps by their unified app IDs.
#'
#' This function is designed for the common workflow of:
#' 1. Search for apps by name using `st_app_info()`
#' 2. Get their unified IDs
#' 3. Fetch enriched metrics for those specific apps using this function
#'
#' @param unified_app_ids Character vector. One or more unified app IDs
#'   (24-character hex strings). Required. Use `st_app_info()` to find these.
#' @param os Character string. Operating system context for the request.
#'   Must be "unified" (default), "ios", or "android".
#' @param regions Character vector. Region codes for data filtering.
#'   Defaults to "WW" (worldwide).
#' @param auth_token Optional. Character string. Your Sensor Tower API token.
#'   Defaults to environment variable SENSORTOWER_AUTH_TOKEN.
#'
#' @return A [tibble][tibble::tibble] with enriched metrics including:
#'   - **Identification**: `unified_app_id`, `unified_app_name`
#'   - **Active Users**: `dau_30d_us`, `dau_30d_ww`, `wau_4w_us`, `wau_4w_ww`,
#'     `mau_month_us`, `mau_month_ww`
#'   - **Retention**: `retention_1d_us/ww`, `retention_7d_us/ww`,
#'     `retention_14d_us/ww`, `retention_30d_us/ww`, `retention_60d_us/ww`
#'   - **Demographics**: `genders_us`, `genders_ww`, `age_us`, `age_ww`,
#'     `male_share_us`, `female_share_us`
#'   - **Revenue/Downloads**: `revenue_30d_ww`, `revenue_90d_ww`, `revenue_alltime_us/ww`,
#'     `downloads_30d_ww`, `downloads_alltime_us/ww`
#'   - **Monetization**: `rpd_alltime_us/ww`, `arpu_90d_us/ww`
#'   - **Launch**: `release_date_us/ww`, `earliest_release_date`
#'
#' @section Recommended Workflow:
#' ```
#' # Step 1: Search for apps by name
#' apps <- st_app_info("Royal Match")
#'
#' # Step 2: Get unified IDs
#' app_ids <- apps$unified_app_id
#'
#' # Step 3: Fetch enriched metrics
#' metrics <- st_app_enriched(app_ids)
#' ```
#'
#' @section Data Availability Notes:
#' - Retention data is aggregated for the "last quarter" - not time-series
#' - Demographics (age/gender) are primarily available for US market
#' - Not all metrics are available for all apps - smaller apps may have NA values
#' - D90 retention is NOT available through the SensorTower API
#'
#' @examples
#' \dontrun
#' # Get enriched data for specific apps
#' royal_match <- st_app_info("Royal Match")
#' enriched <- st_app_enriched(royal_match$unified_app_id)
#'
#' # Get data for multiple apps at once
#' game_ids <- c("5f16a8019f7b275235017614", "660af7c66237390ce7c829fc")
#' multi_enriched <- st_app_enriched(game_ids)
#'
#' # View retention metrics
#' multi_enriched %>%
#'   select(unified_app_name, starts_with("retention"))
#' }
#'
#' @seealso [st_app_info()] for searching apps by name,
#'   [st_app_lookup()] for resolving app IDs,
#'   [st_sales_report()] for time-series revenue/download data,
#'   [st_batch_metrics()] for time-series DAU/WAU/MAU data
#'
#' @export
#' @importFrom rlang %||% abort
#' @importFrom httr2 request req_url_path_append req_url_query req_headers
#' @importFrom httr2 req_perform resp_body_raw
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr filter bind_rows mutate select any_of
st_app_enriched <- function(unified_app_ids,
                            os = "unified",
                            regions = "WW",
                            auth_token = NULL) {


  if (missing(unified_app_ids) || is.null(unified_app_ids) ||
      length(unified_app_ids) == 0) {
    rlang::abort(c(
      "'unified_app_ids' parameter is required.",
      "i" = "Use st_app_info('app name') to search for apps and get their IDs.",
      "x" = "Example: st_app_enriched(c('5f16a8019f7b275235017614'))"
    ))
  }


  unified_app_ids <- as.character(unified_app_ids)
  invalid_ids <- unified_app_ids[!grepl("^[a-f0-9]{24}$", unified_app_ids)]
  if (length(invalid_ids) > 0) {
    rlang::abort(c(
      "Invalid unified app ID format detected.",
      "i" = "Unified IDs must be 24-character hexadecimal strings.",
      "x" = paste("Invalid IDs:", paste(invalid_ids[1:min(3, length(invalid_ids))],
                                         collapse = ", ")),
      "i" = "Use st_app_info('app name') to find valid unified IDs."
    ))
  }

  # Validate OS
  os <- match.arg(os, c("unified", "ios", "android"))

  # Authentication
  auth_token_val <- auth_token %||% Sys.getenv("SENSORTOWER_AUTH_TOKEN")
  if (auth_token_val == "") {
    rlang::abort(
      "Authentication token not found. Set SENSORTOWER_AUTH_TOKEN environment variable."
    )
  }

  # Build filter for specific app IDs
  # The API requires a custom_fields_filter to get aggregate tags
  # We'll create one targeting these specific unified app IDs

  # For apps without a filter, we fetch from top_charts with a large limit
  # and filter client-side. This is the only reliable way to get aggregate_tags.

  # Strategy: Use st_top_charts with category=0 (all) and filter results
  # This is not ideal but is the only way given API limitations

  message(sprintf("Fetching enriched data for %d app(s)...", length(unified_app_ids)))

  # Use the sales_report_estimates_comparison_attributes endpoint
  # which returns aggregate_tags when queried properly

  # Build query parameters (auth via header, not query param)
  query_params <- list(
    measure = "revenue",  # Required parameter
    comparison_attribute = "absolute",
    time_range = "month",
    date = format(Sys.Date() - 30, "%Y-%m-%d"),
    end_date = format(Sys.Date() - 1, "%Y-%m-%d"),
    category = "0",  # All categories
    regions = paste(regions, collapse = ","),
    limit = 2000,  # Fetch enough to hopefully include our apps
    device_type = if (os %in% c("ios", "unified")) "total" else NULL
  )

  # Remove NULL parameters
 query_params <- query_params[!sapply(query_params, is.null)]

  # Build request
  base_url <- "https://api.sensortower.com"

  req <- httr2::request(base_url) %>%
    httr2::req_url_path_append("v1", os, "sales_report_estimates_comparison_attributes") %>%
    httr2::req_url_query(!!!query_params) %>%
    httr2::req_headers(
      "Authorization" = paste("Bearer", auth_token_val),
      "Accept" = "application/json",
      "User-Agent" = "sensortowerR (https://github.com/ge-data-solutions/sensortowerR)"
    ) %>%
    httr2::req_timeout(60)

  # Perform request
  resp <- tryCatch({
    httr2::req_perform(req)
  }, error = function(e) {
    if (inherits(e, "httr2_http")) {
      # httr2 stores response in e$resp, not e$response
      resp_obj <- e$resp
      if (!is.null(resp_obj)) {
        status <- httr2::resp_status(resp_obj)
        body <- tryCatch(httr2::resp_body_string(resp_obj), error = function(.) "")

        if (status == 401) {
          rlang::abort("Invalid authentication token.")
        } else if (status == 403) {
          rlang::abort("API token not authorized for this endpoint.")
        } else {
          rlang::abort(sprintf("API request failed [%d]: %s", status, body))
        }
      }
    }
    rlang::abort(sprintf("Request failed: %s", conditionMessage(e)))
  })

  # Process response with enrichment (reuse existing utilities)
  result_tbl <- process_response(resp, enrich_response = TRUE)

  if (nrow(result_tbl) == 0) {
    message("No results in API response")
    return(tibble::tibble())
  }

  # Filter to our requested app IDs
  if ("unified_app_id" %in% names(result_tbl)) {
    filtered <- result_tbl %>%
      dplyr::filter(unified_app_id %in% unified_app_ids)
  } else if ("app_id" %in% names(result_tbl)) {
    filtered <- result_tbl %>%
      dplyr::filter(app_id %in% unified_app_ids)
  } else {
    message("Warning: Could not find app ID column in response")
    filtered <- tibble::tibble()
  }

  # Report on what we found
  found_ids <- if ("unified_app_id" %in% names(filtered)) {
    unique(filtered$unified_app_id)
  } else if ("app_id" %in% names(filtered)) {
    unique(filtered$app_id)
  } else {
    character(0)
  }

  missing_ids <- setdiff(unified_app_ids, found_ids)

  if (length(found_ids) > 0) {
    message(sprintf("Found enriched data for %d of %d requested apps",
                   length(found_ids), length(unified_app_ids)))
  }

  if (length(missing_ids) > 0) {
    message(sprintf(
      "Note: %d app(s) not found in top charts. These may be smaller apps without aggregate metrics: %s",
      length(missing_ids),
      paste(missing_ids[1:min(3, length(missing_ids))], collapse = ", ")
    ))
  }

  # Select and order key columns
  key_cols <- c(
    "unified_app_id", "unified_app_name",
    # Active users
    "dau_30d_us", "dau_30d_ww",
    "wau_4w_us", "wau_4w_ww",
    "mau_month_us", "mau_month_ww",
    # Retention
    "retention_1d_us", "retention_1d_ww",
    "retention_7d_us", "retention_7d_ww",
    "retention_14d_us", "retention_14d_ww",
    "retention_30d_us", "retention_30d_ww",
    "retention_60d_us", "retention_60d_ww",
    # Demographics
    "genders_us", "genders_ww",
    "age_us", "age_ww",
    "male_share_us", "female_share_us",
    # Revenue
    "revenue_30d_ww", "revenue_90d_ww", "revenue_180d_ww",
    "revenue_alltime_us", "revenue_alltime_ww",
    # Downloads
    "downloads_30d_ww", "downloads_90d_ww", "downloads_180d_ww",
    "downloads_alltime_us", "downloads_alltime_ww",
    # Monetization
    "rpd_alltime_us", "rpd_alltime_ww",
    "arpu_90d_us", "arpu_90d_ww",
    # Launch
    "release_date_us", "release_date_ww", "earliest_release_date"
  )

  # Get columns that exist in our data
  available_key_cols <- intersect(key_cols, names(filtered))
  other_cols <- setdiff(names(filtered), key_cols)

  # Reorder with key columns first
  if (nrow(filtered) > 0 && length(available_key_cols) > 0) {
    filtered <- filtered %>%
      dplyr::select(dplyr::any_of(available_key_cols), dplyr::any_of(other_cols))
  }

  return(filtered)
}
