#' Fetch Retention Data for Apps
#'
#' Retrieves retention metrics (D1-D90) for specific apps from the Sensor Tower
#' Usage Intelligence API. This function queries the retention endpoint directly
#' using platform-specific app IDs.
#'
#' @param unified_app_id Character string. Sensor Tower unified app ID (24-character hex).
#'   Will be resolved to platform-specific IDs automatically.
#' @param ios_app_id Character string. iOS app ID (numeric, e.g., "1234567890").
#' @param android_app_id Character string. Android package name (e.g., "com.example.app").
#' @param os Character string. Operating system: "ios" or "android". Required if using
#'   platform-specific IDs. When using unified_app_id, defaults to "ios" but will
#'   try both platforms.
#' @param country Character string. Country code (e.g., "US", "GB"). Default is "US".
#'   Only single country supported per request.
#' @param date_granularity Character string. Either "all_time" (default) or "quarterly".
#'   All-time data goes back to Q4 2015. Quarterly data begins Q1 2021.
#' @param start_date Date or character string. Start date for quarterly data in "YYYY-MM-DD" format.
#'   Ignored for all_time granularity.
#' @param end_date Date or character string. End date for quarterly data in "YYYY-MM-DD" format.
#'   Ignored for all_time granularity.
#' @param auth_token Optional. Character string. Your Sensor Tower API token.
#'   Defaults to environment variable SENSORTOWER_AUTH_TOKEN.
#' @param verbose Logical. If TRUE, prints progress messages.
#'
#' @return A [tibble][tibble::tibble] with retention metrics including:
#'   - **app_id**: The platform-specific app ID
#'   - **os**: Platform (ios or android)
#'   - **country**: Country code
#'   - **retention_d1** through **retention_d90**: Retention percentages (0-1 scale)
#'   - **confidence**: Data confidence level (red=low, yellow=medium, green=high)
#'   - **baseline_downloads**: Total downloads in baseline period
#'   - **baseline_start_date**, **baseline_end_date**: Dates for baseline period
#'
#' @section Data Availability:
#' - Quarterly data begins Q1 2021

#' - All-time data goes back to Q4 2015
#' - Data is only available for apps with sufficient user base
#' - Confidence levels: red (<=3), yellow (4-6), green (>=7)
#'
#' @section Recommended Workflow:
#' ```
#' # Step 1: Search for app by name
#' app <- st_app_info("Royal Match")
#'
#' # Step 2: Get retention data using unified ID
#' retention <- st_retention(unified_app_id = app$unified_app_id[1])
#' ```
#'
#' @examples
#' \dontrun{
#' # Get retention for an app using unified ID
#' retention <- st_retention(unified_app_id = "5f16a8019f7b275235017614")
#'
#' # Get retention for iOS app directly
#' retention <- st_retention(
#'   ios_app_id = "553834731",
#'   os = "ios",
#'   country = "US"
#' )
#'
#' # Get quarterly retention data
#' retention <- st_retention(
#'   unified_app_id = "5f16a8019f7b275235017614",
#'   date_granularity = "quarterly",
#'   start_date = "2024-01-01",
#'   end_date = "2024-09-30"
#' )
#' }
#'
#' @seealso [st_app_info()] for searching apps by name,
#'   [st_app_lookup()] for resolving app IDs,
#'   [st_demographics()] for user demographics data
#'
#' @export
#' @importFrom rlang %||% abort
#' @importFrom httr2 request req_url_path_append req_url_query req_headers
#' @importFrom httr2 req_perform resp_body_raw resp_status
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr bind_rows mutate select
st_retention <- function(unified_app_id = NULL,
                         ios_app_id = NULL,
                         android_app_id = NULL,
                         os = NULL,
                         country = "US",
                         date_granularity = "all_time",
                         start_date = NULL,
                         end_date = NULL,
                         auth_token = NULL,
                         verbose = TRUE) {

  # Validate that at least one app ID is provided

if (is.null(unified_app_id) && is.null(ios_app_id) && is.null(android_app_id)) {
    rlang::abort(c(
      "At least one app ID must be provided.",
      "i" = "Use unified_app_id, ios_app_id, or android_app_id parameter.",
      "i" = "Use st_app_info('app name') to find app IDs."
    ))
  }

  # Validate date_granularity
  date_granularity <- match.arg(date_granularity, c("all_time", "quarterly"))

  # Validate quarterly parameters
  if (date_granularity == "quarterly") {
    if (is.null(start_date) || is.null(end_date)) {
      rlang::abort(c(
        "start_date and end_date are required for quarterly granularity.",
        "i" = "Use date_granularity = 'all_time' for aggregate data without date ranges."
      ))
    }
    start_date <- as.Date(start_date)
    end_date <- as.Date(end_date)
  }

  # Authentication
  auth_token_val <- resolve_auth_token(
    auth_token,
    error_message = "Authentication token not found. Set SENSORTOWER_AUTH_TOKEN environment variable."
  )

  # Resolve IDs - the retention endpoint requires platform-specific IDs
  platforms_to_query <- list()

  if (!is.null(unified_app_id)) {
    # Resolve unified ID to platform-specific IDs
    if (verbose) message("Resolving unified app ID to platform IDs...")

    app_lookup <- tryCatch({
      st_app_lookup(unified_app_id, auth_token = auth_token_val, verbose = verbose)
    }, error = function(e) {
      if (verbose) message("ID lookup failed: ", e$message)
      NULL
    })

    if (!is.null(app_lookup)) {
      if (!is.null(app_lookup$ios_app_id)) {
        platforms_to_query$ios <- app_lookup$ios_app_id
      }
      if (!is.null(app_lookup$android_app_id)) {
        platforms_to_query$android <- app_lookup$android_app_id
      }
    } else {
      rlang::abort(c(
        "Could not resolve unified app ID to platform IDs.",
        "i" = paste("ID:", unified_app_id),
        "i" = "Try using platform-specific IDs (ios_app_id or android_app_id) directly."
      ))
    }
  }

  # Add explicit platform IDs if provided
  if (!is.null(ios_app_id)) {
    platforms_to_query$ios <- ios_app_id
  }
  if (!is.null(android_app_id)) {
    platforms_to_query$android <- android_app_id
  }

  # Filter to specific OS if requested
  if (!is.null(os)) {
    os <- match.arg(os, c("ios", "android"))
    platforms_to_query <- platforms_to_query[names(platforms_to_query) == os]
  }

  if (length(platforms_to_query) == 0) {
    rlang::abort("No valid platform IDs found to query.")
  }

  # Fetch retention data for each platform
  all_results <- list()

  for (platform in names(platforms_to_query)) {
    app_id <- platforms_to_query[[platform]]

    if (verbose) {
      message(sprintf("Fetching %s retention data for app: %s", platform, app_id))
    }

    result <- fetch_retention_for_platform(
      app_id = app_id,
      os = platform,
      country = country,
      date_granularity = date_granularity,
      start_date = start_date,
      end_date = end_date,
      auth_token = auth_token_val,
      verbose = verbose
    )

    if (!is.null(result) && nrow(result) > 0) {
      all_results[[platform]] <- result
    }
  }

  # Combine results
  if (length(all_results) == 0) {
    if (verbose) message("No retention data found for the specified app(s).")
    return(tibble::tibble())
  }

  final_result <- dplyr::bind_rows(all_results)

  # Add unified_app_id if we have it
  if (!is.null(unified_app_id)) {
    final_result$unified_app_id <- unified_app_id
  }

  if (verbose) {
    message(sprintf("Retrieved retention data for %d platform(s)", length(all_results)))
  }

  return(final_result)
}

#' Fetch retention data for a single platform
#' @noRd
fetch_retention_for_platform <- function(app_id,
                                          os,
                                          country,
                                          date_granularity,
                                          start_date,
                                          end_date,
                                          auth_token,
                                          verbose = FALSE) {

  # Build query parameters - auth_token as query param per API spec
  query_params <- list(
    auth_token = auth_token,
    app_ids = app_id,
    date_granularity = date_granularity,
    country = country
  )

  # Add date parameters - API requires dates even for all_time
  if (date_granularity == "quarterly" && !is.null(start_date) && !is.null(end_date)) {
    query_params$start_date <- format(start_date, "%Y-%m-%d")
    query_params$end_date <- format(end_date, "%Y-%m-%d")
  } else {
    # For all_time or when dates not specified, use a wide date range
    # All-time data goes back to Q4 2015
    query_params$start_date <- "2015-10-01"
    query_params$end_date <- format(Sys.Date() - 1, "%Y-%m-%d")
  }

  # Build request
  req <- build_request(
    base_url = st_api_base_url(),
    path_segments = st_endpoint_segments("usage_retention", os = os),
    query_params = query_params
  ) %>%
    httr2::req_headers(
      "Accept" = "application/json"
    ) %>%
    httr2::req_timeout(30)

  # Perform request
  resp <- tryCatch({
    httr2::req_perform(req)
  }, error = function(e) {
    if (verbose) message("API request failed: ", e$message)
    return(NULL)
  })

  if (is.null(resp)) {
    return(NULL)
  }

  # Check status
  status <- httr2::resp_status(resp)
  if (status != 200) {
    body <- tryCatch(httr2::resp_body_string(resp), error = function(.) "")
    if (verbose) message(sprintf("API returned status %d: %s", status, body))
    return(NULL)
  }

  # Parse response
  body_raw <- httr2::resp_body_raw(resp)
  if (length(body_raw) == 0) {
    return(NULL)
  }

  body_text <- rawToChar(body_raw)
  result <- tryCatch({
    jsonlite::fromJSON(body_text, flatten = TRUE)
  }, error = function(e) {
    if (verbose) message("Failed to parse response: ", e$message)
    return(NULL)
  })

  if (is.null(result) || length(result) == 0) {
    return(NULL)
  }

  # The API returns: { app_data: [...], baseline_data: [...], disabled_app_ids: [...] }
  app_data_list <- result$app_data
  baseline_data <- result$baseline_data

  if (is.null(app_data_list) || length(app_data_list) == 0) {
    return(NULL)
  }

  # Process each app's retention data
  # app_data_list is a data frame when flattened, or a list of lists
  if (is.data.frame(app_data_list)) {
    # Handle data.frame (typical from fromJSON with flatten=TRUE)
    processed <- lapply(seq_len(nrow(app_data_list)), function(i) {
      app_row <- app_data_list[i, ]

      # Extract retention array - it's nested as a list column
      retention <- app_row$corrected_retention
      if (is.list(retention)) {
        retention <- unlist(retention[[1]])
      }
      if (is.null(retention) || length(retention) == 0) {
        retention <- rep(NA_real_, 90)
      }

      # Ensure 90 values
      if (length(retention) < 90) {
        retention <- c(retention, rep(NA_real_, 90 - length(retention)))
      }
      retention <- as.numeric(retention[1:90])

      # Build the row
      row <- tibble::tibble(
        app_id = as.character(app_row$app_id),
        os = os,
        country = country
      )

      # Add retention columns D1-D90
      for (j in 1:90) {
        col_name <- paste0("retention_d", j)
        row[[col_name]] <- retention[j]
      }

      # Add confidence
      row$confidence <- as.integer(app_row$confidence %||% NA_integer_)

      return(row)
    })
  } else {
    # Handle list of lists structure
    processed <- lapply(app_data_list, function(app_item) {
      if (is.null(app_item) || !is.list(app_item)) return(NULL)

      retention <- app_item$corrected_retention
      if (is.null(retention) || length(retention) == 0) {
        retention <- rep(NA_real_, 90)
      }
      retention <- as.numeric(unlist(retention))

      if (length(retention) < 90) {
        retention <- c(retention, rep(NA_real_, 90 - length(retention)))
      }
      retention <- retention[1:90]

      row <- tibble::tibble(
        app_id = as.character(app_item$app_id %||% app_id),
        os = os,
        country = country
      )

      for (j in 1:90) {
        col_name <- paste0("retention_d", j)
        row[[col_name]] <- retention[j]
      }

      row$confidence <- as.integer(app_item$confidence %||% NA_integer_)
      return(row)
    })
  }

  # Combine and return
  result_tbl <- dplyr::bind_rows(processed)

  # Add baseline data if available (at the response level, not per-app)
  if (!is.null(baseline_data) && length(baseline_data) > 0) {
    result_tbl$has_baseline <- TRUE
  }

  return(result_tbl)
}
