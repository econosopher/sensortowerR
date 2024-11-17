#' Fetch Sensor Tower Metrics
#'
#' This function retrieves sales and usage metrics for a specified application from the Sensor Tower API. It fetches data within a specified date range for the provided unified app ID and combines the data into a unified tibble for analysis.
#'
#' @param auth_token Character. Your Sensor Tower API authentication token.
#' @param unified_app_id Character. The unified app ID for which to fetch metrics.
#' @param start_date Date. The start date for data collection (YYYY-MM-DD).
#' @param end_date Date. The end date for data collection (YYYY-MM-DD).
#'
#' @return A tibble containing combined sales and usage metrics for the app, date, and country. The tibble includes columns: date, unified_app_id, country, revenue, downloads, active_users.
#'
#' @examples
#' \dontrun{
#' # Load necessary libraries
#' library(lubridate)
#'
#' # Define your authentication token
#' auth_token <- "your_secure_auth_token_here"
#'
#' # Define the unified app ID
#' unified_app_id <- "602c795c912b51622f233ffe"  # Example: Pokémon GO
#'
#' # Define the date range
#' start_date <- as.Date("2021-09-22")
#' end_date <- as.Date("2021-09-22")
#'
#' # Fetch the metrics
#' metrics <- fetch_sensor_tower_metrics(
#'   auth_token = auth_token,
#'   unified_app_id = unified_app_id,
#'   start_date = start_date,
#'   end_date = end_date
#' )
#'
#' # View the metrics
#' print(metrics)
#' }
#'
#' @export
fetch_sensor_tower_metrics <- function(auth_token, unified_app_id, start_date, end_date) {
  # Load required libraries
  required_packages <- c("httr", "jsonlite", "dplyr", "lubridate", "purrr")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop(paste("Missing required packages:", paste(missing_packages, collapse = ", ")))
  }

  library(httr)
  library(jsonlite)
  library(dplyr)
  library(lubridate)
  library(purrr)

  # Define unified endpoints
  sales_endpoint <- "https://api.sensortower.com/v1/unified/sales_report_estimates"
  usage_endpoint <- "https://api.sensortower.com/v1/unified/usage/active_users"

  # Internal helper function to fetch and parse API data
  fetch_api_data <- function(endpoint, params, data_type = "data") {
    response <- GET(url = endpoint, query = params)

    # Check for HTTP errors
    if (http_error(response)) {
      status <- status_code(response)
      message_content <- content(response, "text", encoding = "UTF-8")
      stop(paste("HTTP error", status, "while fetching", data_type, ":", message_content))
    }

    # Parse JSON content
    parsed_data <- tryCatch({
      data <- content(response, "parsed", simplifyVector = TRUE)
      # Debugging: Print column names
      message(paste("Columns in", data_type, ":", paste(names(data), collapse = ", ")))
      data
    }, error = function(e) {
      stop(paste("Failed to parse JSON for", data_type, ":", e$message))
    })

    return(parsed_data)
  }

  # Function to rename 'app_id' to 'unified_app_id' if necessary
  ensure_unified_app_id <- function(data) {
    if ("app_id" %in% names(data) && !"unified_app_id" %in% names(data)) {
      data <- data %>% rename(unified_app_id = app_id)
      message("Renamed 'app_id' to 'unified_app_id'.")
    } else if (!"unified_app_id" %in% names(data)) {
      # If neither 'app_id' nor 'unified_app_id' exists, attempt to infer or set as NA
      if ("app_ids" %in% names(data)) {
        data <- data %>% rename(unified_app_id = app_ids)
        message("Renamed 'app_ids' to 'unified_app_id'.")
      } else {
        # If no identifier exists, add it manually
        data$unified_app_id <- unified_app_id  # Use the provided unified_app_id
        message("'unified_app_id' column was missing and has been set to the provided unified_app_id.")
      }
    }
    return(data)
  }

  # Adjusted get_sales_data() function
  get_sales_data <- function(endpoint, unified_app_id, start_date, end_date, auth_token) {
    message("Fetching sales data for Unified App ID ", unified_app_id, " from ", start_date, " to ", end_date)

    params <- list(
      app_ids = unified_app_id,
      date_granularity = "daily",
      start_date = as.character(start_date),
      end_date = as.character(end_date),
      auth_token = auth_token
    )

    sales_data <- tryCatch({
      data <- fetch_api_data(endpoint, params, data_type = paste(unified_app_id, "sales data"))
      data <- as_tibble(data)

      # Ensure 'unified_app_id' column exists
      data <- ensure_unified_app_id(data)

      if (nrow(data) == 0) {
        message("No sales data returned for Unified App ID ", unified_app_id, " from ", start_date, " to ", end_date)
      }
      data
    }, error = function(e) {
      message(e$message)
      tibble()  # Return empty tibble on error
    })

    return(sales_data)
  }

  # Adjusted get_usage_data() function
  get_usage_data <- function(endpoint, unified_app_id, start_date, end_date, auth_token) {
    message("Fetching usage data for Unified App ID ", unified_app_id, " from ", start_date, " to ", end_date)

    params <- list(
      app_ids = unified_app_id,
      time_period = "day",
      start_date = as.character(start_date),
      end_date = as.character(end_date),
      auth_token = auth_token
    )

    usage_data <- tryCatch({
      data <- fetch_api_data(endpoint, params, data_type = paste(unified_app_id, "usage data"))
      data <- as_tibble(data)

      # Ensure 'unified_app_id' column exists
      data <- ensure_unified_app_id(data)

      if (nrow(data) == 0) {
        message("No usage data returned for Unified App ID ", unified_app_id, " from ", start_date, " to ", end_date)
      }
      data
    }, error = function(e) {
      message(e$message)
      tibble()  # Return empty tibble on error
    })

    return(usage_data)
  }

  # Validate inputs
  if (!is.character(auth_token) || nchar(auth_token) == 0) {
    stop("Invalid auth_token. It must be a non-empty character string.")
  }

  if (!is.character(unified_app_id) || length(unified_app_id) != 1) {
    stop("Invalid unified_app_id. It must be a single non-empty character string.")
  }

  if (!inherits(start_date, "Date")) {
    stop("'start_date' must be of Date type.")
  }

  if (!inherits(end_date, "Date")) {
    stop("'end_date' must be of Date type.")
  }

  if (start_date > end_date) {
    stop("'start_date' must be earlier than or equal to 'end_date'.")
  }

  # Fetch sales data
  sales <- get_sales_data(
    endpoint = sales_endpoint,
    unified_app_id = unified_app_id,
    start_date = start_date,
    end_date = end_date,
    auth_token = auth_token
  )

  # Fetch usage data
  usage <- get_usage_data(
    endpoint = usage_endpoint,
    unified_app_id = unified_app_id,
    start_date = start_date,
    end_date = end_date,
    auth_token = auth_token
  )

  # Combine sales and usage data
  if (nrow(sales) > 0 && nrow(usage) > 0) {
    combined <- full_join(sales, usage, by = c("unified_app_id", "date", "country"))
  } else if (nrow(sales) > 0) {
    combined <- sales %>%
      mutate(active_users = NA_real_)
  } else if (nrow(usage) > 0) {
    combined <- usage %>%
      mutate(revenue = NA_real_, downloads = NA_real_)
  } else {
    # Define the expected columns for an empty tibble
    combined <- tibble(
      unified_app_id = character(),
      date = as.Date(character()),
      country = character(),
      revenue = numeric(),
      downloads = numeric(),
      active_users = numeric()
    )
    message("No data available for Unified App ID ", unified_app_id)
  }

  # Check if combined data is empty
  if (nrow(combined) == 0) {
    warning("No data was fetched for the specified unified app ID and date range.")
  }

  # Return the final combined data
  return(combined)
}