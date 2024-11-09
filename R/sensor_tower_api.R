#' Fetch Sensor Tower metrics
#'
#' @param auth_token Sensor Tower API authentication token
#' @param unified_app_id Unified App ID from Sensor Tower
#' @param start_date Start date for data collection (YYYY-MM-DD)
#' @param end_date End date for data collection (YYYY-MM-DD)
#' @param grain Granularity of data (daily, weekly, or monthly)
#' @return A data frame containing the metrics
#' @import httr
#' @import jsonlite
#' @export
fetch_sensor_tower_metrics <- function(auth_token, unified_app_id, start_date, end_date, grain = "daily") {
  # Validate inputs
  if (!grain %in% c("daily", "weekly", "monthly")) {
    stop("grain must be one of: daily, weekly, monthly")
  }
  
  # Construct API URL
  base_url <- "https://api.sensortower.com/v1"
  endpoint <- "/unified/metrics"
  url <- paste0(base_url, endpoint)
  
  # Build query parameters
  query <- list(
    auth_token = auth_token,
    unified_app_id = unified_app_id,
    start_date = start_date,
    end_date = end_date,
    granularity = grain
  )
  
  # Make API request
  response <- httr::GET(
    url,
    query = query,
    httr::add_headers(
      "Accept" = "application/json"
    )
  )
  
  # Check for HTTP errors
  httr::stop_for_status(response)
  
  # Parse response
  data <- httr::content(response, "text", encoding = "UTF-8")
  df <- jsonlite::fromJSON(data)
  
  return(df)
}
