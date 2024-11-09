
#' Fetch Sensor Tower Metrics using Unified App ID
#'
#' @param auth_token Sensor Tower API authentication token
#' @param unified_app_id Unified App ID from Sensor Tower
#' @param start_date Start date for data collection (YYYY-MM-DD)
#' @param end_date End date for data collection (YYYY-MM-DD)
#' @param grain Granularity of data (daily, weekly, or monthly)
#' @return A data frame containing the metrics
#' @export
fetch_sensor_tower_metrics <- function(auth_token, unified_app_id = NULL, 
                                     start_date, end_date, grain = "daily") {
  
  # Input validation
  if (!grain %in% c("daily", "weekly", "monthly")) {
    stop("grain must be one of: daily, weekly, monthly")
  }
  
  # Format dates
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  # Construct revenue endpoint URL
  revenue_endpoint <- "https://api.sensortower.com/v1/ios/sales_report_estimates"
  revenue_url <- paste0(
    revenue_endpoint,
    "?unified_app_id=", unified_app_id,
    "&date_granularity=", grain,
    "&start_date=", start_date,
    "&end_date=", end_date,
    "&auth_token=", auth_token
  )
  
  # Make API call for revenue data
  revenue_response <- httr::GET(revenue_url)
  httr::stop_for_status(revenue_response)
  revenue_data <- jsonlite::fromJSON(
    httr::content(revenue_response, "text", encoding = "UTF-8"),
    flatten = TRUE
  )
  
  # Process revenue data
  if (length(revenue_data) > 0) {
    revenue_df <- data.frame(
      date = as.Date(revenue_data$d),
      revenue = revenue_data$ir,
      downloads = revenue_data$iu,
      country = revenue_data$cc,
      unified_app_id = unified_app_id
    )
    
    # Group by date
    daily_summary <- aggregate(
      cbind(revenue, downloads) ~ date + unified_app_id, 
      data = revenue_df, 
      FUN = sum, 
      na.rm = TRUE
    )
    
    return(daily_summary[order(daily_summary$date), ])
  } else {
    # Return empty dataframe with correct structure
    return(data.frame(
      date = as.Date(character()),
      revenue = numeric(),
      downloads = numeric(),
      unified_app_id = character(),
      stringsAsFactors = FALSE
    ))
  }
}

#' Get Unified App ID from Publisher
#'
#' @param auth_token Sensor Tower API authentication token
#' @param publisher_id Publisher ID from Sensor Tower
#' @param app_name Name of the app to search for
#' @return The unified app ID for the specified app
#' @export
get_unified_app_id <- function(auth_token, publisher_id, app_name) {
  publisher_endpoint <- paste0(
    "https://api.sensortower.com/v1/unified/publishers/apps?",
    "unified_id=", publisher_id,
    "&auth_token=", auth_token
  )
  
  # Fetch publisher data
  publisher_response <- httr::GET(publisher_endpoint)
  httr::stop_for_status(publisher_response)
  
  app_data <- jsonlite::fromJSON(
    httr::content(publisher_response, "text", encoding = "UTF-8")
  )
  
  # Find the app in the list
  app_index <- which(app_data$apps$unified_app_name == app_name)
  
  if (length(app_index) == 0) {
    stop(paste("App", app_name, "not found for this publisher"))
  }
  
  return(app_data$apps$unified_app_id[app_index])
}
