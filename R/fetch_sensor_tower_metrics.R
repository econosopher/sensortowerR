#' Fetch Unified Sensor Tower Metrics
#'
#' @param auth_token API authentication token
#' @param app_id Unique identifier for the app
#' @param start_date Start date for data collection (YYYY-MM-DD)
#' @param end_date End date for data collection (YYYY-MM-DD)
#' @param app_name Name of the app
#' @param grain Time granularity for the data ("daily", "weekly", or "monthly")
#'
#' @return A tibble containing unified metrics
#' @export
fetch_sensor_tower_metrics <- function(auth_token, app_id, start_date, end_date, app_name, 
                                     grain = c("daily", "weekly", "monthly")) {
  grain <- match.arg(grain)
  if (!is.character(auth_token) || nchar(auth_token) == 0) {
    stop("Invalid auth_token")
  }
  if (!is.character(app_id) || nchar(app_id) == 0) {
    stop("Invalid app_id")
  }
  
  tryCatch({
    # Fetch revenue data
    revenue_endpoint <- "https://api.sensortower.com/v1/ios/sales_report_estimates"
    revenue_url <- paste0(
      revenue_endpoint,
      "?app_ids=", app_id,
      "&date_granularity=daily",
      "&start_date=", start_date,
      "&end_date=", end_date,
      "&auth_token=", auth_token
    )
    
    revenue_response <- httr::GET(revenue_url)
    httr::stop_for_status(revenue_response)
    revenue_data <- revenue_response %>%
      httr::content("text") %>%
      jsonlite::fromJSON(flatten = TRUE)
    
    # Fetch user metrics
    user_endpoint <- "https://api.sensortower.com/v1/ios/app_intelligence"
    user_url <- paste0(
      user_endpoint,
      "?app_ids=", app_id,
      "&date_granularity=", grain,
      "&start_date=", start_date,
      "&end_date=", end_date,
      "&metrics=dau,wau,mau",
      "&auth_token=", auth_token
    )
    
    user_response <- httr::GET(user_url)
    httr::stop_for_status(user_response)
    user_data <- user_response %>%
      httr::content("text") %>%
      jsonlite::fromJSON(flatten = TRUE)
    
    # Process revenue data based on grain
    revenue_df <- revenue_data %>%
      dplyr::mutate(date = as.Date(date)) %>%
      dplyr::group_by(
        date_group = case_when(
          grain == "daily" ~ date,
          grain == "weekly" ~ lubridate::floor_date(date, "week"),
          grain == "monthly" ~ lubridate::floor_date(date, "month")
        )
      ) %>%
      dplyr::summarise(
        revenue = sum(revenue_estimates, na.rm = TRUE),
        downloads = sum(download_estimates, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Process user data
    user_df <- user_data %>%
      dplyr::mutate(date = as.Date(date)) %>%
      dplyr::select(
        date,
        active_users = case_when(
          grain == "daily" ~ dau,
          grain == "weekly" ~ wau,
          grain == "monthly" ~ mau
        )
      )
    
    # Combine and clean data
    unified_data <- revenue_df %>%
      dplyr::left_join(
        user_df,
        by = c("date_group" = "date")
      ) %>%
      dplyr::mutate(
        app_name = app_name,
        app_id = app_id
      ) %>%
      dplyr::select(
        date = date_group,
        app_name,
        app_id,
        revenue,
        downloads,
        active_users
      )
    
    return(unified_data)
    
  }, error = function(e) {
    stop(paste("Error fetching data:", e$message))
  })
}
