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
