
fetch_sensor_tower_metrics <- function(app_id, metrics = c("downloads", "revenue", "dau"), 
                                     date_range = "last_30_days", 
                                     auth_token = Sys.getenv("SENSORTOWER_AUTH")) {
  
  if (is.null(auth_token) || auth_token == "") {
    stop("Authentication token is required. Set SENSORTOWER_AUTH environment variable or provide auth_token parameter.")
  }

  base_url <- "https://api.sensortower.com/v1"
  endpoint <- sprintf("/unified_apps/%s/metrics", app_id)
  
  # Convert metrics array to comma-separated string
  metrics_str <- paste(metrics, collapse = ",")
  
  # Build query parameters
  query_params <- list(
    auth_token = auth_token,
    metrics = metrics_str,
    date_range = date_range
  )
  
  # Construct the URL with query parameters
  url <- httr::modify_url(
    url = paste0(base_url, endpoint),
    query = query_params
  )
  
  # Make the request
  response <- httr::GET(url)
  
  # Check for errors
  if (httr::http_error(response)) {
    stop(
      sprintf(
        "Sensor Tower API request failed [%s]\n%s",
        httr::status_code(response),
        httr::content(response, "text")
      )
    )
  }
  
  # Parse and return the response
  result <- httr::content(response, "parsed")
  return(result)
}
