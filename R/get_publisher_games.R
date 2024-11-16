#' Get All Games from a Publisher
#'
#' Retrieves all games and their details for a specified publisher.
#'
#' @param auth_token Sensor Tower API authentication token.
#' @param publisher_id Unified Publisher ID from Sensor Tower.
#' @return A data frame containing all games and their details for the publisher.
#' @export
#' @importFrom httr GET add_headers stop_for_status content
#' @importFrom jsonlite fromJSON
get_publisher_games <- function(auth_token, publisher_id) {
  # Validate inputs
  if (!is.character(auth_token) || nchar(auth_token) == 0) {
    stop("Invalid auth_token")
  }
  if (!is.character(publisher_id) || nchar(publisher_id) == 0) {
    stop("Invalid publisher_id")
  }

  tryCatch({
    # Construct the API endpoint
    base_url <- "https://api.sensortower.com/v1"
    endpoint <- "/unified/publishers/apps"
    url <- paste0(base_url, endpoint)

    # Build query parameters
    query <- list(
      auth_token = auth_token,
      unified_id = publisher_id
    )

    # Make the API request
    response <- GET(
      url = url,
      query = query,
      add_headers("Accept" = "application/json")
    )
    stop_for_status(response)

    # Parse the response
    data <- content(response, "text", encoding = "UTF-8") %>%
      fromJSON(flatten = TRUE)

    # Check if 'apps' data is available
    if (!"apps" %in% names(data)) {
      stop("No apps data found for the given publisher_id")
    }

    # Return the apps data frame
    return(data$apps)

  }, error = function(e) {
    stop(paste("Error fetching publisher games:", e$message))
  })
}
