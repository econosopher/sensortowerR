#' Fetch Unified App Information from Sensor Tower
#'
#' This function retrieves information about apps from the Sensor Tower API
#' based on a search term. It fetches app IDs and names for unified app entities.
#'
#' @param term Character. The search term to query apps.
#' @param app_store Character. The app store to query, default is "unified".
#' @param entity_type Character. The type of entity to search, default is "app".
#' @param limit Integer. The maximum number of results to return. Default is 100, with a hard limit of 250.
#' @param auth_token Character. The API authentication token. Default is fetched from the environment variable `SENSORTOWER_AUTH`.
#'
#' @return A tibble with the following columns:
#' - `unified_app_id`: Unified app ID
#' - `unified_app_name`: Name of the unified app
#'
#' @examples
#' \dontrun{
#' # Ensure the SENSORTOWER_AUTH environment variable is set
#' Sys.setenv(SENSORTOWER_AUTH = "your_auth_token_here")
#'
#' # Fetch unified app info
#' app_info <- get_unified_app_info(term = "Clash of Clans")
#' print(app_info)
#' }
#' @export
get_unified_app_info <- function(term,
                                 app_store = "unified",
                                 entity_type = "app",
                                 limit = 100,
                                 auth_token = Sys.getenv("SENSORTOWER_AUTH")) {

  # Ensure the auth_token is provided
  if (is.null(auth_token) || auth_token == "") {
    stop("Authentication token is required. Set SENSORTOWER_AUTH environment variable.")
  }

  # Define the API endpoint
  base_url <- "https://api.sensortower.com/v1"
  endpoint <- file.path(base_url, app_store, "search_entities")

  # Make the GET request to the API
  response <- httr::GET(
    url = endpoint,
    query = list(
      entity_type = entity_type,
      term = term,
      limit = min(limit, 250) # Limit results to a maximum of 250
    ),
    httr::add_headers(
      Authorization = sprintf("Bearer %s", auth_token)
    )
  )

  # Handle potential HTTP errors
  if (httr::http_error(response)) {
    content <- httr::content(response, "parsed")
    stop(sprintf(
      "API request failed [%s]: %s",
      httr::status_code(response),
      if (!is.null(content$error)) content$error else "Unknown error"
    ))
  }

  # Parse the response content as text
  content_text <- httr::content(response, as = "text", encoding = "UTF-8")

  # Convert JSON to R list
  content_list <- jsonlite::fromJSON(content_text, flatten = TRUE)

  # Validate the structure of the API response
  if (!is.list(content_list)) {
    stop("Unexpected API response format: The response is not a list.")
  }

  # Bind rows if the content_list contains multiple entities
  if ("data" %in% names(content_list)) {
    apps <- content_list$data
  } else {
    apps <- bind_rows(content_list)
  }

  # Ensure the response contains expected fields
  if (!"entity_type" %in% colnames(apps)) {
    stop("Unexpected API response format: Missing 'entity_type' field.")
  }

  # Filter for unified_app entities
  unified_apps <- apps %>%
    filter(entity_type == "unified_app")

  # Check if any unified apps are available
  if (nrow(unified_apps) == 0) {
    warning("No unified app entities found for the given search term.")
    return(tibble())
  }

  # Extract and rename desired fields
  extracted_info <- unified_apps %>%
    select(app_id, name) %>%
    rename(
      unified_app_id = app_id,
      unified_app_name = name
    )

  return(extracted_info)
}