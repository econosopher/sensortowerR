#' Fetch Unified App Information from Sensor Tower
#'
#' This function retrieves information about apps from the Sensor Tower API
#' based on a search term. It targets the `/v1/{app_store}/search_entities`
#' endpoint and fetches app IDs and names for unified app entities.
#'
#' @param term Character string. The search term for the app or publisher.
#' @param app_store Character string. The app store to search. Defaults to "unified".
#' @param entity_type Character string. The type of entity to search for. Defaults to "app".
#' @param limit Numeric. The maximum number of results to return. Defaults to 100.
#' @param auth_token Character string. Your Sensor Tower API authentication token.
#'
#' @return A [tibble][tibble::tibble] with app information.
#'
#' @examples
#' \dontrun{
#' # Ensure the SENSORTOWER_AUTH_TOKEN environment variable is set
#' # Sys.setenv(SENSORTOWER_AUTH_TOKEN = "your_auth_token_here")
#'
#' # Fetch unified app info for "Clash of Clans"
#' app_info <- st_app_info(term = "Clash of Clans")
#' print(app_info)
#'
#' # Fetch publisher info
#' # publisher_info <- st_app_info(term = "Supercell", entity_type = "publisher")
#' # print(publisher_info) # Note: returned columns might differ based on entity_type
#' }
#'
#' @import dplyr
#' @importFrom httr GET add_headers http_error status_code content
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#' @export
st_app_info <- function(term,
                                 app_store = "unified",
                                 entity_type = "app",
                                 limit = 100,
                                 auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN")) {

  # Ensure the auth_token is provided
  if (is.null(auth_token) || auth_token == "") {
    stop("Authentication token is required. Set SENSORTOWER_AUTH_TOKEN environment variable.")
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
    filter(.data$entity_type == "unified_app")

  # Check if any unified apps are available
  if (nrow(unified_apps) == 0) {
    warning("No unified app entities found for the given search term.")
    return(tibble())
  }

  # Extract and rename desired fields
  extracted_info <- unified_apps %>%
    select(.data$app_id, .data$name) %>%
    rename(
      unified_app_id = .data$app_id,
      unified_app_name = .data$name
    )

  return(extracted_info)
}