#' Fetch Unified App Information from Sensor Tower
#'
#' This function retrieves information about apps from the Sensor Tower API
#' based on a search term. It targets the `/v1/\{app_store\}/search_entities`
#' endpoint and fetches app IDs and names for unified app entities.
#'
#' @param term Character string. The search term for the app or publisher.
#' @param app_store Character string. The app store to search.
#'   Defaults to "unified".
#' @param entity_type Character string. The type of entity to search for.
#'   Defaults to "app".
#' @param limit Numeric. The maximum number of results to return.
#'   Defaults to 20.
#' @param auth_token Character string. Your Sensor Tower API authentication token.
#' @param return_all_fields Boolean. If TRUE, returns all available fields
#'   from the API response. Defaults to FALSE, which returns only
#'   `unified_app_id` and `unified_app_name`.
#'
#' @return A [tibble][tibble::tibble] with app information. If
#'   `return_all_fields` is FALSE (default), it contains `unified_app_id`,
#'   `unified_app_name`, and `category_details` (when available). If TRUE, it
#'   contains the full, unfiltered data. When category information is present
#'   in the API response, it's automatically enriched into a `category_details`
#'   list-column containing nested tibbles with platform, category_id, and 
#'   category_name for each app's categories.
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
#' # Access nested category details
#' if ("category_details" %in% names(app_info)) {
#'   print("Categories for first app:")
#'   print(app_info$category_details[[1]])
#' }
#'
#' # Fetch publisher info
#' # publisher_info <- st_app_info(
#' #   term = "Supercell", entity_type = "publisher"
#' # )
#' # print(publisher_info) # Note: returned columns might differ
#' }
#'
#' @import dplyr
#' @importFrom dplyr all_of
#' @importFrom httr GET add_headers http_error status_code content
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#' @export
st_app_info <- function(term,
                        app_store = "unified",
                        entity_type = "app",
                        limit = 20,
                        auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN"),
                        return_all_fields = FALSE) {
  # Ensure the auth_token is provided
  if (is.null(auth_token) || auth_token == "") {
    stop(
      "Authentication token is required. ",
      "Set SENSORTOWER_AUTH_TOKEN environment variable."
    )
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

  # Always create nested category_details when categories are present
  if ("categories" %in% colnames(unified_apps) && is.list(unified_apps$categories)) {
    category_lookup <- st_categories()  # Use the full category lookup instead of internal data
    unified_apps$category_details <- lapply(unified_apps$categories, function(ids) {
      if (length(ids) > 0 && !all(is.na(ids))) {
        category_lookup[category_lookup$category_id %in% as.character(ids), ]
      } else {
        tibble::tibble(platform = character(0), category_id = character(0), category_name = character(0))
      }
    })
    
      # Remove the raw categories column in favor of the nested approach
  unified_apps <- unified_apps %>% dplyr::select(-"categories")
  }
  
  # Return all fields if requested, otherwise extract and rename
  if (return_all_fields) {
    return(unified_apps)
  } else {
    # Include category_details in the simplified output if available
    base_columns <- c("app_id", "name")
    if ("category_details" %in% colnames(unified_apps)) {
      base_columns <- c(base_columns, "category_details")
    }
    
    extracted_info <- unified_apps %>%
      select(all_of(base_columns)) %>%
      rename(
        unified_app_id = .data$app_id,
        unified_app_name = .data$name
      )
    return(tibble::as_tibble(extracted_info))
  }
}