
#' Search for apps or publishers
#'
#' @param os Platform (unified)
#' @param entity_type Type of entity to search for (app or publisher)
#' @param term Search term (min 3 Latin chars or 2 non-Latin chars)
#' @param limit Number of results to return (max 250)
#' @param auth_token Authentication token
#'
#' @return List containing search results
#' @export
#'
#' @examples
#' \dontrun{
#' search_entities(os = "unified", entity_type = "app", term = "Lyft", limit = 100)
#' }
search_entities <- function(os = "unified", 
                          entity_type = "app", 
                          term, 
                          limit = 100, 
                          auth_token = Sys.getenv("SENSORTOWER_AUTH")) {
  
  base_url <- "https://api.sensortower.com"
  endpoint <- sprintf("/v1/%s/search_entities", os)
  
  # Parameter validation
  if (nchar(term) < 3) {
    stop("Search term must be at least 3 characters long for Latin characters")
  }
  
  if (limit > 250) {
    warning("Limit cannot exceed 250. Setting limit to 250")
    limit <- 250
  }
  
  # Construct query parameters
  params <- list(
    entity_type = entity_type,
    term = term,
    limit = limit
  )
  
  # Make the API request
  response <- httr::GET(
    url = paste0(base_url, endpoint),
    query = params,
    httr::add_headers(Authorization = paste("Bearer", auth_token))
  )
  
  # Check for errors
  if (httr::http_error(response)) {
    stop(
      sprintf(
        "API request failed [%s]: %s",
        httr::status_code(response),
        httr::content(response)$error
      )
    )
  }
  
  # Parse and return the response
  httr::content(response)
}
