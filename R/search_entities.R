
#' Search for apps or publishers in Sensor Tower
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
#' # Single search
#' search_entities(term = "Spotify")
#'
#' # Search multiple terms
#' terms <- c("Spotify", "Netflix", "TikTok")
#' results <- lapply(terms, function(x) {
#'   search_entities(term = x, limit = 5)
#' })
#'
#' # Process multiple results
#' names(results) <- terms
#' app_names <- lapply(results, function(x) {
#'   sapply(x, function(app) app$name)
#' })
#' }
app_search <- function(term,
                          os = "unified",
                          entity_type = "app",
                          limit = 100,
                          auth_token = Sys.getenv("SENSORTOWER_AUTH")) {
  
  if (is.null(auth_token) || auth_token == "") {
    stop("Authentication token is required. Set SENSORTOWER_AUTH environment variable.")
  }
  
  base_url <- "https://api.sensortower.com/v1"
  endpoint <- file.path(base_url, os, "search_entities")
  
  response <- httr::GET(
    url = endpoint,
    query = list(
      entity_type = entity_type,
      term = term,
      limit = min(limit, 250)
    ),
    httr::add_headers(
      Authorization = sprintf("Bearer %s", auth_token)
    )
  )
  
  if (httr::http_error(response)) {
    content <- httr::content(response, "parsed")
    stop(sprintf("API request failed [%s]: %s", 
                httr::status_code(response),
                if (!is.null(content$error)) content$error else "Unknown error"))
  }
  
  httr::content(response)
}
