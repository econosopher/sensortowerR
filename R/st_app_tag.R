#' Fetch Apps by Custom Fields and Tags
#'
#' Retrieves apps filtered by custom fields and tags from Sensor Tower.
#' This function uses the /v1/app_tag/apps endpoint.
#'
#' @param app_id_type Character string. Operating System. Must be one of
#'   "itunes" (iOS) or "unified". Required.
#' @param custom_fields_filter_id Character string. ID of a Sensor Tower
#'   custom field filter. Required. Use the filter ID from relevant endpoint.
#' @param name Optional. Character string. Name of Custom or Global Field.
#'   Defaults to "Stock Ticker".
#' @param value Optional. Character string. Tag value for custom or global 
#'   field provided. Leave blank to fetch all possible apps.
#' @param global Optional. Logical. Filter by global or organization custom 
#'   fields. Defaults to TRUE (false means organization custom fields).
#' @param last_known_id Optional. Character string. Supply last_known_id from 
#'   previous request to get next page. Leave blank to get first page.
#' @param auth_token Optional. Character string. Your Sensor Tower API token.
#' @param base_url Optional. Character string. The base URL for the API.
#'
#' @return A [tibble][tibble::tibble] with app data including IDs and metadata.
#'
#' @export
#' @importFrom httr2 request req_url_path_append req_url_query req_headers
#' @importFrom httr2 req_perform resp_body_string
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @importFrom rlang abort %||%
st_app_tag <- function(
    app_id_type,
    custom_fields_filter_id,
    name = NULL,
    value = NULL,
    global = TRUE,
    last_known_id = NULL,
    auth_token = NULL,
    base_url = "https://api.sensortower.com"
) {
  
  # --- Input Validation ---
  app_id_type <- match.arg(app_id_type, c("itunes", "unified"))
  
  if (missing(custom_fields_filter_id) || is.null(custom_fields_filter_id)) {
    rlang::abort("'custom_fields_filter_id' parameter is required.")
  }
  
  # --- Authentication ---
  auth_token_val <- auth_token %||% Sys.getenv("SENSORTOWER_AUTH_TOKEN")
  if (auth_token_val == "") {
    rlang::abort(
      "Authentication token not found. Please set it as an environment variable."
    )
  }
  
  # --- Build Query Parameters ---
  query_params <- list(
    custom_fields_filter_id = custom_fields_filter_id
  )
  
  # Add optional parameters
  if (!is.null(name)) {
    query_params$name <- name
  }
  
  if (!is.null(value)) {
    query_params$value <- value
  }
  
  query_params$global <- tolower(as.character(global))
  
  if (!is.null(last_known_id)) {
    query_params$last_known_id <- last_known_id
  }
  
  # --- Build Request ---
  req <- httr2::request(base_url) %>%
    httr2::req_url_path_append("v1", "app_tag", "apps") %>%
    httr2::req_url_query(
      app_id_type = app_id_type,
      !!!query_params
    ) %>%
    httr2::req_headers(
      "Authorization" = paste("Bearer", auth_token_val),
      "Accept" = "application/json",
      "User-Agent" = "sensortowerR"
    ) %>%
    httr2::req_timeout(60)
  
  # --- Perform Request ---
  tryCatch({
    resp <- httr2::req_perform(req)
    
    # Parse response
    content <- httr2::resp_body_string(resp)
    data <- jsonlite::fromJSON(content, flatten = TRUE)
    
    # Convert to tibble
    if (!is.null(data) && length(data) > 0) {
      result <- tibble::as_tibble(data)
    } else {
      result <- tibble::tibble()
    }
    
    return(result)
    
  }, error = function(e) {
    resp_obj <- NULL
    if (!is.null(e$response)) resp_obj <- e$response
    if (is.null(resp_obj) && !is.null(e$resp)) resp_obj <- e$resp
    if (inherits(e, "httr2_http") && !is.null(resp_obj)) {
      status <- httr2::resp_status(resp_obj)
      body <- httr2::resp_body_string(resp_obj)
      
      if (status == 401) {
        rlang::abort("Invalid authentication token.")
      } else if (status == 403) {
        rlang::abort("Your API token is not authorized.")
      } else if (status == 422) {
        rlang::abort(paste("Invalid Query Parameter:", body))
      } else {
        rlang::abort(paste("API request failed with status", status, ":", body))
      }
    } else {
      rlang::abort(paste("Unexpected error:", e$message))
    }
  })
}
