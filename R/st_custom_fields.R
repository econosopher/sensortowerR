#' Custom Fields Filter Functions
#'
#' Functions to work with Sensor Tower custom fields filters.
#' 
#' @name st_custom_fields
#' @rdname st_custom_fields
NULL

#' Create a Custom Fields Filter
#'
#' Creates a custom fields filter ID by posting filter criteria to Sensor Tower.
#' This filter ID can then be used with other endpoints to query filtered data.
#'
#' @param custom_fields List. A list of custom field criteria with the following structure:
#'   - exclude: Logical. Whether to exclude apps matching this criteria
#'   - global: Logical. Whether this is a global field (TRUE) or organization field (FALSE)
#'   - name: Character. The name of the custom field (e.g., "Free", "Release Date (US)")
#'   - values: Character vector. Values to filter by (can be empty)
#'   - true: Logical. For boolean fields, the value to match
#' @param auth_token Optional. Character string. Your Sensor Tower API token.
#' @param base_url Optional. Character string. The base URL for the API.
#'
#' @return Character string containing the custom fields filter ID
#'
#' @examples
#' \dontrun{
#' # Create a filter for free apps
#' filter_id <- st_custom_fields_filter(
#'   custom_fields = list(
#'     list(
#'       exclude = FALSE,
#'       global = TRUE,
#'       name = "Free",
#'       values = list(),
#'       true = TRUE
#'     )
#'   )
#' )
#' }
#'
#' @export
#' @importFrom httr2 request req_url_path_append req_headers req_body_json
#' @importFrom httr2 req_perform resp_body_string req_method
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom rlang abort %||%
st_custom_fields_filter <- function(
    custom_fields,
    auth_token = NULL,
    base_url = "https://api.sensortower.com"
) {
  
  # --- Input Validation ---
  if (missing(custom_fields) || is.null(custom_fields)) {
    rlang::abort("'custom_fields' parameter is required.")
  }
  
  if (!is.list(custom_fields)) {
    rlang::abort("'custom_fields' must be a list.")
  }
  # Require at least one filter entry
  if (length(custom_fields) == 0) {
    rlang::abort("custom_fields must include at least one filter")
  }
  
  # --- Authentication ---
  auth_token_val <- auth_token %||% Sys.getenv("SENSORTOWER_AUTH_TOKEN")
  if (auth_token_val == "") {
    rlang::abort(
      "Authentication token not found. Please set it as an environment variable."
    )
  }
  
  # --- Validate custom fields structure ---
  for (i in seq_along(custom_fields)) {
    field <- custom_fields[[i]]
    if (!is.list(field)) {
      rlang::abort(sprintf("custom_fields[[%d]] must be a list", i))
    }
    
    # Check required fields
    if (is.null(field$name)) {
      rlang::abort(sprintf("custom_fields[[%d]]$name is required", i))
    }
    
    # Set defaults for optional fields
    if (is.null(field$exclude)) {
      custom_fields[[i]]$exclude <- FALSE
    }
    if (is.null(field$global)) {
      custom_fields[[i]]$global <- TRUE
    }
    if (is.null(field$values)) {
      custom_fields[[i]]$values <- list()
    }
    
    # Handle common boolean-like fields when values are empty
    if (length(custom_fields[[i]]$values) == 0 && is.null(field$true)) {
      nm <- as.character(field$name)
      if (grepl("^Has\\s+In-?App\\s+Purchases$", nm, ignore.case = TRUE)) {
        # Use tag-style value
        custom_fields[[i]]$name <- "In-App Purchases"
        custom_fields[[i]]$values <- list("Yes")
        custom_fields[[i]]$true <- NULL
        custom_fields[[i]]$value <- NULL
      } else if (grepl("^Has\\s+Ads$", nm, ignore.case = TRUE)) {
        custom_fields[[i]]$name <- "Contains Ads"
        custom_fields[[i]]$values <- list("Yes")
        custom_fields[[i]]$true <- NULL
        custom_fields[[i]]$value <- NULL
      } else if (grepl("^Is\\s+Free$|^Free$", nm, ignore.case = TRUE)) {
        custom_fields[[i]]$name <- "Free"
        custom_fields[[i]]$true <- TRUE
        custom_fields[[i]]$value <- TRUE
        custom_fields[[i]]$values <- list(TRUE)
      } else if (grepl("^In-?App\\s+Subscription$", nm, ignore.case = TRUE)) {
        custom_fields[[i]]$true <- TRUE
        custom_fields[[i]]$value <- TRUE
        custom_fields[[i]]$values <- list(TRUE)
      }
    }
  }
  
  # --- Build Request Body ---
  request_body <- list(custom_fields = custom_fields)
  
  # --- Build Request ---
  req <- httr2::request(base_url) %>%
    httr2::req_url_path_append("v1", "custom_fields_filter") %>%
    httr2::req_method("POST") %>%
    httr2::req_headers(
      "Authorization" = paste("Bearer", auth_token_val),
      "Content-Type" = "application/json",
      "Accept" = "application/json",
      "User-Agent" = "sensortowerR"
    ) %>%
    httr2::req_body_json(request_body) %>%
    httr2::req_timeout(30)
  
  # --- Perform Request (with simple retry on server errors) ---
  attempt_request <- function() {
    resp <- httr2::req_perform(req)
    content <- httr2::resp_body_string(resp)
    jsonlite::fromJSON(content, flatten = TRUE)
  }
  
  attempt <- 1
  while (attempt <= 2) {
    result <- tryCatch({
      attempt_request()
    }, error = function(e) {
      resp_obj <- NULL
      if (!is.null(e$response)) resp_obj <- e$response
      if (is.null(resp_obj) && !is.null(e$resp)) resp_obj <- e$resp
      if (!is.null(resp_obj)) {
        status <- httr2::resp_status(resp_obj)
        if (status >= 500 && attempt < 2) {
          # Retry once on server error
          attempt <<- attempt + 1
          Sys.sleep(0.5)
          return(structure(list(.retry = TRUE), class = "retry_marker"))
        }
      }
      stop(e)
    })
    
    if (inherits(result, "retry_marker")) next
    
    # Success path
    if (!is.null(result$custom_fields_filter_id)) {
      return(result$custom_fields_filter_id)
    } else {
      rlang::abort("No filter ID returned in response")
    }
  }
  
  # If we somehow fall through, handle error
  tryCatch({
    attempt_request()
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
      } else if (status == 404) {
        rlang::abort("Invalid filter ID or not found")
      } else {
        rlang::abort(paste("API request failed with status", status, ":", body))
      }
    } else {
      rlang::abort(paste("Unexpected error:", e$message))
    }
  })
}

#' Get Custom Fields Filter Details by ID
#'
#' Retrieves the custom field names and tag values associated with a 
#' Custom Fields Filter ID.
#'
#' @param id Character string. The custom fields filter ID to query.
#' @param auth_token Optional. Character string. Your Sensor Tower API token.
#' @param base_url Optional. Character string. The base URL for the API.
#'
#' @return A list containing the custom fields filter details
#'
#' @examples
#' \dontrun{
#' # Get details for a specific filter ID
#' filter_details <- st_custom_fields_filter_by_id(
#'   id = "6009d417241bc16eb8e07e9b"
#' )
#' }
#'
#' @export
#' @importFrom httr2 request req_url_path_append req_headers
#' @importFrom httr2 req_perform resp_body_string
#' @importFrom jsonlite fromJSON
#' @importFrom rlang abort %||%
st_custom_fields_filter_by_id <- function(
    id,
    auth_token = NULL,
    base_url = "https://api.sensortower.com"
) {
  
  # --- Input Validation ---
  if (missing(id) || is.null(id)) {
    rlang::abort("'id' parameter is required.")
  }
  
  # --- Authentication ---
  auth_token_val <- auth_token %||% Sys.getenv("SENSORTOWER_AUTH_TOKEN")
  if (auth_token_val == "") {
    rlang::abort(
      "Authentication token not found. Please set it as an environment variable."
    )
  }
  
  # --- Build Request ---
  req <- httr2::request(base_url) %>%
    httr2::req_url_path_append("v1", "custom_fields_filter", id) %>%
    httr2::req_headers(
      "Authorization" = paste("Bearer", auth_token_val),
      "Accept" = "application/json",
      "User-Agent" = "sensortowerR"
    ) %>%
    httr2::req_timeout(30)
  
  # --- Perform Request ---
  tryCatch({
    resp <- httr2::req_perform(req)
    
    # Parse response
    content <- httr2::resp_body_string(resp)
    data <- jsonlite::fromJSON(content, flatten = TRUE)
    
    return(data)
    
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
      } else if (status == 404) {
        rlang::abort("Invalid filter ID or not found")
      } else {
        rlang::abort(paste("API request failed with status", status, ":", body))
      }
    } else {
      rlang::abort(paste("Unexpected error:", e$message))
    }
  })
}

#' Get Custom Fields Values
#'
#' Retrieves a list of all accessible custom fields and their possible values.
#' This is useful for discovering what custom fields are available to filter by.
#'
#' @param term Optional. Character string. Search term to filter field names.
#' @param auth_token Optional. Character string. Your Sensor Tower API token.
#' @param base_url Optional. Character string. The base URL for the API.
#'
#' @return A tibble containing custom fields and their possible values
#'
#' @examples
#' \dontrun{
#' # Get all custom fields
#' fields <- st_custom_fields_values()
#' 
#' # Search for specific fields
#' date_fields <- st_custom_fields_values(term = "date")
#' }
#'
#' @export
#' @importFrom httr2 request req_url_path_append req_url_query req_headers
#' @importFrom httr2 req_perform resp_body_string
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @importFrom rlang abort %||%
st_custom_fields_values <- function(
    term = NULL,
    auth_token = NULL,
    base_url = "https://api.sensortower.com"
) {
  
  # --- Authentication ---
  auth_token_val <- auth_token %||% Sys.getenv("SENSORTOWER_AUTH_TOKEN")
  if (auth_token_val == "") {
    rlang::abort(
      "Authentication token not found. Please set it as an environment variable."
    )
  }
  
  # --- Build Query Parameters ---
  query_params <- list()
  if (!is.null(term)) {
    query_params$term <- term
  }
  
  # --- Build Request ---
  req <- httr2::request(base_url) %>%
    httr2::req_url_path_append("v1", "custom_fields_filter", "fields_values") %>%
    httr2::req_url_query(!!!query_params) %>%
    httr2::req_headers(
      "Authorization" = paste("Bearer", auth_token_val),
      "Accept" = "application/json",
      "User-Agent" = "sensortowerR"
    ) %>%
    httr2::req_timeout(30)
  
  # --- Perform Request ---
  tryCatch({
    resp <- httr2::req_perform(req)
    
    # Parse response
    content <- httr2::resp_body_string(resp)
    data <- jsonlite::fromJSON(content, flatten = TRUE)
    
    # Convert to tibble if data exists
    if (!is.null(data$custom_fields) && length(data$custom_fields) > 0) {
      result <- tibble::as_tibble(data$custom_fields)
    } else {
      result <- tibble::tibble()
    }
    
    return(result)
    
  }, error = function(e) {
    if (inherits(e, "httr2_http") && !is.null(e$response)) {
      status <- httr2::resp_status(e$response)
      body <- httr2::resp_body_string(e$response)
      
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
