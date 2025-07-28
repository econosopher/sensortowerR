#' Look up app by unified ID
#'
#' This function attempts to find app information when you have a unified app ID
#' (which could be a Sensor Tower internal ID, iOS numeric ID, or Android package name).
#' It returns platform-specific IDs that can be used with other API functions.
#'
#' @param unified_id Character string. The unified app ID to look up.
#' @param auth_token Character string. Your Sensor Tower API authentication token.
#' @param verbose Logical. Whether to show progress messages. Default is FALSE.
#'
#' @return A list with components:
#'   - `ios_app_id`: iOS app ID if found
#'   - `android_app_id`: Android app ID if found
#'   - `app_name`: App name if found
#'   - `publisher_name`: Publisher name if found
#'   Returns NULL if app cannot be found.
#'
#' @examples
#' \dontrun{
#' # Look up Star Trek Fleet Command
#' app_ids <- st_app_lookup("5ba4585f539ce75b97db6bcb")
#' 
#' # Use the IDs with st_ytd_metrics
#' if (!is.null(app_ids)) {
#'   metrics <- st_ytd_metrics(
#'     ios_app_id = app_ids$ios_app_id,
#'     android_app_id = app_ids$android_app_id,
#'     years = 2025,
#'     metrics = "revenue",
#'     countries = "WW"
#'   )
#' }
#' }
#' 
#' @export
st_app_lookup <- function(unified_id, 
                         auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN"),
                         verbose = FALSE) {
  
  # Validate auth token
  if (is.null(auth_token) || auth_token == "") {
    stop("Authentication token is required. Set SENSORTOWER_AUTH_TOKEN environment variable.")
  }
  
  # First, check if it's already a platform-specific ID
  if (grepl("^\\d+$", unified_id)) {
    # Looks like iOS ID
    if (verbose) message("Detected iOS app ID format")
    return(list(
      ios_app_id = unified_id,
      android_app_id = NULL,
      app_name = NULL,
      publisher_name = NULL
    ))
  } else if (grepl("^(com|net|org|io)\\.", unified_id)) {
    # Looks like Android package
    if (verbose) message("Detected Android package name format")
    return(list(
      ios_app_id = NULL,
      android_app_id = unified_id,
      app_name = NULL,
      publisher_name = NULL
    ))
  }
  
  # For Sensor Tower hex IDs, we need to use search instead of unified endpoint
  # The unified endpoint doesn't work well with these IDs
  if (grepl("^[a-f0-9]{24}$", unified_id)) {
    if (verbose) message("Detected Sensor Tower hex ID format")
    
    # Try to find the app by searching with the ID
    search_results <- tryCatch({
      st_app_info(
        term = unified_id,
        return_all_fields = TRUE,
        limit = 1,
        auth_token = auth_token
      )
    }, error = function(e) {
      if (verbose) message("Search by ID failed: ", e$message)
      NULL
    })
    
    if (!is.null(search_results) && nrow(search_results) > 0 && 
        search_results$app_id[1] == unified_id) {
      # Found the app by its ID
      app_name <- search_results$name[1]
      
      # Extract platform IDs
      ios_id <- NULL
      android_id <- NULL
      
      if ("ios_apps" %in% names(search_results) && length(search_results$ios_apps[[1]]) > 0) {
        ios_id <- search_results$ios_apps[[1]]$app_id[1]
      }
      
      if ("android_apps" %in% names(search_results) && length(search_results$android_apps[[1]]) > 0) {
        android_id <- search_results$android_apps[[1]]$app_id[1]
      }
      
      if (verbose) {
        message("Found app: ", app_name)
        message("iOS ID: ", ios_id %||% "none")
        message("Android ID: ", android_id %||% "none")
      }
      
      return(list(
        ios_app_id = ios_id,
        android_app_id = android_id,
        app_name = app_name,
        publisher_name = search_results$publisher_name[1] %||% NA
      ))
    }
    
    # If we couldn't find by hex ID, we can't do much more
    if (verbose) message("Could not find app with hex ID: ", unified_id)
    return(NULL)
  }
  
  # For non-hex IDs, try the unified endpoint
  if (verbose) message("Attempting to fetch app details from unified endpoint...")
  
  app_details <- tryCatch({
    st_app_details(
      app_ids = unified_id,
      os = "unified",
      auth_token = auth_token
    )
  }, error = function(e) {
    if (verbose) message("Failed to fetch from unified endpoint: ", e$message)
    NULL
  })
  
  if (!is.null(app_details) && nrow(app_details) > 0) {
    # Extract what we can from the response
    app_name <- app_details$app_name[1]
    publisher_name <- app_details$publisher_name[1]
    
    if (verbose) {
      message("Found app: ", app_name)
      message("Publisher: ", publisher_name)
    }
    
    # Unfortunately, unified endpoint doesn't return platform IDs
    # So we need to search for the app by name
    if (!is.na(app_name) && nchar(app_name) > 0) {
      if (verbose) message("Searching for platform-specific IDs by app name...")
      
      search_results <- tryCatch({
        st_app_info(
          term = app_name,
          return_all_fields = TRUE,
          limit = 10,
          auth_token = auth_token
        )
      }, error = function(e) {
        if (verbose) message("Search failed: ", e$message)
        NULL
      })
      
      if (!is.null(search_results) && nrow(search_results) > 0) {
        # Try to find the matching app
        # First look for exact name match
        exact_match <- search_results[search_results$name == app_name, ]
        
        if (nrow(exact_match) > 0) {
          result <- exact_match[1, ]
        } else {
          # Use first result as best guess
          result <- search_results[1, ]
          if (verbose) {
            message("No exact match found, using closest match: ", result$name)
          }
        }
        
        # Extract platform IDs
        ios_id <- NULL
        android_id <- NULL
        
        if ("ios_apps" %in% names(result) && length(result$ios_apps[[1]]) > 0) {
          ios_id <- result$ios_apps[[1]]$app_id[1]
        }
        
        if ("android_apps" %in% names(result) && length(result$android_apps[[1]]) > 0) {
          android_id <- result$android_apps[[1]]$app_id[1]
        }
        
        if (verbose) {
          message("Found iOS ID: ", ios_id %||% "none")
          message("Found Android ID: ", android_id %||% "none")
        }
        
        return(list(
          ios_app_id = ios_id,
          android_app_id = android_id,
          app_name = app_name,
          publisher_name = publisher_name
        ))
      }
    }
  }
  
  # If we got here, we couldn't find the app
  if (verbose) message("Could not resolve platform IDs for unified ID: ", unified_id)
  
  return(NULL)
}

# Helper operator
`%||%` <- function(x, y) if (is.null(x)) y else x