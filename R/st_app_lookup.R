#' Look up app information by any ID type
#'
#' This function looks up app information using any type of app ID - unified,
#' iOS, or Android. It returns the unified ID and platform-specific IDs that 
#' can be used with other API functions.
#' 
#' The function automatically detects the ID type:
#' - 24-character hex strings are treated as unified IDs
#' - Numeric strings are treated as iOS app IDs
#' - Strings starting with com/net/org/io are treated as Android package names
#'
#' @param app_id Character string. Can be:
#'   - Sensor Tower unified app ID (24-char hex like "5ba4585f539ce75b97db6bcb")
#'   - iOS app ID (numeric like "943599237")
#'   - Android package name (like "com.bandainamcogames.dbzdokkanww")
#' @param auth_token Character string. Your Sensor Tower API authentication token.
#' @param verbose Logical. Whether to show progress messages. Default is FALSE.
#'
#' @return A list with components:
#'   - `unified_app_id`: The Sensor Tower unified app ID
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
st_app_lookup <- function(app_id, 
                         auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN"),
                         verbose = FALSE) {
  
  # Validate auth token
  if (is.null(auth_token) || auth_token == "") {
    rlang::abort("Authentication token is required. Set SENSORTOWER_AUTH_TOKEN environment variable.")
  }
  
  # Detect the type of ID
  id_type <- NULL
  if (grepl("^[a-f0-9]{24}$", app_id)) {
    id_type <- "unified"
  } else if (grepl("^\\d+$", app_id)) {
    id_type <- "ios"
  } else if (grepl("^(com|net|org|io)\\.", app_id)) {
    id_type <- "android"
  } else {
    rlang::abort(paste0(
      "Invalid app ID format. Expected one of:\n",
      "- 24-character hex ID (unified): '5ba4585f539ce75b97db6bcb'\n",
      "- iOS numeric ID: '1234567890'\n",
      "- Android package name: 'com.example.app'\n",
      "Got: '", app_id, "'"
    ))
  }
  
  if (verbose) message("Looking up ", id_type, " ID: ", app_id)
  
  # For unified IDs, use the original search method
  if (id_type == "unified") {
    search_results <- tryCatch({
      st_app_info(
        term = app_id,
        return_all_fields = TRUE,
        limit = 1,
        auth_token = auth_token
      )
    }, error = function(e) {
      if (verbose) message("Search by ID failed: ", e$message)
      NULL
    })
    
    if (!is.null(search_results) && nrow(search_results) > 0 && 
        search_results$app_id[1] == app_id) {
      # Found the app by its unified ID
      app_name <- search_results$name[1]
      unified_id <- app_id
      
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
        unified_app_id = unified_id,
        ios_app_id = ios_id,
        android_app_id = android_id,
        app_name = app_name,
        publisher_name = search_results$publisher_name[1] %||% NA
      ))
    }
  } else {
    # For platform IDs, search by app name to find the unified ID
    # First, try to find apps that contain this platform ID
    
    # Determine search app store
    search_store <- if (id_type == "ios") "unified" else "unified"
    
    # Search more broadly - the ID might not be searchable
    # So we'll need to look through results manually
    search_results <- NULL
    
    # Try different search strategies
    search_terms <- c()
    
    # For iOS IDs, we can't reliably search by the ID itself
    # For Android IDs, try the package name parts
    if (id_type == "android") {
      # Extract app name from package
      parts <- strsplit(app_id, "\\.")[[1]]
      if (length(parts) > 2) {
        # Try the last part (often the app name)
        search_terms <- c(parts[length(parts)], paste(parts[(length(parts)-1):length(parts)], collapse = " "))
      }
    }
    
    # If we have search terms, try them
    for (term in search_terms) {
      if (verbose) message("Searching for: ", term)
      
      search_results <- tryCatch({
        st_app_info(
          term = term,
          app_store = search_store,
          return_all_fields = TRUE,
          limit = 50,
          auth_token = auth_token
        )
      }, error = function(e) {
        if (verbose) message("Search failed: ", e$message)
        NULL
      })
      
      if (!is.null(search_results) && nrow(search_results) > 0) {
        # Look through results for our platform ID
        for (i in 1:nrow(search_results)) {
          found <- FALSE
          
          if (id_type == "ios" && "ios_apps" %in% names(search_results)) {
            ios_apps <- search_results$ios_apps[[i]]
            if (!is.null(ios_apps) && is.data.frame(ios_apps) && app_id %in% ios_apps$app_id) {
              found <- TRUE
            }
          } else if (id_type == "android" && "android_apps" %in% names(search_results)) {
            android_apps <- search_results$android_apps[[i]]
            if (!is.null(android_apps) && is.data.frame(android_apps) && app_id %in% android_apps$app_id) {
              found <- TRUE
            }
          }
          
          if (found) {
            # Extract all the information
            unified_id <- search_results$app_id[i]
            app_name <- search_results$name[i]
            publisher_name <- search_results$publisher_name[i]
            
            # Get platform IDs
            ios_id <- NULL
            android_id <- NULL
            
            if ("ios_apps" %in% names(search_results) && !is.null(search_results$ios_apps[[i]])) {
              ios_apps <- search_results$ios_apps[[i]]
              if (is.data.frame(ios_apps) && nrow(ios_apps) > 0) {
                ios_id <- ios_apps$app_id[1]
              }
            }
            
            if ("android_apps" %in% names(search_results) && !is.null(search_results$android_apps[[i]])) {
              android_apps <- search_results$android_apps[[i]]
              if (is.data.frame(android_apps) && nrow(android_apps) > 0) {
                android_id <- android_apps$app_id[1]
              }
            }
            
            if (verbose) {
              message("Found app: ", app_name)
              message("Unified ID: ", unified_id)
              message("iOS ID: ", ios_id %||% "none")
              message("Android ID: ", android_id %||% "none")
            }
            
            return(list(
              unified_app_id = unified_id,
              ios_app_id = ios_id,
              android_app_id = android_id,
              app_name = app_name,
              publisher_name = publisher_name %||% NA
            ))
          }
        }
      }
    }
  }
  
  # If we couldn't find the app
  if (verbose) message("Could not find app with ", id_type, " ID: ", app_id)
  
  return(NULL)
}

# Helper operator
`%||%` <- function(x, y) if (is.null(x)) y else x
