#' Look up app by Sensor Tower unified ID
#'
#' This function looks up app information using Sensor Tower's internal unified app ID
#' (24-character hex format like "5ba4585f539ce75b97db6bcb"). It returns platform-specific
#' IDs that can be used with other API functions.
#' 
#' IMPORTANT: This function only works with Sensor Tower's internal unified IDs.
#' Do not pass iOS app IDs or Android package names - use the appropriate
#' platform-specific parameters in other functions instead.
#'
#' @param unified_id Character string. The Sensor Tower unified app ID (24-char hex format).
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
  
  # Validate that it's a proper Sensor Tower unified ID (24-char hex)
  if (!grepl("^[a-f0-9]{24}$", unified_id)) {
    stop(paste0(
      "Invalid unified ID format. Expected 24-character hex ID like '5ba4585f539ce75b97db6bcb'.\n",
      "Got: '", unified_id, "'\n\n",
      "If you have an iOS app ID (e.g., '1234567890'), use ios_app_id parameter instead.\n",
      "If you have an Android package (e.g., 'com.example.app'), use android_app_id parameter instead."
    ))
  }
  
  # Use search to find the app by its unified ID
  if (verbose) message("Looking up Sensor Tower unified ID: ", unified_id)
  
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
    
  # If we couldn't find by hex ID, return NULL
  if (verbose) message("Could not find app with unified ID: ", unified_id)
  
  return(NULL)
}

# Helper operator
`%||%` <- function(x, y) if (is.null(x)) y else x