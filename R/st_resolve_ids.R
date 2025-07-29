#' Resolve App IDs Based on OS Parameter
#'
#' Internal function that resolves app IDs based on the requested OS.
#' The OS parameter always controls what IDs are used for data fetching.
#'
#' @param unified_app_id Character. Unified app ID (24-char hex)
#' @param ios_app_id Character. iOS app ID (numeric)
#' @param android_app_id Character. Android package name
#' @param os Character. Required. One of "ios", "android", or "unified"
#' @param auth_token Character. Sensor Tower API token
#' @param verbose Logical. Show progress messages
#'
#' @return A list with:
#'   - resolved_ids: List of IDs to use for the API call
#'   - app_id_type: Type of ID ("ios", "android", or "unified")
#'   - lookup_performed: Whether ID lookups were needed
#'
#' @keywords internal
#' @noRd
resolve_ids_for_os <- function(
  unified_app_id = NULL,
  ios_app_id = NULL,
  android_app_id = NULL,
  os,
  auth_token,
  verbose = FALSE
) {
  
  # Validate OS parameter
  if (missing(os) || is.null(os) || !os %in% c("ios", "android", "unified")) {
    stop("'os' parameter is required and must be one of: 'ios', 'android', or 'unified'")
  }
  
  # Check that at least one ID is provided
  if (is.null(unified_app_id) && is.null(ios_app_id) && is.null(android_app_id)) {
    stop("At least one app ID must be provided (unified_app_id, ios_app_id, or android_app_id)")
  }
  
  # Initialize result
  result <- list(
    resolved_ids = NULL,
    app_id_type = os,
    lookup_performed = FALSE
  )
  
  # Case 1: OS matches the provided ID type - no lookup needed
  if (os == "ios" && !is.null(ios_app_id)) {
    if (verbose) message("Using provided iOS app ID: ", ios_app_id)
    result$resolved_ids <- list(ios_app_id = ios_app_id)
    return(result)
  }
  
  if (os == "android" && !is.null(android_app_id)) {
    if (verbose) message("Using provided Android app ID: ", android_app_id)
    result$resolved_ids <- list(android_app_id = android_app_id)
    return(result)
  }
  
  if (os == "unified" && !is.null(unified_app_id)) {
    if (verbose) message("Using provided unified app ID: ", unified_app_id)
    result$resolved_ids <- list(unified_app_id = unified_app_id)
    return(result)
  }
  
  # Case 2: Need to look up the appropriate ID
  result$lookup_performed <- TRUE
  
  # If we have a unified ID but need platform-specific
  if (!is.null(unified_app_id) && os %in% c("ios", "android")) {
    if (verbose) message("Looking up ", os, " ID from unified ID: ", unified_app_id)
    
    lookup_result <- tryCatch({
      st_app_lookup(unified_app_id, auth_token = auth_token, verbose = FALSE)
    }, error = function(e) {
      if (verbose) message("Lookup failed: ", e$message)
      NULL
    })
    
    if (!is.null(lookup_result)) {
      if (os == "ios" && !is.null(lookup_result$ios_app_id)) {
        result$resolved_ids <- list(ios_app_id = lookup_result$ios_app_id)
        if (verbose) message("Resolved to iOS ID: ", lookup_result$ios_app_id)
      } else if (os == "android" && !is.null(lookup_result$android_app_id)) {
        result$resolved_ids <- list(android_app_id = lookup_result$android_app_id)
        if (verbose) message("Resolved to Android ID: ", lookup_result$android_app_id)
      } else {
        stop(paste0("Could not find ", os, " ID for unified app: ", unified_app_id))
      }
    } else {
      stop(paste0("Failed to look up ", os, " ID from unified ID: ", unified_app_id))
    }
  }
  
  # If we have platform-specific IDs but need unified
  else if (os == "unified" && (!is.null(ios_app_id) || !is.null(android_app_id))) {
    # Try with iOS ID first
    if (!is.null(ios_app_id)) {
      if (verbose) message("Looking up unified ID from iOS ID: ", ios_app_id)
      
      mapping_result <- tryCatch({
        st_get_unified_mapping(ios_app_id, os = "ios", auth_token = auth_token)
      }, error = function(e) {
        if (verbose) message("iOS lookup failed: ", e$message)
        NULL
      })
      
      if (!is.null(mapping_result) && !is.null(mapping_result$unified_app_id[1])) {
        result$resolved_ids <- list(unified_app_id = mapping_result$unified_app_id[1])
        if (verbose) message("Resolved to unified ID: ", mapping_result$unified_app_id[1])
        return(result)
      }
    }
    
    # Try with Android ID if iOS didn't work
    if (!is.null(android_app_id)) {
      if (verbose) message("Looking up unified ID from Android ID: ", android_app_id)
      
      mapping_result <- tryCatch({
        st_get_unified_mapping(android_app_id, os = "android", auth_token = auth_token)
      }, error = function(e) {
        if (verbose) message("Android lookup failed: ", e$message)
        NULL
      })
      
      if (!is.null(mapping_result) && !is.null(mapping_result$unified_app_id[1])) {
        result$resolved_ids <- list(unified_app_id = mapping_result$unified_app_id[1])
        if (verbose) message("Resolved to unified ID: ", mapping_result$unified_app_id[1])
        return(result)
      }
    }
    
    stop("Could not resolve unified ID from provided platform IDs")
  }
  
  # If we have iOS but need Android (or vice versa), we need to go through unified first
  else if ((os == "ios" && !is.null(android_app_id)) || 
           (os == "android" && !is.null(ios_app_id))) {
    
    # First get unified ID
    source_id <- if (!is.null(ios_app_id)) ios_app_id else android_app_id
    source_os <- if (!is.null(ios_app_id)) "ios" else "android"
    
    if (verbose) message("Looking up unified ID from ", source_os, " ID: ", source_id)
    
    mapping_result <- tryCatch({
      st_get_unified_mapping(source_id, os = source_os, auth_token = auth_token)
    }, error = function(e) {
      if (verbose) message("Unified lookup failed: ", e$message)
      NULL
    })
    
    if (!is.null(mapping_result) && !is.null(mapping_result$unified_app_id[1])) {
      unified_id <- mapping_result$unified_app_id[1]
      
      # Now look up the target platform
      if (verbose) message("Looking up ", os, " ID from unified ID: ", unified_id)
      
      lookup_result <- tryCatch({
        st_app_lookup(unified_id, auth_token = auth_token, verbose = FALSE)
      }, error = function(e) {
        if (verbose) message("Platform lookup failed: ", e$message)
        NULL
      })
      
      if (!is.null(lookup_result)) {
        if (os == "ios" && !is.null(lookup_result$ios_app_id)) {
          result$resolved_ids <- list(ios_app_id = lookup_result$ios_app_id)
          if (verbose) message("Resolved to iOS ID: ", lookup_result$ios_app_id)
        } else if (os == "android" && !is.null(lookup_result$android_app_id)) {
          result$resolved_ids <- list(android_app_id = lookup_result$android_app_id)
          if (verbose) message("Resolved to Android ID: ", lookup_result$android_app_id)
        } else {
          stop(paste0("Could not find ", os, " ID for app"))
        }
      }
    } else {
      stop(paste0("Could not resolve ", os, " ID from ", source_os, " ID"))
    }
  }
  
  if (is.null(result$resolved_ids)) {
    stop("Failed to resolve appropriate IDs for OS: ", os)
  }
  
  return(result)
}

#' Add App ID Metadata to Data Frame
#'
#' Adds app_id and app_id_type columns to a data frame
#'
#' @param data Data frame to modify
#' @param app_id The app ID used
#' @param app_id_type The type of app ID ("ios", "android", or "unified")
#'
#' @return Modified data frame with app_id and app_id_type columns
#' @keywords internal
#' @noRd
add_app_id_metadata <- function(data, app_id, app_id_type) {
  if (is.null(data) || nrow(data) == 0) {
    return(data)
  }
  
  data$app_id <- app_id
  data$app_id_type <- app_id_type
  
  # Move these columns to the front
  id_cols <- c("app_id", "app_id_type")
  other_cols <- setdiff(names(data), id_cols)
  data <- data[, c(id_cols, other_cols)]
  
  return(data)
}