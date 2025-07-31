#' Validate app ID format
#'
#' Checks if an app ID matches the expected format for a given OS.
#'
#' @param app_id Character string. The app ID to validate.
#' @param os Character string. The operating system: "ios", "android", or "unified".
#' @return Logical. TRUE if the app ID format is valid for the OS, FALSE otherwise.
#' @noRd
is_valid_app_id <- function(app_id, os) {
  if (is.null(app_id) || app_id == "") {
    return(FALSE)
  }
  
  switch(os,
    ios = grepl("^[0-9]+$", app_id),  # iOS IDs are numeric strings
    android = grepl("^[a-zA-Z][a-zA-Z0-9_]*(\\.[a-zA-Z][a-zA-Z0-9_]*)*$", app_id),  # Android package names
    unified = grepl("^[a-f0-9]{24}$", app_id),  # Unified IDs are 24-char hex
    FALSE
  )
}

#' Detect app ID type
#'
#' Determines what type of app ID was provided (iOS, Android, or unified).
#'
#' @param app_id Character string. The app ID to check.
#' @return Character string. "ios", "android", "unified", or "unknown".
#' @noRd
detect_app_id_type <- function(app_id) {
  if (is.null(app_id) || app_id == "") {
    return("unknown")
  }
  
  if (grepl("^[0-9]+$", app_id)) {
    return("ios")
  } else if (grepl("^[a-zA-Z][a-zA-Z0-9_]*(\\.[a-zA-Z][a-zA-Z0-9_]*)*$", app_id)) {
    return("android")
  } else if (grepl("^[a-f0-9]{24}$", app_id)) {
    return("unified")
  } else {
    return("unknown")
  }
}

#' Validate and transform app IDs for API calls
#'
#' This function validates app IDs against the requested OS and can optionally
#' look up the correct platform-specific ID when a mismatched ID is provided.
#'
#' @param app_ids Character vector. The app IDs to validate.
#' @param os Character string. The requested operating system: "ios" or "android".
#' @param lookup_mismatched Logical. If TRUE, attempts to look up the correct 
#'   platform ID when a mismatched ID is provided. Default is TRUE.
#' @param auth_token Character string. API token for lookups.
#' @param verbose Logical. Whether to show progress messages.
#' @return Character vector of validated/transformed app IDs.
#' @noRd
validate_and_transform_app_ids <- function(app_ids, os, 
                                         lookup_mismatched = TRUE,
                                         auth_token = NULL,
                                         verbose = TRUE) {
  
  if (is.null(app_ids) || length(app_ids) == 0) {
    return(app_ids)
  }
  
  # For unified OS, we don't transform IDs
  if (os == "unified") {
    # Check if all IDs are unified format
    invalid_ids <- app_ids[!sapply(app_ids, is_valid_app_id, os = "unified")]
    if (length(invalid_ids) > 0) {
      stop(paste0(
        "Invalid unified app ID format. Unified IDs must be 24-character hexadecimal strings.\n",
        "Invalid IDs: ", paste(invalid_ids, collapse = ", "), "\n",
        "Example unified ID: '5ba4585f539ce75b97db6bcb'"
      ))
    }
    return(app_ids)
  }
  
  # For iOS/Android, check each ID
  validated_ids <- character(length(app_ids))
  
  for (i in seq_along(app_ids)) {
    app_id <- app_ids[i]
    id_type <- detect_app_id_type(app_id)
    
    # If the ID matches the requested OS, use it as-is
    if (id_type == os) {
      validated_ids[i] <- app_id
      next
    }
    
    # If it's a unified ID, we need to look it up
    if (id_type == "unified") {
      if (!lookup_mismatched) {
        stop(paste0(
          "Unified app ID provided but ", os, " ID required: ", app_id, "\n",
          "Use st_app_lookup() to get platform-specific IDs."
        ))
      }
      
      if (verbose) {
        message("Looking up ", os, " ID for unified app: ", app_id)
      }
      
      # Use st_app_lookup to get platform IDs
      lookup_result <- tryCatch({
        st_app_lookup(app_id, auth_token = auth_token, verbose = FALSE)
      }, error = function(e) {
        NULL
      })
      
      if (!is.null(lookup_result)) {
        platform_id <- if (os == "ios") lookup_result$ios_app_id else lookup_result$android_app_id
        
        if (!is.null(platform_id)) {
          validated_ids[i] <- platform_id
          if (verbose) {
            message("Found ", os, " ID: ", platform_id, " for app: ", 
                   lookup_result$app_name %||% app_id)
          }
        } else {
          stop(paste0(
            "App does not have an ", os, " version: ", 
            lookup_result$app_name %||% app_id
          ))
        }
      } else {
        stop(paste0("Could not look up unified app ID: ", app_id))
      }
      
    } else if (id_type == "ios" && os == "android") {
      # iOS ID provided but Android requested
      error_msg <- paste0(
        "iOS app ID provided but Android ID required: ", app_id, "\n",
        "iOS app IDs are numeric (e.g., '1234567890').\n",
        "Android app IDs are package names (e.g., 'com.example.app').\n"
      )
      
      if (lookup_mismatched) {
        error_msg <- paste0(error_msg, 
          "Note: Cross-platform lookup from iOS to Android is not supported.\n",
          "Please provide the Android package name directly."
        )
      }
      
      stop(error_msg)
      
    } else if (id_type == "android" && os == "ios") {
      # Android ID provided but iOS requested
      error_msg <- paste0(
        "Android app ID provided but iOS ID required: ", app_id, "\n",
        "Android app IDs are package names (e.g., 'com.example.app').\n",
        "iOS app IDs are numeric (e.g., '1234567890').\n"
      )
      
      if (lookup_mismatched) {
        error_msg <- paste0(error_msg, 
          "Note: Cross-platform lookup from Android to iOS is not supported.\n",
          "Please provide the iOS app ID directly."
        )
      }
      
      stop(error_msg)
      
    } else {
      # Unknown format
      stop(paste0(
        "Invalid app ID format for ", os, ": ", app_id, "\n",
        "iOS IDs are numeric (e.g., '1234567890').\n",
        "Android IDs are package names (e.g., 'com.example.app').\n",
        "Unified IDs are 24-character hex strings (e.g., '5ba4585f539ce75b97db6bcb')."
      ))
    }
  }
  
  return(validated_ids)
}

#' Create helpful error message for app ID format issues
#'
#' @param app_id The invalid app ID
#' @param expected_os The expected OS format
#' @param detected_type The detected ID type
#' @return Character string with error message
#' @noRd
create_app_id_error_message <- function(app_id, expected_os, detected_type = NULL) {
  if (is.null(detected_type)) {
    detected_type <- detect_app_id_type(app_id)
  }
  
  base_msg <- paste0("Invalid app ID for ", expected_os, " platform: '", app_id, "'\n\n")
  
  format_info <- paste0(
    "Expected formats:\n",
    "  - iOS: Numeric ID (e.g., '1234567890')\n",
    "  - Android: Package name (e.g., 'com.example.app')\n",
    "  - Unified: 24-char hex ID (e.g., '5ba4585f539ce75b97db6bcb')\n"
  )
  
  detected_msg <- if (detected_type != "unknown") {
    paste0("\nDetected format: ", detected_type, "\n")
  } else {
    "\nDetected format: unknown\n"
  }
  
  suggestion <- switch(detected_type,
    ios = if (expected_os == "android") {
      "To query this iOS app on Android, you need the Android package name.\n"
    } else "",
    android = if (expected_os == "ios") {
      "To query this Android app on iOS, you need the iOS numeric ID.\n"
    } else "",
    unified = paste0(
      "This appears to be a Sensor Tower unified ID.\n",
      "Use st_app_lookup() to get platform-specific IDs, or\n",
      "use os='unified' with functions that support it.\n"
    ),
    ""
  )
  
  paste0(base_msg, format_info, detected_msg, suggestion)
}