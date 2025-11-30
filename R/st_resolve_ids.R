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

  # Centralized mapping using cache
  input_ids <- c(unified_app_id, ios_app_id, android_app_id)
  input_ids <- input_ids[!vapply(input_ids, is.null, logical(1))]
  input_ids <- as.character(input_ids)[1]

  # Use the cached resolution function
  mapping <- tryCatch(
    {
      resolve_app_id(input_ids, auth_token = auth_token, verbose = verbose)
    },
    error = function(e) NULL
  )

  # Extract IDs from the mapping result
  # resolve_app_id returns: unified_app_id, ios_app_id, android_app_id
  # Prefer mapped ID, but fall back to input ID if mapped ID is missing/NA
  unified_mapped <- if (!is.null(mapping) && !is.null(mapping$unified_app_id) && !is.na(mapping$unified_app_id)) mapping$unified_app_id else unified_app_id
  ios_mapped <- if (!is.null(mapping) && !is.null(mapping$ios_app_id) && !is.na(mapping$ios_app_id)) mapping$ios_app_id else ios_app_id
  android_mapped <- if (!is.null(mapping) && !is.null(mapping$android_app_id) && !is.na(mapping$android_app_id)) mapping$android_app_id else android_app_id
  lookup_performed <- !is.null(mapping)

  if (os == "ios" && !is.null(ios_mapped) && !is.na(ios_mapped) && nzchar(as.character(ios_mapped))) {
    return(list(resolved_ids = list(ios_app_id = ios_mapped), app_id_type = "ios", lookup_performed = lookup_performed))
  }
  if (os == "android" && !is.null(android_mapped) && !is.na(android_mapped) && nzchar(as.character(android_mapped))) {
    return(list(resolved_ids = list(android_app_id = android_mapped), app_id_type = "android", lookup_performed = lookup_performed))
  }
  if (os == "unified") {
    if (!is.null(unified_mapped) && !is.na(unified_mapped) && nzchar(as.character(unified_mapped))) {
      return(list(resolved_ids = list(unified_app_id = unified_mapped), app_id_type = "unified", lookup_performed = lookup_performed))
    }
    # Fallback: if we have platform IDs, return them
    if ((!is.null(ios_mapped) && !is.na(ios_mapped)) || (!is.null(android_mapped) && !is.na(android_mapped))) {
      return(list(
        resolved_ids = list(
          unified_app_id = NULL,
          ios_app_id = ios_mapped,
          android_app_id = android_mapped
        ),
        app_id_type = "unified",
        lookup_performed = lookup_performed
      ))
    }
  }

  stop("Failed to resolve appropriate IDs for OS: ", os)
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
