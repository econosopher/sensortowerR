#' Get Unified ID Mapping for Apps
#'
#' Retrieves the mapping between platform-specific app IDs and unified app IDs.
#' This function handles cases where platform IDs from st_top_charts may not be
#' directly searchable, using app names as a fallback resolution method.
#'
#' @param app_ids Character vector of app IDs (can be iOS, Android, or unified hex IDs)
#' @param os Character string. Operating system: "ios", "android", or "unified"
#' @param auth_token Character string. Sensor Tower API authentication token.
#'   Defaults to environment variable SENSORTOWER_AUTH_TOKEN.
#'
#' @return A data frame with columns:
#'   - `input_id`: The original ID provided
#'   - `unified_app_id`: The unified app ID (hex format)
#'   - `unified_app_name`: The unified app name
#'   - `ios_app_id`: iOS app ID (if available)
#'   - `android_app_id`: Android app ID (if available)
#'   - `publisher_id`: Publisher ID
#'   - `publisher_name`: Publisher name
#'
#' @details
#' This function uses an ID-first approach (no name-based resolution):
#' 1. For hex IDs (24-char), uses st_app_lookup to get platform IDs
#' 2. For platform IDs, first tries to look them up via st_app_lookup
#' 3. If direct lookup fails, searches the unified index using the platform ID
#'    as the term and matches exact IDs within nested ios_apps/android_apps
#' 4. Returns the best available mapping for each app using IDs only
#'
#' Note: Platform IDs from st_top_charts may be regional or legacy IDs that
#' aren't directly searchable. In these cases, name-based search provides
#' the most reliable resolution to unified IDs.
#'
#' @examples
#' \dontrun{
#' # Get mapping with app names for better resolution
#' mapping <- st_get_unified_mapping(
#'   app_ids = c("943599237", "com.bandainamcogames.dbzdokkan"),
#'   app_names = c("Dragon Ball Z Dokkan Battle", "Dragon Ball Z Dokkan Battle"),
#'   os = "unified"
#' )
#' }
#'
#' @importFrom httr2 request req_url_path_append req_url_query req_headers req_timeout req_perform resp_body_string
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr %>%
#' @export
st_get_unified_mapping <- function(app_ids, 
                                   os = "unified",
                                   auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN")) {
  
  # Validate inputs
  if (missing(app_ids) || length(app_ids) == 0) {
    stop("At least one app_id is required")
  }
  
  # Get auth token
  auth_token_val <- auth_token %||% Sys.getenv("SENSORTOWER_AUTH_TOKEN")
  if (auth_token_val == "") {
    stop("Authentication token is required. Set SENSORTOWER_AUTH_TOKEN environment variable.")
  }
  
  # Initialize result data frame
  result_df <- data.frame(
    input_id = app_ids,
    unified_app_id = NA_character_,
    unified_app_name = NA_character_,
    ios_app_id = NA_character_,
    android_app_id = NA_character_,
    publisher_id = NA_character_,
    publisher_name = NA_character_,
    stringsAsFactors = FALSE
  )
  
  # Process each app ID
  for (i in seq_along(app_ids)) {
    app_id <- app_ids[i]
    id_type <- if (grepl("^[a-f0-9]{24}$", app_id)) "unified" else if (grepl("^\\d+$", app_id)) "ios" else if (grepl("^(com|net|org|io)\\.", app_id)) "android" else NA
    
    # Skip if already a hex ID (unified ID)
    if (grepl("^[a-f0-9]{24}$", app_id)) {
      # Use st_app_lookup to get platform IDs for unified ID
      lookup_result <- tryCatch({
        st_app_lookup(app_id, auth_token = auth_token_val, verbose = FALSE)
      }, error = function(e) NULL)
      
      if (!is.null(lookup_result)) {
        result_df$unified_app_id[i] <- app_id
        result_df$unified_app_name[i] <- lookup_result$app_name
        result_df$ios_app_id[i] <- lookup_result$ios_app_id
        result_df$android_app_id[i] <- lookup_result$android_app_id
        result_df$publisher_name[i] <- lookup_result$publisher_name
      }
      next
    }
    # First attempt: use st_app_lookup on platform IDs as well
    if (!grepl("^[a-f0-9]{24}$", app_id)) {
      lk <- tryCatch({ st_app_lookup(app_id, auth_token = auth_token_val, verbose = FALSE) }, error = function(e) NULL)
      if (!is.null(lk) && !is.null(lk$unified_app_id)) {
        result_df$unified_app_id[i] <- lk$unified_app_id
        result_df$unified_app_name[i] <- lk$app_name %||% NA_character_
        result_df$ios_app_id[i] <- lk$ios_app_id %||% NA_character_
        result_df$android_app_id[i] <- lk$android_app_id %||% NA_character_
        result_df$publisher_name[i] <- lk$publisher_name %||% NA_character_
        next
      }
    }

    # Second attempt: search unified index by the platform ID as term, then match nested platforms exactly
    unified_search <- tryCatch({
      st_app_info(
        term = app_id,
        app_store = "unified",
        return_all_fields = TRUE,
        limit = 50,
        auth_token = auth_token_val
      )
    }, error = function(e) NULL)
    if (!is.null(unified_search) && nrow(unified_search) > 0) {
      match_idx <- NA_integer_
      if (!is.na(id_type) && id_type == "ios" && "ios_apps" %in% names(unified_search)) {
        for (ri in seq_len(nrow(unified_search))) {
          ios_tbl <- unified_search$ios_apps[[ri]]
          if (is.data.frame(ios_tbl) && nrow(ios_tbl) > 0 && app_id %in% ios_tbl$app_id) { match_idx <- ri; break }
        }
      } else if (!is.na(id_type) && id_type == "android" && "android_apps" %in% names(unified_search)) {
        for (ri in seq_len(nrow(unified_search))) {
          and_tbl <- unified_search$android_apps[[ri]]
          if (is.data.frame(and_tbl) && nrow(and_tbl) > 0 && app_id %in% and_tbl$app_id) { match_idx <- ri; break }
        }
      }
      if (!is.na(match_idx)) {
        result_df$unified_app_id[i] <- unified_search$app_id[match_idx]
        result_df$unified_app_name[i] <- unified_search$name[match_idx]
        result_df$publisher_name[i] <- unified_search$publisher_name[match_idx] %||% NA_character_
        # Populate platform IDs from the matched row
        if ("ios_apps" %in% names(unified_search) && !is.null(unified_search$ios_apps[[match_idx]]) && nrow(unified_search$ios_apps[[match_idx]]) > 0) {
          result_df$ios_app_id[i] <- unified_search$ios_apps[[match_idx]]$app_id[1]
        }
        if ("android_apps" %in% names(unified_search) && !is.null(unified_search$android_apps[[match_idx]]) && nrow(unified_search$android_apps[[match_idx]]) > 0) {
          result_df$android_app_id[i] <- unified_search$android_apps[[match_idx]]$app_id[1]
        }
        next
      }
    }

    # If we still don't have a unified ID, use the input as a fallback for platform fields
    if (is.na(result_df$unified_app_id[i])) {
      # Just populate the appropriate platform field
      if (grepl("^\\d+$", app_id)) {
        # iOS ID
        result_df$ios_app_id[i] <- app_id
      } else if (grepl("^(com|net|org|io)\\.", app_id)) {
        # Android ID
        result_df$android_app_id[i] <- app_id
      }
      
      # Use the provided name if available
      if (!is.na(app_name)) {
        result_df$unified_app_name[i] <- app_name
      }
    }
  }
  
  return(result_df)
}

# Helper operator
`%||%` <- function(x, y) if (is.null(x)) y else x