#' Get Unified ID Mapping for Apps
#'
#' Retrieves the mapping between platform-specific app IDs and unified app IDs
#' using Sensor Tower's comparison attributes endpoint. This function helps resolve
#' the relationship between iOS/Android app IDs and their unified counterparts.
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
#' @examples
#' \dontrun{
#' # Get mapping for specific apps
#' mapping <- st_get_unified_mapping(c("1427744264", "com.scopely.startrek"))
#' 
#' # Use with st_ytd_metrics
#' map_data <- st_get_unified_mapping("5ba4585f539ce75b97db6bcb")
#' if (!is.null(map_data)) {
#'   metrics <- st_ytd_metrics(
#'     ios_app_id = map_data$ios_app_id,
#'     android_app_id = map_data$android_app_id,
#'     years = 2025
#'   )
#' }
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
  
  # Detect OS if not specified
  if (os == "unified") {
    # Try to detect based on first ID
    first_id <- app_ids[1]
    if (grepl("^\\d+$", first_id)) {
      os <- "ios"
    } else if (grepl("^(com|net|org|io)\\.", first_id)) {
      os <- "android"
    }
  }
  
  # Use comparison attributes endpoint to get entity structure
  # This endpoint returns richer data including unified mapping
  tryCatch({
    # Build request - we'll use a minimal category query
    base_url <- "https://api.sensortower.com"
    path <- c("v1", os, "sales_report_estimates_comparison_attributes")
    
    # For iOS, use games category; for Android use "game"
    category <- if (os == "ios") "6014" else "game"
    
    query_params <- list(
      auth_token = auth_token_val,
      comparison_attribute = "absolute",
      time_range = "day",
      date = format(Sys.Date() - 1, "%Y-%m-%d"),
      category = category,
      countries = "US",
      limit = 100,  # Get enough to likely include our apps
      offset = 0
    )
    
    # Make request
    req <- httr2::request(base_url) %>%
      httr2::req_url_path_append(path) %>%
      httr2::req_url_query(!!!query_params) %>%
      httr2::req_headers(Accept = "application/json") %>%
      httr2::req_timeout(30)
    
    resp <- httr2::req_perform(req)
    
    # Parse response
    body_text <- httr2::resp_body_string(resp)
    data <- jsonlite::fromJSON(body_text, flatten = FALSE)
    
    # Extract mapping from entities structure
    if (!is.null(data$data$entities)) {
      entities <- data$data$entities
      
      # Build mapping table
      mapping_list <- lapply(entities, function(entity) {
        # Get platform apps
        ios_id <- NA_character_
        android_id <- NA_character_
        
        if (!is.null(entity$apps)) {
          for (app in entity$apps) {
            if (!is.null(app$os) && !is.null(app$app_id)) {
              if (app$os == "ios") {
                ios_id <- as.character(app$app_id)
              } else if (app$os == "android") {
                android_id <- as.character(app$app_id)
              }
            }
          }
        }
        
        data.frame(
          unified_app_id = entity$unified_app_id %||% NA_character_,
          unified_app_name = entity$unified_app_name %||% NA_character_,
          ios_app_id = ios_id,
          android_app_id = android_id,
          publisher_id = entity$publisher_id %||% NA_character_,
          publisher_name = entity$publisher_name %||% NA_character_,
          stringsAsFactors = FALSE
        )
      })
      
      mapping_df <- do.call(rbind, mapping_list)
      
      # Filter to requested apps
      result <- data.frame(
        input_id = app_ids,
        stringsAsFactors = FALSE
      )
      
      # Match based on input type
      for (i in seq_along(app_ids)) {
        id <- app_ids[i]
        
        # Try different matching strategies
        match_row <- NULL
        
        if (grepl("^[a-f0-9]{24}$", id)) {
          # Hex ID - match unified
          match_row <- mapping_df[mapping_df$unified_app_id == id, , drop = FALSE]
        } else if (grepl("^\\d+$", id)) {
          # iOS ID
          match_row <- mapping_df[mapping_df$ios_app_id == id, , drop = FALSE]
        } else if (grepl("^(com|net|org|io)\\.", id)) {
          # Android ID
          match_row <- mapping_df[mapping_df$android_app_id == id, , drop = FALSE]
        }
        
        if (!is.null(match_row) && nrow(match_row) > 0) {
          result[i, names(match_row)] <- match_row[1, ]
        }
      }
      
      # If we didn't find matches in the category listing, try app_lookup
      missing_idx <- which(is.na(result$unified_app_id))
      if (length(missing_idx) > 0) {
        for (idx in missing_idx) {
          lookup_result <- tryCatch({
            st_app_lookup(result$input_id[idx], auth_token = auth_token_val, verbose = FALSE)
          }, error = function(e) NULL)
          
          if (!is.null(lookup_result)) {
            result$unified_app_id[idx] <- lookup_result$unified_app_id
            result$unified_app_name[idx] <- lookup_result$app_name
            result$ios_app_id[idx] <- lookup_result$ios_app_id
            result$android_app_id[idx] <- lookup_result$android_app_id
            result$publisher_name[idx] <- lookup_result$publisher_name
          }
        }
      }
      
      return(result)
      
    } else {
      # Fallback to using st_app_lookup for each ID
      results <- lapply(app_ids, function(id) {
        lookup <- tryCatch({
          st_app_lookup(id, auth_token = auth_token_val, verbose = FALSE)
        }, error = function(e) NULL)
        
        if (!is.null(lookup)) {
          data.frame(
            input_id = id,
            unified_app_id = lookup$unified_app_id %||% NA_character_,
            unified_app_name = lookup$app_name %||% NA_character_,
            ios_app_id = lookup$ios_app_id %||% NA_character_,
            android_app_id = lookup$android_app_id %||% NA_character_,
            publisher_id = NA_character_,
            publisher_name = lookup$publisher_name %||% NA_character_,
            stringsAsFactors = FALSE
          )
        } else {
          data.frame(
            input_id = id,
            unified_app_id = NA_character_,
            unified_app_name = NA_character_,
            ios_app_id = NA_character_,
            android_app_id = NA_character_,
            publisher_id = NA_character_,
            publisher_name = NA_character_,
            stringsAsFactors = FALSE
          )
        }
      })
      
      return(do.call(rbind, results))
    }
    
  }, error = function(e) {
    warning("Failed to get unified mapping: ", e$message)
    
    # Return empty mapping structure
    data.frame(
      input_id = app_ids,
      unified_app_id = NA_character_,
      unified_app_name = NA_character_,
      ios_app_id = NA_character_,
      android_app_id = NA_character_,
      publisher_id = NA_character_,
      publisher_name = NA_character_,
      stringsAsFactors = FALSE
    )
  })
}