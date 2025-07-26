#' Fetch Detailed App Metadata
#'
#' Retrieves comprehensive metadata for one or more apps including descriptions,
#' screenshots, ratings, publisher information, and more. This function provides
#' rich app store listing data for apps when you already know their IDs.
#'
#' @param app_ids Character vector. App IDs to fetch details for. 
#'   - For iOS: numeric app IDs (e.g., "553834731")
#'   - For Android: bundle IDs (e.g., "com.king.candycrushsaga")
#'   - For unified: unified app IDs
#'   Maximum 100 apps per request.
#' @param os Character string. Operating system: "ios", "android", or "unified".
#'   Defaults to "ios".
#' @param include_developer_contacts Logical. Include developer contact information
#'   (email, address). Defaults to TRUE.
#' @param auth_token Character string. Sensor Tower API authentication token.
#'   Defaults to environment variable SENSORTOWER_AUTH_TOKEN.
#'
#' @return A [tibble][tibble::tibble] containing detailed app metadata with columns:
#'   - `app_id`: The app's store ID
#'   - `app_name`: The app's display name
#'   - `publisher_name`: Publisher/developer name
#'   - `publisher_id`: Publisher ID
#'   - `categories`: App store categories
#'   - `description`: Full app description
#'   - `subtitle`: App subtitle (iOS) or short description (Android)
#'   - `rating`: Current average rating
#'   - `rating_count`: Total number of ratings
#'   - `rating_current_version`: Rating for current version
#'   - `rating_count_current_version`: Rating count for current version
#'   - `content_rating`: Age rating/content rating
#'   - `price`: App price
#'   - `currency`: Price currency
#'   - `release_date`: Initial release date
#'   - `last_update`: Last update date
#'   - `version`: Current version
#'   - `size_bytes`: App size in bytes
#'   - `screenshots`: List of screenshot URLs
#'   - `icon_url`: App icon URL
#'   - `publisher_email`: Developer email (if include_developer_contacts = TRUE)
#'   - `publisher_address`: Developer address (if include_developer_contacts = TRUE)
#'   - `publisher_country`: Developer country
#'   - Additional platform-specific fields
#'
#' @section API Endpoint Used:
#'   - `GET /v1/{os}/apps`
#'
#' @examples
#' \dontrun{
#' # Get details for a single iOS app
#' candy_crush <- st_app_details(
#'   app_ids = "553834731",
#'   os = "ios"
#' )
#'
#' # Get details for multiple Android apps
#' android_games <- st_app_details(
#'   app_ids = c("com.king.candycrushsaga", "com.supercell.clashofclans"),
#'   os = "android"
#' )
#'
#' # Get details without developer contacts
#' apps <- st_app_details(
#'   app_ids = c("553834731", "1053012308"),
#'   include_developer_contacts = FALSE
#' )
#' }
#'
#' @importFrom rlang %||% abort
#' @importFrom httr2 resp_body_raw
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble as_tibble as_tibble_row
#' @importFrom dplyr rename bind_rows
#' @export
st_app_details <- function(app_ids,
                          os = "ios",
                          include_developer_contacts = TRUE,
                          auth_token = NULL) {
  
  # Input validation
  if (missing(app_ids) || length(app_ids) == 0) {
    rlang::abort("At least one app_id is required.")
  }
  
  if (length(app_ids) > 100) {
    rlang::abort("Maximum 100 app IDs allowed per request.")
  }
  
  os <- match.arg(os, c("ios", "android", "unified"))
  
  # Authentication
  auth_token_val <- auth_token %||% Sys.getenv("SENSORTOWER_AUTH_TOKEN")
  if (auth_token_val == "") {
    rlang::abort(
      c("Authentication token not found.",
        "Set SENSORTOWER_AUTH_TOKEN environment variable or pass via auth_token argument.")
    )
  }
  
  # Build query parameters
  query_params <- list(
    auth_token = auth_token_val,
    app_ids = paste(app_ids, collapse = ",")
  )
  
  if (include_developer_contacts) {
    query_params$attributes = "contact_info"
  }
  
  # Build and perform request
  path <- c("v1", os, "apps")
  req <- build_request("https://api.sensortower.com", path, query_params)
  resp <- perform_request(req)
  
  # Process response
  result <- process_app_details_response(resp, os)
  
  return(result)
}

#' Process App Details API Response
#'
#' Internal function to process and enrich app details API responses.
#'
#' @param resp Response object from httr2
#' @param os Operating system
#'
#' @return A processed tibble with app details
#' @keywords internal
process_app_details_response <- function(resp, os) {
  
  # Get raw response
  body_raw <- httr2::resp_body_raw(resp)
  if (length(body_raw) == 0) {
    return(tibble::tibble())
  }
  
  body_text <- rawToChar(body_raw)
  result <- jsonlite::fromJSON(body_text, flatten = TRUE)
  
  if (length(result) == 0) {
    return(tibble::tibble())
  }
  
  # The API returns a list with an "apps" key containing a data frame
  if (is.list(result) && "apps" %in% names(result)) {
    result_tbl <- tibble::as_tibble(result$apps)
  } else if (is.data.frame(result)) {
    result_tbl <- tibble::as_tibble(result)
  } else {
    # Fallback - shouldn't reach here normally
    return(tibble::tibble())
  }
  
  if (nrow(result_tbl) == 0) {
    return(result_tbl)
  }
  
  # Platform-specific field mappings
  if (os == "ios") {
    # iOS field mappings
    if ("name" %in% names(result_tbl) && !"app_name" %in% names(result_tbl)) {
      result_tbl <- dplyr::rename(result_tbl, app_name = name)
    }
    
    # Handle categories - iOS uses primary_category and category_ids
    if ("primary_category" %in% names(result_tbl) || "category_ids" %in% names(result_tbl)) {
      # Categories might be a complex structure, keep as is for now
    }
    
    # Handle version-specific ratings
    if ("user_rating" %in% names(result_tbl)) {
      result_tbl <- dplyr::rename(result_tbl, rating = user_rating)
    }
    if ("user_rating_count" %in% names(result_tbl)) {
      result_tbl <- dplyr::rename(result_tbl, rating_count = user_rating_count)
    }
    
  } else if (os == "android") {
    # Android field mappings
    if ("title" %in% names(result_tbl) && !"app_name" %in% names(result_tbl)) {
      result_tbl <- dplyr::rename(result_tbl, app_name = title)
    }
    # Also check for "name" field
    if ("name" %in% names(result_tbl) && !"app_name" %in% names(result_tbl)) {
      result_tbl <- dplyr::rename(result_tbl, app_name = name)
    }
    
    # Android uses score instead of rating
    if ("score" %in% names(result_tbl) && !"rating" %in% names(result_tbl)) {
      result_tbl <- dplyr::rename(result_tbl, rating = score)
    }
    
    # Handle developer info
    if ("developer" %in% names(result_tbl) && !"publisher_name" %in% names(result_tbl)) {
      result_tbl <- dplyr::rename(result_tbl, publisher_name = developer)
    }
    if ("developer_id" %in% names(result_tbl) && !"publisher_id" %in% names(result_tbl)) {
      result_tbl <- dplyr::rename(result_tbl, publisher_id = developer_id)
    }
    if ("developer_email" %in% names(result_tbl) && !"publisher_email" %in% names(result_tbl)) {
      result_tbl <- dplyr::rename(result_tbl, publisher_email = developer_email)
    }
    if ("developer_address" %in% names(result_tbl) && !"publisher_address" %in% names(result_tbl)) {
      result_tbl <- dplyr::rename(result_tbl, publisher_address = developer_address)
    }
  }
  
  # Ensure app_id is character type for consistent joining
  if ("app_id" %in% names(result_tbl)) {
    result_tbl$app_id <- as.character(result_tbl$app_id)
  }
  
  # Common field processing
  # Convert dates to Date objects
  date_fields <- c("release_date", "last_update", "current_version_release_date")
  for (field in date_fields) {
    if (field %in% names(result_tbl)) {
      result_tbl[[field]] <- as.Date(result_tbl[[field]])
    }
  }
  
  # Handle numeric fields
  numeric_fields <- c("price", "size_bytes", "rating", "rating_count")
  for (field in numeric_fields) {
    if (field %in% names(result_tbl)) {
      result_tbl[[field]] <- as.numeric(result_tbl[[field]])
    }
  }
  
  # Define preferred column order
  preferred_cols <- c(
    "app_id", "app_name", "publisher_name", "publisher_id",
    "categories", "description", "subtitle", "short_description",
    "rating", "rating_count", "rating_current_version", "rating_count_current_version",
    "content_rating", "price", "currency", "release_date", "last_update",
    "version", "size_bytes", "screenshots", "icon_url",
    "publisher_email", "publisher_address", "publisher_country"
  )
  
  # Reorder columns, keeping any additional fields at the end
  existing_cols <- intersect(preferred_cols, names(result_tbl))
  other_cols <- setdiff(names(result_tbl), preferred_cols)
  
  result_tbl <- result_tbl[, c(existing_cols, other_cols)]
  
  # Add OS information
  result_tbl$os <- os
  
  return(result_tbl)
}

