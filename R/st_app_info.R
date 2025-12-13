#' Fetch Unified App or Publisher Information from Sensor Tower
#'
#' This function retrieves information about apps or publishers from the Sensor Tower API
#' based on a search term. It targets the `/v1/\{app_store\}/search_entities`
#' endpoint and fetches IDs and names for unified app or publisher entities.
#'
#' @param term Character string. The search term for the app or publisher.
#' @param app_store Character string. The app store to search.
#'   Defaults to "unified".
#' @param entity_type Character string. The type of entity to search for.
#'   Either "app" (default) or "publisher".
#' @param limit Numeric. The maximum number of results to return.
#'   Defaults to 20.
#' @param auth_token Character string. Your Sensor Tower API authentication token.
#' @param return_all_fields Boolean. If TRUE, returns all available fields
#'   from the API response. Defaults to FALSE.
#'
#' @return A [tibble][tibble::tibble] with entity information:
#'
#'   **For apps** (`entity_type = "app"`):
#'   - `unified_app_id`: The unified app ID (24-char hex)
#'   - `unified_app_name`: The app name
#'   - `category_details`: (when available) Nested tibble with category info
#'
#'   **For publishers** (`entity_type = "publisher"`):
#'   - `unified_publisher_id`: The unified publisher ID (24-char hex)
#'   - `unified_publisher_name`: The publisher name
#'
#'   Use the returned publisher ID with `st_publisher_apps()` to get the
#'   publisher's apps.
#'
#' @examples
#' \dontrun{
#' # Search for an app by name
#' app_info <- st_app_info(term = "Clash of Clans")
#' print(app_info)
#'
#' # Search for a publisher by name
#' publisher_info <- st_app_info(term = "Lilith", entity_type = "publisher")
#' print(publisher_info)
#'
#' # Get publisher's apps
#' lilith_apps <- st_publisher_apps(
#'   unified_id = publisher_info$unified_publisher_id[1],
#'   aggregate_related = TRUE
#' )
#'
#' # ---- Piping Workflow Examples ----
#' library(dplyr)
#'
#' # Pipe-friendly workflow: Find publisher -> Get apps -> Fetch sales
#' lilith_sales <- st_app_info("Lilith", entity_type = "publisher") %>%
#'   slice(1) %>%
#'   pull(unified_publisher_id) %>%
#'   st_publisher_apps(aggregate_related = TRUE) %>%
#'   pull(unified_app_id) %>%
#'   st_unified_sales_report(
#'     countries = "WW",
#'     start_date = "2024-01-01",
#'     end_date = "2024-12-31",
#'     date_granularity = "monthly"
#'   )
#' }
#'
#' @import dplyr
#' @importFrom dplyr all_of
#' @importFrom httr GET add_headers http_error status_code content
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#' @importFrom rlang abort
#' @export
st_app_info <- function(term,
                        app_store = "unified",
                        entity_type = "app",
                        limit = 20,
                        auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN"),
                        return_all_fields = FALSE) {
  # Ensure the auth_token is provided
  if (is.null(auth_token) || auth_token == "") {
    rlang::abort(
      "Authentication token is required. Set SENSORTOWER_AUTH_TOKEN environment variable."
    )
  }

  # Define the API endpoint
  base_url <- "https://api.sensortower.com/v1"
  endpoint <- file.path(base_url, app_store, "search_entities")

  # Make the GET request to the API
  response <- httr::GET(
    url = endpoint,
    query = list(
      entity_type = entity_type,
      term = term,
      limit = min(limit, 250) # Limit results to a maximum of 250
    ),
    httr::add_headers(
      Authorization = sprintf("Bearer %s", auth_token)
    )
  )

  # Handle potential HTTP errors
  if (httr::http_error(response)) {
    content <- httr::content(response, "parsed")
    rlang::abort(sprintf(
      "API request failed [%s]: %s",
      httr::status_code(response),
      if (!is.null(content$error)) content$error else "Unknown error"
    ))
  }

  # Parse the response content as text
  content_text <- httr::content(response, as = "text", encoding = "UTF-8")

  # Convert JSON to R list
  content_list <- jsonlite::fromJSON(content_text, flatten = TRUE)

  # Validate the structure of the API response
  if (!is.list(content_list)) {
    rlang::abort("Unexpected API response format: The response is not a list.")
  }

  # Bind rows if the content_list contains multiple entities
  if ("data" %in% names(content_list)) {
    apps <- content_list$data
  } else {
    apps <- bind_rows(content_list)
  }

  # Ensure the response contains expected fields
  if (!"entity_type" %in% colnames(apps)) {
    rlang::abort("Unexpected API response format: Missing 'entity_type' field.")
  }

  # Determine the target entity type based on parameter
  target_entity_type <- if (entity_type == "publisher") "unified_publisher" else "unified_app"

  # Filter for the appropriate entity type
  filtered_entities <- apps %>%
    filter(.data$entity_type == target_entity_type)

  # Check if any entities are available
  if (nrow(filtered_entities) == 0) {
    warning(paste0("No ", target_entity_type, " entities found for the given search term."))
    return(tibble::tibble())
  }

  # Handle app-specific processing
  if (entity_type != "publisher") {
    # Always create nested category_details when categories are present
    if ("categories" %in% colnames(filtered_entities) && is.list(filtered_entities$categories)) {
      category_lookup <- st_categories()
      filtered_entities$category_details <- lapply(filtered_entities$categories, function(ids) {
        if (length(ids) > 0 && !all(is.na(ids))) {
          category_lookup[category_lookup$category_id %in% as.character(ids), ]
        } else {
          tibble::tibble(platform = character(0), category_id = character(0), category_name = character(0))
        }
      })
      # Remove the raw categories column in favor of the nested approach
      filtered_entities <- filtered_entities %>% dplyr::select(-"categories")
    }
  }

  # Return all fields if requested, otherwise extract and rename
  if (return_all_fields) {
    return(filtered_entities)
  } else {
    if (entity_type == "publisher") {
      # Publisher output - API returns publisher_id and publisher_name
      # Check which columns exist
      id_col <- if ("publisher_id" %in% colnames(filtered_entities)) "publisher_id" else "app_id"
      name_col <- if ("publisher_name" %in% colnames(filtered_entities)) "publisher_name" else "name"

      base_columns <- c(id_col, name_col)
      extracted_info <- filtered_entities %>%
        dplyr::select(dplyr::any_of(base_columns))

      # Rename to consistent output columns
      if (id_col %in% names(extracted_info)) {
        names(extracted_info)[names(extracted_info) == id_col] <- "unified_publisher_id"
      }
      if (name_col %in% names(extracted_info)) {
        names(extracted_info)[names(extracted_info) == name_col] <- "unified_publisher_name"
      }
    } else {
      # App output - include category_details in the simplified output if available
      base_columns <- c("app_id", "name")
      if ("category_details" %in% colnames(filtered_entities)) {
        base_columns <- c(base_columns, "category_details")
      }

      extracted_info <- filtered_entities %>%
        dplyr::select(dplyr::all_of(base_columns)) %>%
        dplyr::rename(
          unified_app_id = app_id,
          unified_app_name = name
        )
    }
    return(tibble::as_tibble(extracted_info))
  }
}
