#' Custom Fields Filter Workflow Helper Functions
#'
#' Helper functions to streamline working with custom fields filters in Sensor Tower.
#' These functions combine the custom fields endpoints with data retrieval functions
#' to provide a complete workflow.
#'
#' @name st_custom_fields_workflow
#' @rdname st_custom_fields_workflow
NULL

#' Create or Get Filter ID for Custom Criteria
#'
#' Creates a custom fields filter or retrieves existing filter ID if the same
#' criteria already exists. This is a convenience wrapper around st_custom_fields_filter.
#'
#' @param field_name Character. Name of the custom field to filter by
#' @param field_values Character vector. Values to filter for
#' @param global Logical. Whether this is a global field (TRUE) or organization field (FALSE)
#' @param exclude Logical. Whether to exclude apps matching criteria (FALSE = include)
#' @param auth_token Optional. Character string. Your Sensor Tower API token.
#'
#' @return Character string containing the filter ID
#'
#' @examples
#' \dontrun{
#' # Get filter ID for Word games
#' filter_id <- st_create_simple_filter(
#'   field_name = "Game Sub-genre",
#'   field_values = "Word"
#' )
#' 
#' # Get filter ID for multiple genres
#' filter_id <- st_create_simple_filter(
#'   field_name = "Game Genre",
#'   field_values = c("Puzzle", "Word")
#' )
#' }
#'
#' @export
st_create_simple_filter <- function(
    field_name,
    field_values,
    global = TRUE,
    exclude = FALSE,
    auth_token = NULL
) {
  
  # Input validation
  if (missing(field_name) || is.null(field_name) || field_name == "") {
    rlang::abort("field_name is required and cannot be empty")
  }
  
  # Convert single value to list for consistency
  if (!is.list(field_values)) {
    if (is.null(field_values)) {
      field_values <- list()
    } else {
      field_values <- as.list(field_values)
    }
  }
  
  # If possible, validate values for the given field and drop invalid entries
  valid_values <- NULL
  try({
    fields_df <- st_custom_fields_values(auth_token = auth_token)
    if (nrow(fields_df) > 0 && "name" %in% names(fields_df) && "values" %in% names(fields_df)) {
      row <- fields_df[fields_df$name == field_name, , drop = FALSE]
      if (nrow(row) == 1 && length(row$values[[1]]) > 0) {
        valid_values <- as.character(unlist(row$values[[1]]))
      }
    }
  }, silent = TRUE)
  if (!is.null(valid_values)) {
    # Keep only values present for this field
    field_values <- Filter(function(v) v %in% valid_values, field_values)
  }
  
  # Create filter criteria
  custom_fields <- list(
    list(
      exclude = exclude,
      global = global,
      name = field_name,
      values = field_values
    )
  )
  
  # Create or get filter ID
  filter_id <- st_custom_fields_filter(
    custom_fields = custom_fields,
    auth_token = auth_token
  )
  
  return(filter_id)
}

#' Discover Available Custom Fields
#'
#' Searches and displays available custom fields that can be used for filtering.
#' This is helpful for discovering what fields are available before creating filters.
#'
#' @param search_term Optional. Character string to search for in field names
#' @param show_values Logical. Whether to show possible values for each field
#' @param auth_token Optional. Character string. Your Sensor Tower API token.
#'
#' @return A tibble with custom fields information
#'
#' @examples
#' \dontrun{
#' # Find all game-related fields
#' game_fields <- st_discover_fields("game")
#' 
#' # Find all date fields
#' date_fields <- st_discover_fields("date")
#' 
#' # Show all fields with their values
#' all_fields <- st_discover_fields(show_values = TRUE)
#' }
#'
#' @export
#' @importFrom dplyr filter mutate select
#' @importFrom stringr str_detect str_to_lower
st_discover_fields <- function(
    search_term = NULL,
    show_values = FALSE,
    auth_token = NULL
) {
  
  # Get all custom fields
  fields <- st_custom_fields_values(term = search_term, auth_token = auth_token)
  
  if (nrow(fields) == 0) {
    message("No custom fields found")
    return(fields)
  }
  
  # Filter by search term if provided
  if (!is.null(search_term)) {
    fields <- fields %>%
      dplyr::filter(stringr::str_detect(
        stringr::str_to_lower(name), 
        stringr::str_to_lower(search_term)
      ))
  }
  
  # Format output
  if (!show_values && "values" %in% names(fields)) {
    # Show count of possible values instead of the values themselves
    fields <- fields %>%
      dplyr::mutate(
        value_count = sapply(values, function(x) {
          if (is.null(x)) return(0)
          if (is.list(x)) return(length(x))
          return(1)
        })
      ) %>%
      dplyr::select(-values)
  }
  
  # Sort by name for easier reading
  fields <- fields %>%
    dplyr::arrange(name)
  
  message(sprintf("Found %d custom fields%s", 
                  nrow(fields),
                  ifelse(!is.null(search_term), 
                         sprintf(" matching '%s'", search_term), 
                         "")))
  
  return(fields)
}

#' Get Top Apps with Custom Filter
#'
#' Retrieves top apps using a custom fields filter. This combines filter creation
#' with data retrieval in a single workflow.
#'
#' @param field_name Character. Name of the custom field to filter by (or NULL to use filter_id)
#' @param field_values Character vector. Values to filter for (or NULL to use filter_id)
#' @param filter_id Character. Existing filter ID to use (alternative to field_name/values)
#' @param measure Character. Metric to measure: "DAU", "WAU", "MAU", "revenue", or "units"
#' @param regions Character vector. Region codes (e.g., "US", "WW")
#' @param date Character or Date. Start date for the query
#' @param end_date Optional. Character or Date. End date for the query
#' @param limit Integer. Maximum number of apps to return (default 100)
#' @param enrich_response Logical. Whether to enrich with additional metrics
#' @param auth_token Optional. Character string. Your Sensor Tower API token.
#' @param ... Additional parameters passed to st_top_charts
#'
#' @return A tibble with top apps data
#'
#' @examples
#' \dontrun{
#' # Get top Word games by DAU
#' word_games <- st_get_filtered_apps(
#'   field_name = "Game Sub-genre",
#'   field_values = "Word",
#'   measure = "DAU",
#'   regions = "US",
#'   limit = 20
#' )
#' 
#' # Use existing filter ID
#' apps <- st_get_filtered_apps(
#'   filter_id = "603697f4241bc16eb8570d37",
#'   measure = "revenue",
#'   regions = "US"
#' )
#' }
#'
#' @export
st_get_filtered_apps <- function(
    field_name = NULL,
    field_values = NULL,
    filter_id = NULL,
    measure = "DAU",
    regions = "US",
    date = NULL,
    end_date = NULL,
    limit = 100,
    enrich_response = TRUE,
    auth_token = NULL,
    ...
) {
  
  # Determine filter ID
  if (is.null(filter_id)) {
    if (is.null(field_name) || is.null(field_values)) {
      stop("Either provide filter_id or both field_name and field_values")
    }
    
    # Create filter
    filter_id <- st_create_simple_filter(
      field_name = field_name,
      field_values = field_values,
      auth_token = auth_token
    )
    
    message(sprintf("Using filter ID: %s", filter_id))
  }
  
  # Get filter details for logging
  filter_details <- tryCatch({
    st_custom_fields_filter_by_id(id = filter_id, auth_token = auth_token)
  }, error = function(e) NULL)
  
  if (!is.null(filter_details) && !is.null(filter_details$custom_fields)) {
    fields_desc <- paste(
      sprintf("%s=%s", 
              filter_details$custom_fields$name,
              sapply(filter_details$custom_fields$values, 
                     function(x) paste(x, collapse = ","))),
      collapse = "; "
    )
    message(sprintf("Filter criteria: %s", fields_desc))
  }
  
  # Retrieve data using st_top_charts
  result <- st_top_charts(
    os = "unified",
    category = 0,  # Use 0 when using custom filter
    custom_fields_filter_id = filter_id,
    custom_tags_mode = "include_unified_apps",
    measure = measure,
    regions = regions,
    date = date,
    end_date = end_date,
    limit = limit,
    device_type = "total",
    enrich_response = enrich_response,
    auth_token = auth_token,
    ...
  )
  
  return(result)
}

#' Analyze Custom Filter Performance
#'
#' Provides a summary analysis of apps matching a custom filter, including
#' top performers, growth metrics, and category breakdown.
#'
#' @param filter_id Character. The custom fields filter ID to analyze
#' @param measure Character. Metric to analyze: "DAU", "revenue", or "units"
#' @param regions Character vector. Region codes (default "US")
#' @param top_n Integer. Number of top apps to show (default 10)
#' @param auth_token Optional. Character string. Your Sensor Tower API token.
#'
#' @return A list containing summary statistics and top apps
#'
#' @export
st_analyze_filter <- function(
    filter_id,
    measure = "DAU",
    regions = "US",
    top_n = 10,
    auth_token = NULL
) {
  
  # Get filter details
  filter_details <- st_custom_fields_filter_by_id(
    id = filter_id, 
    auth_token = auth_token
  )
  
  # Get current period data
  current_data <- st_top_charts(
    os = "unified",
    category = 0,
    custom_fields_filter_id = filter_id,
    custom_tags_mode = "include_unified_apps",
    measure = measure,
    regions = regions,
    limit = 100,
    device_type = "total",
    enrich_response = TRUE,
    auth_token = auth_token
  )
  
  if (nrow(current_data) == 0) {
    message("No apps found for this filter")
    return(NULL)
  }
  
  # Prepare summary
  summary <- list(
    filter_id = filter_id,
    filter_criteria = filter_details$custom_fields,
    total_apps = nrow(current_data),
    measure = measure,
    regions = regions,
    top_apps = head(current_data, top_n)
  )
  
  # Add measure-specific summaries
  if (measure == "DAU" && "aggregate_tags.Last 30 Days Average DAU (US)" %in% names(current_data)) {
    summary$total_dau <- sum(current_data$`aggregate_tags.Last 30 Days Average DAU (US)`, na.rm = TRUE)
    summary$avg_dau_per_app <- mean(current_data$`aggregate_tags.Last 30 Days Average DAU (US)`, na.rm = TRUE)
  }
  
  message(sprintf("\nAnalysis Summary for Filter: %s", filter_id))
  message(sprintf("Total apps found: %d", summary$total_apps))
  message(sprintf("Measure: %s | Region: %s", measure, regions))
  
  if (measure == "DAU" && !is.null(summary$total_dau)) {
    message(sprintf("Total DAU: %s", format(summary$total_dau, big.mark = ",")))
    message(sprintf("Average DAU per app: %s", format(round(summary$avg_dau_per_app), big.mark = ",")))
  }
  
  return(summary)
}
