#' Custom Fields Utility Functions
#'
#' Utility functions for common custom fields filtering scenarios in Sensor Tower.
#' These functions provide pre-built filters for frequently used queries.
#'
#' @name st_custom_fields_utils
#' @rdname st_custom_fields_utils
NULL

#' Create Genre-Based Filter
#'
#' Creates a filter for apps in specific game genres or sub-genres.
#'
#' @param genres Character vector. Game genres to filter (e.g., "Puzzle", "Action")
#' @param sub_genres Character vector. Game sub-genres to filter (e.g., "Word", "Match-3")
#' @param exclude_genres Logical. Whether to exclude these genres (FALSE = include)
#' @param auth_token Optional. Character string. Your Sensor Tower API token.
#'
#' @return Character string containing the filter ID
#'
#' @examples
#' \dontrun{
#' # Get Puzzle and Word games
#' filter_id <- st_filter_by_genre(
#'   genres = "Puzzle",
#'   sub_genres = "Word"
#' )
#' 
#' # Exclude Action and Shooter games
#' filter_id <- st_filter_by_genre(
#'   genres = c("Action", "Shooter"),
#'   exclude_genres = TRUE
#' )
#' }
#'
#' @export
st_filter_by_genre <- function(
    genres = NULL,
    sub_genres = NULL,
    exclude_genres = FALSE,
    auth_token = NULL
) {
  
  if (is.null(genres) && is.null(sub_genres)) {
    rlang::abort("At least one of 'genres' or 'sub_genres' must be provided")
  }
  
  custom_fields <- list()
  
  # Add genre filter
  if (!is.null(genres)) {
    custom_fields <- append(custom_fields, list(list(
      name = "Game Genre",
      values = as.list(genres),
      global = TRUE,
      exclude = exclude_genres
    )))
  }
  
  # Add sub-genre filter
  if (!is.null(sub_genres)) {
    custom_fields <- append(custom_fields, list(list(
      name = "Game Sub-genre",
      values = as.list(sub_genres),
      global = TRUE,
      exclude = exclude_genres
    )))
  }
  
  st_custom_fields_filter(
    custom_fields = custom_fields,
    auth_token = auth_token
  )
}

#' Create Monetization-Based Filter
#'
#' Creates a filter for apps based on their monetization model.
#'
#' @param free_only Logical. Only free apps
#' @param has_iap Logical. Has in-app purchases
#' @param has_ads Logical. Contains ads
#' @param has_subscription Logical. Has subscription model
#' @param auth_token Optional. Character string. Your Sensor Tower API token.
#'
#' @return Character string containing the filter ID
#'
#' @examples
#' \dontrun{
#' # Get free apps with IAP but no ads
#' filter_id <- st_filter_by_monetization(
#'   free_only = TRUE,
#'   has_iap = TRUE,
#'   has_ads = FALSE
#' )
#' }
#'
#' @export
st_filter_by_monetization <- function(
    free_only = NULL,
    has_iap = NULL,
    has_ads = NULL,
    has_subscription = NULL,
    auth_token = NULL
) {
  
  custom_fields <- list()
  
  if (!is.null(free_only)) {
    custom_fields <- append(custom_fields, list(list(
      name = "Free",
      global = TRUE,
      true = free_only
    )))
  }
  
  if (!is.null(has_iap)) {
    custom_fields <- append(custom_fields, list(list(
      name = "In-App Purchases",
      global = TRUE,
      true = has_iap
    )))
  }
  
  if (!is.null(has_ads)) {
    custom_fields <- append(custom_fields, list(list(
      name = "Contains Ads",
      global = TRUE,
      true = has_ads
    )))
  }
  
  if (!is.null(has_subscription)) {
    custom_fields <- append(custom_fields, list(list(
      name = "In-App Subscription",
      global = TRUE,
      true = has_subscription
    )))
  }
  
  if (length(custom_fields) == 0) {
    rlang::abort("At least one monetization parameter must be specified")
  }
  
  st_custom_fields_filter(
    custom_fields = custom_fields,
    auth_token = auth_token
  )
}

#' Create Publisher-Based Filter
#'
#' Creates a filter for apps from specific publishers.
#'
#' @param publisher_names Character vector. Publisher names to filter
#' @param exclude Logical. Whether to exclude these publishers (FALSE = include)
#' @param auth_token Optional. Character string. Your Sensor Tower API token.
#'
#' @return Character string containing the filter ID
#'
#' @examples
#' \dontrun{
#' # Get apps from specific publishers
#' filter_id <- st_filter_by_publisher(
#'   publisher_names = c("Electronic Arts", "Activision")
#' )
#' }
#'
#' @export
st_filter_by_publisher <- function(
    publisher_names,
    exclude = FALSE,
    auth_token = NULL
) {
  
  if (missing(publisher_names) || is.null(publisher_names)) {
    rlang::abort("publisher_names is required")
  }
  
  st_custom_fields_filter(
    custom_fields = list(
      list(
        name = "Publisher",
        values = as.list(publisher_names),
        global = TRUE,
        exclude = exclude
      )
    ),
    auth_token = auth_token
  )
}

#' Create SDK-Based Filter
#'
#' Creates a filter for apps using specific SDKs or technologies.
#'
#' @param sdk_names Character vector. SDK names to filter (e.g., "Unity", "Firebase")
#' @param exclude Logical. Whether to exclude apps with these SDKs
#' @param auth_token Optional. Character string. Your Sensor Tower API token.
#'
#' @return Character string containing the filter ID
#'
#' @examples
#' \dontrun{
#' # Get Unity-based games
#' filter_id <- st_filter_by_sdk(sdk_names = "Unity")
#' 
#' # Get apps using both Firebase and AdMob
#' filter_id <- st_filter_by_sdk(
#'   sdk_names = c("Firebase", "AdMob")
#' )
#' }
#'
#' @export
st_filter_by_sdk <- function(
    sdk_names,
    exclude = FALSE,
    auth_token = NULL
) {
  
  if (missing(sdk_names) || is.null(sdk_names)) {
    rlang::abort("sdk_names is required")
  }
  
  # Create filters for each SDK
  custom_fields <- lapply(sdk_names, function(sdk) {
    list(
      name = paste0("SDK: ", sdk),
      values = list(),
      global = TRUE,
      exclude = exclude,
      true = !exclude
    )
  })
  
  st_custom_fields_filter(
    custom_fields = custom_fields,
    auth_token = auth_token
  )
}

#' Create Date-Based Filter
#'
#' Creates a filter for apps based on release date criteria.
#'
#' @param released_after Date or character. Apps released after this date
#' @param released_before Date or character. Apps released before this date
#' @param region Character. Region for release date ("US", "WW", "JP", "CN")
#' @param auth_token Optional. Character string. Your Sensor Tower API token.
#'
#' @return Character string containing the filter ID
#'
#' @examples
#' \dontrun{
#' # Get apps released in 2024
#' filter_id <- st_filter_by_date(
#'   released_after = "2024-01-01",
#'   released_before = "2024-12-31",
#'   region = "US"
#' )
#' }
#'
#' @export
st_filter_by_date <- function(
    released_after = NULL,
    released_before = NULL,
    region = "US",
    auth_token = NULL
) {
  
  if (is.null(released_after) && is.null(released_before)) {
    rlang::abort("At least one of 'released_after' or 'released_before' must be provided")
  }
  
  # Format dates
  if (!is.null(released_after)) {
    released_after <- format(as.Date(released_after), "%Y-%m-%d")
  }
  if (!is.null(released_before)) {
    released_before <- format(as.Date(released_before), "%Y-%m-%d")
  }
  
  # Determine field name based on region
  field_name <- switch(region,
    "US" = "Release Date (US)",
    "WW" = "Release Date (WW)",
    "JP" = "Release Date (JP)",
    "CN" = "Release Date (CN)",
    "Release Date (US)"  # Default
  )
  
  # Create date range values
  values <- list()
  if (!is.null(released_after) && !is.null(released_before)) {
    values <- list(paste(released_after, released_before, sep = " to "))
  } else if (!is.null(released_after)) {
    values <- list(paste("after", released_after))
  } else {
    values <- list(paste("before", released_before))
  }
  
  st_custom_fields_filter(
    custom_fields = list(
      list(
        name = field_name,
        values = values,
        global = TRUE,
        exclude = FALSE
      )
    ),
    auth_token = auth_token
  )
}

#' Get Pre-Built Filter Collections
#'
#' Returns commonly used filter IDs for quick access to pre-defined app segments.
#'
#' @param collection Character. Name of the collection:
#'   - "top_genres": Major game genres
#'   - "monetization_models": Different monetization approaches
#'   - "platform_exclusive": Platform-specific apps
#'   - "market_segments": Market segment filters
#' @param auth_token Optional. Character string. Your Sensor Tower API token.
#'
#' @return A named list of filter IDs
#'
#' @examples
#' \dontrun{
#' # Get filter IDs for top game genres
#' genre_filters <- st_get_filter_collection("top_genres")
#' 
#' # Use a filter from the collection
#' puzzle_apps <- st_get_filtered_apps(
#'   filter_id = genre_filters$puzzle,
#'   measure = "DAU",
#'   regions = "US"
#' )
#' }
#'
#' @export
st_get_filter_collection <- function(
    collection = c("top_genres", "monetization_models", "platform_exclusive", "market_segments"),
    auth_token = NULL
) {
  
  collection <- match.arg(collection)
  
  filters <- switch(collection,
    "top_genres" = list(
      puzzle = st_filter_by_genre(genres = "Puzzle", auth_token = auth_token),
      action = st_filter_by_genre(genres = "Action", auth_token = auth_token),
      strategy = st_filter_by_genre(genres = "Strategy", auth_token = auth_token),
      casual = st_filter_by_genre(genres = "Casual", auth_token = auth_token),
      simulation = st_filter_by_genre(genres = "Simulation", auth_token = auth_token),
      rpg = st_filter_by_genre(genres = "Role Playing", auth_token = auth_token),
      word = st_filter_by_genre(sub_genres = "Word", auth_token = auth_token),
      match3 = st_filter_by_genre(sub_genres = "Match-3", auth_token = auth_token)
    ),
    
    "monetization_models" = list(
      free_with_ads = st_filter_by_monetization(
        free_only = TRUE, has_ads = TRUE, auth_token = auth_token
      ),
      free_with_iap = st_filter_by_monetization(
        free_only = TRUE, has_iap = TRUE, has_ads = FALSE, auth_token = auth_token
      ),
      premium = st_filter_by_monetization(
        free_only = FALSE, has_iap = FALSE, auth_token = auth_token
      ),
      subscription = st_filter_by_monetization(
        has_subscription = TRUE, auth_token = auth_token
      ),
      hybrid = st_filter_by_monetization(
        has_iap = TRUE, has_ads = TRUE, auth_token = auth_token
      )
    ),
    
    "platform_exclusive" = list(
      ios_only = st_create_simple_filter(
        field_name = "Platform",
        field_values = "iOS",
        auth_token = auth_token
      ),
      android_only = st_create_simple_filter(
        field_name = "Platform", 
        field_values = "Android",
        auth_token = auth_token
      ),
      cross_platform = st_create_simple_filter(
        field_name = "Is Unified",
        field_values = list(),
        auth_token = auth_token
      )
    ),
    
    "market_segments" = list(
      editors_choice = st_create_simple_filter(
        field_name = "Editors' Choice",
        field_values = list(),
        auth_token = auth_token
      ),
      new_releases = st_filter_by_date(
        released_after = Sys.Date() - 30,
        region = "US",
        auth_token = auth_token
      ),
      established = st_filter_by_date(
        released_before = Sys.Date() - 365,
        region = "US",
        auth_token = auth_token
      )
    )
  )
  
  return(filters)
}

#' Combine Multiple Filters
#'
#' Creates a compound filter by combining multiple filter criteria.
#'
#' @param filter_ids Character vector. Existing filter IDs to combine
#' @param operator Character. How to combine filters ("AND" or "OR")
#' @param auth_token Optional. Character string. Your Sensor Tower API token.
#'
#' @return Character string containing the combined filter ID
#'
#' @note OR operations may not be supported by all Sensor Tower endpoints
#'
#' @examples
#' \dontrun{
#' # Get Word Puzzle games (AND operation)
#' word_filter <- st_filter_by_genre(sub_genres = "Word")
#' puzzle_filter <- st_filter_by_genre(genres = "Puzzle")
#' combined <- st_combine_filters(
#'   filter_ids = c(word_filter, puzzle_filter),
#'   operator = "AND"
#' )
#' }
#'
#' @export
st_combine_filters <- function(
    filter_ids,
    operator = c("AND", "OR"),
    auth_token = NULL
) {
  
  operator <- match.arg(operator)
  
  if (length(filter_ids) < 2) {
    rlang::abort("At least two filter_ids must be provided to combine")
  }
  
  # Get details of each filter
  all_fields <- list()
  
  for (filter_id in filter_ids) {
    details <- st_custom_fields_filter_by_id(
      id = filter_id,
      auth_token = auth_token
    )
    
    if (!is.null(details$custom_fields)) {
      # Convert data frame to list format
      for (i in seq_len(nrow(details$custom_fields))) {
        field <- list(
          name = details$custom_fields$name[i],
          values = details$custom_fields$values[[i]],
          global = details$custom_fields$global[i],
          exclude = details$custom_fields$exclude[i]
        )
        
        if (operator == "AND") {
          # For AND, simply add all fields
          all_fields <- append(all_fields, list(field))
        } else {
          # OR operation might need special handling
          # This is a simplified approach
          all_fields <- append(all_fields, list(field))
        }
      }
    }
  }
  
  if (length(all_fields) == 0) {
    rlang::abort("No valid fields found in provided filter IDs")
  }
  
  # Create combined filter
  st_custom_fields_filter(
    custom_fields = all_fields,
    auth_token = auth_token
  )
}