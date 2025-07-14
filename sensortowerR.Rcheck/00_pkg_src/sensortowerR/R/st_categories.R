#' List Available Sensor Tower Categories
#'
#' Returns a tibble of app categories recognized by the Sensor Tower API,
#' mapping category IDs to category names for different platforms (iOS/Android).
#' Useful for finding valid inputs for the `category` parameter in other
#' functions.
#'
#' @param platform Optional character string. Filter results for a specific
#'   platform ("ios" or "android"). If NULL (default), returns categories for
#'   both platforms.
#'
#' @return A tibble with columns `platform` (character, "ios" or "android"),
#'   `category_id` (character, e.g., "6014"), and `category_name`
#'   (character, e.g., "Games").
#' @export
#' @examples
#' \dontrun{
#' # Get all categories
#' all_cats <- st_categories() # Updated example call
#' print(head(all_cats))
#'
#' # Get only iOS categories
#' ios_cats <- st_categories(platform = "ios") # Updated example call
#' print(head(ios_cats))
#'
#' # Find the ID for Android "Music & Audio"
#' subset(st_categories("android"), category_name == "Music & Audio")
#' }
st_categories <- function(platform = NULL) { # Renamed function
  # Access the internal data object st_category_data
  data <- st_category_data

  if (!is.null(platform)) {
    platform <- tolower(platform)
    if (!platform %in% c("ios", "android")) {
      warning(
        "Invalid platform specified. Choose 'ios' or 'android'. ",
        "Returning all categories.",
        call. = FALSE
      )
    } else {
      data <- data[data$platform == platform, ]
    }
  }
  return(data)
} 