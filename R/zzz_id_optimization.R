#' Package Load Hook for ID Optimization
#'
#' Sets up ID caching and optimization on package load
#'
#' @keywords internal

.onLoad <- function(libname, pkgname) {
  # Set default options
  op <- options()
  op.sensortowerR <- list(
    sensortowerR.use_cache = TRUE,
    sensortowerR.cache_dir = file.path(Sys.getenv("HOME"), ".sensortowerR"),
    sensortowerR.cache_max_age_days = 30,
    sensortowerR.preload_common = FALSE,
    sensortowerR.verbose = FALSE,
    sensortowerR.auto_resolve = TRUE
  )
  
  toset <- !(names(op.sensortowerR) %in% names(op))
  if (any(toset)) options(op.sensortowerR[toset])
  
  # Load ID cache if it exists
  tryCatch({
    load_id_cache()
  }, error = function(e) {
    # Silently continue if no cache
  })
  
  invisible()
}

#' Enhanced st_metrics with Smart ID Resolution
#'
#' Override the default st_metrics to use smart ID resolution
#' @rdname st_metrics
#' @export
st_metrics_smart <- function(
  app_id = NULL,
  ios_app_id = NULL,
  android_app_id = NULL,
  unified_app_id = NULL,
  start_date = NULL,
  end_date = NULL,
  countries,
  date_granularity,
  auto_platform_fetch = TRUE,
  combine_platforms = TRUE,
  auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN"),
  verbose = TRUE
) {
  
  # If only app_id provided, try smart resolution first
  if (!is.null(app_id) && is.null(ios_app_id) && is.null(android_app_id)) {
    
    # Check cache first
    cached <- lookup_cached_id(app_id)
    if (!is.null(cached)) {
      ios_app_id <- cached$ios_id
      android_app_id <- cached$android_id
      
      if (verbose && (!is.null(ios_app_id) || !is.null(android_app_id))) {
        message("  Using cached platform IDs for ", app_id)
      }
    }
  }
  
  # Determine OS from the provided IDs
  os <- if (!is.null(unified_app_id)) {
    "unified"
  } else if (!is.null(ios_app_id) && !is.null(android_app_id)) {
    "unified"
  } else if (!is.null(ios_app_id)) {
    "ios"
  } else if (!is.null(android_app_id)) {
    "android"
  } else {
    "unified"  # Default
  }
  
  # Call updated st_metrics with OS parameter
  st_metrics(
    os = os,
    app_id = app_id,
    ios_app_id = ios_app_id,
    android_app_id = android_app_id,
    unified_app_id = unified_app_id,
    start_date = start_date,
    end_date = end_date,
    countries = countries,
    date_granularity = date_granularity,
    auth_token = auth_token,
    verbose = verbose
  )
}

#' ID Resolution Best Practices
#'
#' @description
#' The sensortowerR package now includes intelligent ID caching and resolution
#' to minimize API calls:
#'
#' 1. **Automatic Caching**: All ID lookups are cached for 30 days
#' 2. **Batch Resolution**: Multiple IDs are resolved in a single API call when possible
#' 3. **Smart Detection**: Automatically detects ID types (iOS, Android, unified)
#' 4. **Persistent Cache**: Cache persists between R sessions
#'
#' @section Configuration:
#' Set these options to customize behavior:
#' - `options(sensortowerR.use_cache = TRUE)` - Enable/disable caching
#' - `options(sensortowerR.cache_max_age_days = 30)` - Cache expiry in days
#' - `options(sensortowerR.preload_common = TRUE)` - Preload common app IDs on startup
#' - `options(sensortowerR.auto_resolve = TRUE)` - Auto-resolve IDs in smart functions
#'
#' @section Cache Management:
#' - `st_cache_info()` - View cache statistics
#' - `st_clear_id_cache()` - Clear the cache
#' - `save_id_cache()` - Manually save cache to disk
#' - `load_id_cache()` - Manually load cache from disk
#'
#' @name id_resolution
#' @keywords internal
NULL