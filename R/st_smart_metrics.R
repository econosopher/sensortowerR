#' Smart Metrics Fetching with Automatic ID Resolution
#'
#' Enhanced metrics fetching that automatically handles ID resolution,
#' caching, and fallbacks to minimize API calls.
#'
#' @param app_ids Character vector. Can be any mix of iOS IDs, Android IDs, 
#'   or Sensor Tower unified IDs.
#' @param metrics Character vector. Metrics to fetch (e.g., "revenue", "downloads", "dau").
#' @param start_date Date or character string. Start date for metrics.
#' @param end_date Date or character string. End date for metrics.
#' @param countries Character vector. Country codes (default "WW").
#' @param granularity Character. Date granularity ("daily", "weekly", "monthly").
#' @param auto_resolve Logical. Automatically resolve IDs using cache/API (default TRUE).
#' @param use_cache Logical. Use ID cache to minimize lookups (default TRUE).
#' @param parallel Logical. Use parallel processing (default TRUE).
#' @param auth_token Character string. Your Sensor Tower API authentication token.
#' @param verbose Logical. Print progress messages.
#'
#' @return A tibble with metrics in long format
#'
#' @examples
#' \dontrun{
#' # Mixed ID types - automatically resolved
#' metrics <- st_smart_metrics(
#'   app_ids = c(
#'     "553834731",                    # Candy Crush iOS
#'     "com.king.candycrushsaga",      # Candy Crush Android
#'     "5ba4585f539ce75b97db6bcb"      # Star Trek unified ID
#'   ),
#'   metrics = c("revenue", "downloads", "dau"),
#'   start_date = "2024-01-01",
#'   end_date = "2024-12-31"
#' )
#' }
#'
#' @export
st_smart_metrics <- function(
  app_ids,
  metrics = c("revenue", "downloads"),
  start_date = Sys.Date() - 30,
  end_date = Sys.Date() - 1,
  countries = "WW",
  granularity = "daily",
  auto_resolve = TRUE,
  use_cache = TRUE,
  parallel = TRUE,
  auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN"),
  verbose = TRUE
) {
  
  # Load cache if needed
  if (use_cache && !exists("id_cache", envir = .sensortowerR_env)) {
    load_id_cache()
  }
  
  # Step 1: Analyze and categorize IDs
  if (verbose) message("Analyzing ", length(app_ids), " app IDs...")
  
  id_analysis <- data.frame(
    input_id = app_ids,
    id_type = sapply(app_ids, detect_id_type),
    stringsAsFactors = FALSE
  )
  
  if (verbose) {
    type_counts <- table(id_analysis$id_type)
    message("  ID types detected: ", 
            paste(names(type_counts), type_counts, sep = ":", collapse = ", "))
  }
  
  # Step 2: Resolve IDs if needed
  if (auto_resolve) {
    # Get all unique IDs that need resolution
    needs_resolution <- unique(app_ids)
    
    # Batch resolve with caching
    resolved <- batch_resolve_ids(needs_resolution, auth_token, use_cache, verbose)
    
    # Create enhanced app list with all available IDs
    app_list <- lapply(app_ids, function(id) {
      res <- resolved[[id]]
      if (is.null(res)) {
        # Fallback to basic type detection
        id_type <- detect_id_type(id)
        list(
          input_id = id,
          ios_id = if (id_type == "ios") id else NULL,
          android_id = if (id_type == "android") id else NULL,
          unified_id = if (id_type == "unified_hex") id else NULL,
          app_name = NULL
        )
      } else {
        list(
          input_id = id,
          ios_id = res$ios_id,
          android_id = res$android_id,
          unified_id = res$unified_id,
          app_name = res$app_name
        )
      }
    })
  } else {
    # Use IDs as-is based on type detection
    app_list <- lapply(seq_along(app_ids), function(i) {
      id <- app_ids[i]
      id_type <- id_analysis$id_type[i]
      
      list(
        input_id = id,
        ios_id = if (id_type == "ios") id else NULL,
        android_id = if (id_type == "android") id else NULL,
        unified_id = if (id_type == "unified_hex") id else NULL,
        app_name = NULL
      )
    })
  }
  
  # Step 3: Group apps by optimal fetching strategy
  fetch_groups <- list()
  
  for (app in app_list) {
    # Prefer platform-specific IDs over unified
    if (!is.null(app$ios_id) && !is.null(app$android_id)) {
      # Has both platforms
      key <- paste0(app$ios_id, "_", app$android_id)
      if (is.null(fetch_groups[["both"]])) fetch_groups[["both"]] <- list()
      fetch_groups[["both"]][[key]] <- app
    } else if (!is.null(app$ios_id)) {
      # iOS only
      if (is.null(fetch_groups[["ios"]])) fetch_groups[["ios"]] <- list()
      fetch_groups[["ios"]][[app$ios_id]] <- app
    } else if (!is.null(app$android_id)) {
      # Android only
      if (is.null(fetch_groups[["android"]])) fetch_groups[["android"]] <- list()
      fetch_groups[["android"]][[app$android_id]] <- app
    } else if (!is.null(app$unified_id)) {
      # Unified only (last resort)
      if (is.null(fetch_groups[["unified"]])) fetch_groups[["unified"]] <- list()
      fetch_groups[["unified"]][[app$unified_id]] <- app
    }
  }
  
  if (verbose) {
    message("\nOptimized fetch groups:")
    for (group in names(fetch_groups)) {
      message("  ", group, ": ", length(fetch_groups[[group]]), " apps")
    }
  }
  
  # Step 4: Fetch metrics using st_batch_metrics
  all_results <- list()
  
  # Process each group
  for (group_name in names(fetch_groups)) {
    group_apps <- fetch_groups[[group_name]]
    
    if (length(group_apps) == 0) next
    
    if (verbose) message("\nFetching ", group_name, " group...")
    
    # Create appropriate app list for st_batch_metrics
    if (group_name == "both") {
      # Extract iOS and Android IDs
      ios_ids <- sapply(group_apps, function(x) x$ios_id)
      android_ids <- sapply(group_apps, function(x) x$android_id)
      
      result <- st_ytd_metrics(
        ios_app_id = ios_ids,
        android_app_id = android_ids,
        period_start = format(as.Date(start_date), "%m-%d"),
        period_end = format(as.Date(end_date), "%m-%d"),
        years = unique(as.numeric(format(c(as.Date(start_date), as.Date(end_date)), "%Y"))),
        metrics = metrics,
        countries = countries,
        auth_token = auth_token,
        verbose = FALSE
      )
      
    } else if (group_name == "ios") {
      ios_ids <- names(group_apps)
      
      result <- st_ytd_metrics(
        ios_app_id = ios_ids,
        period_start = format(as.Date(start_date), "%m-%d"),
        period_end = format(as.Date(end_date), "%m-%d"),
        years = unique(as.numeric(format(c(as.Date(start_date), as.Date(end_date)), "%Y"))),
        metrics = metrics,
        countries = countries,
        auth_token = auth_token,
        verbose = FALSE
      )
      
    } else if (group_name == "android") {
      android_ids <- names(group_apps)
      
      result <- st_ytd_metrics(
        android_app_id = android_ids,
        period_start = format(as.Date(start_date), "%m-%d"),
        period_end = format(as.Date(end_date), "%m-%d"),
        years = unique(as.numeric(format(c(as.Date(start_date), as.Date(end_date)), "%Y"))),
        metrics = metrics,
        countries = countries,
        auth_token = auth_token,
        verbose = FALSE
      )
      
    } else if (group_name == "unified") {
      unified_ids <- names(group_apps)
      
      result <- st_ytd_metrics(
        unified_app_id = unified_ids,
        period_start = format(as.Date(start_date), "%m-%d"),
        period_end = format(as.Date(end_date), "%m-%d"),
        years = unique(as.numeric(format(c(as.Date(start_date), as.Date(end_date)), "%Y"))),
        metrics = metrics,
        countries = countries,
        auth_token = auth_token,
        verbose = FALSE
      )
    }
    
    # Map results back to input IDs
    if (!is.null(result) && nrow(result) > 0) {
      # Add input_id column for tracking
      result$group <- group_name
      all_results[[group_name]] <- result
    }
  }
  
  # Combine all results
  final_results <- dplyr::bind_rows(all_results)
  
  # Map back to original input IDs
  if (nrow(final_results) > 0) {
    # Create a mapping from entity_id to input_id
    id_map <- data.frame(
      input_id = app_ids,
      stringsAsFactors = FALSE
    )
    
    # Add app names if available
    for (app in app_list) {
      if (!is.null(app$app_name)) {
        final_results$app_name[final_results$entity_id %in% 
                              c(app$ios_id, app$android_id, app$unified_id,
                                paste0(app$ios_id, "_", app$android_id))] <- app$app_name
      }
    }
  }
  
  # Save cache if it's grown
  if (use_cache) {
    save_id_cache()
  }
  
  if (verbose) {
    message("\nFetch complete:")
    message("  Total records: ", nrow(final_results))
    message("  Unique apps: ", length(unique(final_results$entity_id)))
    message("  Metrics: ", paste(unique(final_results$metric), collapse = ", "))
  }
  
  return(final_results)
}

#' Clear App ID Cache
#' 
#' Clears the in-memory and on-disk cache of app ID mappings
#' 
#' @param disk Logical. Also remove the on-disk cache file (default TRUE).
#' 
#' @export
st_clear_id_cache <- function(disk = TRUE) {
  
  # Clear in-memory cache
  .sensortowerR_env$id_cache <- list()
  
  # Clear disk cache
  if (disk) {
    cache_file <- file.path(Sys.getenv("HOME"), ".sensortowerR", "id_cache.rds")
    if (file.exists(cache_file)) {
      unlink(cache_file)
      message("Cleared app ID cache")
    }
  }
  
  invisible(TRUE)
}

#' Show App ID Cache Statistics
#' 
#' Display information about the current app ID cache
#' 
#' @export
st_cache_info <- function() {
  cache <- get_id_cache()
  
  if (length(cache) == 0) {
    message("App ID cache is empty")
    return(invisible(NULL))
  }
  
  # Analyze cache
  cache_ages <- sapply(cache, function(x) {
    if (!is.null(x$cached_at)) {
      as.numeric(difftime(Sys.time(), x$cached_at, units = "days"))
    } else {
      NA
    }
  })
  
  has_ios <- sum(sapply(cache, function(x) !is.null(x$ios_id)))
  has_android <- sum(sapply(cache, function(x) !is.null(x$android_id)))
  has_both <- sum(sapply(cache, function(x) !is.null(x$ios_id) && !is.null(x$android_id)))
  
  cat("App ID Cache Statistics:\n")
  cat("  Total entries: ", length(cache), "\n")
  cat("  Apps with iOS ID: ", has_ios, "\n")
  cat("  Apps with Android ID: ", has_android, "\n")
  cat("  Apps with both platforms: ", has_both, "\n")
  cat("  Average cache age: ", round(mean(cache_ages, na.rm = TRUE), 1), " days\n")
  cat("  Oldest entry: ", round(max(cache_ages, na.rm = TRUE), 1), " days\n")
  cat("  Cache location: ", file.path(Sys.getenv("HOME"), ".sensortowerR", "id_cache.rds"), "\n")
  
  invisible(cache)
}