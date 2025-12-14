#' Intelligent App ID Cache and Resolution System
#'
#' Internal functions to manage a persistent cache of app ID mappings,
#' reducing API calls and improving performance.
#'
#' @keywords internal
#' @name id_cache

# Package environment for storing cache
.sensortowerR_env <- new.env(parent = emptyenv())

#' Initialize or Get ID Cache
#' @noRd
get_id_cache <- function() {
  if (!exists("id_cache", envir = .sensortowerR_env)) {
    .sensortowerR_env$id_cache <- list()
  }
  .sensortowerR_env$id_cache
}

#' Save ID Mapping to Cache
#' @noRd
cache_id_mapping <- function(input_id, ios_id = NULL, android_id = NULL,
                           unified_id = NULL, app_name = NULL, publisher_id = NULL) {
  cache <- get_id_cache()

  # Create cache entry with consistent naming (use _app_id suffix to match st_app_lookup output)
  entry <- list(
    ios_app_id = ios_id,
    android_app_id = android_id,
    unified_app_id = unified_id,
    app_name = app_name,
    publisher_id = publisher_id,
    cached_at = Sys.time()
  )

  # Store under all provided IDs for quick lookup
  if (!is.null(input_id)) cache[[as.character(input_id)]] <- entry
  if (!is.null(ios_id)) cache[[as.character(ios_id)]] <- entry
  if (!is.null(android_id)) cache[[as.character(android_id)]] <- entry
  if (!is.null(unified_id)) cache[[as.character(unified_id)]] <- entry

  .sensortowerR_env$id_cache <- cache
  invisible(entry)
}

#' Look Up ID in Cache
#' @noRd
lookup_cached_id <- function(id) {
  if (is.null(id)) return(NULL)
  cache <- get_id_cache()
  cache[[as.character(id)]]
}

#' Get Default Cache Directory
#'
#' Returns the CRAN-compliant cache directory using tools::R_user_dir()
#' @noRd
get_cache_dir <- function() {
  tools::R_user_dir("sensortowerR", "cache")
}

#' Save Cache to Disk
#'
#' Saves the ID cache to disk. Only creates the cache directory when
#' this function is explicitly called.
#' @noRd
save_id_cache <- function(path = NULL) {
  if (is.null(path)) {
    cache_dir <- get_cache_dir()
    # Only create directory when user explicitly saves cache
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    }
    path <- file.path(cache_dir, "id_cache.rds")
  }

  cache <- get_id_cache()
  if (length(cache) > 0) {
    saveRDS(cache, path)
    if (getOption("sensortowerR.verbose", FALSE)) {
      message("Saved ", length(cache), " ID mappings to cache")
    }
  }
}

#' Load Cache from Disk
#'
#' Loads the ID cache from disk if it exists. Does NOT create any directories.
#' @noRd
load_id_cache <- function(path = NULL) {
  if (is.null(path)) {
    path <- file.path(get_cache_dir(), "id_cache.rds")
  }

  if (file.exists(path)) {
    cache <- readRDS(path)
    .sensortowerR_env$id_cache <- cache
    if (getOption("sensortowerR.verbose", FALSE)) {
      message("Loaded ", length(cache), " ID mappings from cache")
    }
  }
}

#' Smart ID Resolution with Caching
#' 
#' Resolves app IDs using cache first, then API if needed
#' @noRd
resolve_app_id <- function(id, auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN"), 
                          use_cache = TRUE, verbose = FALSE) {
  
  # Check cache first
  if (use_cache) {
    cached <- lookup_cached_id(id)
    if (!is.null(cached)) {
      # Check if cache is fresh (within 30 days)
      age_days <- as.numeric(difftime(Sys.time(), cached$cached_at, units = "days"))
      if (age_days < 30) {
        if (verbose) message("  Found in cache (", round(age_days, 1), " days old)")
        return(cached)
      }
    }
  }
  
  # Not in cache or stale - make API call
  if (verbose) message("  Looking up ID via API...")
  
  result <- tryCatch({
    st_app_lookup(id, auth_token = auth_token, verbose = FALSE)
  }, error = function(e) NULL)
  
  # Cache the result
  if (!is.null(result) && use_cache) {
    cache_id_mapping(
      input_id = id,
      ios_id = result$ios_app_id,
      android_id = result$android_app_id,
      unified_id = result$unified_id,
      app_name = result$app_name,
      publisher_id = result$publisher_id
    )
  }
  
  result
}

#' Batch Resolve Multiple IDs
#' 
#' Efficiently resolves multiple app IDs with minimal API calls
#' @noRd
batch_resolve_ids <- function(ids, auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN"),
                            use_cache = TRUE, verbose = FALSE) {
  
  ids <- as.character(unique(ids))
  ids <- ids[!is.na(ids) & nzchar(ids)]
  if (length(ids) == 0) {
    return(list())
  }
  
  if (verbose) message("Resolving ", length(ids), " app IDs...")
  
  # First pass: check cache
  results <- list()
  needs_api <- character()
  
  for (id in ids) {
    if (use_cache) {
      cached <- lookup_cached_id(id)
      if (!is.null(cached)) {
        age_days <- as.numeric(difftime(Sys.time(), cached$cached_at, units = "days"))
        if (age_days < 30) {
          results[[id]] <- cached
          next
        }
      }
    }
    needs_api <- c(needs_api, id)
  }
  
  if (verbose && length(results) > 0) {
    message("  Found ", length(results), " IDs in cache")
  }
  
  # Second pass: batch API lookup for remaining IDs
  if (length(needs_api) > 0) {
    if (verbose) message("  Looking up ", length(needs_api), " IDs via API...")
    
    # Try to use comparison attributes endpoint for batch lookup
    if (length(needs_api) > 1) {
      batch_result <- tryCatch({
        st_get_unified_mapping(needs_api, auth_token = auth_token)
      }, error = function(e) NULL)
      
      if (!is.null(batch_result)) {
        # Process batch results
        for (i in seq_len(nrow(batch_result))) {
          id <- batch_result$input_id[i]
          results[[id]] <- list(
            ios_app_id = batch_result$ios_app_id[i],
            android_app_id = batch_result$android_app_id[i],
            unified_app_id = batch_result$unified_app_id[i],
            app_name = batch_result$app_name[i],
            publisher_id = batch_result$publisher_id[i]
          )
          
          # Cache the result
          if (use_cache) {
            cache_id_mapping(
              input_id = id,
              ios_id = batch_result$ios_app_id[i],
              android_id = batch_result$android_app_id[i],
              unified_id = batch_result$unified_app_id[i],
              app_name = batch_result$app_name[i],
              publisher_id = batch_result$publisher_id[i]
            )
          }
        }
        needs_api <- character() # All resolved
      }
    }
    
    # Fall back to individual lookups if batch failed
    for (id in needs_api) {
      result <- resolve_app_id(id, auth_token, use_cache, verbose = FALSE)
      if (!is.null(result)) {
        results[[id]] <- result
      }
    }
  }
  
  # Auto-save cache periodically
  if (use_cache && length(get_id_cache()) %% 50 == 0) {
    save_id_cache()
  }
  
  results
}

#' Smart ID Type Detection
#' 
#' Intelligently detects the type of ID and optimal resolution strategy
#' @noRd
detect_id_type <- function(id) {
  if (is.null(id) || is.na(id)) return("unknown")
  
  id_str <- as.character(id)
  
  # iOS numeric ID (e.g., "553834731")
  if (grepl("^\\d{9,10}$", id_str)) {
    return("ios")
  }
  
  # Android package name (e.g., "com.king.candycrushsaga")
  if (grepl("^(com|net|org|io|app|game)\\.", id_str)) {
    return("android")
  }
  
  # Sensor Tower hex ID (24 chars, e.g., "5ba4585f539ce75b97db6bcb")
  if (grepl("^[a-f0-9]{24}$", id_str)) {
    return("unified_hex")
  }
  
  # Publisher ID patterns
  if (grepl("^pub", id_str) || grepl("^[a-f0-9]{24}pub", id_str)) {
    return("publisher")
  }
  
  # Combined iOS_Android format
  if (grepl("^\\d+_com\\.", id_str)) {
    return("combined")
  }
  
  return("unknown")
}

#' Preload Common App IDs
#' 
#' Preloads cache with commonly used app IDs to minimize API calls
#' @noRd
preload_common_apps <- function(auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN")) {
  
  # Common game IDs that are frequently used
  common_apps <- c(
    "553834731",           # Candy Crush iOS
    "1195621598",          # Homescapes iOS  
    "1094591345",          # Pokemon GO iOS
    "529479190",           # Clash of Clans iOS
    "1053012308",          # MONOPOLY GO! iOS
    "com.king.candycrushsaga",
    "com.playrix.homescapes",
    "com.nianticlabs.pokemongo",
    "com.supercell.clashofclans",
    "com.scopely.monopolygo"
  )
  
  # Check which ones aren't cached
  needs_lookup <- character()
  for (id in common_apps) {
    if (is.null(lookup_cached_id(id))) {
      needs_lookup <- c(needs_lookup, id)
    }
  }
  
  if (length(needs_lookup) > 0) {
    message("Preloading ", length(needs_lookup), " common app IDs...")
    batch_resolve_ids(needs_lookup, auth_token, use_cache = TRUE, verbose = FALSE)
    save_id_cache()
  }
}

# NOTE: Removed .onAttach and .onDetach hooks that automatically created
# ~/.sensortowerR directory to comply with CRAN policy.
# Cache is now only loaded/saved when explicitly requested by user functions.
# Users can call save_id_cache() explicitly to persist the cache.
