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
#'     "553834731", # Candy Crush iOS
#'     "com.king.candycrushsaga", # Candy Crush Android
#'     "5ba4585f539ce75b97db6bcb" # Star Trek unified ID
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
    message(
      "  ID types detected: ",
      paste(names(type_counts), type_counts, sep = ":", collapse = ", ")
    )
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

  # Convert resolved app metadata into a data frame for grouping
  to_safe_char <- function(x) {
    if (is.null(x)) {
      return(NA_character_)
    }
    val <- as.character(x)[1]
    if (is.na(val) || !nzchar(val)) {
      return(NA_character_)
    }
    val
  }
  resolved_apps <- dplyr::bind_rows(lapply(app_list, function(app) {
    data.frame(
      input_id = to_safe_char(app$input_id),
      ios_id = to_safe_char(app$ios_id),
      android_id = to_safe_char(app$android_id),
      unified_id = to_safe_char(app$unified_id),
      app_name = to_safe_char(app$app_name),
      stringsAsFactors = FALSE
    )
  }))

  valid_ios <- !is.na(resolved_apps$ios_id) & nzchar(resolved_apps$ios_id)
  valid_android <- !is.na(resolved_apps$android_id) & nzchar(resolved_apps$android_id)
  valid_unified <- !is.na(resolved_apps$unified_id) & nzchar(resolved_apps$unified_id)

  fetch_groups <- list()
  fetch_groups$both <- resolved_apps[valid_ios & valid_android, , drop = FALSE]
  fetch_groups$ios <- resolved_apps[valid_ios & !valid_android, , drop = FALSE]
  fetch_groups$android <- resolved_apps[!valid_ios & valid_android, , drop = FALSE]
  fetch_groups$unified <- resolved_apps[!valid_ios & !valid_android & valid_unified, , drop = FALSE]

  # Remove empty groups
  fetch_groups <- fetch_groups[vapply(fetch_groups, nrow, integer(1)) > 0]
  unresolved <- resolved_apps[!valid_ios & !valid_android & !valid_unified, , drop = FALSE]

  if (verbose) {
    message("\nOptimized fetch groups:")
    if (length(fetch_groups) == 0) {
      message("  None - no resolvable IDs")
    } else {
      for (group in names(fetch_groups)) {
        message("  ", group, ": ", nrow(fetch_groups[[group]]), " apps")
      }
    }
    if (nrow(unresolved) > 0) {
      message("  Skipped ", nrow(unresolved), " app(s) with unresolved identifiers")
    }
  }

  # Step 4: Fetch metrics using st_batch_metrics
  all_results <- list()

  for (group_name in names(fetch_groups)) {
    group_apps <- fetch_groups[[group_name]]
    if (nrow(group_apps) == 0) next
    if (verbose) message("\nFetching ", group_name, " group...")

    app_list_df <- data.frame(
      app_id = group_apps$input_id,
      ios_id = group_apps$ios_id,
      android_id = group_apps$android_id,
      unified_id = group_apps$unified_id,
      app_name = group_apps$app_name,
      stringsAsFactors = FALSE
    )

    result <- switch(group_name,
      both = st_batch_metrics(
        os = "unified",
        app_list = app_list_df,
        metrics = metrics,
        date_range = list(start_date = start_date, end_date = end_date),
        countries = countries,
        granularity = granularity,
        parallel = parallel,
        auth_token = auth_token,
        verbose = verbose
      ),
      ios = st_batch_metrics(
        os = "ios",
        app_list = app_list_df,
        metrics = metrics,
        date_range = list(start_date = start_date, end_date = end_date),
        countries = countries,
        granularity = granularity,
        parallel = parallel,
        auth_token = auth_token,
        verbose = verbose
      ),
      android = st_batch_metrics(
        os = "android",
        app_list = app_list_df,
        metrics = metrics,
        date_range = list(start_date = start_date, end_date = end_date),
        countries = countries,
        granularity = granularity,
        parallel = parallel,
        auth_token = auth_token,
        verbose = verbose
      ),
      unified = st_batch_metrics(
        os = "unified",
        app_list = app_list_df,
        metrics = metrics,
        date_range = list(start_date = start_date, end_date = end_date),
        countries = countries,
        granularity = granularity,
        parallel = parallel,
        auth_token = auth_token,
        verbose = verbose
      )
    )

    if (!is.null(result) && nrow(result) > 0) {
      result$group <- group_name
      all_results[[group_name]] <- result
    }
  }

  final_results <- if (length(all_results) > 0) {
    dplyr::bind_rows(all_results)
  } else {
    tibble::tibble()
  }

  if (nrow(final_results) > 0) {
    resolved_apps$input_position <- seq_len(nrow(resolved_apps))
    ordering <- resolved_apps[, c("input_id", "input_position"), drop = FALSE]
    names(ordering)[1] <- "original_id"
    final_results <- final_results %>%
      dplyr::left_join(ordering, by = "original_id")
    if ("date" %in% names(final_results)) {
      final_results <- final_results %>%
        dplyr::arrange(.data$input_position, .data$original_id, .data$date)
    } else {
      final_results <- final_results %>%
        dplyr::arrange(.data$input_position, .data$original_id)
    }
    final_results <- final_results %>% dplyr::select(-"input_position")
  }

  # Save cache if it's grown
  if (use_cache) {
    save_id_cache()
  }

  if (verbose) {
    message("\nFetch complete:")
    message("  Total records: ", nrow(final_results))
    unique_apps <- if ("app_id" %in% names(final_results)) length(unique(final_results$app_id)) else 0
    message("  Unique apps: ", unique_apps)
    if ("metric" %in% names(final_results) && nrow(final_results) > 0) {
      message("  Metrics: ", paste(unique(final_results$metric), collapse = ", "))
    }
  }

  return(final_results)
}

#' Clear App ID Cache
#'
#' Clears the in-memory and on-disk cache of app ID mappings
#'
#' @param disk Logical. Also remove the on-disk cache file (default TRUE).
#' @return No return value, called for side effects (clearing the cache).
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
#' @return No return value, called for side effects (displaying cache statistics).
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
