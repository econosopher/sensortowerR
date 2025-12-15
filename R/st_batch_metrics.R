#' Batch Fetch Metrics for Multiple Apps
#'
#' Efficiently fetch metrics for multiple apps by batching API calls and
#' automatically handling platform-specific requirements. The OS parameter
#' controls which platform's data is returned for all apps.
#'
#' @param os Character. Required. Operating system: "ios", "android", or "unified".
#'   This determines which platform's data is returned for all apps.
#' @param app_list List or data frame containing app information. Can be:
#'   - Character vector of app IDs
#'   - Data frame with columns: app_id, app_name (optional), platform (optional)
#'   - List of lists with app_id and optional metadata
#' @param metrics Character vector. Metrics to fetch. Supported values:
#'   - "revenue" - App revenue estimates
#'   - "downloads" - App download estimates
#'   - "dau" - Daily Active Users
#'   - "wau" - Weekly Active Users
#'   - "mau" - Monthly Active Users
#' @param date_range List with start_date and end_date, or "ytd" for year-to-date
#' @param countries Character vector. Country codes. Required.
#' @param granularity Character. Date granularity (default "monthly")
#' @param parallel Logical. Use parallel processing (default FALSE)
#' @param cache_dir Character. Directory for caching results (optional)
#' @param verbose Logical. Show progress messages (default TRUE)
#' @param auth_token Character string. Your Sensor Tower API authentication token.
#' @param max_cores Integer. Maximum number of cores to use for parallel processing.
#' @param max_concurrent_requests Integer. Max concurrent requests (deprecated/unused).
#' @param retry Logical. Whether to retry failed requests.
#' @param max_retries Integer. Max retries.
#' @param publisher_ids Character vector. Publisher IDs to fetch data for.
#'
#' @return A tibble with all metrics for all apps.
#' @export
st_batch_metrics <- function(os,
                             app_list,
                             metrics = c("revenue", "downloads"),
                             date_range = list(
                               start_date = Sys.Date() - 90,
                               end_date = Sys.Date() - 1
                             ),
                             countries,
                             granularity,
                             parallel = FALSE,
                             cache_dir = NULL,
                             verbose = TRUE,
                             auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN"),
                             max_cores = 2,
                             max_concurrent_requests = 2,
                             retry = TRUE,
                             max_retries = 3,
                             publisher_ids = NULL) {
  # Validate OS parameter
  if (missing(os) || is.null(os) || !os %in% c("ios", "android", "unified")) {
    rlang::abort("'os' parameter is required and must be one of: 'ios', 'android', or 'unified'")
  }

  # Validate auth token
  if (is.null(auth_token) || auth_token == "") {
    rlang::abort("Authentication token is required. Set SENSORTOWER_AUTH_TOKEN environment variable.")
  }

  # Validate countries parameter
  if (missing(countries) || is.null(countries)) {
    rlang::abort("'countries' parameter is required")
  }

  # Validate granularity explicitly
  if (missing(granularity) || is.null(granularity)) {
    rlang::abort("'granularity' parameter is required. Specify one of: 'daily', 'weekly', 'monthly', 'quarterly'.")
  }

  # Validate date range
  if (identical(date_range, "ytd")) {
    rlang::abort("YTD mode is no longer supported in st_batch_metrics. Please specify explicit start_date and end_date.")
  }

  # Publisher mode
  if (!is.null(publisher_ids)) {
    return(fetch_publisher_batch(publisher_ids, os, metrics, countries, date_range, granularity, auth_token, verbose))
  }

  # Step 1: Normalize app list
  app_count <- if (is.data.frame(app_list)) nrow(app_list) else length(app_list)
  if (verbose) message("Processing ", app_count, " apps...")
  apps_df <- normalize_app_list(app_list, os, auth_token, verbose)

  if (verbose) {
    message("Resolved apps:")
    message("  iOS apps: ", sum(!is.na(apps_df$ios_id)))
    message("  Android apps: ", sum(!is.na(apps_df$android_id)))
    message("  Unified only: ", sum(is.na(apps_df$ios_id) & is.na(apps_df$android_id)))
  }

  # Step 2: Group apps by fetch strategy
  fetch_groups <- group_apps_by_platform(apps_df)

  # Step 3: Define worker function
  fetch_worker <- function(group, group_name) {
    if (verbose) message("\nFetching metrics for ", group_name, " group (", nrow(group), " apps)...")

    # Fetch Active Users
    active_res <- fetch_active_users_batch(group, group_name, metrics, countries, date_range, granularity, auth_token, verbose)

    # Fetch Sales (Revenue/Downloads)
    sales_res <- fetch_sales_batch(group, group_name, metrics, countries, date_range, granularity, auth_token, verbose, os)

    # Combine results
    if (verbose) {
      if (!is.null(active_res) && nrow(active_res) > 0) {
        message("Active res app_id type: ", typeof(active_res$app_id), " class: ", class(active_res$app_id))
      }
      if (!is.null(sales_res) && nrow(sales_res) > 0) {
        message("Sales res app_id type: ", typeof(sales_res$app_id), " class: ", class(sales_res$app_id))
      }
    }

    # Ensure app_id is character in both
    if (!is.null(active_res) && nrow(active_res) > 0 && "app_id" %in% names(active_res)) {
      active_res$app_id <- as.character(active_res$app_id)
    }
    if (!is.null(sales_res) && nrow(sales_res) > 0 && "app_id" %in% names(sales_res)) {
      sales_res$app_id <- as.character(sales_res$app_id)
    }

    dplyr::bind_rows(active_res, sales_res)
  }

  # Step 4: Execute fetches
  results <- execute_batch_fetch(fetch_groups, fetch_worker, parallel, max_cores, verbose)

  # Step 5: Finalize results
  finalize_batch_results_internal(results, apps_df)
}

# --- Helper Functions ---

normalize_app_list <- function(app_list, os, auth_token, verbose) {
  # Convert input to data frame
  if (is.vector(app_list) && is.character(app_list)) {
    apps_df <- tibble::tibble(app_id = app_list)
  } else if (is.data.frame(app_list)) {
    apps_df <- app_list
  } else if (is.list(app_list)) {
    # Handle list of lists or list of named vectors
    apps_df <- dplyr::bind_rows(app_list)
  } else {
    rlang::abort("Invalid app_list format. Must be character vector, data frame, or list.")
  }

  # Ensure app_id column exists
  if (!"app_id" %in% names(apps_df)) {
    rlang::abort("app_list must contain an 'app_id' column or be a vector of IDs")
  }

  # Add metadata columns if missing
  if (!"app_name" %in% names(apps_df)) apps_df$app_name <- NA_character_
  if (!"unified_id" %in% names(apps_df)) apps_df$unified_id <- NA_character_
  if (!"ios_id" %in% names(apps_df)) apps_df$ios_id <- NA_character_
  if (!"android_id" %in% names(apps_df)) apps_df$android_id <- NA_character_

  # Resolve IDs if needed
  # We need to ensure we have platform-specific IDs for fetching

  # Identify which apps need resolution
  # If we are in unified mode, we need both IDs
  # If we are in ios mode, we need ios_id
  # If we are in android mode, we need android_id

  # First, try to resolve unified IDs for all apps if not present
  missing_unified <- is.na(apps_df$unified_id) & !is.na(apps_df$app_id)
  if (any(missing_unified)) {
    # If app_id looks like a unified ID (no dots, no plain numbers usually, but let's assume input is unified if not specified)
    # Actually, let's use resolve_app_id to get the mapping

    if (verbose) message("Resolving ", sum(missing_unified), " app IDs...")

    # We can use batch resolution if available, or loop
    # For now, let's loop or use lapply with resolve_app_id which is cached

    resolved_list <- lapply(apps_df$app_id[missing_unified], function(id) {
      resolve_app_id(id, auth_token = auth_token)
    })

    # Update the dataframe
    for (i in seq_along(resolved_list)) {
      res <- resolved_list[[i]]
      idx <- which(missing_unified)[i]

      if (!is.null(res)) {
        if (!is.null(res$unified_app_id)) apps_df$unified_id[idx] <- res$unified_app_id
        if (!is.null(res$ios_app_id)) apps_df$ios_id[idx] <- res$ios_app_id
        if (!is.null(res$android_app_id)) apps_df$android_id[idx] <- res$android_app_id
        if (is.na(apps_df$app_name[idx]) && !is.null(res$name)) apps_df$app_name[idx] <- res$name
      }
    }
  }

  # Handle OS-specific requirements
  if (os == "ios") {
    # If ios_id is missing but we have app_id and it looks like an iOS ID (all digits), use it
    missing_ios <- is.na(apps_df$ios_id) & grepl("^\\d+$", apps_df$app_id)
    apps_df$ios_id[missing_ios] <- apps_df$app_id[missing_ios]
  } else if (os == "android") {
    # If android_id is missing but app_id looks like android (contains dots), use it
    missing_android <- is.na(apps_df$android_id) & grepl("\\.", apps_df$app_id)
    apps_df$android_id[missing_android] <- apps_df$app_id[missing_android]
  }

  apps_df
}

fetch_publisher_batch <- function(publisher_ids, os, metrics, countries, date_range, granularity, auth_token, verbose) {
  active_user_metrics <- intersect(metrics, c("dau", "wau", "mau"))
  if (length(active_user_metrics) > 0 && verbose) {
    message("Active user metrics are not supported for publisher IDs. Skipping: ", paste(active_user_metrics, collapse = ", "))
  }
  revenue_download_metrics <- intersect(metrics, c("revenue", "downloads"))
  if (length(revenue_download_metrics) == 0) {
    return(tibble::tibble())
  }

  # Helper to fetch one publisher for one platform
  fetch_publisher_platform <- function(platform_os, pid) {
    tryCatch(
      {
        st_sales_report(
          os = platform_os,
          countries = countries,
          start_date = as.character(date_range$start_date),
          end_date = as.character(date_range$end_date),
          date_granularity = granularity,
          publisher_ids = pid,
          auth_token = auth_token,
          auto_segment = TRUE,
          verbose = FALSE
        )
      },
      error = function(e) NULL
    )
  }

  results <- lapply(seq_along(publisher_ids), function(i) {
    pid <- publisher_ids[i]
    df <- NULL

    if (os == "ios") {
      df <- fetch_publisher_platform("ios", pid)
    } else if (os == "android") {
      df <- fetch_publisher_platform("android", pid)
    } else {
      ios_res <- fetch_publisher_platform("ios", pid)
      if (!is.null(ios_res) && "app_id" %in% names(ios_res)) ios_res$app_id <- as.character(ios_res$app_id)

      and_res <- fetch_publisher_platform("android", pid)
      if (!is.null(and_res) && "app_id" %in% names(and_res)) and_res$app_id <- as.character(and_res$app_id)

      df <- dplyr::bind_rows(ios_res %||% tibble::tibble(), and_res %||% tibble::tibble())
      if (nrow(df) > 0) {
        df <- df %>%
          dplyr::group_by(.data$date, .data$country) %>%
          dplyr::summarise(
            revenue = sum(.data$revenue, na.rm = TRUE),
            downloads = sum(.data$downloads, na.rm = TRUE),
            .groups = "drop"
          )
      }
    }

    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }

    df %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(intersect(revenue_download_metrics, c("revenue", "downloads"))),
        names_to = "metric",
        values_to = "value"
      ) %>%
      dplyr::mutate(
        original_id = pid,
        app_id = pid,
        app_id_type = "publisher",
        entity_id = pid,
        entity_type = "publisher"
      )
  })

  results <- results[!vapply(results, is.null, logical(1))]
  final <- if (length(results) > 0) dplyr::bind_rows(results) else tibble::tibble()

  if (verbose) {
    message("\nBatch fetch complete (publisher mode)!")
    message("Total records: ", nrow(final))
  }
  return(final)
}

group_apps_by_platform <- function(apps_df) {
  fetch_groups <- list()

  # Group 1: Apps with both iOS and Android IDs
  both_platforms <- apps_df[!is.na(apps_df$ios_id) & !is.na(apps_df$android_id), ]
  if (nrow(both_platforms) > 0) {
    fetch_groups$both <- both_platforms
  }

  # Group 2: iOS only
  ios_only <- apps_df[!is.na(apps_df$ios_id) & is.na(apps_df$android_id), ]
  if (nrow(ios_only) > 0) {
    fetch_groups$ios <- ios_only
  }

  # Group 3: Android only
  android_only <- apps_df[is.na(apps_df$ios_id) & !is.na(apps_df$android_id), ]
  if (nrow(android_only) > 0) {
    fetch_groups$android <- android_only
  }

  # Group 4: Unified only (fallback)
  unified_only <- apps_df[is.na(apps_df$ios_id) & is.na(apps_df$android_id), ]
  if (nrow(unified_only) > 0) {
    fetch_groups$unified <- unified_only
  }

  fetch_groups
}

execute_batch_fetch <- function(fetch_groups, fetch_worker, parallel, max_cores, verbose) {
  can_parallel <- parallel && length(fetch_groups) > 1 &&
    .Platform$OS.type != "windows" && max(1L, as.integer(max_cores)) > 1

  if (can_parallel) {
    if (verbose) message("\nUsing parallel processing...")

    results <- parallel::mclapply(
      names(fetch_groups),
      function(group_name) {
        result <- fetch_worker(fetch_groups[[group_name]], group_name)
        if (!is.null(result) && "app_id" %in% names(result)) {
          result$app_id <- as.character(result$app_id)
        }
        result
      },
      mc.cores = min(length(fetch_groups), max(1L, as.integer(max_cores)))
    )
  } else {
    if (parallel && .Platform$OS.type == "windows" && verbose) {
      message("Parallel processing is not supported on Windows; defaulting to sequential execution.")
    }
    results <- lapply(
      names(fetch_groups),
      function(group_name) {
        result <- fetch_worker(fetch_groups[[group_name]], group_name)
        if (!is.null(result) && "app_id" %in% names(result)) {
          result$app_id <- as.character(result$app_id)
        }
        result
      }
    )
  }

  if (verbose) message("Batch fetch complete. Combining results...")
  res <- Filter(Negate(is.null), results)
  if (verbose) message("Filtered results length: ", length(res))
  res
}

finalize_batch_results <- function(results, apps_df) {
  if (length(results) == 0) {
    return(tibble::tibble())
  }

  # Check if results is a list of data frames or a single data frame
  if (is.data.frame(results)) {
    results <- list(results)
  }

  results <- dplyr::bind_rows(results)

  if (is.null(results)) {
    return(tibble::tibble())
  }

  if (nrow(results) == 0) {
    return(tibble::tibble())
  }

  # Create a mapping table for all possible entity_id values
  id_mapping <- apps_df %>%
    dplyr::mutate(
      app_id = as.character(.data$app_id),
      unified_id = as.character(.data$unified_id),
      ios_id = as.character(.data$ios_id),
      android_id = as.character(.data$android_id),
      both_id = ifelse(!is.na(.data$ios_id) & !is.na(.data$android_id),
        paste0(.data$ios_id, "_", .data$android_id),
        NA_character_
      ),
      ios_only = .data$ios_id,
      android_only = .data$android_id
    )

  # Create lookup for all possible entity_id formats
  lookup_df <- dplyr::bind_rows(
    id_mapping %>%
      dplyr::filter(!is.na(.data$both_id)) %>%
      dplyr::mutate(entity_id = .data$both_id) %>%
      dplyr::select(.data$entity_id, original_id = .data$app_id, dplyr::any_of(c("app_name", "unified_id"))),
    id_mapping %>%
      dplyr::filter(!is.na(.data$ios_only)) %>%
      dplyr::mutate(entity_id = .data$ios_only) %>%
      dplyr::select(.data$entity_id, original_id = .data$app_id, dplyr::any_of(c("app_name", "unified_id"))),
    id_mapping %>%
      dplyr::filter(!is.na(.data$android_only)) %>%
      dplyr::mutate(entity_id = .data$android_only) %>%
      dplyr::select(.data$entity_id, original_id = .data$app_id, dplyr::any_of(c("app_name", "unified_id"))),
    id_mapping %>%
      dplyr::mutate(entity_id = .data$unified_id) %>%
      dplyr::select(.data$entity_id, original_id = .data$app_id, dplyr::any_of(c("app_name", "unified_id")))
  ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      entity_id = as.character(.data$entity_id),
      original_id = as.character(.data$original_id)
    )

  # Merge results back with original app identifiers
  final_results <- results %>%
    dplyr::left_join(lookup_df, by = "entity_id")

  # Reorder and clean columns
  if (nrow(final_results) > 0) {
    final_results$app_id <- final_results$entity_id

    # Determine app_id_type
    final_results$app_id_type <- ifelse(grepl("_", final_results$app_id), "unified_pair",
      ifelse(grepl("^\\d+$", final_results$app_id), "ios",
        ifelse(grepl("\\.", final_results$app_id), "android", "unified")
      )
    )

    cols_to_keep <- c("original_id", "app_name", "app_id", "app_id_type", "date", "country", "metric", "value")
    existing_cols <- intersect(cols_to_keep, names(final_results))
    final_results <- final_results[, existing_cols]
  }

  final_results
}

fetch_active_users_batch <- function(group, group_name, metrics, countries, date_range, granularity, auth_token, verbose) {
  active_user_metrics <- intersect(metrics, c("dau", "wau", "mau"))
  if (length(active_user_metrics) == 0) {
    return(tibble::tibble())
  }

  if (verbose) message("  Fetching active user metrics (platform endpoints)...")

  # Map granularity to time_period for active users API
  time_period_map <- list(
    "daily" = "day",
    "weekly" = "week",
    "monthly" = "month",
    "quarterly" = "quarter"
  )
  default_time_period <- time_period_map[[granularity]]
  if (is.null(default_time_period)) default_time_period <- "month"

  # Helper to fetch one app's active users
  fetch_active_for_one <- function(platform_os, one_app_id, metric_name) {
    metric_time_period <- switch(metric_name,
      "dau" = "day",
      "wau" = "week",
      "mau" = "month",
      default_time_period
    )

    params <- list(
      app_ids = one_app_id,
      countries = countries,
      start_date = as.character(date_range$start_date),
      end_date = as.character(date_range$end_date),
      time_period = metric_time_period
    )

    active_processor <- function(resp, ...) {
      body_raw <- httr2::resp_body_raw(resp)
      if (length(body_raw) == 0) {
        return(NULL)
      }

      body_text <- rawToChar(body_raw)
      if (nchar(body_text) == 0 || body_text == "[]" || body_text == "{}") {
        return(NULL)
      }

      data <- jsonlite::fromJSON(body_text, flatten = TRUE)
      if (is.null(data) || length(data) == 0) {
        return(NULL)
      }

      df <- tryCatch(dplyr::bind_rows(data), error = function(e) NULL)
      if (is.null(df) || nrow(df) == 0) {
        return(NULL)
      }

      if (platform_os == "ios") {
        if (!"iphone_users" %in% names(df)) df$iphone_users <- 0
        if (!"ipad_users" %in% names(df)) df$ipad_users <- 0
        df <- df %>% dplyr::mutate(users = dplyr::coalesce(.data$iphone_users, 0) + dplyr::coalesce(.data$ipad_users, 0), .keep = "all")
      } else if (platform_os == "android") {
        if (!"users" %in% names(df) && "android_users" %in% names(df)) df <- df %>% dplyr::rename(users = .data$android_users)
      }

      if (!"date" %in% names(df)) {
        return(NULL)
      }
      df$date <- as.Date(df$date)

      df %>%
        dplyr::transmute(
          app_id = as.character(.data$app_id),
          date = .data$date,
          country = .data$country,
          metric = metric_name,
          value = .data$users
        )
    }

    fetch_data_core(
      endpoint = paste0(platform_os, "/usage/active_users"),
      params = params,
      auth_token = auth_token,
      processor = active_processor,
      verbose = verbose
    )
  }

  all_group_results <- list()

  if (group_name == "ios") {
    id_vec <- stats::na.omit(group$ios_id)
    work <- expand.grid(app_id = id_vec, metric = active_user_metrics, stringsAsFactors = FALSE)
    results_list <- lapply(seq_len(nrow(work)), function(idx) {
      fetch_active_for_one("ios", work$app_id[idx], work$metric[idx])
    })
    results_list <- results_list[!vapply(results_list, is.null, logical(1))]
    if (length(results_list) > 0) {
      active_df <- dplyr::bind_rows(results_list)
      active_df$entity_id <- active_df$app_id
      all_group_results[[1]] <- active_df
    }
  } else if (group_name == "android") {
    id_vec <- stats::na.omit(group$android_id)
    work <- expand.grid(app_id = id_vec, metric = active_user_metrics, stringsAsFactors = FALSE)
    results_list <- lapply(seq_len(nrow(work)), function(idx) {
      fetch_active_for_one("android", work$app_id[idx], work$metric[idx])
    })
    results_list <- results_list[!vapply(results_list, is.null, logical(1))]
    if (length(results_list) > 0) {
      active_df <- dplyr::bind_rows(results_list)
      active_df$entity_id <- active_df$app_id
      all_group_results[[1]] <- active_df
    }
  } else if (group_name == "both") {
    pair_results <- lapply(seq_len(nrow(group)), function(i) {
      ios_id <- group$ios_id[i]
      android_id <- group$android_id[i]
      pair_id <- paste0(ios_id, "_", android_id)

      per_metric <- lapply(active_user_metrics, function(m) {
        ios_df <- fetch_active_for_one("ios", ios_id, m)
        android_df <- fetch_active_for_one("android", android_id, m)

        if (is.null(ios_df) && is.null(android_df)) {
          return(NULL)
        }

        combined <- dplyr::bind_rows(
          dplyr::mutate(ios_df %||% tibble::tibble(app_id = character(), date = as.Date(character()), country = character(), metric = character(), value = numeric()), platform = "ios"),
          dplyr::mutate(android_df %||% tibble::tibble(app_id = character(), date = as.Date(character()), country = character(), metric = character(), value = numeric()), platform = "android")
        )

        if (nrow(combined) == 0) {
          return(NULL)
        }

        combined %>%
          dplyr::group_by(.data$date, .data$country, .data$metric) %>%
          dplyr::summarise(value = sum(.data$value, na.rm = TRUE), .groups = "drop") %>%
          dplyr::mutate(app_id = pair_id)
      })

      per_metric <- per_metric[!vapply(per_metric, is.null, logical(1))]
      if (length(per_metric) == 0) {
        return(NULL)
      }

      df <- dplyr::bind_rows(per_metric)
      df$entity_id <- pair_id
      df
    })
    pair_results <- pair_results[!vapply(pair_results, is.null, logical(1))]
    if (length(pair_results) > 0) {
      all_group_results[[1]] <- dplyr::bind_rows(pair_results)
    }
  }

  if (length(all_group_results) > 0) dplyr::bind_rows(all_group_results) else tibble::tibble()
}

fetch_sales_batch <- function(group, group_name, metrics, countries, date_range, granularity, auth_token, verbose, os) {
  revenue_download_metrics <- intersect(metrics, c("revenue", "downloads"))
  if (length(revenue_download_metrics) == 0) {
    return(tibble::tibble())
  }

  # Helper to process individual results
  process_individual <- function(idx, fetch_func) {
    result <- fetch_func(idx)
    if (is.null(result) || nrow(result) == 0) {
      return(NULL)
    }

    # Standardize columns
    if ("total_revenue" %in% names(result)) result$revenue <- result$total_revenue
    if ("total_downloads" %in% names(result)) result$downloads <- result$total_downloads
    if ("iphone_revenue" %in% names(result) && "ipad_revenue" %in% names(result)) result$revenue <- result$iphone_revenue + result$ipad_revenue
    if ("iphone_downloads" %in% names(result) && "ipad_downloads" %in% names(result)) result$downloads <- result$iphone_downloads + result$ipad_downloads

    if (!"revenue" %in% names(result)) result$revenue <- 0
    if (!"downloads" %in% names(result)) result$downloads <- 0L
    if (!"country" %in% names(result) && "c" %in% names(result)) result$country <- result$c

    result %>%
      dplyr::select(date, country, revenue, downloads) %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(intersect(revenue_download_metrics, c("revenue", "downloads"))),
        names_to = "metric",
        values_to = "value"
      )
  }

  if (group_name == "both") {
    # Fetch revenue/downloads for both platforms per app and sum
    results_list <- lapply(seq_len(nrow(group)), function(i) {
      ios_res <- tryCatch(
        {
          res <- st_sales_report(
            os = "ios",
            ios_app_id = group$ios_id[i],
            countries = countries,
            start_date = as.character(date_range$start_date),
            end_date = as.character(date_range$end_date),
            date_granularity = granularity,
            auth_token = auth_token,
            auto_segment = TRUE,
            verbose = FALSE
          )
          if (!is.null(res) && "app_id" %in% names(res)) res$app_id <- as.character(res$app_id)
          res
        },
        error = function(e) NULL
      )

      android_res <- tryCatch(
        {
          res <- st_sales_report(
            os = "android",
            android_app_id = group$android_id[i],
            countries = countries,
            start_date = as.character(date_range$start_date),
            end_date = as.character(date_range$end_date),
            date_granularity = granularity,
            auth_token = auth_token,
            auto_segment = TRUE,
            verbose = FALSE
          )
          if (!is.null(res) && "app_id" %in% names(res)) res$app_id <- as.character(res$app_id)
          res
        },
        error = function(e) NULL
      )

      combined <- dplyr::bind_rows(ios_res %||% tibble::tibble(), android_res %||% tibble::tibble())
      if (nrow(combined) == 0) {
        return(NULL)
      }

      combined %>%
        dplyr::group_by(.data$date, .data$country) %>%
        dplyr::summarise(
          revenue = sum(.data$revenue, na.rm = TRUE),
          downloads = sum(.data$downloads, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        tidyr::pivot_longer(cols = dplyr::all_of(intersect(revenue_download_metrics, c("revenue", "downloads"))), names_to = "metric", values_to = "value") %>%
        dplyr::mutate(entity_id = as.character(paste0(group$ios_id[i], "_", group$android_id[i])), app_id = entity_id)
    })
    return(dplyr::bind_rows(results_list))
  } else if (group_name == "ios") {
    results_list <- lapply(seq_len(nrow(group)), function(i) {
      res <- process_individual(i, function(idx) {
        tryCatch(
          {
            st_sales_report(
              os = "ios",
              ios_app_id = group$ios_id[idx],
              countries = countries,
              start_date = as.character(date_range$start_date),
              end_date = as.character(date_range$end_date),
              date_granularity = granularity,
              auth_token = auth_token,
              auto_segment = TRUE,
              verbose = FALSE
            )
          },
          error = function(e) NULL
        )
      })
      if (!is.null(res)) {
        res$entity_id <- as.character(group$ios_id[i])
        res$app_id <- as.character(group$ios_id[i])
      }
      res
    })
    return(dplyr::bind_rows(results_list))
  } else if (group_name == "android") {
    results_list <- lapply(seq_len(nrow(group)), function(i) {
      res <- process_individual(i, function(idx) {
        tryCatch(
          {
            st_sales_report(
              os = "android",
              android_app_id = group$android_id[idx],
              countries = countries,
              start_date = as.character(date_range$start_date),
              end_date = as.character(date_range$end_date),
              date_granularity = granularity,
              auth_token = auth_token,
              auto_segment = TRUE,
              verbose = FALSE
            )
          },
          error = function(e) NULL
        )
      })
      if (!is.null(res)) {
        res$entity_id <- as.character(group$android_id[i])
        res$app_id <- as.character(group$android_id[i])
      }
      res
    })
    return(dplyr::bind_rows(results_list))
  } else if (group_name == "unified") {
    # Fallback to st_metrics for unified IDs
    results_list <- lapply(seq_len(nrow(group)), function(i) {
      # Try to use fetch_unified_data if we have platform IDs resolved
      ios_id <- group$ios_id[i]
      android_id <- group$android_id[i]

      if (!is.na(ios_id) || !is.na(android_id)) {
        res <- fetch_unified_data(
          ios_app_id = if (!is.na(ios_id)) ios_id else NULL,
          android_app_id = if (!is.na(android_id)) android_id else NULL,
          start_date = date_range$start_date,
          end_date = date_range$end_date,
          countries = countries,
          date_granularity = granularity,
          auth_token = auth_token,
          verbose = FALSE,
          combine_to_unified = TRUE
        )
        if (nrow(res) > 0) {
          return(res %>%
            tidyr::pivot_longer(cols = dplyr::all_of(intersect(revenue_download_metrics, c("revenue", "downloads"))), names_to = "metric", values_to = "value") %>%
            dplyr::mutate(entity_id = as.character(group$unified_id[i]), app_id = entity_id))
        }
      }

      # Last resort: st_metrics with unified ID
      tryCatch(
        {
          res <- st_metrics(
            os = os,
            unified_app_id = group$unified_id[i],
            date_granularity = granularity,
            start_date = as.character(date_range$start_date),
            end_date = as.character(date_range$end_date),
            countries = countries,
            auth_token = auth_token,
            verbose = FALSE
          )
          if (nrow(res) > 0) {
            res %>%
              tidyr::pivot_longer(cols = dplyr::all_of(intersect(revenue_download_metrics, c("revenue", "downloads"))), names_to = "metric", values_to = "value") %>%
              dplyr::mutate(entity_id = as.character(group$unified_id[i]), app_id = entity_id)
          } else {
            NULL
          }
        },
        error = function(e) NULL
      )
    })
    return(dplyr::bind_rows(results_list))
  }

  return(tibble::tibble())
}

finalize_batch_results_internal <- function(results, apps_df) {
  if (length(results) == 0) {
    return(tibble::tibble())
  }

  # Check if results is a list of data frames or a single data frame
  if (is.data.frame(results)) {
    results <- list(results)
  }

  results <- dplyr::bind_rows(results)

  if (nrow(results) == 0) {
    return(tibble::tibble())
  }

  # Create a mapping table for all possible entity_id values
  id_mapping <- apps_df %>%
    dplyr::mutate(
      app_id = as.character(app_id),
      unified_id = as.character(unified_id),
      ios_id = as.character(ios_id),
      android_id = as.character(android_id),
      both_id = ifelse(!is.na(ios_id) & !is.na(android_id),
        paste0(ios_id, "_", android_id),
        NA_character_
      ),
      ios_only = ios_id,
      android_only = android_id
    )

  # Create lookup for all possible entity_id formats
  lookup_df <- dplyr::bind_rows(
    id_mapping %>%
      dplyr::filter(!is.na(both_id)) %>%
      dplyr::mutate(entity_id = both_id) %>%
      dplyr::select(entity_id, original_id = app_id, dplyr::any_of(c("app_name", "unified_id"))),
    id_mapping %>%
      dplyr::filter(!is.na(ios_only)) %>%
      dplyr::mutate(entity_id = ios_only) %>%
      dplyr::select(entity_id, original_id = app_id, dplyr::any_of(c("app_name", "unified_id"))),
    id_mapping %>%
      dplyr::filter(!is.na(android_only)) %>%
      dplyr::mutate(entity_id = android_only) %>%
      dplyr::select(entity_id, original_id = app_id, dplyr::any_of(c("app_name", "unified_id"))),
    id_mapping %>%
      dplyr::mutate(entity_id = unified_id) %>%
      dplyr::select(entity_id, original_id = app_id, dplyr::any_of(c("app_name", "unified_id")))
  ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      entity_id = as.character(entity_id),
      original_id = as.character(original_id)
    )

  # Merge results back with original app identifiers
  final_results <- results %>%
    dplyr::left_join(lookup_df, by = "entity_id")

  # Reorder and clean columns
  if (nrow(final_results) > 0) {
    final_results$app_id <- final_results$entity_id

    # Determine app_id_type
    final_results$app_id_type <- ifelse(grepl("_", final_results$app_id), "unified_pair",
      ifelse(grepl("^\\d+$", final_results$app_id), "ios",
        ifelse(grepl("\\.", final_results$app_id), "android", "unified")
      )
    )

    cols_to_keep <- c("original_id", "app_name", "app_id", "entity_id", "app_id_type", "date", "country", "metric", "value")
    existing_cols <- intersect(cols_to_keep, names(final_results))
    final_results <- final_results[, existing_cols]
  }

  final_results
}
