#' Batch Fetch Metrics for Multiple Apps
#'
#' Efficiently fetch metrics for multiple apps by batching API calls and
#' automatically handling platform-specific requirements. This function
#' minimizes API calls by grouping compatible requests.
#'
#' @param app_list List or data frame containing app information. Can be:
#'   - Character vector of app IDs
#'   - Data frame with columns: app_id, app_name (optional), platform (optional)
#'   - List of lists with app_id and optional metadata
#' @param metrics Character vector. Metrics to fetch (e.g., "revenue", "downloads", "dau")
#' @param date_range List with start_date and end_date, or "ytd" for year-to-date
#' @param countries Character vector. Country codes (default "WW")
#' @param granularity Character. Date granularity (default "monthly")
#' @param parallel Logical. Use parallel processing (default TRUE)
#' @param cache_dir Character. Directory for caching results (optional)
#' @param verbose Logical. Show progress messages (default TRUE)
#' @param auth_token Character string. Your Sensor Tower API authentication token.
#'
#' @return A tibble with all metrics for all apps, with columns:
#'   - app_id, app_name, platform, date, country, metric, value
#'
#' @examples
#' \dontrun{
#' # Simple usage with app IDs
#' apps <- c("553834731", "com.supercell.clashofclans", "5ba4585f539ce75b97db6bcb")
#' metrics <- st_batch_metrics(
#'   app_list = apps,
#'   metrics = c("revenue", "downloads"),
#'   date_range = list(start_date = "2025-01-01", end_date = "2025-06-30")
#' )
#' 
#' # With app metadata
#' app_df <- data.frame(
#'   app_id = c("553834731", "com.king.candycrushsaga"),
#'   app_name = c("Candy Crush iOS", "Candy Crush Android"),
#'   platform = c("ios", "android")
#' )
#' metrics <- st_batch_metrics(app_df, metrics = c("revenue", "dau"))
#' }
#' 
#' @importFrom dplyr bind_rows select distinct left_join %>%
#' @importFrom parallel mclapply detectCores
#' @importFrom stats na.omit
#' @importFrom rlang .data
#' @export
st_batch_metrics <- function(app_list,
                           metrics = c("revenue", "downloads"),
                           date_range = list(start_date = Sys.Date() - 90, 
                                           end_date = Sys.Date() - 1),
                           countries = "WW",
                           granularity = "monthly",
                           parallel = TRUE,
                           cache_dir = NULL,
                           verbose = TRUE,
                           auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN")) {
  
  # Validate auth token
  if (is.null(auth_token) || auth_token == "") {
    stop("Authentication token is required. Set SENSORTOWER_AUTH_TOKEN environment variable.")
  }
  
  # Step 1: Normalize app list
  if (verbose) message("Processing ", length(app_list), " apps...")
  
  apps_df <- normalize_app_list(app_list, auth_token, verbose)
  
  if (verbose) {
    message("Resolved apps:")
    message("  iOS apps: ", sum(!is.na(apps_df$ios_id)))
    message("  Android apps: ", sum(!is.na(apps_df$android_id)))
    message("  Unified only: ", sum(is.na(apps_df$ios_id) & is.na(apps_df$android_id)))
  }
  
  # Step 2: Group apps by fetch strategy
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
  
  # Step 3: Prepare fetch parameters
  if (identical(date_range, "ytd")) {
    # Year-to-date mode
    fetch_func <- function(group, group_name) {
      if (verbose) message("\nFetching YTD metrics for ", group_name, " group (", nrow(group), " apps)...")
      
      if (group_name == "both") {
        st_ytd_metrics(
          ios_app_id = group$ios_id,
          android_app_id = group$android_id,
          years = as.numeric(format(Sys.Date(), "%Y")),
          metrics = metrics,
          countries = countries,
          auth_token = auth_token,
          verbose = FALSE
        )
      } else if (group_name == "ios") {
        st_ytd_metrics(
          ios_app_id = group$ios_id,
          years = as.numeric(format(Sys.Date(), "%Y")),
          metrics = metrics,
          countries = countries,
          auth_token = auth_token,
          verbose = FALSE
        )
      } else if (group_name == "android") {
        st_ytd_metrics(
          android_app_id = group$android_id,
          years = as.numeric(format(Sys.Date(), "%Y")),
          metrics = metrics,
          countries = countries,
          auth_token = auth_token,
          verbose = FALSE
        )
      } else {
        # Unified - try each one with automatic fallback
        dplyr::bind_rows(lapply(seq_len(nrow(group)), function(i) {
          result <- st_ytd_metrics(
            unified_app_id = group$unified_id[i],
            years = as.numeric(format(Sys.Date(), "%Y")),
            metrics = metrics,
            countries = countries,
            auth_token = auth_token,
            verbose = FALSE
          )
          
          # Check if we got zero revenue/downloads - might need platform IDs
          if (nrow(result) > 0 && "revenue" %in% metrics) {
            revenue_sum <- sum(result$value[result$metric == "revenue"], na.rm = TRUE)
            if (revenue_sum == 0 && verbose) {
              message("  Warning: Unified ID ", group$unified_id[i], " returned zero revenue")
              
              # Try to resolve platform IDs
              lookup <- tryCatch({
                st_app_lookup(group$unified_id[i], auth_token = auth_token, verbose = FALSE)
              }, error = function(e) NULL)
              
              if (!is.null(lookup) && (!is.null(lookup$ios_app_id) || !is.null(lookup$android_app_id))) {
                message("  Retrying with platform IDs...")
                result <- st_ytd_metrics(
                  ios_app_id = lookup$ios_app_id,
                  android_app_id = lookup$android_app_id,
                  years = as.numeric(format(Sys.Date(), "%Y")),
                  metrics = metrics,
                  countries = countries,
                  auth_token = auth_token,
                  verbose = FALSE
                )
              }
            }
          }
          
          result
        }))
      }
    }
  } else {
    # Regular date range mode
    fetch_func <- function(group, group_name) {
      if (verbose) message("\nFetching metrics for ", group_name, " group (", nrow(group), " apps)...")
      
      # Check which metrics are requested
      revenue_download_metrics <- intersect(metrics, c("revenue", "downloads"))
      active_user_metrics <- intersect(metrics, c("dau", "wau", "mau"))
      
      if (group_name == "both") {
        # Fetch with both iOS and Android IDs
        dplyr::bind_rows(lapply(seq_len(nrow(group)), function(i) {
          all_results <- list()
          
          # Fetch revenue/downloads if requested
          if (length(revenue_download_metrics) > 0) {
            result <- st_metrics(
              ios_app_id = group$ios_id[i],
              android_app_id = group$android_id[i],
              date_granularity = granularity,
              start_date = as.character(date_range$start_date),
              end_date = as.character(date_range$end_date),
              countries = countries,
              auth_token = auth_token,
              verbose = FALSE
            )
            
            # Transform to long format for consistency
            if (!is.null(result) && nrow(result) > 0) {
              result <- result %>%
                tidyr::pivot_longer(
                  cols = dplyr::all_of(revenue_download_metrics),
                  names_to = "metric",
                  values_to = "value"
                ) %>%
                dplyr::mutate(entity_id = paste0(group$ios_id[i], "_", group$android_id[i]))
              all_results[[length(all_results) + 1]] <- result
            }
          }
          
          # For active user metrics, we need to aggregate by period
          # since st_ytd_metrics handles these differently
          if (length(active_user_metrics) > 0) {
            # Calculate period in months for determining which metric to use
            period_days <- as.numeric(as.Date(date_range$end_date) - as.Date(date_range$start_date))
            
            # Use st_ytd_metrics approach but for custom date range
            ytd_result <- st_ytd_metrics(
              ios_app_id = group$ios_id[i],
              android_app_id = group$android_id[i],
              years = unique(as.numeric(format(c(as.Date(date_range$start_date), as.Date(date_range$end_date)), "%Y"))),
              period_start = format(as.Date(date_range$start_date), "%m-%d"),
              period_end = format(as.Date(date_range$end_date), "%m-%d"),
              metrics = active_user_metrics,
              countries = countries,
              auth_token = auth_token,
              verbose = FALSE
            )
            
            if (!is.null(ytd_result) && nrow(ytd_result) > 0) {
              # Transform YTD result to match expected format
              au_result <- ytd_result %>%
                dplyr::mutate(
                  date = as.Date(date_start),
                  entity_id = paste0(group$ios_id[i], "_", group$android_id[i])
                ) %>%
                dplyr::select(date, entity_id, country, metric, value)
              all_results[[length(all_results) + 1]] <- au_result
            }
          }
          
          dplyr::bind_rows(all_results)
        }))
        
      } else if (group_name == "ios") {
        # iOS only apps
        dplyr::bind_rows(lapply(seq_len(nrow(group)), function(i) {
          all_results <- list()
          
          # Fetch revenue/downloads if requested
          if (length(revenue_download_metrics) > 0) {
            result <- st_metrics(
              ios_app_id = group$ios_id[i],
              date_granularity = granularity,
              start_date = as.character(date_range$start_date),
              end_date = as.character(date_range$end_date),
              countries = countries,
              auth_token = auth_token,
              verbose = FALSE
            )
            
            # Transform to long format
            if (!is.null(result) && nrow(result) > 0) {
              result <- result %>%
                tidyr::pivot_longer(
                  cols = dplyr::all_of(intersect(revenue_download_metrics, names(result))),
                  names_to = "metric",
                  values_to = "value"
                ) %>%
                dplyr::mutate(entity_id = group$ios_id[i])
              all_results[[length(all_results) + 1]] <- result
            }
          }
          
          # Fetch active user metrics if requested
          if (length(active_user_metrics) > 0) {
            ytd_result <- st_ytd_metrics(
              ios_app_id = group$ios_id[i],
              years = unique(as.numeric(format(c(as.Date(date_range$start_date), as.Date(date_range$end_date)), "%Y"))),
              period_start = format(as.Date(date_range$start_date), "%m-%d"),
              period_end = format(as.Date(date_range$end_date), "%m-%d"),
              metrics = active_user_metrics,
              countries = countries,
              auth_token = auth_token,
              verbose = FALSE
            )
            
            if (!is.null(ytd_result) && nrow(ytd_result) > 0) {
              au_result <- ytd_result %>%
                dplyr::mutate(
                  date = as.Date(date_start),
                  entity_id = group$ios_id[i]
                ) %>%
                dplyr::select(date, entity_id, country, metric, value)
              all_results[[length(all_results) + 1]] <- au_result
            }
          }
          
          dplyr::bind_rows(all_results)
        }))
        
      } else if (group_name == "android") {
        # Android only apps
        dplyr::bind_rows(lapply(seq_len(nrow(group)), function(i) {
          all_results <- list()
          
          # Fetch revenue/downloads if requested
          if (length(revenue_download_metrics) > 0) {
            result <- st_metrics(
              android_app_id = group$android_id[i],
              date_granularity = granularity,
              start_date = as.character(date_range$start_date),
              end_date = as.character(date_range$end_date),
              countries = countries,
              auth_token = auth_token,
              verbose = FALSE
            )
            
            # Transform to long format
            if (!is.null(result) && nrow(result) > 0) {
              result <- result %>%
                tidyr::pivot_longer(
                  cols = dplyr::all_of(intersect(revenue_download_metrics, names(result))),
                  names_to = "metric",
                  values_to = "value"
                ) %>%
                dplyr::mutate(entity_id = group$android_id[i])
              all_results[[length(all_results) + 1]] <- result
            }
          }
          
          # Fetch active user metrics if requested
          if (length(active_user_metrics) > 0) {
            ytd_result <- st_ytd_metrics(
              android_app_id = group$android_id[i],
              years = unique(as.numeric(format(c(as.Date(date_range$start_date), as.Date(date_range$end_date)), "%Y"))),
              period_start = format(as.Date(date_range$start_date), "%m-%d"),
              period_end = format(as.Date(date_range$end_date), "%m-%d"),
              metrics = active_user_metrics,
              countries = countries,
              auth_token = auth_token,
              verbose = FALSE
            )
            
            if (!is.null(ytd_result) && nrow(ytd_result) > 0) {
              au_result <- ytd_result %>%
                dplyr::mutate(
                  date = as.Date(date_start),
                  entity_id = group$android_id[i]
                ) %>%
                dplyr::select(date, entity_id, country, metric, value)
              all_results[[length(all_results) + 1]] <- au_result
            }
          }
          
          dplyr::bind_rows(all_results)
        }))
        
      } else {
        # Unified - use unified ID with st_metrics
        dplyr::bind_rows(lapply(seq_len(nrow(group)), function(i) {
          all_results <- list()
          
          # Fetch revenue/downloads if requested
          if (length(revenue_download_metrics) > 0) {
            result <- st_metrics(
              app_id = group$unified_id[i],
              date_granularity = granularity,
              start_date = as.character(date_range$start_date),
              end_date = as.character(date_range$end_date),
              countries = countries,
              auth_token = auth_token,
              verbose = FALSE
            )
            
            # Transform to long format
            if (!is.null(result) && nrow(result) > 0) {
              result <- result %>%
                tidyr::pivot_longer(
                  cols = dplyr::all_of(intersect(revenue_download_metrics, names(result))),
                  names_to = "metric",
                  values_to = "value"
                ) %>%
                dplyr::mutate(entity_id = group$unified_id[i])
              all_results[[length(all_results) + 1]] <- result
            }
          }
          
          # Fetch active user metrics if requested
          if (length(active_user_metrics) > 0) {
            ytd_result <- st_ytd_metrics(
              unified_app_id = group$unified_id[i],
              years = unique(as.numeric(format(c(as.Date(date_range$start_date), as.Date(date_range$end_date)), "%Y"))),
              period_start = format(as.Date(date_range$start_date), "%m-%d"),
              period_end = format(as.Date(date_range$end_date), "%m-%d"),
              metrics = active_user_metrics,
              countries = countries,
              auth_token = auth_token,
              verbose = FALSE
            )
            
            if (!is.null(ytd_result) && nrow(ytd_result) > 0) {
              au_result <- ytd_result %>%
                dplyr::mutate(
                  date = as.Date(date_start),
                  entity_id = group$unified_id[i]
                ) %>%
                dplyr::select(date, entity_id, country, metric, value)
              all_results[[length(all_results) + 1]] <- au_result
            }
          }
          
          dplyr::bind_rows(all_results)
        }))
      }
    }
  }
  
  # Step 4: Execute fetches (with optional parallelization)
  if (parallel && length(fetch_groups) > 1) {
    if (verbose) message("\nUsing parallel processing...")
    
    # Simple parallel execution
    results <- parallel::mclapply(
      names(fetch_groups),
      function(group_name) {
        result <- fetch_func(fetch_groups[[group_name]], group_name)
        # Ensure app_id is character
        if (!is.null(result) && "app_id" %in% names(result)) {
          result$app_id <- as.character(result$app_id)
        }
        result
      },
      mc.cores = min(length(fetch_groups), parallel::detectCores() - 1)
    )
    
    all_results <- dplyr::bind_rows(results)
  } else {
    # Sequential execution
    results_list <- lapply(
      names(fetch_groups),
      function(group_name) {
        result <- fetch_func(fetch_groups[[group_name]], group_name)
        # Ensure app_id is character
        if (!is.null(result) && "app_id" %in% names(result)) {
          result$app_id <- as.character(result$app_id)
        }
        result
      }
    )
    
    all_results <- dplyr::bind_rows(results_list)
  }
  
  # Step 5: Add mapping back to original IDs
  # Create a mapping table for all possible entity_id values
  id_mapping <- apps_df %>%
    dplyr::mutate(
      # For apps with both platforms, entity_id will be ios_android
      both_id = ifelse(!is.na(.data$ios_id) & !is.na(.data$android_id), 
                       paste0(.data$ios_id, "_", .data$android_id), 
                       NA),
      # Keep individual platform IDs
      ios_only = .data$ios_id,
      android_only = .data$android_id
    ) %>%
    dplyr::select(original_id = .data$app_id, unified_id = .data$unified_id, both_id = .data$both_id, ios_only = .data$ios_only, android_only = .data$android_only, everything())
  
  # Create lookup for all possible entity_id formats
  lookup_df <- dplyr::bind_rows(
    # Both platforms
    id_mapping %>% 
      dplyr::filter(!is.na(.data$both_id)) %>%
      dplyr::select(entity_id = .data$both_id, original_id = .data$original_id, app_name = .data$app_name),
    # iOS only
    id_mapping %>% 
      dplyr::filter(!is.na(.data$ios_only)) %>%
      dplyr::select(entity_id = .data$ios_only, original_id = .data$original_id, app_name = .data$app_name),
    # Android only  
    id_mapping %>% 
      dplyr::filter(!is.na(.data$android_only)) %>%
      dplyr::select(entity_id = .data$android_only, original_id = .data$original_id, app_name = .data$app_name),
    # Unified ID (unchanged)
    id_mapping %>%
      dplyr::select(entity_id = .data$unified_id, original_id = .data$original_id, app_name = .data$app_name)
  ) %>%
    dplyr::distinct(.data$entity_id, .data$original_id, .data$app_name)
  
  # Merge results back with original app identifiers
  all_results <- all_results %>%
    dplyr::left_join(lookup_df, by = "entity_id")

  # Reorder and clean columns
  if (nrow(all_results) > 0) {
    all_results <- all_results %>%
      dplyr::select(.data$original_id, .data$app_name, everything(), -.data$entity_id) %>%
      dplyr::arrange(.data$original_id)
  }
  
  if (verbose) {
    message("\nBatch fetch complete!")
    message("Total records: ", nrow(all_results))
    message("Unique apps: ", length(unique(all_results$entity_id)))
  }
  
  return(all_results)
}

# Helper function to normalize app list input
normalize_app_list <- function(app_list, auth_token, verbose) {
  
  # Handle different input types
  if (is.character(app_list)) {
    # Simple character vector
    apps_df <- data.frame(
      app_id = app_list,
      stringsAsFactors = FALSE
    )
  } else if (is.data.frame(app_list)) {
    apps_df <- app_list
  } else if (is.list(app_list)) {
    # List of lists
    apps_df <- dplyr::bind_rows(app_list)
  } else {
    stop("app_list must be a character vector, data frame, or list")
  }
  
  # Ensure we have app_id column
  if (!"app_id" %in% names(apps_df)) {
    stop("app_list must contain 'app_id' column or be a character vector")
  }
  
  # Add unified_id column
  apps_df$unified_id <- apps_df$app_id
  
  # Detect and resolve platform IDs
  apps_df$ios_id <- NA_character_
  apps_df$android_id <- NA_character_
  
  for (i in seq_len(nrow(apps_df))) {
    id <- apps_df$app_id[i]
    
    # Quick detection
    if (grepl("^\\d+$", id)) {
      apps_df$ios_id[i] <- id
    } else if (grepl("^(com|net|org|io)\\.", id)) {
      apps_df$android_id[i] <- id
    } else if (grepl("^[a-f0-9]{24}$", id)) {
      # Hex ID - try to resolve
      if (verbose) message("  Resolving hex ID: ", id)
      lookup <- tryCatch({
        st_app_lookup(id, auth_token = auth_token, verbose = FALSE)
      }, error = function(e) NULL)
      
      if (!is.null(lookup)) {
        apps_df$ios_id[i] <- lookup$ios_app_id
        apps_df$android_id[i] <- lookup$android_app_id
        # Initialize app_name column if it doesn't exist
        if (!"app_name" %in% names(apps_df)) {
          apps_df$app_name <- NA_character_
        }
        if (!is.null(lookup$app_name)) {
          apps_df$app_name[i] <- lookup$app_name
        }
      }
    }
  }
  
  return(apps_df)
}