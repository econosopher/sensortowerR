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
#' @param parallel Logical. Use parallel processing (default TRUE)
#' @param cache_dir Character. Directory for caching results (optional)
#' @param verbose Logical. Show progress messages (default TRUE)
#' @param auth_token Character string. Your Sensor Tower API authentication token.
#'
#' @return A tibble with all metrics for all apps, with columns:
#'   - original_id: The app ID as provided in the input
#'   - app_name: App name (if available)
#'   - app_id: The app ID used for the API call (based on OS parameter)
#'   - app_id_type: Type of app_id returned ("ios", "android", or "unified")
#'   - date, country, metric, value: Metric data
#'
#' @details 
#' Active user metrics (DAU, WAU, MAU) are fetched using batch API calls for
#' efficiency. When fetching active users for many apps (>10), the function
#' will display a warning about potential rate limits.
#' 
#' The function automatically maps the granularity parameter to the appropriate
#' time period for active user metrics:
#' - DAU requires daily data
#' - WAU requires weekly data  
#' - MAU requires monthly data
#'
#' @examples
#' \dontrun{
#' # Simple usage with app IDs
#' apps <- c("553834731", "com.supercell.clashofclans", "5ba4585f539ce75b97db6bcb")
#' metrics <- st_batch_metrics(
#'   os = "unified",
#'   app_list = apps,
#'   metrics = c("revenue", "downloads"),
#'   date_range = list(start_date = "2025-01-01", end_date = "2025-06-30"),
#'   countries = "WW"
#' )
#' 
#' # With app metadata
#' app_df <- data.frame(
#'   app_id = c("553834731", "com.king.candycrushsaga"),
#'   app_name = c("Candy Crush iOS", "Candy Crush Android"),
#'   platform = c("ios", "android")
#' )
#' metrics <- st_batch_metrics(
#'   os = "unified",
#'   app_list = app_df,
#'   metrics = c("revenue", "downloads"),
#'   countries = "US"
#' )
#' }
#' 
#' @importFrom dplyr bind_rows select distinct left_join %>%
#' @importFrom parallel mclapply detectCores
#' @importFrom stats na.omit
#' @importFrom rlang .data
#' @importFrom tidyr pivot_longer
#' @export
st_batch_metrics <- function(os,
                           app_list,
                           metrics = c("revenue", "downloads"),
                           date_range = list(start_date = Sys.Date() - 90, 
                                           end_date = Sys.Date() - 1),
                           countries,
                           granularity = "monthly",
                           parallel = TRUE,
                           cache_dir = NULL,
                           verbose = TRUE,
                           auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN")) {
  
  # Validate OS parameter
  if (missing(os) || is.null(os) || !os %in% c("ios", "android", "unified")) {
    stop("'os' parameter is required and must be one of: 'ios', 'android', or 'unified'")
  }
  
  # Validate auth token
  if (is.null(auth_token) || auth_token == "") {
    stop("Authentication token is required. Set SENSORTOWER_AUTH_TOKEN environment variable.")
  }
  
  # Validate countries parameter
  if (missing(countries) || is.null(countries)) {
    stop("'countries' parameter is required")
  }
  
  # Step 1: Normalize app list
  if (verbose) message("Processing ", length(app_list), " apps...")
  
  apps_df <- normalize_app_list(app_list, os, auth_token, verbose)
  
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
    # Year-to-date mode is no longer supported
    stop("YTD mode is no longer supported in st_batch_metrics. The underlying st_ytd_metrics function has been removed. Please specify explicit start_date and end_date in the date_range parameter.")
    
    # Original code commented out:
    # fetch_func <- function(group, group_name) {
    #   if (verbose) message("\nFetching YTD metrics for ", group_name, " group (", nrow(group), " apps)...")
    #   
    #   if (group_name == "both") {
    #     st_ytd_metrics(
    #       os = os,
    #       ios_app_id = group$ios_id,
    #       android_app_id = group$android_id,
    #       end_dates = Sys.Date(),
    #       metrics = metrics,
    #       countries = countries,
    #       auth_token = auth_token,
    #       verbose = FALSE
    #     )
    #   } else if (group_name == "ios") {
    #     st_ytd_metrics(
    #       os = os,
    #       ios_app_id = group$ios_id,
    #       end_dates = Sys.Date(),
    #       metrics = metrics,
    #       countries = countries,
    #       auth_token = auth_token,
    #       verbose = FALSE
    #     )
    #   } else if (group_name == "android") {
    #     st_ytd_metrics(
    #       os = os,
    #       android_app_id = group$android_id,
    #       end_dates = Sys.Date(),
    #       metrics = metrics,
    #       countries = countries,
    #       auth_token = auth_token,
    #       verbose = FALSE
    #     )
    #   } else {
    #     # Unified - try each one with automatic fallback
    #     dplyr::bind_rows(lapply(seq_len(nrow(group)), function(i) {
    #       result <- st_ytd_metrics(
    #         os = os,
    #         unified_app_id = group$unified_id[i],
    #         end_dates = Sys.Date(),
    #         metrics = metrics,
    #         countries = countries,
    #         auth_token = auth_token,
    #         verbose = FALSE
    #       )
    #       
    #       # Check if we got zero revenue/downloads - might need platform IDs
    #       if (nrow(result) > 0 && "revenue" %in% metrics) {
    #         revenue_sum <- sum(result$value[result$metric == "revenue"], na.rm = TRUE)
    #         if (revenue_sum == 0 && verbose) {
    #           message("  Warning: Unified ID ", group$unified_id[i], " returned zero revenue")
    #           
    #           # Try to resolve platform IDs
    #           lookup <- tryCatch({
    #             st_app_lookup(group$unified_id[i], auth_token = auth_token, verbose = FALSE)
    #           }, error = function(e) NULL)
    #           
    #           if (!is.null(lookup) && (!is.null(lookup$ios_app_id) || !is.null(lookup$android_app_id))) {
    #             message("  Retrying with platform IDs...")
    #             result <- st_ytd_metrics(
    #               os = os,
    #               ios_app_id = lookup$ios_app_id,
    #               android_app_id = lookup$android_app_id,
    #               end_dates = Sys.Date(),
    #               metrics = metrics,
    #               countries = countries,
    #               auth_token = auth_token,
    #               verbose = FALSE
    #             )
    #           }
    #         }
    #       }
    #       
    #       result
    #     }))
    #   }
    # }
  } else {
    # Regular date range mode
    fetch_func <- function(group, group_name) {
      if (verbose) message("\nFetching metrics for ", group_name, " group (", nrow(group), " apps)...")
      
      # Check which metrics are requested
      revenue_download_metrics <- intersect(metrics, c("revenue", "downloads"))
      active_user_metrics <- intersect(metrics, c("dau", "wau", "mau"))
      
      all_group_results <- list()
      
      # Handle active user metrics separately as batch for the entire group
      if (length(active_user_metrics) > 0 && group_name != "both") {
        if (verbose) message("  Fetching active user metrics in batch...")
        
        # Warn about potential rate limits for large numbers of apps
        if (nrow(group) > 10 && verbose) {
          message("  ⚠️  Warning: Fetching active users for ", nrow(group), " apps.")
          message("     Consider smaller batches to avoid rate limits.")
        }
        
        # Map granularity to time_period for active users API
        time_period_map <- list(
          "daily" = "day",
          "weekly" = "week", 
          "monthly" = "month",
          "quarterly" = "quarter"
        )
        time_period <- time_period_map[[granularity]]
        if (is.null(time_period)) time_period <- "month"
        
        # Get appropriate app IDs based on group
        if (group_name == "ios") {
          app_ids <- group$ios_id
        } else if (group_name == "android") {
          app_ids <- group$android_id
        } else if (group_name == "unified") {
          app_ids <- group$unified_id
        }
        
        # Fetch active user metrics using the API directly
        if (TRUE) {  # Always execute inline
          for (metric in active_user_metrics) {
            # Map metric to time period
            metric_time_period <- switch(metric,
              "dau" = "day",
              "wau" = "week",
              "mau" = "month",
              time_period  # fallback
            )
            
            # Build active users API request
            active_result <- tryCatch({
              # Construct the active users endpoint
              base_url <- paste0("https://api.sensortower.com/v1/", 
                                if (group_name == "unified") "unified" else group_name, 
                                "/usage/active_users")
              
              # Make the request
              req <- httr2::request(base_url) %>%
                httr2::req_url_query(
                  app_ids = paste(app_ids, collapse = ","),
                  countries = countries,
                  start_date = as.character(date_range$start_date),
                  end_date = as.character(date_range$end_date),
                  time_period = metric_time_period,
                  auth_token = auth_token
                )
              
              response <- httr2::req_perform(req)
              
              if (httr2::resp_status(response) == 200) {
                data <- httr2::resp_body_json(response)
                
                if (!is.null(data) && length(data) > 0) {
                  # Convert to data frame
                  result <- dplyr::bind_rows(data)
                  
                  # Standardize column names based on OS
                  if (group_name == "ios") {
                    result <- result %>%
                      dplyr::mutate(
                        users = iphone_users + ipad_users,
                        .keep = "all"
                      )
                  } else if (group_name == "android") {
                    result <- result %>%
                      dplyr::rename(users = android_users)
                  } else {
                    # Unified - sum all platforms
                    result <- result %>%
                      dplyr::mutate(
                        users = dplyr::coalesce(android_users, 0) + 
                                dplyr::coalesce(iphone_users, 0) + 
                                dplyr::coalesce(ipad_users, 0),
                        .keep = "all"
                      )
                  }
                  
                  # Convert date and reshape to long format
                  result$date <- as.Date(result$date)
                  
                  result <- result %>%
                    dplyr::select(app_id, date, country, users) %>%
                    dplyr::mutate(
                      app_id = as.character(app_id),
                      metric = metric,
                      value = users
                    ) %>%
                    dplyr::select(-users)
                  
                  if (verbose) {
                    message("    Retrieved ", nrow(result), " ", metric, " records")
                  }
                  
                  result
                } else {
                  NULL
                }
              } else {
                if (verbose) message("    API request failed with status: ", httr2::resp_status(response))
                NULL
              }
            }, error = function(e) {
              if (verbose) message("    Warning: Failed to fetch ", metric, " - ", e$message)
              NULL
            })
            
            if (!is.null(active_result) && nrow(active_result) > 0) {
              # Add entity_id for consistency
              active_result$entity_id <- as.character(active_result$app_id)
              all_group_results[[length(all_group_results) + 1]] <- active_result
            }
          }
        } else {
          if (verbose) {
            message("  Note: Active user metrics support not available. Skipping DAU/WAU/MAU.")
          }
        }
      }
      
      if (group_name == "both") {
        # Fetch with both iOS and Android IDs
        dplyr::bind_rows(lapply(seq_len(nrow(group)), function(i) {
          all_results <- list()
          
          # Fetch revenue/downloads if requested
          if (length(revenue_download_metrics) > 0) {
            result <- st_metrics(
              os = os,
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
                dplyr::mutate(entity_id = as.character(paste0(group$ios_id[i], "_", group$android_id[i])))
              all_results[[length(all_results) + 1]] <- result
            }
          }
          
          # For active user metrics, handle them separately
          # Active users are fetched at group level, not individual app level
          
          if (length(all_results) > 0) {
            dplyr::bind_rows(all_results)
          } else {
            tibble::tibble(entity_id = character())
          }
        }))
        
      } else if (group_name == "ios") {
        # iOS only apps
        if (verbose) message("  Processing ", nrow(group), " iOS apps")
        
        # Process individual apps for revenue/downloads
        individual_results <- dplyr::bind_rows(lapply(seq_len(nrow(group)), function(i) {
          if (verbose) message("  Processing app ", i, " of ", nrow(group), ": ", group$ios_id[i])
          all_results <- list()
          
          # Fetch revenue/downloads if requested
          if (length(revenue_download_metrics) > 0) {
            if (verbose) message("    Fetching metrics: ", paste(revenue_download_metrics, collapse = ", "))
            result <- st_metrics(
              os = os,
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
              if (verbose) {
                message("  Got ", nrow(result), " rows from st_metrics")
                message("  Columns: ", paste(names(result), collapse = ", "))
                message("  revenue_download_metrics: ", paste(revenue_download_metrics, collapse = ", "))
                message("  Intersection: ", paste(intersect(revenue_download_metrics, names(result)), collapse = ", "))
              }
              result <- result %>%
                tidyr::pivot_longer(
                  cols = dplyr::all_of(intersect(revenue_download_metrics, names(result))),
                  names_to = "metric",
                  values_to = "value"
                ) %>%
                dplyr::mutate(entity_id = as.character(group$ios_id[i]))
              all_results[[length(all_results) + 1]] <- result
            }
          }
          
          # For active user metrics, handle them separately
          # Active users are fetched at group level, not individual app level
          
          if (length(all_results) > 0) {
            dplyr::bind_rows(all_results)
          } else {
            tibble::tibble(entity_id = character())
          }
        }))
        
        # Combine batch active user results with individual revenue/download results
        dplyr::bind_rows(c(all_group_results, list(individual_results)))
        
      } else if (group_name == "android") {
        # Android only apps
        if (verbose) message("  Processing ", nrow(group), " Android apps")
        
        # Process individual apps for revenue/downloads
        individual_results <- dplyr::bind_rows(lapply(seq_len(nrow(group)), function(i) {
          all_results <- list()
          
          # Fetch revenue/downloads if requested
          if (length(revenue_download_metrics) > 0) {
            result <- st_metrics(
              os = os,
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
                dplyr::mutate(entity_id = as.character(group$android_id[i]))
              all_results[[length(all_results) + 1]] <- result
            }
          }
          
          # For active user metrics, handle them separately
          # Active users are fetched at group level, not individual app level
          
          if (length(all_results) > 0) {
            dplyr::bind_rows(all_results)
          } else {
            tibble::tibble(entity_id = character())
          }
        }))
        
        # Combine batch active user results with individual revenue/download results
        dplyr::bind_rows(c(all_group_results, list(individual_results)))
        
      } else {
        # Unified - use unified ID with st_metrics
        if (verbose) message("  Processing ", nrow(group), " unified apps")
        
        # Process individual apps for revenue/downloads
        individual_results <- dplyr::bind_rows(lapply(seq_len(nrow(group)), function(i) {
          all_results <- list()
          
          # Fetch revenue/downloads if requested
          if (length(revenue_download_metrics) > 0) {
            result <- st_metrics(
              os = os,
              unified_app_id = group$unified_id[i],
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
                dplyr::mutate(entity_id = as.character(group$unified_id[i]))
              all_results[[length(all_results) + 1]] <- result
            }
          }
          
          # For active user metrics, handle them separately
          # Active users are fetched at group level, not individual app level
          
          if (length(all_results) > 0) {
            dplyr::bind_rows(all_results)
          } else {
            tibble::tibble(entity_id = character())
          }
        }))
        
        # Combine batch active user results with individual revenue/download results
        dplyr::bind_rows(c(all_group_results, list(individual_results)))
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
      # Ensure all IDs are character type
      app_id = as.character(app_id),
      unified_id = as.character(unified_id),
      ios_id = as.character(ios_id),
      android_id = as.character(android_id),
      # For apps with both platforms, entity_id will be ios_android
      both_id = ifelse(!is.na(ios_id) & !is.na(android_id), 
                       paste0(ios_id, "_", android_id), 
                       NA_character_),
      # Keep individual platform IDs
      ios_only = as.character(ios_id),
      android_only = as.character(android_id)
    ) %>%
    dplyr::select(original_id = app_id, unified_id = unified_id, both_id = both_id, ios_only = ios_only, android_only = android_only, everything())
  
  # Create lookup for all possible entity_id formats
  # Check if app_name column exists
  has_app_name <- "app_name" %in% names(id_mapping)
  
  if (has_app_name) {
    lookup_df <- dplyr::bind_rows(
      # Both platforms
      id_mapping %>% 
        dplyr::filter(!is.na(both_id)) %>%
        dplyr::select(entity_id = both_id, original_id, app_name),
      # iOS only
      id_mapping %>% 
        dplyr::filter(!is.na(ios_only)) %>%
        dplyr::select(entity_id = ios_only, original_id, app_name),
      # Android only  
      id_mapping %>% 
        dplyr::filter(!is.na(android_only)) %>%
        dplyr::select(entity_id = android_only, original_id, app_name),
      # Unified ID (unchanged)
      id_mapping %>%
        dplyr::select(entity_id = unified_id, original_id, app_name)
    ) %>%
      dplyr::distinct(entity_id, original_id, app_name) %>%
      dplyr::mutate(entity_id = as.character(entity_id), original_id = as.character(original_id))
  } else {
    lookup_df <- dplyr::bind_rows(
      # Both platforms
      id_mapping %>% 
        dplyr::filter(!is.na(both_id)) %>%
        dplyr::select(entity_id = both_id, original_id),
      # iOS only
      id_mapping %>% 
        dplyr::filter(!is.na(ios_only)) %>%
        dplyr::select(entity_id = ios_only, original_id),
      # Android only  
      id_mapping %>% 
        dplyr::filter(!is.na(android_only)) %>%
        dplyr::select(entity_id = android_only, original_id),
      # Unified ID (unchanged)
      id_mapping %>%
        dplyr::select(entity_id = unified_id, original_id)
    ) %>%
      dplyr::distinct(entity_id, original_id) %>%
      dplyr::mutate(entity_id = as.character(entity_id), original_id = as.character(original_id))
  }
  
  # Merge results back with original app identifiers
  all_results <- all_results %>%
    dplyr::left_join(lookup_df, by = "entity_id")

  # Reorder and clean columns
  if (nrow(all_results) > 0) {
    # Add app_id and app_id_type columns based on OS parameter
    all_results <- all_results %>%
      dplyr::mutate(
        app_id = entity_id,  # Use entity_id as app_id for now
        app_id_type = dplyr::case_when(
          os == "ios" ~ "ios",
          os == "android" ~ "android",
          os == "unified" ~ "unified",
          TRUE ~ NA_character_
        )
      ) %>%
      dplyr::select(original_id, dplyr::any_of("app_name"), app_id, app_id_type, 
                    dplyr::everything(), -entity_id) %>%
      dplyr::arrange(original_id)
  }
  
  if (verbose) {
    message("\nBatch fetch complete!")
    message("Total records: ", nrow(all_results))
    message("Unique apps: ", length(unique(all_results$entity_id)))
  }
  
  return(all_results)
}

# Helper function to normalize app list input
normalize_app_list <- function(app_list, os, auth_token, verbose) {
  
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
  
  # Ensure app_name column exists
  if (!"app_name" %in% names(apps_df)) {
    apps_df$app_name <- NA_character_
  }
  
  # Detect and resolve platform IDs
  apps_df$ios_id <- NA_character_
  apps_df$android_id <- NA_character_
  
  # Load the resolve_ids_for_os function if available
  if (exists("resolve_ids_for_os", mode = "function")) {
    # Use centralized ID resolution for each app
    for (i in seq_len(nrow(apps_df))) {
      id <- apps_df$app_id[i]
      
      # Quick detection of ID type
      unified_id <- NULL
      ios_id <- NULL
      android_id <- NULL
      
      if (grepl("^\\d+$", id)) {
        ios_id <- id
      } else if (grepl("^(com|net|org|io)\\.", id)) {
        android_id <- id
      } else if (grepl("^[a-f0-9]{24}$", id)) {
        unified_id <- id
      }
      
      # Resolve IDs based on OS
      resolved <- resolve_ids_for_os(
        unified_app_id = unified_id,
        ios_app_id = ios_id,
        android_app_id = android_id,
        os = os,
        auth_token = auth_token,
        verbose = FALSE
      )
      
      if (!is.null(resolved$resolved_ids)) {
        # Store the resolved IDs based on OS
        if (os == "ios" && !is.null(resolved$resolved_ids$ios_app_id)) {
          apps_df$ios_id[i] <- resolved$resolved_ids$ios_app_id
        } else if (os == "android" && !is.null(resolved$resolved_ids$android_app_id)) {
          apps_df$android_id[i] <- resolved$resolved_ids$android_app_id
        } else if (os == "unified") {
          # For unified, we need both platform IDs
          if (!is.null(resolved$resolved_ids$ios_app_id)) {
            apps_df$ios_id[i] <- resolved$resolved_ids$ios_app_id
          }
          if (!is.null(resolved$resolved_ids$android_app_id)) {
            apps_df$android_id[i] <- resolved$resolved_ids$android_app_id
          }
        }
      }
    }
  } else {
    # Fallback to original logic if resolve_ids_for_os is not available
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
  }
  
  return(apps_df)
}