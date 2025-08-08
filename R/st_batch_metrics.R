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
                           granularity,
                           parallel = TRUE,
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
  
  # Validate granularity explicitly
  if (missing(granularity) || is.null(granularity)) {
    stop("'granularity' parameter is required. Specify one of: 'daily', 'weekly', 'monthly', 'quarterly'.")
  }
  
  # Publisher mode: fetch publisher-level revenue/downloads (early return)
  if (!is.null(publisher_ids)) {
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
      tryCatch({
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
      }, error = function(e) NULL)
    }
    results <- lapply(seq_along(publisher_ids), function(i) {
      pid <- publisher_ids[i]
      if (os == "ios") {
        ios_res <- fetch_publisher_platform("ios", pid)
        df <- ios_res
      } else if (os == "android") {
        and_res <- fetch_publisher_platform("android", pid)
        df <- and_res
      } else {
        ios_res <- fetch_publisher_platform("ios", pid)
        and_res <- fetch_publisher_platform("android", pid)
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
      if (is.null(df) || nrow(df) == 0) return(NULL)
      out <- df %>%
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
      out
    })
    results <- results[!vapply(results, is.null, logical(1))]
    final <- if (length(results) > 0) dplyr::bind_rows(results) else tibble::tibble()
    if (verbose) {
      message("\nBatch fetch complete (publisher mode)!")
      message("Total records: ", nrow(final))
      message("Unique publishers: ", length(unique(final$entity_id)))
    }
    return(final)
  }
  
  # Step 1: Normalize app list (non-publisher mode)
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
    # Simple retry helper for httr2 requests (used for active users)
    request_with_retry <- function(req_fun, retries = max_retries) {
      attempt <- 0
      repeat {
        attempt <- attempt + 1
        resp <- tryCatch(req_fun(), error = function(e) e)
        # If resp is an error object, or status indicates retryable, retry
        is_error <- inherits(resp, "error")
        status <- if (!is_error) httr2::resp_status(resp) else NA_integer_
        if (!is_error && !status %in% c(429, 500, 502, 503, 504)) {
          return(resp)
        }
        if (!retry || attempt > retries) {
          return(resp)
        }
        # Exponential backoff with jitter
        delay <- min(30, 2 ^ attempt) + runif(1, 0, 1)
        Sys.sleep(delay)
      }
    }

    fetch_func <- function(group, group_name) {
      if (verbose) message("\nFetching metrics for ", group_name, " group (", nrow(group), " apps)...")
      
      # Check which metrics are requested
      revenue_download_metrics <- intersect(metrics, c("revenue", "downloads"))
      active_user_metrics <- intersect(metrics, c("dau", "wau", "mau"))
      
      all_group_results <- list()
      
      # Handle active user metrics: per-app calls with capped concurrency; unified endpoint not supported
      if (length(active_user_metrics) > 0) {
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

        # Helper to fetch one app's active users for a given platform and metric
        fetch_active_for_one <- function(platform_os, one_app_id, metric_name) {
          metric_time_period <- switch(metric_name,
            "dau" = "day",
            "wau" = "week",
            "mau" = "month",
            default_time_period
          )
          base_url <- paste0("https://api.sensortower.com/v1/", platform_os, "/usage/active_users")
          req_fun <- function() {
            httr2::request(base_url) %>%
              httr2::req_url_query(
                app_ids = one_app_id,
                countries = countries,
                start_date = as.character(date_range$start_date),
                end_date = as.character(date_range$end_date),
                time_period = metric_time_period,
                auth_token = auth_token
              ) %>%
              httr2::req_perform()
          }
          resp <- if (retry) request_with_retry(req_fun) else req_fun()
          if (inherits(resp, "error")) return(NULL)
          if (httr2::resp_status(resp) != 200) return(NULL)
          data <- httr2::resp_body_json(resp)
          if (is.null(data) || length(data) == 0) return(NULL)
          df <- tryCatch(dplyr::bind_rows(data), error = function(e) NULL)
          if (is.null(df) || nrow(df) == 0) return(NULL)
          if (platform_os == "ios") {
            df <- df %>% dplyr::mutate(users = dplyr::coalesce(.data$iphone_users, 0) + dplyr::coalesce(.data$ipad_users, 0), .keep = "all")
          } else if (platform_os == "android") {
            # API returns 'users' for android active users
            if (!"users" %in% names(df) && "android_users" %in% names(df)) df <- df %>% dplyr::rename(users = .data$android_users)
          }
          df$date <- as.Date(df$date)
          df <- df %>%
            dplyr::transmute(app_id = as.character(.data$app_id), date = .data$date, country = .data$country, metric = metric_name, value = .data$users)
          df
        }

        # Build a work list depending on group
        if (group_name == "ios") {
          id_vec <- stats::na.omit(group$ios_id)
          work <- expand.grid(app_id = id_vec, metric = active_user_metrics, stringsAsFactors = FALSE)
          # Sequential or limited parallel
          runner <- function(idx) {
            fetch_active_for_one("ios", work$app_id[idx], work$metric[idx])
          }
          results_list <- lapply(seq_len(nrow(work)), runner)
          results_list <- results_list[!vapply(results_list, is.null, logical(1))]
          if (length(results_list) > 0) {
            active_df <- dplyr::bind_rows(results_list)
            active_df$entity_id <- active_df$app_id
            all_group_results[[length(all_group_results) + 1]] <- active_df
          }
        } else if (group_name == "android") {
          id_vec <- stats::na.omit(group$android_id)
          work <- expand.grid(app_id = id_vec, metric = active_user_metrics, stringsAsFactors = FALSE)
          runner <- function(idx) {
            fetch_active_for_one("android", work$app_id[idx], work$metric[idx])
          }
          results_list <- lapply(seq_len(nrow(work)), runner)
          results_list <- results_list[!vapply(results_list, is.null, logical(1))]
          if (length(results_list) > 0) {
            active_df <- dplyr::bind_rows(results_list)
            active_df$entity_id <- active_df$app_id
            all_group_results[[length(all_group_results) + 1]] <- active_df
          }
        } else if (group_name == "both") {
          # Sum iOS + Android for each app pair
          pair_results <- lapply(seq_len(nrow(group)), function(i) {
            ios_id <- group$ios_id[i]
            android_id <- group$android_id[i]
            pair_id <- paste0(ios_id, "_", android_id)
            per_metric <- lapply(active_user_metrics, function(m) {
              ios_df <- fetch_active_for_one("ios", ios_id, m)
              android_df <- fetch_active_for_one("android", android_id, m)
              if (is.null(ios_df) && is.null(android_df)) return(NULL)
              if (is.null(ios_df) && verbose) message(sprintf("No iOS %s rows for %s in %s (%s..%s)", m, ios_id, paste(countries, collapse=","), as.character(date_range$start_date), as.character(date_range$end_date)))
              if (is.null(android_df) && verbose) message(sprintf("No Android %s rows for %s in %s (%s..%s)", m, android_id, paste(countries, collapse=","), as.character(date_range$start_date), as.character(date_range$end_date)))
              combined <- dplyr::bind_rows(
                dplyr::mutate(ios_df %||% tibble::tibble(app_id = character(), date = as.Date(character()), country = character(), metric = character(), value = numeric()), platform = "ios"),
                dplyr::mutate(android_df %||% tibble::tibble(app_id = character(), date = as.Date(character()), country = character(), metric = character(), value = numeric()), platform = "android")
              )
              if (nrow(combined) == 0) return(NULL)
              summed <- combined %>%
                dplyr::group_by(.data$date, .data$country, .data$metric) %>%
                dplyr::summarise(value = sum(.data$value, na.rm = TRUE), .groups = "drop") %>%
                dplyr::mutate(app_id = pair_id)
              summed
            })
            per_metric <- per_metric[!vapply(per_metric, is.null, logical(1))]
            if (length(per_metric) == 0) return(NULL)
            df <- dplyr::bind_rows(per_metric)
            df$entity_id <- pair_id
            df
          })
          pair_results <- pair_results[!vapply(pair_results, is.null, logical(1))]
          if (length(pair_results) > 0) {
            all_group_results[[length(all_group_results) + 1]] <- dplyr::bind_rows(pair_results)
          }
        } else if (group_name == "unified") {
          if (verbose) message("  Skipping unified active users (endpoint unsupported)")
        }
      }
      
      if (group_name == "both") {
        # Fetch revenue/downloads for both platforms per app and sum
        dplyr::bind_rows(lapply(seq_len(nrow(group)), function(i) {
          collect <- list()
          if (length(revenue_download_metrics) > 0) {
            ios_res <- tryCatch({
              st_sales_report(
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
            }, error = function(e) NULL)
            if ((is.null(ios_res) || nrow(ios_res) == 0) && verbose) message(sprintf("No iOS rows for %s in %s (%s..%s)", group$ios_id[i], paste(countries, collapse=","), as.character(date_range$start_date), as.character(date_range$end_date)))
            android_res <- tryCatch({
              st_sales_report(
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
            }, error = function(e) NULL)
            if ((is.null(android_res) || nrow(android_res) == 0) && verbose) message(sprintf("No Android rows for %s in %s (%s..%s)", group$android_id[i], paste(countries, collapse=","), as.character(date_range$start_date), as.character(date_range$end_date)))
            # Ensure app_id is character in each before binding
            if (!is.null(ios_res) && nrow(ios_res) > 0 && "app_id" %in% names(ios_res)) ios_res$app_id <- as.character(ios_res$app_id)
            if (!is.null(android_res) && nrow(android_res) > 0 && "app_id" %in% names(android_res)) android_res$app_id <- as.character(android_res$app_id)
            combined <- dplyr::bind_rows(ios_res %||% tibble::tibble(), android_res %||% tibble::tibble())
            if (nrow(combined) > 0) {
              combined <- combined %>%
                dplyr::group_by(.data$date, .data$country) %>%
                dplyr::summarise(
                  revenue = sum(.data$revenue, na.rm = TRUE),
                  downloads = sum(.data$downloads, na.rm = TRUE),
                  .groups = "drop"
                ) %>%
                tidyr::pivot_longer(cols = dplyr::all_of(intersect(revenue_download_metrics, c("revenue", "downloads"))), names_to = "metric", values_to = "value") %>%
                dplyr::mutate(entity_id = as.character(paste0(group$ios_id[i], "_", group$android_id[i])), app_id = entity_id)
              collect[[length(collect) + 1]] <- combined
            }
          }
          if (length(collect) > 0) dplyr::bind_rows(collect) else tibble::tibble(entity_id = character())
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
            result <- tryCatch({
              st_sales_report(
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
            }, error = function(e) NULL)
            if ((is.null(result) || nrow(result) == 0) && verbose) message(sprintf("No iOS rows for %s in %s (%s..%s)", group$ios_id[i], paste(countries, collapse=","), as.character(date_range$start_date), as.character(date_range$end_date)))
            if (!is.null(result) && nrow(result) > 0) {
              # Standardize to revenue/downloads without referencing missing columns
              if ("total_revenue" %in% names(result)) {
                result$revenue <- result$total_revenue
              } else if (all(c("iphone_revenue", "ipad_revenue") %in% names(result))) {
                result$revenue <- result$iphone_revenue + result$ipad_revenue
              }
              if ("total_downloads" %in% names(result)) {
                result$downloads <- result$total_downloads
              } else if (all(c("iphone_downloads", "ipad_downloads") %in% names(result))) {
                result$downloads <- result$iphone_downloads + result$ipad_downloads
              }
              # Ensure required columns exist
              if (!"revenue" %in% names(result)) result$revenue <- 0
              if (!"downloads" %in% names(result)) result$downloads <- 0L
              result <- result %>%
                dplyr::select(.data$date, .data$country, .data$revenue, .data$downloads) %>%
                tidyr::pivot_longer(
                  cols = dplyr::all_of(intersect(revenue_download_metrics, c("revenue", "downloads"))),
                  names_to = "metric",
                  values_to = "value"
                ) %>%
                dplyr::mutate(entity_id = as.character(group$ios_id[i]), app_id = as.character(group$ios_id[i]))
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
            result <- tryCatch({
              st_sales_report(
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
            }, error = function(e) NULL)
            if ((is.null(result) || nrow(result) == 0) && verbose) message(sprintf("No Android rows for %s in %s (%s..%s)", group$android_id[i], paste(countries, collapse=","), as.character(date_range$start_date), as.character(date_range$end_date)))
            if (!is.null(result) && nrow(result) > 0) {
              # Ensure standard columns exist
              result <- result %>%
                dplyr::mutate(
                  country = dplyr::coalesce(.data$country, .data$c)
                ) %>%
                dplyr::select(.data$date, .data$country, .data$revenue, .data$downloads) %>%
                tidyr::pivot_longer(
                  cols = dplyr::all_of(intersect(revenue_download_metrics, c("revenue", "downloads"))),
                  names_to = "metric",
                  values_to = "value"
                ) %>%
                dplyr::mutate(entity_id = as.character(group$android_id[i]), app_id = as.character(group$android_id[i]))
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
            # Try to resolve platform IDs to use platform endpoints; fallback to st_metrics
            ios_id <- group$ios_id[i]
            android_id <- group$android_id[i]
            combined <- tibble::tibble()
            if (!is.na(ios_id) || !is.na(android_id)) {
              ios_res <- if (!is.na(ios_id)) tryCatch({
                st_sales_report(
                  os = "ios",
                  ios_app_id = ios_id,
                  countries = countries,
                  start_date = as.character(date_range$start_date),
                  end_date = as.character(date_range$end_date),
                  date_granularity = granularity,
                  auth_token = auth_token,
                  auto_segment = TRUE,
                  verbose = FALSE
                )
              }, error = function(e) NULL) else NULL
              android_res <- if (!is.na(android_id)) tryCatch({
                st_sales_report(
                  os = "android",
                  android_app_id = android_id,
                  countries = countries,
                  start_date = as.character(date_range$start_date),
                  end_date = as.character(date_range$end_date),
                  date_granularity = granularity,
                  auth_token = auth_token,
                  auto_segment = TRUE,
                  verbose = FALSE
                )
              }, error = function(e) NULL) else NULL
              combined <- dplyr::bind_rows(ios_res %||% tibble::tibble(), android_res %||% tibble::tibble())
              if (nrow(combined) > 0) {
                combined <- combined %>%
                  dplyr::group_by(.data$date, .data$country) %>%
                  dplyr::summarise(
                    revenue = sum(.data$revenue, na.rm = TRUE),
                    downloads = sum(.data$downloads, na.rm = TRUE),
                    .groups = "drop"
                  )
              }
            }
            if (nrow(combined) == 0) {
              # Fallback to st_metrics unified flow
              result <- tryCatch({
                st_metrics(
                  os = os,
                  unified_app_id = group$unified_id[i],
                  date_granularity = granularity,
                  start_date = as.character(date_range$start_date),
                  end_date = as.character(date_range$end_date),
                  countries = countries,
                  auth_token = auth_token,
                  verbose = FALSE
                )
              }, error = function(e) NULL)
              if (!is.null(result) && nrow(result) > 0) {
               if ("app_id" %in% names(result)) result$app_id <- as.character(result$app_id)
               combined <- result %>% dplyr::select(.data$date, .data$country, .data$revenue, .data$downloads)
              }
            }
            if (nrow(combined) > 0) {
              out <- combined %>%
                tidyr::pivot_longer(
                  cols = dplyr::all_of(intersect(revenue_download_metrics, c("revenue", "downloads"))),
                  names_to = "metric",
                  values_to = "value"
                ) %>%
                dplyr::mutate(entity_id = as.character(group$unified_id[i]), app_id = as.character(group$unified_id[i]))
              all_results[[length(all_results) + 1]] <- out
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
      mc.cores = min(length(fetch_groups), max(1L, as.integer(max_cores)))
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
      app_id = as.character(.data$app_id),
      unified_id = as.character(.data$unified_id),
      ios_id = as.character(.data$ios_id),
      android_id = as.character(.data$android_id),
      # For apps with both platforms, entity_id will be ios_android
      both_id = ifelse(!is.na(.data$ios_id) & !is.na(.data$android_id), 
                       paste0(.data$ios_id, "_", .data$android_id), 
                       NA_character_),
      # Keep individual platform IDs
      ios_only = as.character(.data$ios_id),
      android_only = as.character(.data$android_id)
    ) %>%
    dplyr::select(original_id = .data$app_id, unified_id = .data$unified_id, both_id = .data$both_id, ios_only = .data$ios_only, android_only = .data$android_only, dplyr::everything())
  
  # Create lookup for all possible entity_id formats
  # Check if app_name column exists
  has_app_name <- "app_name" %in% names(id_mapping)
  
  if (has_app_name) {
    lookup_df <- dplyr::bind_rows(
      # Both platforms
      id_mapping %>% 
        dplyr::filter(!is.na(.data$both_id)) %>%
        dplyr::select(entity_id = .data$both_id, .data$original_id, .data$app_name),
      # iOS only
      id_mapping %>% 
        dplyr::filter(!is.na(.data$ios_only)) %>%
        dplyr::select(entity_id = .data$ios_only, .data$original_id, .data$app_name),
      # Android only  
      id_mapping %>% 
        dplyr::filter(!is.na(.data$android_only)) %>%
        dplyr::select(entity_id = .data$android_only, .data$original_id, .data$app_name),
      # Unified ID (unchanged)
      id_mapping %>%
        dplyr::select(entity_id = .data$unified_id, .data$original_id, .data$app_name)
    ) %>%
      dplyr::distinct(.data$entity_id, .data$original_id, .data$app_name) %>%
      dplyr::mutate(entity_id = as.character(.data$entity_id), original_id = as.character(.data$original_id))
  } else {
    lookup_df <- dplyr::bind_rows(
      # Both platforms
      id_mapping %>% 
        dplyr::filter(!is.na(.data$both_id)) %>%
        dplyr::select(entity_id = .data$both_id, .data$original_id),
      # iOS only
      id_mapping %>% 
        dplyr::filter(!is.na(.data$ios_only)) %>%
        dplyr::select(entity_id = .data$ios_only, .data$original_id),
      # Android only  
      id_mapping %>% 
        dplyr::filter(!is.na(.data$android_only)) %>%
        dplyr::select(entity_id = .data$android_only, .data$original_id),
      # Unified ID (unchanged)
      id_mapping %>%
        dplyr::select(entity_id = .data$unified_id, .data$original_id)
    ) %>%
      dplyr::distinct(.data$entity_id, .data$original_id) %>%
      dplyr::mutate(entity_id = as.character(.data$entity_id), original_id = as.character(.data$original_id))
  }
  
  # Merge results back with original app identifiers
  all_results <- all_results %>%
    dplyr::left_join(lookup_df, by = "entity_id")

  # Reorder and clean columns
  if (nrow(all_results) > 0) {
    # Add app_id and app_id_type columns based on OS parameter
    all_results <- all_results %>%
      dplyr::mutate(
        app_id = .data$entity_id,  # Use entity_id as app_id for now
        app_id_type = dplyr::case_when(
          os == "ios" ~ "ios",
          os == "android" ~ "android",
          os == "unified" ~ "unified",
          TRUE ~ NA_character_
        )
      ) %>%
      dplyr::select(.data$original_id, dplyr::any_of("app_name"), .data$app_id, .data$app_id_type, 
                    dplyr::everything(), -.data$entity_id) %>%
      dplyr::arrange(.data$original_id)
  }
  
  if (verbose) {
    message("\nBatch fetch complete!")
    message("Total records: ", nrow(all_results))
    # Use app_id which is guaranteed to exist post-reordering
    unique_count <- if ("app_id" %in% names(all_results)) length(unique(all_results$app_id)) else 0
    message("Unique apps: ", unique_count)
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
  
  # Detect and resolve platform IDs via bulk resolution + cache when available
  apps_df$ios_id <- NA_character_
  apps_df$android_id <- NA_character_
  
  if (exists("batch_resolve_ids", mode = "function")) {
    ids <- as.character(apps_df$app_id)
    resolved_list <- batch_resolve_ids(ids, auth_token = auth_token, use_cache = TRUE, verbose = verbose)
    for (i in seq_len(nrow(apps_df))) {
      id <- as.character(apps_df$app_id[i])
      entry <- resolved_list[[id]]
      if (!is.null(entry)) {
        if (!is.null(entry$ios_id)) apps_df$ios_id[i] <- entry$ios_id
        if (!is.null(entry$android_id)) apps_df$android_id[i] <- entry$android_id
        if (!is.null(entry$unified_id)) apps_df$unified_id[i] <- entry$unified_id
        if (!is.null(entry$app_name) && is.na(apps_df$app_name[i])) apps_df$app_name[i] <- entry$app_name
      } else {
        # Heuristic fallback (no API call): infer platform from pattern
        if (grepl("^\\d+$", id)) apps_df$ios_id[i] <- id
        if (grepl("^(com|net|org|io)\\.", id)) apps_df$android_id[i] <- id
      }
    }
  } else {
    # Minimal fallback without API helpers
    for (i in seq_len(nrow(apps_df))) {
      id <- apps_df$app_id[i]
      if (grepl("^\\d+$", id)) {
        apps_df$ios_id[i] <- id
      } else if (grepl("^(com|net|org|io)\\.", id)) {
        apps_df$android_id[i] <- id
      }
    }
  }
  
  return(apps_df)
}