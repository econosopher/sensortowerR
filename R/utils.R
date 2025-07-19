# --- Utility Functions for sensortowerR ---

# This file contains helper functions used by the main data-fetching functions
# in the package. They handle tasks like input validation, query parameter
# preparation, and API request execution.

#' @importFrom rlang abort
#' @importFrom stats setNames
#' @importFrom httr2 request req_user_agent req_url_path_append req_url_query
#'   req_error req_perform resp_status resp_body_raw resp_check_status
#'   resp_body_string
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @importFrom utils URLencode
#' @importFrom tidyr unnest
#' @importFrom dplyr rename all_of
#'
# --- Input Validation ---

validate_inputs <- function(os,
                            comparison_attribute,
                            time_range,
                            measure,
                            date,
                            category,
                            regions,
                            end_date = NULL,
                            limit = 25,
                            offset = NULL,
                            device_type = NULL,
                            custom_fields_filter_id = NULL,
                            custom_tags_mode = NULL,
                            data_model = NULL) {
  # Validation checks for common parameters
  stopifnot(
    "`os` must be one of 'ios', 'android', or 'unified'" =
      os %in% c("ios", "android", "unified"),
    "`comparison_attribute` must be one of 'absolute', 'delta', 'transformed_delta'" =
      comparison_attribute %in% c("absolute", "delta", "transformed_delta"),
    "`time_range` must be a non-empty string" =
      is.character(time_range) && nzchar(time_range),
    "`measure` must be a non-empty string" =
      is.character(measure) && nzchar(measure),
    "`date` must be provided" = !is.null(date),
    "Either `category` or `custom_fields_filter_id` must be provided" =
      !is.null(category) || !is.null(custom_fields_filter_id),
    "`regions` must be provided" = !is.null(regions),
    "`limit` must be a positive integer" =
      is.numeric(limit) && limit > 0 && limit == round(limit)
  )

  # Specific validations
  if (os %in% c("ios", "unified") && is.null(device_type)) {
    message("`device_type` is not specified for `os = '", os, "'`. Defaulting to 'total'.")
    device_type <- "total"
  }

  if (!is.null(custom_fields_filter_id) && os == "unified" && is.null(custom_tags_mode)) {
    rlang::abort("`custom_tags_mode` must be provided when `os` is 'unified' and `custom_fields_filter_id` is used.")
  }
}

# --- Query Parameter Preparation ---

prepare_query_params_sales <- function(auth_token,
                                       comparison_attribute,
                                       time_range,
                                       measure,
                                       date,
                                       category,
                                       end_date,
                                       regions,
                                       limit,
                                       offset,
                                       device_type,
                                       custom_fields_filter_id,
                                       custom_tags_mode,
                                       os) {
  params <- list(
    auth_token = auth_token,
    comparison_attribute = comparison_attribute,
    time_range = time_range,
    measure = measure,
    date = as.character(date),
    category = category,
    end_date = if (!is.null(end_date)) as.character(end_date) else NULL,
    regions = paste(regions, collapse = ","),
    limit = limit,
    offset = offset,
    device_type = if (os %in% c("ios", "unified")) device_type else NULL,
    custom_fields_filter_id = custom_fields_filter_id,
    custom_tags_mode = if (os == "unified") custom_tags_mode else NULL
  )
  # Remove NULLs
  params[!sapply(params, is.null)]
}

prepare_query_params_active_users <- function(auth_token,
                                              comparison_attribute,
                                              time_range,
                                              measure,
                                              date,
                                              category,
                                              regions,
                                              limit,
                                              offset,
                                              device_type,
                                              custom_fields_filter_id,
                                              custom_tags_mode,
                                              data_model,
                                              os) {
  params <- list(
    auth_token = auth_token,
    comparison_attribute = comparison_attribute,
    time_range = time_range,
    measure = measure,
    date = as.character(date),
    category = category,
    regions = paste(regions, collapse = ","),
    limit = limit,
    offset = offset,
    device_type = if (os %in% c("ios", "unified")) device_type else NULL,
    custom_fields_filter_id = custom_fields_filter_id,
    custom_tags_mode = if (os == "unified") custom_tags_mode else NULL,
    data_model = data_model
  )
  # Remove NULLs
  params[!sapply(params, is.null)]
}


# --- API Request Building and Performance ---

build_request <- function(base_url, path_segments, query_params) {
  req <- httr2::request(base_url) %>%
    httr2::req_user_agent("sensortowerR (https://github.com/ge-data-solutions/sensortowerR)")

  for (segment in path_segments) {
    req <- req %>% httr2::req_url_path_append(segment)
  }

  req %>% httr2::req_url_query(!!!query_params)
}


perform_request <- function(req) {
  tryCatch(
    {
      resp <- httr2::req_perform(req)
      httr2::resp_check_status(resp) # Check for HTTP errors
      resp
    },
    httr2_error = function(e) {
      status <- httr2::resp_status(e$resp)
      body <- httr2::resp_body_string(e$resp)
      rlang::abort(
        message = sprintf("API request failed with status %d.", status),
        body = body,
        parent = e
      )
    },
    error = function(e) {
      rlang::abort("An unexpected error occurred during the API request.", parent = e)
    }
  )
}

# --- Response Processing ---

process_response <- function(resp, enrich_response = TRUE) {
  body_raw <- httr2::resp_body_raw(resp)
  if (length(body_raw) == 0) {
    return(tibble::tibble())
  }

  body_text <- rawToChar(body_raw)
  result <- jsonlite::fromJSON(body_text, flatten = TRUE)

  if (length(result) == 0 || nrow(result) == 0) {
    return(tibble::tibble())
  }

  result_tbl <- tibble::as_tibble(result)

  # Enrich with app names if requested and possible
  if (enrich_response && "entities" %in% names(result_tbl) && is.list(result_tbl$entities)) {
    # Proactively coerce app_id to character in the nested data frames
    # to prevent type errors during the unnest operation. The API can return
    # a mix of integer and character IDs, which vctrs cannot combine.
    result_tbl$entities <- lapply(result_tbl$entities, function(df) {
      if (!is.null(df) && "app_id" %in% names(df)) {
        df$app_id <- as.character(df$app_id)
      }
      df
    })

    result_tbl <- tidyr::unnest(result_tbl, dplyr::all_of("entities"), names_sep = ".")
    
    # Clean up duplicate columns - prefer the entities.* versions for detailed data
    base_cols <- setdiff(names(result_tbl), grep("^entities\\.", names(result_tbl), value = TRUE))
    entities_cols <- grep("^entities\\.", names(result_tbl), value = TRUE)
    
    # Remove base columns that have entities.* equivalents (except unified_app_id)
    duplicated_bases <- intersect(
      gsub("^entities\\.", "", entities_cols),
      setdiff(base_cols, "unified_app_id")
    )
    result_tbl <- result_tbl[, !names(result_tbl) %in% duplicated_bases]
    
    # Handle app name column - check multiple possible sources
    if ("entities.custom_tags.unified_product_name" %in% names(result_tbl)) {
      result_tbl <- dplyr::rename(result_tbl, app.name = "entities.custom_tags.unified_product_name")
    } else if ("entities.name" %in% names(result_tbl)) {
      result_tbl <- dplyr::rename(result_tbl, app.name = "entities.name")
    } else if ("name" %in% names(result_tbl) && !"app.name" %in% names(result_tbl)) {
      result_tbl <- dplyr::rename(result_tbl, app.name = "name")
    }
    
    # Create unified_app_name and unified_app_id for consistency
    if ("app.name" %in% names(result_tbl)) {
      result_tbl$unified_app_name <- result_tbl$app.name
    } else if ("entities.app_id" %in% names(result_tbl)) {
      # For sales endpoint, lookup app names using app IDs
      result_tbl <- lookup_app_names_by_id(result_tbl)
    }
    
    if ("entities.app_id" %in% names(result_tbl)) {
      result_tbl$unified_app_id <- result_tbl$entities.app_id
    }
    
    # Extract custom metrics with clean names
    result_tbl <- extract_custom_metrics(result_tbl)
  }

  return(result_tbl)
}

# Helper function to extract useful custom metrics from entities.custom_tags and aggregate_tags columns
extract_custom_metrics <- function(data) {
  if (nrow(data) == 0) return(data)
  
  # Define the metrics we want to extract with cleaner names
  # Based on actual API response structure
  metrics_map <- c(
    # Performance metrics (from aggregate_tags - these are available)
    "aggregate_tags.Last 180 Days Downloads (WW)" = "downloads_180d_ww",
    "aggregate_tags.Last 180 Days Revenue (WW)" = "revenue_180d_ww", 
    "aggregate_tags.Last 30 Days Average DAU (US)" = "dau_30d_us",
    "aggregate_tags.Last 30 Days Average DAU (WW)" = "dau_30d_ww",
    "aggregate_tags.Last 30 Days Downloads (WW)" = "downloads_30d_ww",
    "aggregate_tags.Last 30 Days Revenue (WW)" = "revenue_30d_ww",
    "aggregate_tags.Last 4 Weeks Average WAU (US)" = "wau_4w_us",
    "aggregate_tags.Last 4 Weeks Average WAU (WW)" = "wau_4w_ww",
    "aggregate_tags.Last Month Average MAU (US)" = "mau_month_us",
    "aggregate_tags.Last Month Average MAU (WW)" = "mau_month_ww",
    
    # Historical metrics
    "aggregate_tags.All Time Downloads (US)" = "downloads_alltime_us",
    "aggregate_tags.All Time Downloads (WW)" = "downloads_alltime_ww",
    "aggregate_tags.All Time Revenue (US)" = "revenue_alltime_us", 
    "aggregate_tags.All Time Revenue (WW)" = "revenue_alltime_ww",
    "aggregate_tags.All Time Publisher Downloads (WW)" = "publisher_downloads_alltime_ww",
    "aggregate_tags.All Time Publisher Revenue (WW)" = "publisher_revenue_alltime_ww",
    
    # RPD metrics
    "aggregate_tags.RPD (All Time, US)" = "rpd_alltime_us",
    "aggregate_tags.RPD (All Time, WW)" = "rpd_alltime_ww",
    
    # Launch metrics
    "aggregate_tags.Release Date (US)" = "release_date_us",
    "aggregate_tags.Release Date (WW)" = "release_date_ww",
    "aggregate_tags.Release Date (JP)" = "release_date_jp",
    "aggregate_tags.Earliest Release Date" = "earliest_release_date",
    "aggregate_tags.Revenue First 30 Days (WW)" = "revenue_first30d_ww",
    "aggregate_tags.Downloads First 30 Days (WW)" = "downloads_first30d_ww",
    
    # Retention metrics
    "aggregate_tags.Day 1 Retention (Last Quarter, US)" = "retention_1d_us",
    "aggregate_tags.Day 1 Retention (Last Quarter, WW)" = "retention_1d_ww",
    "aggregate_tags.Day 7 Retention (Last Quarter, US)" = "retention_7d_us",
    "aggregate_tags.Day 7 Retention (Last Quarter, WW)" = "retention_7d_ww",
    "aggregate_tags.Day 14 Retention (Last Quarter, US)" = "retention_14d_us",
    "aggregate_tags.Day 14 Retention (Last Quarter, WW)" = "retention_14d_ww",
    "aggregate_tags.Day 30 Retention (Last Quarter, US)" = "retention_30d_us",
    "aggregate_tags.Day 30 Retention (Last Quarter, WW)" = "retention_30d_ww",
    "aggregate_tags.Day 60 Retention (Last Quarter, US)" = "retention_60d_us",
    "aggregate_tags.Day 60 Retention (Last Quarter, WW)" = "retention_60d_ww",
    
    # Demographics
    "aggregate_tags.Age (Last Quarter, US)" = "age_us",
    "aggregate_tags.Age (Last Quarter, WW)" = "age_ww",
    
    # Additional useful metrics from entities.custom_tags (when available)
    "entities.custom_tags.Game Sub-genre" = "game_subgenre",
    "entities.custom_tags.Game Genre" = "game_genre",
    "entities.custom_tags.Game Art Style" = "game_art_style",
    "entities.custom_tags.Primary Category" = "primary_category",
    "entities.custom_tags.Overall US Rating" = "us_rating",
    "entities.custom_tags.Current US Rating" = "current_us_rating",
    "entities.custom_tags.Free" = "is_free",
    "entities.custom_tags.In-App Purchases" = "has_iap",
    "entities.custom_tags.Contains Ads" = "has_ads"
  )
  
  # Extract only the metrics that exist in the data
  available_metrics <- intersect(names(metrics_map), names(data))
  
  if (length(available_metrics) > 0) {
    # Create a new data frame with the extracted metrics
    extracted_data <- data[, available_metrics, drop = FALSE]
    names(extracted_data) <- metrics_map[available_metrics]
    
    # Remove the original entities.custom_tags columns to avoid duplication
    data <- data[, !names(data) %in% available_metrics, drop = FALSE]
    
    # Bind the cleaned metrics back to the data using dplyr::bind_cols for safety
    data <- dplyr::bind_cols(data, extracted_data)
  }
  
    return(data)
}

# Helper function to lookup app names by app ID (for sales endpoint which doesn't provide names)
lookup_app_names_by_id <- function(data) {
  if (!"entities.app_id" %in% names(data) || nrow(data) == 0) {
    return(data)
  }
  
  message("Looking up app names for sales data...")
  
  # Get unique app IDs to avoid duplicate API calls
  unique_app_ids <- unique(data$entities.app_id)
  unique_app_ids <- unique_app_ids[!is.na(unique_app_ids) & unique_app_ids != ""]
  
  if (length(unique_app_ids) == 0) {
    # No valid app IDs, use IDs as names
    data$unified_app_name <- data$entities.app_id
    return(data)
  }
  
  # Initialize lookup results
  app_id_to_name <- setNames(rep(NA_character_, length(unique_app_ids)), unique_app_ids)
  
  # Look up each unique app ID
  for (i in seq_along(unique_app_ids)) {
    app_id <- unique_app_ids[i]
    
    # Progress indicator for larger datasets
    if (length(unique_app_ids) > 5) {
      message(sprintf("  Looking up app %d/%d: %s", i, length(unique_app_ids), app_id))
    }
    
    tryCatch({
      # Search for the app using the app_id as a search term
      # This works for both iOS App Store IDs and Android package names
      app_search <- st_app_info(term = app_id, limit = 1)
      
      if (nrow(app_search) > 0 && !is.na(app_search$unified_app_name[1])) {
        app_id_to_name[app_id] <- app_search$unified_app_name[1]
        if (length(unique_app_ids) <= 5) {
          message(sprintf("  Found: %s -> %s", app_id, app_search$unified_app_name[1]))
        }
      } else {
        if (length(unique_app_ids) <= 5) {
          message(sprintf("  Not found: %s (will use app ID)", app_id))
        }
      }
      
      # Add small delay to be respectful to API
      if (length(unique_app_ids) > 1) {
        Sys.sleep(0.1)
      }
      
    }, error = function(e) {
      # Silently continue if lookup fails
      if (length(unique_app_ids) <= 5) {
        message(sprintf("  Error looking up %s: %s", app_id, e$message))
      }
    })
  }
  
  # Create unified_app_name based on lookup results, fallback to app_id
  data$unified_app_name <- ifelse(
    is.na(app_id_to_name[data$entities.app_id]), 
    data$entities.app_id,  # Fallback to app ID if lookup failed
    app_id_to_name[data$entities.app_id]  # Use looked up name
  )
  
  # Report success rate
  successful_lookups <- sum(!is.na(app_id_to_name))
  total_unique <- length(unique_app_ids)
  message(sprintf("App name lookup completed: %d/%d successful (%.1f%%)", 
                  successful_lookups, total_unique, 100 * successful_lookups / total_unique))
  
  return(data)
}

 