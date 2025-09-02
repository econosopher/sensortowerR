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
#' @importFrom utils URLencode head
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
    category = category,  # Keep category even with custom filter
    end_date = if (!is.null(end_date)) as.character(end_date) else NULL,
    regions = paste(regions, collapse = ","),
    limit = limit,
    offset = offset,
    device_type = if (os %in% c("ios", "unified")) device_type else NULL,
    custom_fields_filter_id = custom_fields_filter_id,
    custom_tags_mode = if (os == "unified") custom_tags_mode else NULL
  )
  # Remove NULLs
  params <- params[!sapply(params, is.null)]
  
  # Debug: print params when using custom filter
  if (!is.null(custom_fields_filter_id)) {
    message("Debug: Query params with custom filter (sales):")
    message("  custom_fields_filter_id: ", params$custom_fields_filter_id)
    message("  category: ", if(is.null(params$category)) "NULL" else params$category)
    message("  custom_tags_mode: ", params$custom_tags_mode)
  }
  
  params
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
    category = category,  # Keep category even with custom filter
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

# Helper function to perform unnesting of entities
perform_unnest <- function(result_tbl) {
  # Proactively coerce app_id to character in the nested data frames
  # to prevent type errors during the unnest operation. The API can return
  # a mix of integer and character IDs, which vctrs cannot combine.
  result_tbl$entities <- lapply(result_tbl$entities, function(df) {
    if (!is.null(df) && "app_id" %in% names(df)) {
      df$app_id <- as.character(df$app_id)
    }
    df
  })
  
  tidyr::unnest(result_tbl, dplyr::all_of("entities"), names_sep = ".")
}

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
    # IMPORTANT: Preserve the original app_id as unified_app_id BEFORE unnesting
    # This is the true unified hex ID from the API
    if ("app_id" %in% names(result_tbl)) {
      result_tbl$unified_app_id <- result_tbl$app_id
      
      # OPTIMIZATION: If we already have unified hex IDs, check if unnesting is necessary
      # When using os="unified" with custom filters, the API returns unified data already
      non_na_ids <- result_tbl$app_id[!is.na(result_tbl$app_id)]
      if (length(non_na_ids) > 0 && all(grepl("^[a-f0-9]{24}$", non_na_ids))) {
        # We have unified hex IDs - check if entities just contains redundant platform data
        # If the entities only contain platform-specific versions of the same app,
        # we can skip unnesting to avoid creating duplicates that need consolidation
        
        # Check first entity to see if it would create duplicates
        first_entity <- result_tbl$entities[[1]]
        if (!is.null(first_entity) && is.data.frame(first_entity) && nrow(first_entity) > 1) {
          # Multiple rows in entity = iOS + Android versions = will create duplicates
          # For unified data, we can use aggregate_tags instead of unnesting entities
          message("Streamlined processing: Using unified data without unnesting platform entities")
          
          # Extract important demographic data from entities before removing
          # Gender data is only in entities, not in aggregate_tags
          # Pre-create columns to avoid tibble recycling issues
          if (!"aggregate_tags.Genders (Last Quarter, US)" %in% names(result_tbl)) {
            result_tbl[["aggregate_tags.Genders (Last Quarter, US)"]] <- rep(NA_character_, nrow(result_tbl))
          }
          if (!"aggregate_tags.Genders (Last Quarter, WW)" %in% names(result_tbl)) {
            result_tbl[["aggregate_tags.Genders (Last Quarter, WW)"]] <- rep(NA_character_, nrow(result_tbl))
          }
          for (i in seq_len(nrow(result_tbl))) {
            entity_df <- result_tbl$entities[[i]]
            if (!is.null(entity_df) && is.data.frame(entity_df)) {
              # Extract gender data from first entity row (they should be the same for unified)
              if ("custom_tags.Genders (Last Quarter, US)" %in% names(entity_df)) {
                gender_val <- entity_df$`custom_tags.Genders (Last Quarter, US)`[1]
                if (!is.null(gender_val) && !is.na(gender_val)) {
                  result_tbl$`aggregate_tags.Genders (Last Quarter, US)`[i] <- gender_val
                }
              }
              # Also extract other demographic fields if needed
              if ("custom_tags.Genders (Last Quarter, WW)" %in% names(entity_df)) {
                gender_ww <- entity_df$`custom_tags.Genders (Last Quarter, WW)`[1]
                if (!is.null(gender_ww) && !is.na(gender_ww)) {
                  result_tbl$`aggregate_tags.Genders (Last Quarter, WW)`[i] <- gender_ww
                }
              }
            }
          }
          
          # Now safe to remove entities after extracting key fields
          if ("aggregate_tags" %in% names(result_tbl)) {
            # The aggregate_tags column already contains the unified metrics
            # We've extracted gender data, so now we can remove entities
            result_tbl$entities <- NULL  # Remove entities to prevent confusion
          }
        } else {
          # Single row or empty entity - safe to unnest
          result_tbl <- perform_unnest(result_tbl)
        }
      } else {
        # Not all unified IDs - need to unnest for platform resolution
        result_tbl <- perform_unnest(result_tbl)
      }
    } else {
      # No app_id column - proceed with normal unnesting
      result_tbl <- perform_unnest(result_tbl)
    }
    
    # Clean up duplicate columns - prefer the entities.* versions for detailed data
    base_cols <- setdiff(names(result_tbl), grep("^entities\\.", names(result_tbl), value = TRUE))
    entities_cols <- grep("^entities\\.", names(result_tbl), value = TRUE)
    
    # Remove base columns that have entities.* equivalents
    # But preserve unified_app_id and app_id if they're hex format unified IDs
    preserve_cols <- "unified_app_id"
    if ("app_id" %in% base_cols && all(grepl("^[a-f0-9]{24}$", result_tbl$app_id[!is.na(result_tbl$app_id)]))) {
      # If app_id contains hex unified IDs, don't remove it
      preserve_cols <- c(preserve_cols, "app_id")
    }
    duplicated_bases <- intersect(
      gsub("^entities\\.", "", entities_cols),
      setdiff(base_cols, preserve_cols)
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
    }
    
    # Note: unified_app_id was already set above before unnesting
    # The entities.app_id contains platform-specific IDs, not unified IDs
    
    # Store platform-specific app_id separately for reference
    if ("entities.app_id" %in% names(result_tbl) && !"platform_app_id" %in% names(result_tbl)) {
      result_tbl$platform_app_id <- result_tbl$entities.app_id
    }
    
    # If we have app_ids but missing app names, look them up
    if ((!"unified_app_name" %in% names(result_tbl) || 
         any(is.na(result_tbl$unified_app_name))) &&
        "unified_app_id" %in% names(result_tbl)) {
      result_tbl <- lookup_app_names_by_id(result_tbl)
    }
    
    # Extract custom metrics with clean names
    result_tbl <- extract_custom_metrics(result_tbl)
    
    # Clean special characters from numeric values
    result_tbl <- clean_numeric_values(result_tbl)
    
    # Convert date columns to proper Date class
    result_tbl <- clean_date_values(result_tbl)
  } else {
    # Even without enrichment, clean any numeric values that might have special characters
    result_tbl <- clean_numeric_values(result_tbl)
    
    # Convert date columns to proper Date class
    result_tbl <- clean_date_values(result_tbl)
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
    "aggregate_tags.Genders (Last Quarter, US)" = "genders_us",
    "aggregate_tags.Genders (Last Quarter, WW)" = "genders_ww",
    "aggregate_tags.Gender (Last Quarter, US)" = "gender_us",
    "aggregate_tags.Gender (Last Quarter, WW)" = "gender_ww",
    
    # Additional revenue metrics
    "aggregate_tags.Most Popular Country by Revenue" = "most_popular_country_revenue",
    "aggregate_tags.Last 90 Days Downloads (US)" = "downloads_90d_us",
    "aggregate_tags.Last 90 Days Downloads (WW)" = "downloads_90d_ww",
    "aggregate_tags.Last 90 Days Revenue (US)" = "revenue_90d_us",
    "aggregate_tags.Last 90 Days Revenue (WW)" = "revenue_90d_ww",
    "aggregate_tags.ARPU (90 Days, US)" = "arpu_90d_us",
    "aggregate_tags.ARPU (90 Days, WW)" = "arpu_90d_ww",
    
    # Platform shares
    "aggregate_tags.Android Share (WW)" = "android_share_ww",
    "aggregate_tags.iOS Share (WW)" = "ios_share_ww",
    "aggregate_tags.Female Share (US)" = "female_share_us",
    "aggregate_tags.Male Share (US)" = "male_share_us",
    
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

# Helper function to clean special characters from numeric values
clean_numeric_values <- function(data) {
  if (nrow(data) == 0) return(data)
  
  # Define patterns of metrics that should be treated as numeric
  # These patterns match the metric names we extract in extract_custom_metrics
  numeric_metric_patterns <- c(
    "downloads", "revenue", "users", "dau", "wau", "mau", "rpd", 
    "retention", "age", "rating", "count", "days", "size", "absolute",
    "delta", "share", "percent", "time", "last", "average", "total",
    "first", "all time", "180", "30", "90", "7d", "14d", "month"
  )
  
  # Find columns that likely contain numeric data  
  numeric_cols <- names(data)[sapply(names(data), function(col_name) {
    # Skip non-character columns
    if (!is.character(data[[col_name]])) {
      return(FALSE)
    }
    
    # Check if column name matches our numeric patterns
    matches_pattern <- any(sapply(numeric_metric_patterns, function(pattern) {
      grepl(pattern, col_name, ignore.case = TRUE)
    }))
    
    # Also check for aggregate_tags columns that might contain numeric data
    is_aggregate_tag <- grepl("^aggregate_tags\\.", col_name)
    
    # Check for entities.custom_tags columns that might be numeric
    is_custom_tag <- grepl("^entities\\.custom_tags\\.", col_name)
    
    # Skip obvious text columns 
    is_text_column <- grepl("\\bname$|\\burl$|\\bdate$|app_id$|country$|gender|genre|style|category", col_name, ignore.case = TRUE)
    
    # If column matches our criteria and isn't obviously text, check the content
    if ((matches_pattern || is_aggregate_tag || is_custom_tag) && !is_text_column) {
      sample_values <- head(data[[col_name]][!is.na(data[[col_name]]) & data[[col_name]] != ""], 10)
      if (length(sample_values) > 0) {
        # Check if values contain digits with special characters OR pure numbers
        has_numeric_with_special <- any(grepl("[0-9].*[%$,]|[%$,].*[0-9]", sample_values))
        has_pure_numeric <- any(grepl("^[0-9]+\\.?[0-9]*$", sample_values))
        
        # Also check for formatted numbers like "1,234" or scientific notation
        has_formatted_numeric <- any(grepl("^[0-9,]+\\.?[0-9]*$", sample_values))
        
        return(has_numeric_with_special || has_pure_numeric || has_formatted_numeric)
      }
    }
    return(FALSE)
  })]
  
  # Clean each numeric column
  for (col in numeric_cols) {
    if (is.character(data[[col]])) {
      original_values <- data[[col]]
      
      # Handle percentage values (convert "45%" to 45, not 0.45)
      # Most analytics metrics use percentages as whole numbers
      has_percentages <- any(grepl("%", original_values, fixed = TRUE), na.rm = TRUE)
      
      # Remove currency symbols, commas, spaces, and other formatting
      cleaned_values <- gsub("[$,\\s]", "", original_values)
      
      # Remove percentage signs (but track that they were there)
      cleaned_values <- gsub("%", "", cleaned_values)
      
      # Remove any remaining non-numeric characters except decimal points and minus signs
      cleaned_values <- gsub("[^0-9.-]", "", cleaned_values)
      
      # Convert empty strings to NA
      cleaned_values[cleaned_values == ""] <- NA
      
      # Convert to numeric, suppressing warnings for values that can't be converted
      numeric_values <- suppressWarnings(as.numeric(cleaned_values))
      
      # Only replace if we successfully converted most values
      # This prevents accidentally converting text columns that happen to match patterns
      non_na_original <- sum(!is.na(original_values))
      non_na_converted <- sum(!is.na(numeric_values))
      
      if (non_na_original > 0) {
        conversion_rate <- non_na_converted / non_na_original
        if (conversion_rate > 0.5) {  # If more than 50% converted successfully
          data[[col]] <- numeric_values
          
          # Special handling for retention metrics - convert to decimals
          if (grepl("retention", col, ignore.case = TRUE) && has_percentages) {
            # Convert retention percentages to decimals (15.5% becomes 0.155)
            data[[col]] <- numeric_values / 100
            message(sprintf("Converted column '%s' to numeric (percentages to decimals)", col))
          } else {
            # Log the cleaning for transparency
            if (has_percentages) {
              message(sprintf("Converted column '%s' to numeric (removed %% symbols)", col))
            } else {
              message(sprintf("Converted column '%s' to numeric", col))
            }
          }
        }
      }
    }
  }
  
  return(data)
}

# Global cache for app name lookups to avoid redundant API calls across function calls
.app_name_cache <- new.env()

# Helper function to lookup app names by app ID (for sales endpoint which doesn't provide names)
lookup_app_names_by_id <- function(data) {
  # Determine which ID column to use for lookups
  # Priority: entities.app_id (for backward compatibility), then unified_app_id, then app_id
  id_column <- NULL
  if ("entities.app_id" %in% names(data)) {
    id_column <- "entities.app_id"
  } else if ("unified_app_id" %in% names(data)) {
    id_column <- "unified_app_id"
  } else if ("app_id" %in% names(data)) {
    id_column <- "app_id"
  } else {
    return(data)  # No ID column available
  }
  
  if (nrow(data) == 0) {
    return(data)
  }
  
  message("Looking up missing app names...")
  
  # Get unique app IDs that need lookup (missing app names)
  # If unified_app_name already exists, only lookup IDs where name is missing
  if ("unified_app_name" %in% names(data)) {
    # Find rows where app_id exists but unified_app_name is missing
    needs_lookup <- !is.na(data[[id_column]]) & 
                    data[[id_column]] != "" & 
                    (is.na(data$unified_app_name) | data$unified_app_name == "")
    unique_app_ids <- unique(data[[id_column]][needs_lookup])
  } else {
    # If no unified_app_name column exists, lookup all valid app_ids
    unique_app_ids <- unique(data[[id_column]])
    unique_app_ids <- unique_app_ids[!is.na(unique_app_ids) & unique_app_ids != ""]
  }
  
  if (length(unique_app_ids) == 0) {
    # No valid app IDs to lookup
    if (!"unified_app_name" %in% names(data)) {
      # If no unified_app_name column exists, create it using app IDs
      data$unified_app_name <- data[[id_column]]
    }
    return(data)
  }
  
  # Initialize lookup results - first check cache
  app_id_to_name <- setNames(rep(NA_character_, length(unique_app_ids)), unique_app_ids)
  
  # Check cache first to avoid redundant API calls
  cached_count <- 0
  for (app_id in unique_app_ids) {
    if (exists(app_id, envir = .app_name_cache)) {
      app_id_to_name[app_id] <- get(app_id, envir = .app_name_cache)
      cached_count <- cached_count + 1
    }
  }
  
  if (cached_count > 0) {
    message(sprintf("  Found %d app names in cache (skipping API calls)", cached_count))
  }
  
  # Only lookup app IDs that are not in cache
  unique_app_ids <- unique_app_ids[is.na(app_id_to_name)]
  
  # Early exit if all app IDs were found in cache
  if (length(unique_app_ids) == 0) {
    message("All app names found in cache - no API calls needed!")
    # Proceed to create unified_app_name column
  } else {
    # Batch lookup strategy: Use larger search queries to find multiple apps at once
    batch_size <- 10  # Process apps in batches to reduce API calls
    successful_lookups <- cached_count  # Include cached results in success count
  
  # Process unique app IDs in batches
  for (batch_start in seq(1, length(unique_app_ids), by = batch_size)) {
    batch_end <- min(batch_start + batch_size - 1, length(unique_app_ids))
    current_batch <- unique_app_ids[batch_start:batch_end]
    
    # Strategy 1: Try searching for exact app IDs individually (optimized)
    for (app_id in current_batch) {
      if (!is.na(app_id_to_name[app_id])) next  # Skip if already found
      
      tryCatch({
        # Search for the app using the app_id as a search term
        app_search <- st_app_info(term = app_id, limit = 3)  # Get more results to find exact matches
        
        if (nrow(app_search) > 0) {
          # Look for exact app ID match in the results
          exact_match_idx <- which(app_search$unified_app_id == app_id)
          
          if (length(exact_match_idx) > 0) {
            # Found exact match
            app_name <- app_search$unified_app_name[exact_match_idx[1]]
                         if (!is.na(app_name) && app_name != "") {
               app_id_to_name[app_id] <- app_name
               # Cache the result for future use
               assign(app_id, app_name, envir = .app_name_cache)
               successful_lookups <- successful_lookups + 1
               message(sprintf("  Found: %s = %s", app_id, app_name))
             }
                     } else if (!is.na(app_search$unified_app_name[1])) {
             # Use first result as fallback (for package names that might match)
             app_name <- app_search$unified_app_name[1]
             app_id_to_name[app_id] <- app_name
             # Cache the result for future use
             assign(app_id, app_name, envir = .app_name_cache)
             successful_lookups <- successful_lookups + 1
             message(sprintf("  Found: %s -> %s", app_id, app_name))
           }
        }
        
      }, error = function(e) {
        # Silently continue if lookup fails
        if (length(unique_app_ids) <= 10) {
          message(sprintf("  Error looking up %s: %s", app_id, e$message))
        }
      })
    }
    
    # Add batch delay to be respectful to API (reduced total delay)
    if (batch_end < length(unique_app_ids)) {
      Sys.sleep(0.5)  # Longer delay between batches, shorter overall time
    }
  }
  
    # Report lookup success rate for API calls only
    not_found_count <- sum(is.na(app_id_to_name))
    total_lookups <- cached_count + length(unique_app_ids)
    message(sprintf("App name lookup completed: %d/%d successful (%.1f%%)", 
                    successful_lookups, total_lookups, 
                    (successful_lookups / total_lookups) * 100))
  }
  
  # Create or update unified_app_name based on lookup results
  if (!"unified_app_name" %in% names(data)) {
    # If no unified_app_name column exists, create it
    data$unified_app_name <- ifelse(
      is.na(app_id_to_name[data[[id_column]]]), 
      data[[id_column]],  # Fallback to app ID if lookup failed
      app_id_to_name[data[[id_column]]]  # Use looked up name
    )
  } else {
    # If unified_app_name exists, only update missing values
    missing_names <- is.na(data$unified_app_name) | data$unified_app_name == ""
    data$unified_app_name[missing_names] <- ifelse(
      is.na(app_id_to_name[data[[id_column]][missing_names]]), 
      data[[id_column]][missing_names],  # Fallback to app ID if lookup failed
      app_id_to_name[data[[id_column]][missing_names]]  # Use looked up name
    )
  }
  
  # Report success rate
  successful_lookups <- sum(!is.na(app_id_to_name))
  total_unique <- length(unique_app_ids)
  message(sprintf("App name lookup completed: %d/%d successful (%.1f%%)", 
                  successful_lookups, total_unique, 100 * successful_lookups / total_unique))
  
  return(data)
}

# Helper function to deduplicate by a specific grouping column
deduplicate_by_group_id <- function(data, group_col) {
  if (nrow(data) == 0 || !group_col %in% names(data)) {
    return(data)
  }
  
  # Separate numeric and non-numeric columns
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  non_numeric_cols <- setdiff(names(data), numeric_cols)
  
  # Metrics to sum (downloads, revenue, counts)
  sum_metrics <- numeric_cols[grepl("downloads|revenue|units|count|absolute", numeric_cols, ignore.case = TRUE)]
  
  # Metrics to average (DAU, MAU, WAU, retention, ratings, percentages)
  avg_metrics <- numeric_cols[grepl("dau|mau|wau|retention|rating|percentage|arpdau|rpd|avg|average", numeric_cols, ignore.case = TRUE)]
  
  # Everything else (first value)
  other_metrics <- setdiff(numeric_cols, c(sum_metrics, avg_metrics))
  
  # Group by the specified column and aggregate
  result <- data %>%
    dplyr::group_by(!!rlang::sym(group_col)) %>%
    dplyr::summarise(
      # Keep the first unified_app_name
      unified_app_name = dplyr::first(.data$unified_app_name),
      
      # Keep first unified_app_id
      unified_app_id = dplyr::first(.data$unified_app_id),
      
      # Sum metrics that should be additive
      dplyr::across(dplyr::all_of(sum_metrics), ~ sum(.x, na.rm = TRUE)),
      
      # Average metrics that should be averaged
      dplyr::across(dplyr::all_of(avg_metrics), ~ mean(.x, na.rm = TRUE)),
      
      # Keep first value for other numeric metrics
      dplyr::across(dplyr::all_of(other_metrics), ~ dplyr::first(.x[!is.na(.x)])),
      
      # Keep first value for non-numeric columns
      dplyr::across(dplyr::all_of(setdiff(non_numeric_cols, c("unified_app_name", "unified_app_id", group_col))), 
                   ~ dplyr::first(.x[!is.na(.x)])),
      
      # Keep first non-NA date
      dplyr::across(where(lubridate::is.Date), ~ dplyr::first(.x[!is.na(.x)])),
      dplyr::across(where(lubridate::is.POSIXt), ~ dplyr::first(.x[!is.na(.x)])),
      
      .groups = "drop"
    ) %>%
    # Remove the grouping column if it starts with a dot (temporary column)
    { if (startsWith(group_col, ".")) dplyr::select(., -!!rlang::sym(group_col)) else . }
  
  # Convert 0 values back to NA where appropriate for averaged metrics
  for (col in avg_metrics) {
    if (col %in% names(result)) {
      result[[col]][result[[col]] == 0] <- NA
    }
  }
  
  return(result)
}

# Helper function to deduplicate apps by consolidating metrics for the same app name
deduplicate_apps_by_name <- function(data) {
  if (nrow(data) == 0 || !"unified_app_name" %in% names(data)) {
    return(data)
  }
  
  # Create normalized names first to check for duplicates
  # Group similar app names more aggressively
  data_check <- data %>%
    dplyr::mutate(
      .name_check = .data$unified_app_name %>%
        # Remove special characters and symbols
        gsub("™|®|©|:|\\*|¤", "", .) %>%
        # Handle specific known patterns
        gsub("NYT Games.*|NYTimes.*", "nyt crossword", ., ignore.case = TRUE) %>%
        gsub("Scrabble.*GO.*", "scrabble go", ., ignore.case = TRUE) %>%
        gsub("Words With Friends.*", "words with friends", ., ignore.case = TRUE) %>%
        gsub("Elevate.*Brain.*", "elevate brain training", ., ignore.case = TRUE) %>%
        gsub("Word Trip.*|WordTrip.*", "word trip", ., ignore.case = TRUE) %>%
        gsub("Heads Up.*|Warner.*Heads Up", "heads up", ., ignore.case = TRUE) %>%
        gsub("Word Connect.*", "word connect", ., ignore.case = TRUE) %>%
        # General cleanup
        gsub("\\s+", " ", .) %>%
        trimws() %>%
        tolower()
    )
  
  # Check if there are actually duplicates to consolidate (after normalization)
  if (length(unique(data_check$.name_check)) == nrow(data)) {
    return(data)  # No duplicates, return as-is
  }
  
  message(sprintf("Consolidating %d app entries into %d unique apps...", 
                  nrow(data), length(unique(data_check$.name_check))))
  
  # The function seems to be crashing here - let's check
  message("DEBUG: About to start deduplication logic...")
  
  # Debug: Check Star Wars entries
  tryCatch({
    sw_count <- sum(grepl("Star Wars.*Galaxy", data$unified_app_name, ignore.case = TRUE))
    message(sprintf("DEBUG: Star Wars entries before dedup: %d", sw_count))
  }, error = function(e) {
    message("DEBUG ERROR: ", e$message)
  })
  
  # Identify numeric columns to sum vs average
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  
  # Metrics to SUM (additive across platforms)
  # IMPORTANT: DAU/MAU/WAU/users are NOT additive - same user can be on multiple platforms
  sum_metrics <- numeric_cols[grepl(
    "downloads|revenue|units|count|absolute", 
    numeric_cols, ignore.case = TRUE
  )]
  # Explicitly exclude user metrics from sum
  sum_metrics <- sum_metrics[!grepl("dau|mau|wau|users", sum_metrics, ignore.case = TRUE)]
  
  # Metrics to take MAX of (user counts - same users across platforms)
  max_metrics <- numeric_cols[grepl(
    "dau|mau|wau|users", 
    numeric_cols, ignore.case = TRUE
  )]
  # Exclude from max if it's a ratio/rate
  max_metrics <- max_metrics[!grepl("rate|ratio|percent", max_metrics, ignore.case = TRUE)]
  
  # Metrics to AVERAGE (rates, percentages, ratios)
  avg_metrics <- numeric_cols[grepl(
    "retention|rpd|rating|age|share|percent|rate|ratio|transformed", 
    numeric_cols, ignore.case = TRUE
  )]
  
  # Everything else (first value)
  other_metrics <- setdiff(numeric_cols, c(sum_metrics, avg_metrics, max_metrics))
  
  # Create normalized name for grouping using the same aggressive patterns
  data <- data %>%
    dplyr::mutate(
      .name_normalized = .data$unified_app_name %>%
        # Remove special characters and symbols
        gsub("™|®|©|:|\\*|¤", "", .) %>%
        # Handle specific known patterns - SAME AS ABOVE
        gsub("NYT Games.*|NYTimes.*", "nyt crossword", ., ignore.case = TRUE) %>%
        gsub("Scrabble.*GO.*", "scrabble go", ., ignore.case = TRUE) %>%
        gsub("Words With Friends.*", "words with friends", ., ignore.case = TRUE) %>%
        gsub("Elevate.*Brain.*", "elevate brain training", ., ignore.case = TRUE) %>%
        gsub("Word Trip.*|WordTrip.*", "word trip", ., ignore.case = TRUE) %>%
        gsub("Heads Up.*|Warner.*Heads Up", "heads up", ., ignore.case = TRUE) %>%
        gsub("Word Connect.*", "word connect", ., ignore.case = TRUE) %>%
        # General cleanup
        gsub("\\s+", " ", .) %>%
        trimws() %>%
        tolower()
    )
  
  # Group by normalized name
  message("Starting grouping operation...")
  
  # Debug: Check if we have the columns we need
  if (!all(c("unified_app_name", "unified_app_id") %in% names(data))) {
    message("ERROR: Missing required columns")
    return(data)
  }
  
  result <- tryCatch({
    data %>%
    dplyr::group_by(.data$.name_normalized) %>%
    dplyr::summarise(
      # Keep the first unified_app_name
      unified_app_name = dplyr::first(.data$unified_app_name),
      
      # Keep first unified_app_id
      unified_app_id = dplyr::first(.data$unified_app_id),
      
      # Sum metrics that should be additive
      dplyr::across(dplyr::all_of(sum_metrics), ~ sum(.x, na.rm = TRUE)),
      
      # Max for user metrics (same users across platforms)
      dplyr::across(dplyr::all_of(max_metrics), ~ max(.x, na.rm = TRUE)),
      
      # Average metrics that are rates/percentages  
      dplyr::across(dplyr::all_of(avg_metrics), ~ mean(.x, na.rm = TRUE)),
      
      # First value for other metrics
      dplyr::across(dplyr::all_of(other_metrics), ~ dplyr::first(.x[!is.na(.x)])),
      
      # First value for character/date columns
      dplyr::across(where(is.character), ~ dplyr::first(.x[!is.na(.x) & .x != ""])),
      dplyr::across(where(lubridate::is.Date), ~ dplyr::first(.x[!is.na(.x)])),
      dplyr::across(where(lubridate::is.POSIXt), ~ dplyr::first(.x[!is.na(.x)])),
      
      .groups = "drop"
    ) %>%
    # Re-order to put unified_app_name first and remove temporary column
    dplyr::select(.data$unified_app_name, .data$unified_app_id, dplyr::everything(), -.data$.name_normalized)
  }, error = function(e) {
    message("ERROR in deduplication: ", e$message)
    return(data)  # Return original data on error
  })
  
  # Convert 0 values back to NA where appropriate for averaged and max metrics
  for (col in c(avg_metrics, max_metrics)) {
    if (col %in% names(result)) {
      # For max metrics, -Inf means all were NA, convert to NA
      if (col %in% max_metrics) {
        result[[col]][is.infinite(result[[col]])] <- NA
      }
      # For avg metrics, 0 from all NAs should be NA
      result[[col]][result[[col]] == 0] <- NA
    }
  }
  
  # Debug: Check Star Wars entries after dedup
  sw_after <- result %>% 
    dplyr::filter(grepl("Star Wars.*Galaxy", unified_app_name, ignore.case = TRUE))
  if (nrow(sw_after) > 0) {
    message(sprintf("DEBUG: Star Wars entries after dedup: %d", nrow(sw_after)))
  }
  
  message(sprintf("Deduplication complete: returning %d rows (was %d)", nrow(result), nrow(data)))
  return(result)
}

# Helper function to convert date columns to proper Date class
clean_date_values <- function(data) {
  if (nrow(data) == 0) return(data)
  
  # Find columns that likely contain date data
  date_cols <- names(data)[sapply(names(data), function(col_name) {
    # Skip non-character columns
    if (!is.character(data[[col_name]])) {
      return(FALSE)
    }
    
    # Check if column name suggests it contains dates
    is_date_column <- grepl("date|release", col_name, ignore.case = TRUE)
    
    # Skip columns that are clearly not dates (like frequency, days ago)
    is_non_date <- grepl("frequency|days ago|update|freq", col_name, ignore.case = TRUE)
    
    if (is_date_column && !is_non_date) {
      # Check if the column actually contains date-like values
      sample_values <- head(data[[col_name]][!is.na(data[[col_name]]) & data[[col_name]] != ""], 5)
      if (length(sample_values) > 0) {
        # Check for various date patterns
        has_iso_dates <- any(grepl("\\d{4}-\\d{2}-\\d{2}", sample_values))
        has_slash_dates <- any(grepl("\\d{4}/\\d{2}/\\d{2}", sample_values))
        has_datetime <- any(grepl("\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}", sample_values))
        
        return(has_iso_dates || has_slash_dates || has_datetime)
      }
    }
    return(FALSE)
  })]
  
  if (length(date_cols) == 0) {
    return(data)
  }
  
  # Convert each date column
  for (col in date_cols) {
    if (is.character(data[[col]])) {
      original_values <- data[[col]]
      
      # Try to convert to dates
      converted_dates <- tryCatch({
        # Handle different date formats
        parsed_dates <- rep(as.Date(NA), length(original_values))
        
        # Handle ISO datetime format (e.g., "2025-07-01T00:00:00Z")
        iso_pattern <- grepl("\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}", original_values)
        if (any(iso_pattern, na.rm = TRUE)) {
          parsed_dates[iso_pattern] <- as.Date(lubridate::ymd_hms(original_values[iso_pattern]))
        }
        
        # Handle YYYY-MM-DD format
        ymd_pattern <- grepl("^\\d{4}-\\d{2}-\\d{2}$", original_values) & !iso_pattern
        if (any(ymd_pattern, na.rm = TRUE)) {
          parsed_dates[ymd_pattern] <- as.Date(original_values[ymd_pattern])
        }
        
        # Handle YYYY/MM/DD format
        slash_pattern <- grepl("^\\d{4}/\\d{2}/\\d{2}$", original_values)
        if (any(slash_pattern, na.rm = TRUE)) {
          parsed_dates[slash_pattern] <- as.Date(original_values[slash_pattern], format = "%Y/%m/%d")
        }
        
        parsed_dates
      }, error = function(e) {
        # If conversion fails, return original
        original_values
      })
      
      # Only replace if we successfully converted most values
      if (inherits(converted_dates, "Date")) {
        non_na_original <- sum(!is.na(original_values) & original_values != "")
        non_na_converted <- sum(!is.na(converted_dates))
        
        if (non_na_original > 0) {
          conversion_rate <- non_na_converted / non_na_original
          if (conversion_rate > 0.5) {  # If more than 50% converted successfully
            data[[col]] <- converted_dates
            message(sprintf("Converted column '%s' to Date class", col))
          }
        }
      }
    }
  }
  
  return(data)
}

#' Clear App Name Cache
#'
#' Clears the internal cache of app name lookups. Useful for testing or when
#' you want to refresh app name data.
#'
#' @return No return value, called for side effects (clearing the cache).
#' @export
st_clear_app_cache <- function() {
  rm(list = ls(envir = .app_name_cache), envir = .app_name_cache)
  message("App name cache cleared")
}

 
