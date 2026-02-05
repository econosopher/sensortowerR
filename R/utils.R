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
# --- Shared API Helpers ---

st_api_base_url <- function() {
  "https://api.sensortower.com"
}

.st_endpoint_registry <- list(
  sales_report_estimates = c("{os}", "sales_report_estimates"),
  sales_report_estimates_comparison_attributes = c("{os}", "sales_report_estimates_comparison_attributes"),
  usage_active_users = c("{os}", "usage", "active_users"),
  usage_demographics = c("{os}", "usage", "demographics"),
  usage_retention = c("{os}", "usage", "retention"),
  top_and_trending_active_users = c("{os}", "top_and_trending", "active_users"),
  top_and_trending_publishers = c("{os}", "top_and_trending", "publishers"),
  ranking = c("{os}", "ranking"),
  apps = c("{os}", "apps"),
  app_tag_apps = c("app_tag", "apps"),
  custom_fields_filter = c("custom_fields_filter"),
  custom_fields_filter_id = c("custom_fields_filter", "{id}"),
  custom_fields_filter_fields_values = c("custom_fields_filter", "fields_values"),
  apps_timeseries = c("apps", "timeseries"),
  apps_timeseries_unified = c("apps", "timeseries", "unified_apps"),
  unified_publishers_apps = c("unified", "publishers", "apps"),
  unified_sales_report_estimates = c("unified", "sales_report_estimates"),
  search_entities = c("{app_store}", "search_entities"),
  games_breakdown = c("{os}", "games_breakdown")
)

resolve_endpoint_segment <- function(segment, placeholders) {
  if (!grepl("^\\{[A-Za-z0-9_]+\\}$", segment)) {
    return(segment)
  }

  key <- gsub("^\\{|\\}$", "", segment)
  value <- placeholders[[key]]
  value_chr <- if (is.null(value)) "" else as.character(value[1])

  if (!nzchar(value_chr)) {
    rlang::abort(sprintf("Missing required endpoint placeholder: '%s'.", key))
  }

  value_chr
}

st_endpoint_relative_segments <- function(endpoint_key, ...) {
  template <- .st_endpoint_registry[[endpoint_key]]
  if (is.null(template)) {
    rlang::abort(sprintf("Unknown endpoint key '%s'.", endpoint_key))
  }

  placeholders <- list(...)

  vapply(
    template,
    resolve_endpoint_segment,
    placeholders = placeholders,
    FUN.VALUE = character(1)
  )
}

st_endpoint_segments <- function(endpoint_key, ...) {
  c("v1", st_endpoint_relative_segments(endpoint_key, ...))
}

st_endpoint_relative_path <- function(endpoint_key, ...) {
  paste(st_endpoint_relative_segments(endpoint_key, ...), collapse = "/")
}

st_endpoint_path <- function(endpoint_key, ...) {
  paste(st_endpoint_segments(endpoint_key, ...), collapse = "/")
}

resolve_auth_token <- function(auth_token = NULL,
                               env_var = "SENSORTOWER_AUTH_TOKEN",
                               error_message = NULL) {
  token <- if (is.null(auth_token)) Sys.getenv(env_var) else auth_token
  token <- trimws(as.character(token[1]))

  if (!nzchar(token)) {
    msg <- if (!is.null(error_message) && nzchar(error_message)) {
      error_message
    } else {
      sprintf("Authentication token not found. Set %s environment variable.", env_var)
    }
    rlang::abort(msg)
  }

  token
}

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
  if (!os %in% c("ios", "android", "unified")) {
    rlang::abort("`os` must be one of 'ios', 'android', or 'unified'")
  }
  if (!comparison_attribute %in% c("absolute", "delta", "transformed_delta")) {
    rlang::abort("`comparison_attribute` must be one of 'absolute', 'delta', 'transformed_delta'")
  }
  if (!is.character(time_range) || !nzchar(time_range)) {
    rlang::abort("`time_range` must be a non-empty string")
  }
  if (!is.character(measure) || !nzchar(measure)) {
    rlang::abort("`measure` must be a non-empty string")
  }
  if (is.null(date)) {
    rlang::abort("`date` must be provided")
  }
  if (is.null(category) && is.null(custom_fields_filter_id)) {
    rlang::abort("Either `category` or `custom_fields_filter_id` must be provided")
  }
  if (is.null(regions)) {
    rlang::abort("`regions` must be provided")
  }
  if (!is.numeric(limit) || limit <= 0 || limit != round(limit)) {
    rlang::abort("`limit` must be a positive integer")
  }

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
    category = category, # Keep category even with custom filter
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
    message("  category: ", if (is.null(params$category)) "NULL" else params$category)
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
    category = category, # Keep category even with custom filter
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

#' Core Data Fetching Function
#'
#' A reusable function to handle the common pattern of fetching data from the API.
#' Handles URL building, request execution, error handling, and response processing.
#'
#' @param endpoint Character. API endpoint path (e.g. "ios/sales_report_estimates").
#' @param params List. Query parameters.
#' @param auth_token Character. API token.
#' @param verbose Logical. Whether to print debug messages.
#' @param enrich_response Logical. Whether to enrich the response with metadata.
#' @param processor Function. Function to process the response. Defaults to process_response.
#'
#' @return A tibble with the results.
#' @keywords internal
fetch_data_core <- function(endpoint, params, auth_token, verbose = FALSE, enrich_response = TRUE, processor = process_response) {
  base_url <- paste0(st_api_base_url(), "/v1")

  # Clean up params
  params <- params[!sapply(params, is.null)]
  params$auth_token <- auth_token

  if (verbose) {
    message("Requesting: ", endpoint)
    # Don't print auth token in debug
    debug_params <- params
    debug_params$auth_token <- "HIDDEN"
    print(debug_params)
  }

  req <- httr2::request(base_url) %>%
    httr2::req_url_path_append(endpoint) %>%
    httr2::req_url_query(!!!params) %>%
    httr2::req_user_agent("sensortowerR") %>%
    httr2::req_retry(max_tries = 3, backoff = function(i) 2^i)

  resp <- tryCatch(
    {
      httr2::req_perform(req)
    },
    error = function(e) {
      if (verbose) message("API Request Failed: ", e$message)
      return(NULL)
    }
  )

  if (is.null(resp)) {
    return(NULL)
  }

  # Call the processor function
  # Check if processor accepts enrich_response argument
  if ("enrich_response" %in% names(formals(processor))) {
    processor(resp, enrich_response = enrich_response)
  } else {
    processor(resp)
  }
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

          custom_tag_cols <- unique(unlist(lapply(result_tbl$entities, function(df) {
            if (is.null(df) || !is.data.frame(df)) {
              return(character())
            }
            names(df)[grepl("^custom_tags\\.", names(df))]
          })))
          for (col in custom_tag_cols) {
            agg_col <- sub("^custom_tags\\.", "aggregate_tags.", col)
            if (!agg_col %in% names(result_tbl)) {
              result_tbl[[agg_col]] <- rep(NA_character_, nrow(result_tbl))
            }
          }
          for (i in seq_len(nrow(result_tbl))) {
            entity_df <- result_tbl$entities[[i]]
            if (is.null(entity_df) || !is.data.frame(entity_df)) next
            for (col in custom_tag_cols) {
              agg_col <- sub("^custom_tags\\.", "aggregate_tags.", col)
              value <- entity_df[[col]][1]
              if (!is.null(value) && !is.na(value) && (is.na(result_tbl[[agg_col]][i]) || result_tbl[[agg_col]][i] == "")) {
                result_tbl[[agg_col]][i] <- value
              }
            }
          }

          # Now safe to remove entities after extracting key fields
          if ("aggregate_tags" %in% names(result_tbl)) {
            # The aggregate_tags column already contains the unified metrics
            # We've extracted gender data, so now we can remove entities
            result_tbl$entities <- NULL # Remove entities to prevent confusion
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
  if (nrow(data) == 0) {
    return(data)
  }

  # Use the global constant METRIC_MAPPING
  metrics_map <- METRIC_MAPPING

  # Also add variations for custom_tags and entities.custom_tags prefixes
  metrics_map <- c(
    metrics_map,
    stats::setNames(metrics_map, gsub("^aggregate_tags\\.", "custom_tags.", names(metrics_map))),
    stats::setNames(metrics_map, gsub("^aggregate_tags\\.", "entities.custom_tags.", names(metrics_map)))
  )
  metrics_map <- metrics_map[!duplicated(names(metrics_map))]

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
# Helper function to clean special characters from numeric values
clean_numeric_values <- function(data) {
  if (nrow(data) == 0) {
    return(data)
  }

  # Define patterns of metrics that should be treated as numeric
  # These patterns match the metric names we extract in extract_custom_metrics
  numeric_metric_patterns <- NUMERIC_METRIC_PATTERNS

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
        if (conversion_rate > 0.5) { # If more than 50% converted successfully
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
  id_column <- NULL
  if ("entities.app_id" %in% names(data)) {
    id_column <- "entities.app_id"
  } else if ("unified_app_id" %in% names(data)) {
    id_column <- "unified_app_id"
  } else if ("app_id" %in% names(data)) {
    id_column <- "app_id"
  } else {
    return(data) # No ID column available
  }

  if (nrow(data) == 0) {
    return(data)
  }

  # Determine which rows still need a unified app name
  needs_lookup <- if ("unified_app_name" %in% names(data)) {
    is.na(data$unified_app_name) | data$unified_app_name == ""
  } else {
    rep(TRUE, nrow(data))
  }

  candidate_ids <- unique(as.character(data[[id_column]][needs_lookup]))
  candidate_ids <- candidate_ids[!is.na(candidate_ids) & candidate_ids != ""]

  if (length(candidate_ids) == 0) {
    if (!"unified_app_name" %in% names(data)) {
      data$unified_app_name <- data[[id_column]]
    }
    return(data)
  }

  verbose_lookup <- getOption("sensortowerR.verbose", FALSE)

  # For unified hex IDs, prefer direct /v1/unified/apps lookup.
  # This avoids search-based resolution returning IDs as fallback names.
  auth_token <- Sys.getenv("SENSORTOWER_AUTH_TOKEN")
  all_hex_ids <- length(candidate_ids) > 0 && all(grepl("^[a-f0-9]{24}$", candidate_ids))
  if (all_hex_ids && nzchar(auth_token)) {
    chunk_size <- 100
    id_chunks <- split(candidate_ids, ceiling(seq_along(candidate_ids) / chunk_size))
    direct_details <- lapply(id_chunks, function(ids) {
      tryCatch(
        st_app_details(
          app_ids = ids,
          os = "unified",
          include_developer_contacts = FALSE,
          auth_token = auth_token
        ),
        error = function(e) NULL
      )
    })

    direct_details <- direct_details[!vapply(direct_details, is.null, logical(1))]
    if (length(direct_details) > 0) {
      direct_tbl <- dplyr::bind_rows(direct_details)
      if (nrow(direct_tbl) > 0 && all(c("app_id", "app_name") %in% names(direct_tbl))) {
        direct_lookup <- direct_tbl %>%
          dplyr::transmute(
            lookup_id = as.character(.data$app_id),
            resolved_app_name = as.character(.data$app_name),
            resolved_unified_id = as.character(.data$app_id)
          ) %>%
          dplyr::distinct(.data$lookup_id, .keep_all = TRUE)

        data <- dplyr::left_join(data, direct_lookup, by = stats::setNames("lookup_id", id_column))

        if (!"unified_app_name" %in% names(data)) {
          data$unified_app_name <- NA_character_
        }
        fill_names <- is.na(data$unified_app_name) | data$unified_app_name == ""
        data$unified_app_name[fill_names] <- dplyr::coalesce(
          data$resolved_app_name[fill_names],
          as.character(data[[id_column]][fill_names])
        )

        if (!"unified_app_id" %in% names(data)) {
          data$unified_app_id <- NA_character_
        }
        fill_unified <- is.na(data$unified_app_id) | data$unified_app_id == ""
        data$unified_app_id[fill_unified] <- dplyr::coalesce(
          data$resolved_unified_id[fill_unified],
          data$unified_app_id[fill_unified]
        )

        data <- data %>%
          dplyr::select(-dplyr::any_of(c("resolved_app_name", "resolved_unified_id")))

        unresolved <- is.na(data$unified_app_name) | data$unified_app_name == "" |
          grepl("^[a-f0-9]{24}$", data$unified_app_name)
        if (!any(unresolved)) {
          return(data)
        }
      }
    }
  }

  # Attempt to satisfy requests from the ID cache first
  cache_entries <- lapply(candidate_ids, lookup_cached_id)
  names(cache_entries) <- candidate_ids
  missing_ids <- candidate_ids[vapply(cache_entries, is.null, logical(1))]

  # Resolve any missing IDs using the shared resolution pipeline
  if (length(missing_ids) > 0) {
    if (nzchar(auth_token)) {
      if (verbose_lookup) {
        message("Resolving ", length(missing_ids), " app IDs for name enrichment...")
      }
      batch_resolve_ids(missing_ids, auth_token = auth_token, use_cache = TRUE, verbose = verbose_lookup)
      cache_entries[missing_ids] <- lapply(missing_ids, lookup_cached_id)
    } else if (verbose_lookup) {
      message("Skipping name lookup for ", length(missing_ids), " IDs (missing auth token)")
    }
  }

  # Build a lookup table from cache results
  lookup_table <- data.frame(
    lookup_id = candidate_ids,
    resolved_app_name = NA_character_,
    resolved_unified_id = NA_character_,
    stringsAsFactors = FALSE
  )

  for (i in seq_along(candidate_ids)) {
    entry <- cache_entries[[candidate_ids[i]]]
    if (!is.null(entry)) {
      name_val <- entry$app_name
      if (!is.null(name_val)) {
        name_val <- as.character(name_val)[1]
        if (!is.na(name_val) && nzchar(name_val)) {
          lookup_table$resolved_app_name[i] <- name_val
          assign(candidate_ids[i], name_val, envir = .app_name_cache)
        }
      }
      unified_val <- entry$unified_app_id
      if (!is.null(unified_val)) {
        unified_val <- as.character(unified_val)[1]
        if (!is.na(unified_val) && nzchar(unified_val)) {
          lookup_table$resolved_unified_id[i] <- unified_val
        }
      }
    }
  }

  join_cols <- stats::setNames("lookup_id", id_column)
  data <- dplyr::left_join(data, lookup_table, by = join_cols)

  if (!"unified_app_name" %in% names(data)) {
    data$unified_app_name <- NA_character_
  }
  fill_names <- is.na(data$unified_app_name) | data$unified_app_name == ""
  data$unified_app_name[fill_names] <- dplyr::coalesce(
    data$resolved_app_name[fill_names],
    as.character(data[[id_column]][fill_names])
  )

  if (!"unified_app_id" %in% names(data)) {
    data$unified_app_id <- NA_character_
  }
  fill_unified <- is.na(data$unified_app_id) | data$unified_app_id == ""
  data$unified_app_id[fill_unified] <- dplyr::coalesce(
    data$resolved_unified_id[fill_unified],
    data$unified_app_id[fill_unified]
  )

  data <- data %>%
    dplyr::select(-dplyr::any_of(c("resolved_app_name", "resolved_unified_id")))

  data
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
      dplyr::across(
        dplyr::all_of(setdiff(non_numeric_cols, c("unified_app_name", "unified_app_id", group_col))),
        ~ dplyr::first(.x[!is.na(.x)])
      ),

      # Keep first non-NA date
      dplyr::across(where(lubridate::is.Date), ~ dplyr::first(.x[!is.na(.x)])),
      dplyr::across(where(lubridate::is.POSIXt), ~ dplyr::first(.x[!is.na(.x)])),
      .groups = "drop"
    ) %>%
    # Remove the grouping column if it starts with a dot (temporary column)
    {
      if (startsWith(group_col, ".")) dplyr::select(., -!!rlang::sym(group_col)) else .
    }

  # Convert 0 values back to NA where appropriate for averaged metrics
  for (col in avg_metrics) {
    if (col %in% names(result)) {
      result[[col]][result[[col]] == 0] <- NA
    }
  }

  return(result)
}

# Helper function to deduplicate apps by consolidating metrics for the same app name
deduplicate_apps_by_name <- function(data, fuzzy_match = TRUE) {
  if (nrow(data) == 0 || !"unified_app_name" %in% names(data)) {
    return(data)
  }

  # Create normalized names first to check for duplicates
  if (fuzzy_match) {
    # Group similar app names more aggressively
    data <- data %>%
      dplyr::mutate(
        .name_normalized = .data$unified_app_name %>%
          # Remove special characters and symbols
          gsub("\u2122|\u00AE|\u00A9|:|\\*|\u00A4", "", .) %>%
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
  } else {
    # Simple normalization
    data <- data %>%
      dplyr::mutate(
        .name_normalized = tolower(trimws(.data$unified_app_name))
      )
  }

  # Check if there are actually duplicates to consolidate (after normalization)
  if (length(unique(data$.name_normalized)) == nrow(data)) {
    return(dplyr::select(data, -dplyr::any_of(".name_normalized"))) # No duplicates, return as-is
  }

  message(sprintf(
    "Consolidating %d app entries into %d unique apps...",
    nrow(data), length(unique(data$.name_normalized))
  ))

  # Identify numeric columns to sum vs average
  numeric_cols <- names(data)[sapply(data, is.numeric)]

  # Metrics to SUM (additive across platforms)
  # IMPORTANT: DAU/MAU/WAU/users are NOT additive - same user can be on multiple platforms
  sum_metrics <- numeric_cols[grepl(
    "downloads|revenue|units|count|absolute",
    numeric_cols,
    ignore.case = TRUE
  )]
  # Explicitly exclude user metrics from sum
  sum_metrics <- sum_metrics[!grepl("dau|mau|wau|users", sum_metrics, ignore.case = TRUE)]

  # Metrics to TAKE MAX (user counts - same users across platforms)
  max_metrics <- numeric_cols[grepl(
    "dau|mau|wau|users",
    numeric_cols,
    ignore.case = TRUE
  )]
  # Exclude from max if it's a ratio/rate
  max_metrics <- max_metrics[!grepl("rate|ratio|percent", max_metrics, ignore.case = TRUE)]

  # Metrics to AVERAGE (rates, percentages, ratios)
  avg_metrics <- numeric_cols[grepl(
    "retention|rpd|rating|age|share|percent|rate|ratio|transformed",
    numeric_cols,
    ignore.case = TRUE
  )]

  # Everything else (first value)
  other_metrics <- setdiff(numeric_cols, c(sum_metrics, avg_metrics, max_metrics))

  # Group by normalized name
  result <- tryCatch(
    {
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
        dplyr::relocate(unified_app_name, unified_app_id) %>%
        dplyr::select(-dplyr::any_of(".name_normalized"))
    },
    error = function(e) {
      message("Warning: Deduplication failed, returning original data. Error: ", e$message)
      return(dplyr::select(data, -dplyr::any_of(".name_normalized")))
    }
  )

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

  return(result)
}

# Helper function to convert date columns to proper Date class
clean_date_values <- function(data) {
  if (nrow(data) == 0) {
    return(data)
  }

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
      converted_dates <- tryCatch(
        {
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
        },
        error = function(e) {
          # If conversion fails, return original
          original_values
        }
      )

      # Only replace if we successfully converted most values
      if (inherits(converted_dates, "Date")) {
        non_na_original <- sum(!is.na(original_values) & original_values != "")
        non_na_converted <- sum(!is.na(converted_dates))

        if (non_na_original > 0) {
          conversion_rate <- non_na_converted / non_na_original
          if (conversion_rate > 0.5) { # If more than 50% converted successfully
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

#' Fetch and Unified Data from Platforms
#'
#' Fetches data from iOS and/or Android and optionally combines them.
#' Handles missing data from one platform gracefully.
#'
#' @param ios_app_id Character. iOS App ID.
#' @param android_app_id Character. Android App ID.
#' @param start_date Date. Start date.
#' @param end_date Date. End date.
#' @param countries Character vector. Country codes.
#' @param date_granularity Character. Granularity.
#' @param auth_token Character. API token.
#' @param verbose Logical. Verbose output.
#' @param combine_to_unified Logical. Whether to sum metrics into a unified view.
#'
#' @return A tibble with columns date, country, revenue, downloads, and optionally platform/app_id.
#' @keywords internal
fetch_unified_data <- function(
  ios_app_id = NULL,
  android_app_id = NULL,
  start_date,
  end_date,
  countries,
  date_granularity,
  auth_token,
  verbose = FALSE,
  combine_to_unified = TRUE
) {
  all_data <- tibble::tibble()

  # Track what we requested vs what we got
  ios_requested <- !is.null(ios_app_id) && !is.na(ios_app_id)
  android_requested <- !is.null(android_app_id) && !is.na(android_app_id)
  ios_has_data <- FALSE
  android_has_data <- FALSE

  # Fetch iOS data
  if (ios_requested) {
    if (verbose) message("Fetching iOS data for: ", ios_app_id)
    ios_result <- tryCatch(
      {
        st_sales_report(
          os = "ios",
          ios_app_id = ios_app_id,
          countries = countries,
          start_date = start_date,
          end_date = end_date,
          date_granularity = date_granularity,
          auth_token = auth_token,
          verbose = FALSE # Suppress inner verbose to avoid noise
        )
      },
      error = function(e) {
        if (verbose) message("iOS fetch warning: ", e$message)
        NULL
      }
    )

    if (!is.null(ios_result) && nrow(ios_result) > 0) {
      ios_has_data <- TRUE
      # Standardize columns
      ios_result <- ios_result %>%
        dplyr::mutate(
          platform = "ios",
          app_id = as.character(ios_app_id),
          app_id_type = "ios",
          revenue = if ("total_revenue" %in% names(.)) total_revenue else if ("revenue" %in% names(.)) revenue else 0,
          downloads = if ("total_downloads" %in% names(.)) total_downloads else if ("downloads" %in% names(.)) downloads else 0
        ) %>%
        dplyr::select(date, country, revenue, downloads, platform, app_id, app_id_type)

      all_data <- dplyr::bind_rows(all_data, ios_result)
    }
  }

  # Fetch Android data
  if (android_requested) {
    if (verbose) message("Fetching Android data for: ", android_app_id)
    android_result <- tryCatch(
      {
        st_sales_report(
          os = "android",
          android_app_id = android_app_id,
          countries = countries,
          start_date = start_date,
          end_date = end_date,
          date_granularity = date_granularity,
          auth_token = auth_token,
          verbose = FALSE
        )
      },
      error = function(e) {
        if (verbose) message("Android fetch warning: ", e$message)
        NULL
      }
    )

    if (!is.null(android_result) && nrow(android_result) > 0) {
      android_has_data <- TRUE
      # Standardize columns
      android_result <- android_result %>%
        dplyr::mutate(
          platform = "android",
          app_id = as.character(android_app_id),
          app_id_type = "android",
          country = if ("c" %in% names(.)) c else country
        ) %>%
        dplyr::select(date, country, revenue, downloads, platform, app_id, app_id_type)

      all_data <- dplyr::bind_rows(all_data, android_result)
    }
  }

  # Handle missing data scenarios
  if (ios_requested && android_requested) {
    if (!ios_has_data && !android_has_data) {
      if (verbose) message("No data available for either platform.")
      return(tibble::tibble(date = as.Date(character()), country = character(), revenue = numeric(), downloads = numeric()))
    } else if (!ios_has_data && verbose) {
      message("Missing iOS data (returning Android only).")
    } else if (!android_has_data && verbose) {
      message("Missing Android data (returning iOS only).")
    }
  } else if ((ios_requested && !ios_has_data) || (android_requested && !android_has_data)) {
    if (verbose) message("No data returned for requested platform.")
    return(tibble::tibble(date = as.Date(character()), country = character(), revenue = numeric(), downloads = numeric()))
  }

  # Combine or return raw
  if (nrow(all_data) > 0) {
    if (combine_to_unified) {
      combined <- all_data %>%
        dplyr::group_by(date, country) %>%
        dplyr::summarise(
          revenue = sum(revenue, na.rm = TRUE),
          downloads = sum(downloads, na.rm = TRUE),
          .groups = "drop"
        )
      return(combined)
    } else {
      return(all_data)
    }
  }

  return(all_data)
}
