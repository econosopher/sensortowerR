#' Parse Sensor Tower Web URL to API Parameters
#' 
#' Converts a Sensor Tower web interface URL into API-compatible parameters
#' that can be used with sensortowerR functions. This is helpful when you want
#' to replicate a web query in R.
#' 
#' @param url Character string. A Sensor Tower web interface URL
#' @param verbose Logical. Whether to print parameter mapping details. 
#'   Defaults to TRUE.
#' @return List of API-compatible parameters suitable for use with st_top_charts()
#'   and other sensortowerR functions
#' @export
#' @examples
#' \dontrun{
#' # Parse a web URL
#' url <- "https://app.sensortower.com/market-analysis/top-apps?os=unified&measure=DAU"
#' params <- st_parse_web_url(url)
#' 
#' # Use the parameters in an API call
#' data <- do.call(st_top_charts, params)
#' 
#' # Or modify parameters before using
#' params$limit <- 50
#' data <- do.call(st_top_charts, params)
#' }
st_parse_web_url <- function(url, verbose = TRUE) {
  if (is.null(url) || !is.character(url)) {
    stop("URL must be a character string")
  }
  
  # Parse the URL
  parsed <- httr::parse_url(url)
  
  if (is.null(parsed$query)) {
    stop("No query parameters found in URL")
  }
  
  query <- parsed$query
  
  if (verbose) {
    cat("=== Parsing Sensor Tower Web URL ===\n")
    cat("Path:", parsed$path, "\n")
    cat("Parameters found:", length(unique(names(query))), "\n\n")
  }
  
  # Initialize API parameters
  api_params <- list()
  
  # Direct parameter mappings
  direct_mappings <- list(
    os = "os",
    measure = "measure",
    comparison_attribute = "comparison_attribute",
    custom_fields_filter_id = "custom_fields_filter_id"
  )
  
  for (web_param in names(direct_mappings)) {
    if (!is.null(query[[web_param]])) {
      api_params[[direct_mappings[[web_param]]]] <- query[[web_param]]
      if (verbose) {
        cat(sprintf("  %s: %s\n", web_param, query[[web_param]]))
      }
    }
  }
  
  # Date handling
  if (!is.null(query$start_date)) {
    api_params$date <- query$start_date
    if (verbose) cat("  start_date ->", api_params$date, "\n")
  }
  
  if (!is.null(query$end_date)) {
    api_params$end_date <- query$end_date
    if (verbose) cat("  end_date:", query$end_date, "\n")
  }
  
  # Granularity/time range mapping
  if (!is.null(query$granularity)) {
    time_range_map <- c(
      "daily" = "day",
      "weekly" = "week", 
      "monthly" = "month",
      "quarterly" = "quarter"
    )
    
    api_params$time_range <- time_range_map[query$granularity] %||% query$granularity
    if (verbose) {
      cat(sprintf("  granularity: %s -> time_range: %s\n", 
                  query$granularity, api_params$time_range))
    }
  }
  
  # Category handling (0 = all categories)
  if (!is.null(query$category)) {
    if (query$category == "0") {
      if (verbose) cat("  category: 0 (All Categories) - omitting from API call\n")
    } else {
      api_params$category <- query$category
      if (verbose) cat("  category:", query$category, "\n")
    }
  }
  
  # Country/regions handling
  countries <- query[names(query) == "country"]
  if (length(countries) > 0) {
    unique_countries <- unique(unlist(countries))
    
    if (verbose) {
      cat(sprintf("  countries: %d unique countries specified\n", 
                  length(unique_countries)))
    }
    
    # If too many countries, suggest using WW
    if (length(unique_countries) > 50) {
      api_params$regions <- "WW"
      if (verbose) {
        cat("    -> Using 'WW' (worldwide) due to large country list\n")
      }
    } else {
      api_params$regions <- paste(unique_countries, collapse = ",")
      if (verbose && length(unique_countries) <= 10) {
        cat("    ->", api_params$regions, "\n")
      }
    }
  }
  
  # Pagination
  if (!is.null(query$page_size)) {
    api_params$limit <- as.integer(query$page_size)
    if (verbose) cat("  page_size ->", api_params$limit, "\n")
  }
  
  if (!is.null(query$page) && !is.null(query$page_size)) {
    page <- as.integer(query$page)
    limit <- as.integer(query$page_size)
    api_params$offset <- (page - 1) * limit
    if (verbose) {
      cat(sprintf("  page %d (size %d) -> offset: %d\n", 
                  page, limit, api_params$offset))
    }
  }
  
  # Custom fields filter mode
  if (!is.null(query$custom_fields_filter_mode)) {
    api_params$custom_tags_mode <- query$custom_fields_filter_mode
    if (verbose) {
      cat(sprintf("  custom_fields_filter_mode -> custom_tags_mode: %s\n",
                  api_params$custom_tags_mode))
    }
  }
  
  # Device handling (aggregate multiple device parameters)
  devices <- query[names(query) == "device"]
  if (length(devices) > 0) {
    unique_devices <- unique(unlist(devices))
    
    # If all devices are selected, it's essentially unified
    if (all(c("iphone", "ipad", "android") %in% unique_devices)) {
      if (verbose) {
        cat("  devices: all platforms selected (iphone, ipad, android)\n")
        cat("    -> Implicit in os=unified\n")
      }
    } else if (api_params$os %in% c("ios", "unified") && 
               any(c("iphone", "ipad") %in% unique_devices)) {
      # For iOS, we can specify device_type
      if (all(c("iphone", "ipad") %in% unique_devices)) {
        api_params$device_type <- "total"
      } else if ("iphone" %in% unique_devices) {
        api_params$device_type <- "iphone"
      } else if ("ipad" %in% unique_devices) {
        api_params$device_type <- "ipad"
      }
      
      if (verbose && !is.null(api_params$device_type)) {
        cat("  device_type:", api_params$device_type, "\n")
      }
    }
  }
  
  # Web-specific parameters to ignore
  ignored_params <- c("edit", "duration", "period", "metric")
  web_only <- intersect(names(query), ignored_params)
  
  if (verbose && length(web_only) > 0) {
    cat("\nWeb-only parameters (ignored):\n")
    for (param in web_only) {
      cat(sprintf("  %s: %s\n", param, query[[param]]))
    }
  }
  
  # Validation warnings
  if (verbose) {
    cat("\n=== Validation ===\n")
    
    # Check for required parameters
    if (is.null(api_params$regions)) {
      cat("!  No regions specified - you may need to add regions parameter\n")
    }
    
    # Check for custom filter without tags mode
    if (!is.null(api_params$custom_fields_filter_id) && 
        is.null(api_params$custom_tags_mode) &&
        api_params$os == "unified") {
      cat("!  custom_fields_filter_id with unified OS requires custom_tags_mode\n")
    }
    
    cat("\nReady to use with sensortowerR functions!\n")
  }
  
  # Return the API parameters
  invisible(api_params)
}

#' Extract All Parameters from Sensor Tower URL
#' 
#' Extracts and displays all parameters from a Sensor Tower web URL
#' in a readable format. Useful for understanding complex URLs.
#' 
#' @param url Character string. A Sensor Tower web interface URL
#' @return Data frame with parameter names and values
#' @export
#' @examples
#' \dontrun{
#' url <- "https://app.sensortower.com/market-analysis/top-apps?os=unified&measure=DAU"
#' params_df <- st_extract_url_params(url)
#' View(params_df)
#' }
st_extract_url_params <- function(url) {
  parsed <- httr::parse_url(url)
  
  if (is.null(parsed$query)) {
    stop("No query parameters found in URL")
  }
  
  # Convert to data frame
  params_list <- list()
  
  for (name in unique(names(parsed$query))) {
    values <- parsed$query[names(parsed$query) == name]
    if (length(values) == 1) {
      params_list[[length(params_list) + 1]] <- data.frame(
        parameter = name,
        value = values[[1]],
        count = 1,
        stringsAsFactors = FALSE
      )
    } else {
      # Multiple values (like country)
      params_list[[length(params_list) + 1]] <- data.frame(
        parameter = name,
        value = paste(unlist(values), collapse = ", "),
        count = length(values),
        stringsAsFactors = FALSE
      )
    }
  }
  
  params_df <- do.call(rbind, params_list)
  
  # Add interpretation
  params_df$interpretation <- ""
  
  # Add interpretations for common parameters
  interpretations <- list(
    os = "Operating system filter",
    measure = "Metric type (DAU, WAU, MAU, revenue, units)",
    comparison_attribute = "How to compare apps",
    granularity = "Time aggregation level",
    start_date = "Beginning of date range",
    end_date = "End of date range",
    category = "App category filter (0 = all)",
    country = "Country/region filters",
    device = "Device type filters",
    page_size = "Results per page",
    page = "Current page number",
    custom_fields_filter_id = "Custom filter from web UI",
    custom_fields_filter_mode = "How custom filter applies to unified apps"
  )
  
  for (i in 1:nrow(params_df)) {
    param <- params_df$parameter[i]
    if (param %in% names(interpretations)) {
      params_df$interpretation[i] <- interpretations[[param]]
    }
  }
  
  params_df
}

#' Create Sensor Tower Web URL from Parameters
#' 
#' Builds a Sensor Tower web interface URL from API parameters.
#' This is the reverse of st_parse_web_url().
#' 
#' @param os Operating system
#' @param measure Measure type
#' @param category Category ID
#' @param regions Region codes (converted to country parameters)
#' @param start_date Start date
#' @param end_date End date
#' @param custom_fields_filter_id Custom filter ID
#' @param custom_tags_mode Custom tags mode
#' @param ... Additional parameters
#' @return Character string URL
#' @export
st_build_web_url <- function(os = "unified",
                            measure = "revenue",
                            category = NULL,
                            regions = "US",
                            start_date = NULL,
                            end_date = NULL,
                            custom_fields_filter_id = NULL,
                            custom_tags_mode = NULL,
                            ...) {
  
  base_url <- "https://app.sensortower.com/market-analysis/top-apps"
  
  # Build query parameters
  query <- list(os = os, measure = measure)
  
  # Add optional parameters
  if (!is.null(category)) query$category <- category
  if (!is.null(start_date)) query$start_date <- start_date
  if (!is.null(end_date)) query$end_date <- end_date
  if (!is.null(custom_fields_filter_id)) {
    query$custom_fields_filter_id <- custom_fields_filter_id
  }
  if (!is.null(custom_tags_mode)) {
    query$custom_fields_filter_mode <- custom_tags_mode
  }
  
  # Handle regions -> countries
  if (!is.null(regions)) {
    if (regions == "WW") {
      # Don't add country parameters for worldwide
      message("Note: 'WW' (worldwide) may require selecting all countries in web UI")
    } else {
      countries <- unlist(strsplit(regions, ","))
      # Add each country as a separate parameter
      for (country in countries) {
        query[[paste0("country[", length(query) + 1, "]")]] <- trimws(country)
      }
    }
  }
  
  # Build URL
  url <- httr::modify_url(base_url, query = query)
  
  # Fix country parameter format (remove the indexing)
  url <- gsub("country%5B\\d+%5D", "country", url)
  
  message("Generated URL:")
  message(url)
  
  invisible(url)
}