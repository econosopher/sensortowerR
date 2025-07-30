#' Custom Filter Helper Functions
#' 
#' Functions to validate, test, and manage Sensor Tower custom field filters
#' 
#' @name filter_helpers
NULL

#' Validate Custom Field Filter ID Format
#' 
#' Checks if a filter ID matches the expected 24-character hexadecimal format
#' used by Sensor Tower.
#' 
#' @param filter_id Character string. The filter ID to validate
#' @return Logical. TRUE if valid format, FALSE otherwise
#' @export
#' @examples
#' \dontrun{
#' # Valid filter ID
#' st_is_valid_filter_id("687df26ac5a19ebcfe817d7f")  # TRUE
#' 
#' # Invalid filter IDs
#' st_is_valid_filter_id("invalid")  # FALSE
#' st_is_valid_filter_id("687df26ac5a19ebcfe817d7")  # FALSE (too short)
#' }
st_is_valid_filter_id <- function(filter_id) {
  if (is.null(filter_id) || !is.character(filter_id) || length(filter_id) != 1) {
    return(FALSE)
  }
  grepl("^[a-f0-9]{24}$", filter_id, ignore.case = TRUE)
}

#' Test a Custom Filter ID
#' 
#' Tests whether a custom filter ID works with the Sensor Tower API by making
#' a minimal test request. This helps verify that the filter exists and is
#' accessible with your authentication.
#' 
#' @param filter_id Character string. The filter ID to test
#' @param os Character string. Operating system to test with. One of "ios", 
#'   "android", or "unified". Defaults to "ios".
#' @param verbose Logical. Whether to print detailed test results. Defaults to TRUE.
#' @param auth_token Optional. Character string. Your Sensor Tower API token.
#'   Defaults to environment variable SENSORTOWER_AUTH_TOKEN.
#' @return List with test results including success status and any error messages
#' @export
#' @examples
#' \dontrun{
#' # Test a filter ID
#' result <- st_test_filter("687df26ac5a19ebcfe817d7f")
#' 
#' # Test silently
#' result <- st_test_filter("687df26ac5a19ebcfe817d7f", verbose = FALSE)
#' 
#' # Test with different OS
#' result <- st_test_filter("687df26ac5a19ebcfe817d7f", os = "unified")
#' }
st_test_filter <- function(filter_id, 
                          os = "ios", 
                          verbose = TRUE,
                          auth_token = NULL) {
  
  # Validate filter format first
  if (!st_is_valid_filter_id(filter_id)) {
    result <- list(
      success = FALSE,
      filter_id = filter_id,
      error = "Invalid filter ID format. Expected 24-character hexadecimal string.",
      os_tested = os
    )
    
    if (verbose) {
      cat("❌ Invalid filter ID format:", filter_id, "\n")
    }
    
    return(invisible(result))
  }
  
  if (verbose) {
    cat("Testing filter ID:", filter_id, "\n")
    cat("Operating System:", os, "\n")
  }
  
  # Prepare test parameters
  test_params <- list(
    os = os,
    measure = "revenue",
    custom_fields_filter_id = filter_id,
    regions = "US",
    limit = 1,
    auth_token = auth_token
  )
  
  # Add custom_tags_mode if unified
  if (os == "unified") {
    test_params$custom_tags_mode <- "include"
  }
  
  # Make test API call
  test_result <- tryCatch(
    {
      data <- do.call(st_top_charts, test_params)
      list(
        success = TRUE,
        filter_id = filter_id,
        os_tested = os,
        rows_returned = nrow(data),
        data_sample = if (nrow(data) > 0) data[1, ] else NULL
      )
    },
    error = function(e) {
      error_details <- list(
        success = FALSE,
        filter_id = filter_id,
        os_tested = os,
        error = e$message
      )
      
      # Categorize error type
      if (grepl("422", e$message)) {
        error_details$error_type <- "invalid_filter"
        error_details$suggestion <- "Filter doesn't exist or belongs to different account"
      } else if (grepl("403", e$message)) {
        error_details$error_type <- "access_denied"
        error_details$suggestion <- "Check your API authentication"
      } else if (grepl("401", e$message)) {
        error_details$error_type <- "authentication"
        error_details$suggestion <- "Invalid or missing API token"
      } else {
        error_details$error_type <- "unknown"
        error_details$suggestion <- "Check API status and parameters"
      }
      
      error_details
    }
  )
  
  if (verbose) {
    if (test_result$success) {
      cat("✅ Filter test successful!\n")
      if (!is.null(test_result$rows_returned)) {
        cat("   Returned", test_result$rows_returned, "rows\n")
      }
    } else {
      cat("❌ Filter test failed\n")
      cat("   Error:", test_result$error, "\n")
      if (!is.null(test_result$suggestion)) {
        cat("   Suggestion:", test_result$suggestion, "\n")
      }
    }
  }
  
  invisible(test_result)
}

#' Generate Example Filter IDs for Testing
#' 
#' Generates example filter IDs in the correct format for testing purposes.
#' Note: These are randomly generated and won't work with the actual API unless
#' they happen to match a real filter in your account.
#' 
#' @param n Integer. Number of example IDs to generate. Defaults to 5.
#' @param seed Integer. Random seed for reproducibility. Optional.
#' @return Character vector of example filter IDs
#' @export
#' @examples
#' # Generate example IDs
#' st_generate_example_filter_ids(3)
#' 
#' # Generate with seed for reproducibility
#' st_generate_example_filter_ids(3, seed = 123)
st_generate_example_filter_ids <- function(n = 5, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Generate random hex strings
  ids <- character(n)
  for (i in seq_len(n)) {
    ids[i] <- paste0(
      sprintf("%x", sample(0:15, 24, replace = TRUE)),
      collapse = ""
    )
  }
  
  ids
}

#' Build Sensor Tower Filter URL
#' 
#' Constructs a URL for the Sensor Tower web interface where you can create
#' custom filters. Visit this URL, configure your filters, and then copy the
#' custom_fields_filter_id from the resulting URL.
#' 
#' @param os Character string. Operating system filter. One of "ios", "android", 
#'   or "unified". Defaults to "unified".
#' @param category Optional. Category ID to pre-select.
#' @param countries Optional. Character vector of country codes to pre-select.
#' @param base_url Character string. Base URL for Sensor Tower. Defaults to 
#'   "https://app.sensortower.com/top-charts".
#' @return Character string. The constructed URL.
#' @export
#' @examples
#' \dontrun{
#' # Build URL for iOS games in US
#' url <- st_build_filter_url(os = "ios", category = 6014, countries = "US")
#' 
#' # Open in browser
#' browseURL(url)
#' }
st_build_filter_url <- function(os = "unified",
                               category = NULL,
                               countries = NULL,
                               base_url = "https://app.sensortower.com/top-charts") {
  
  # Validate OS
  os <- match.arg(os, c("ios", "android", "unified"))
  
  # Build query parameters
  params <- list(os = os)
  
  if (!is.null(category)) {
    params$category <- as.character(category)
  }
  
  if (!is.null(countries)) {
    if (is.character(countries)) {
      params$countries <- paste(countries, collapse = ",")
    }
  }
  
  # Construct URL
  url <- httr::modify_url(base_url, query = params)
  
  message("Filter creation URL generated:")
  message(url)
  message("\nSteps to create a custom filter:")
  message("1. Visit the URL above")
  message("2. Configure your desired filters")
  message("3. Copy the custom_fields_filter_id from the URL")
  message("4. Use it in sensortowerR functions")
  
  invisible(url)
}

#' Extract Filter ID from Sensor Tower URL
#' 
#' Extracts the custom_fields_filter_id parameter from a Sensor Tower URL.
#' This is helpful when copying URLs from the web interface.
#' 
#' @param url Character string. A Sensor Tower URL containing custom_fields_filter_id
#' @return Character string. The extracted filter ID, or NULL if not found.
#' @export
#' @examples
#' \dontrun{
#' url <- "https://app.sensortower.com/top-charts?custom_fields_filter_id=687df26ac5a19ebcfe817d7f"
#' filter_id <- st_extract_filter_id(url)
#' }
st_extract_filter_id <- function(url) {
  if (is.null(url) || !is.character(url)) {
    return(NULL)
  }
  
  # Parse URL
  parsed <- httr::parse_url(url)
  
  # Extract from query parameters
  filter_id <- parsed$query$custom_fields_filter_id
  
  if (is.null(filter_id)) {
    # Try regex as fallback
    match <- regmatches(url, regexpr("custom_fields_filter_id=([a-f0-9]{24})", url))
    if (length(match) > 0) {
      filter_id <- sub("custom_fields_filter_id=", "", match[1])
    }
  }
  
  # Validate format
  if (!is.null(filter_id) && st_is_valid_filter_id(filter_id)) {
    message("Extracted filter ID: ", filter_id)
    return(filter_id)
  }
  
  message("No valid filter ID found in URL")
  return(NULL)
}

#' Compare Results With and Without Custom Filter
#' 
#' Compares the results of API calls with a custom filter versus a standard
#' category filter to understand what the custom filter is doing.
#' 
#' @param filter_id Character string. The custom filter ID to test
#' @param category Category ID to use for comparison
#' @param os Character string. Operating system. Defaults to "ios".
#' @param regions Character string. Region code. Defaults to "US".
#' @param limit Integer. Number of results to fetch. Defaults to 20.
#' @return List containing both result sets and comparison statistics
#' @export
st_compare_filter_results <- function(filter_id,
                                     category = 6014,  # Games
                                     os = "ios",
                                     regions = "US",
                                     limit = 20) {
  
  if (!st_is_valid_filter_id(filter_id)) {
    stop("Invalid filter ID format")
  }
  
  message("Fetching results with custom filter...")
  filtered_results <- tryCatch(
    st_top_charts(
      os = os,
      custom_fields_filter_id = filter_id,
      regions = regions,
      limit = limit
    ),
    error = function(e) {
      stop("Failed to fetch filtered results: ", e$message)
    }
  )
  
  message("Fetching results with category filter...")
  category_results <- tryCatch(
    st_top_charts(
      os = os,
      category = category,
      regions = regions,
      limit = limit
    ),
    error = function(e) {
      stop("Failed to fetch category results: ", e$message)
    }
  )
  
  # Compare results
  comparison <- list(
    filtered = filtered_results,
    category = category_results,
    stats = list(
      filtered_count = nrow(filtered_results),
      category_count = nrow(category_results)
    )
  )
  
  # Find apps in filtered but not in category
  if ("unified_app_id" %in% names(filtered_results) && 
      "unified_app_id" %in% names(category_results)) {
    
    comparison$stats$unique_to_filtered <- setdiff(
      filtered_results$unified_app_id,
      category_results$unified_app_id
    )
    
    comparison$stats$unique_to_category <- setdiff(
      category_results$unified_app_id,
      filtered_results$unified_app_id
    )
    
    comparison$stats$common_apps <- intersect(
      filtered_results$unified_app_id,
      category_results$unified_app_id
    )
  }
  
  message("\nComparison Results:")
  message("Filtered results: ", comparison$stats$filtered_count, " apps")
  message("Category results: ", comparison$stats$category_count, " apps")
  
  if (!is.null(comparison$stats$common_apps)) {
    message("Common apps: ", length(comparison$stats$common_apps))
    message("Unique to filter: ", length(comparison$stats$unique_to_filtered))
    message("Unique to category: ", length(comparison$stats$unique_to_category))
  }
  
  invisible(comparison)
}