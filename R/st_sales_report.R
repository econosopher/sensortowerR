#' Fetch Sales Report Estimates
#'
#' Retrieves download and revenue estimates of apps by country and date.
#' Note: All revenues are returned in cents and need to be divided by 100 for dollar amounts.
#'
#' @param app_ids Character vector. App IDs to query. At least one app ID or publisher ID is required.
#' @param publisher_ids Character vector. Publisher IDs to query. Some Android publisher IDs contain commas.
#' @param os Character string. Operating system: "ios" or "android".
#' @param countries Character vector. Country codes (e.g., c("US", "GB", "JP")).
#' @param start_date Date or character string. Start date in "YYYY-MM-DD" format.
#' @param end_date Date or character string. End date in "YYYY-MM-DD" format.
#' @param date_granularity Character string. One of "daily", "weekly", "monthly", "quarterly".
#' @param auth_token Optional. Character string. Your Sensor Tower API token.
#' @param auto_segment Logical. If TRUE, automatically segments date ranges to avoid timeouts.
#' @param verbose Logical. If TRUE, prints progress messages.
#'
#' @return A tibble with download and revenue estimates.
#'
#' @details
#' The API has timeout limitations based on date granularity:
#' - daily: limit to 1 week segments
#' - weekly: limit to 3 month segments  
#' - monthly: limit to 1 year segments
#' - quarterly: limit to 2 year segments
#' 
#' When auto_segment = TRUE, the function automatically breaks up the date range
#' into appropriate segments and combines the results.
#'
#' @examples
#' \dontrun{
#' # Get daily sales for an app
#' sales <- st_sales_report(
#'   app_ids = "553834731",  # Candy Crush
#'   countries = c("US", "GB"),
#'   start_date = "2024-01-01",
#'   end_date = "2024-01-07",
#'   date_granularity = "daily"
#' )
#' 
#' # Get monthly sales with auto-segmentation
#' sales <- st_sales_report(
#'   app_ids = c("553834731", "1621328561"),  # Multiple apps
#'   countries = "US",
#'   start_date = "2023-01-01",
#'   end_date = "2024-12-31",
#'   date_granularity = "monthly",
#'   auto_segment = TRUE
#' )
#' }
#'
#' @export
st_sales_report <- function(app_ids = NULL,
                           publisher_ids = NULL,
                           os = "ios",
                           countries = "US",
                           start_date = Sys.Date() - 30,
                           end_date = Sys.Date() - 1,
                           date_granularity = "daily",
                           auth_token = NULL,
                           auto_segment = TRUE,
                           verbose = TRUE) {
  
  # Input validation
  if (is.null(app_ids) && is.null(publisher_ids)) {
    stop("At least one app_id or publisher_id is required")
  }
  
  os <- match.arg(os, c("ios", "android"))
  date_granularity <- match.arg(date_granularity, c("daily", "weekly", "monthly", "quarterly"))
  
  # Convert dates to Date objects
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  # Authentication
  auth_token_val <- auth_token %||% Sys.getenv("SENSORTOWER_AUTH_TOKEN")
  if (auth_token_val == "") {
    stop("Authentication token not found. Please set SENSORTOWER_AUTH_TOKEN environment variable.")
  }
  
  # Determine date segments if auto_segment is TRUE
  if (auto_segment) {
    segments <- get_date_segments(start_date, end_date, date_granularity)
    if (verbose && nrow(segments) > 1) {
      message(sprintf("Breaking date range into %d segments to avoid timeouts", nrow(segments)))
    }
  } else {
    segments <- tibble(start = start_date, end = end_date)
  }
  
  # Collect results from all segments
  all_results <- list()
  
  for (i in seq_len(nrow(segments))) {
    segment_start <- segments$start[i]
    segment_end <- segments$end[i]
    
    if (verbose && nrow(segments) > 1) {
      message(sprintf("Fetching segment %d/%d: %s to %s", 
                     i, nrow(segments), segment_start, segment_end))
    }
    
    # Build query parameters
    query_params <- list(
      auth_token = auth_token_val,
      start_date = format(segment_start, "%Y-%m-%d"),
      end_date = format(segment_end, "%Y-%m-%d"),
      date_granularity = date_granularity,
      countries = paste(countries, collapse = ",")
    )
    
    # Add app IDs or publisher IDs
    if (!is.null(app_ids)) {
      query_params$app_ids <- paste(app_ids, collapse = ",")
    }
    
    if (!is.null(publisher_ids)) {
      # Handle publisher IDs with commas using array format
      if (any(grepl(",", publisher_ids))) {
        query_params$`publisher_ids[]` <- publisher_ids
      } else {
        query_params$publisher_ids <- paste(publisher_ids, collapse = ",")
      }
    }
    
    # Build and perform request
    path <- c("v1", os, "sales_report_estimates")
    req <- build_request("https://api.sensortower.com", path, query_params)
    resp <- perform_request(req)
    
    # Process response
    result <- process_sales_response(resp, os)
    
    if (!is.null(result) && nrow(result) > 0) {
      all_results[[i]] <- result
    }
  }
  
  # Combine all results
  if (length(all_results) > 0) {
    final_result <- bind_rows(all_results)
    
    if (verbose) {
      message(sprintf("Retrieved %d records", nrow(final_result)))
    }
    
    return(final_result)
  } else {
    return(tibble())
  }
}

#' Get date segments based on granularity
#' @noRd
get_date_segments <- function(start_date, end_date, granularity) {
  # Define segment lengths based on API recommendations
  segment_days <- switch(granularity,
    daily = 7,      # 1 week
    weekly = 90,    # ~3 months
    monthly = 365,  # 1 year
    quarterly = 730 # 2 years
  )
  
  # Create segments
  segments <- tibble()
  current_start <- start_date
  
  while (current_start <= end_date) {
    current_end <- min(current_start + segment_days - 1, end_date)
    segments <- bind_rows(segments, 
                         tibble(start = current_start, end = current_end))
    current_start <- current_end + 1
  }
  
  return(segments)
}

#' Process sales report response
#' @noRd
process_sales_response <- function(resp, os) {
  body_raw <- httr2::resp_body_raw(resp)
  if (length(body_raw) == 0) {
    return(NULL)
  }
  
  body_text <- rawToChar(body_raw)
  result <- jsonlite::fromJSON(body_text, flatten = TRUE)
  
  if (length(result) == 0 || nrow(result) == 0) {
    return(NULL)
  }
  
  # Convert to tibble
  result_tbl <- as_tibble(result)
  
  # Process based on OS
  if (os == "ios") {
    # Rename columns for clarity
    result_tbl <- result_tbl %>%
      rename_with(~ case_when(
        . == "aid" ~ "app_id",
        . == "cc" ~ "country",
        . == "d" ~ "date",
        . == "iu" ~ "iphone_downloads",
        . == "ir" ~ "iphone_revenue_cents",
        . == "au" ~ "ipad_downloads",
        . == "ar" ~ "ipad_revenue_cents",
        TRUE ~ .
      ))
    
    # Add calculated fields
    result_tbl <- result_tbl %>%
      mutate(
        date = as.Date(date),
        iphone_revenue = iphone_revenue_cents / 100,
        ipad_revenue = ipad_revenue_cents / 100,
        total_downloads = iphone_downloads + ipad_downloads,
        total_revenue = iphone_revenue + ipad_revenue,
        .after = date
      )
  } else {
    # Android
    result_tbl <- result_tbl %>%
      rename_with(~ case_when(
        . == "aid" ~ "app_id",
        . == "cc" ~ "country",
        . == "d" ~ "date",
        . == "u" ~ "downloads",
        . == "r" ~ "revenue_cents",
        TRUE ~ .
      ))
    
    # Add calculated fields
    result_tbl <- result_tbl %>%
      mutate(
        date = as.Date(date),
        revenue = revenue_cents / 100,
        .after = date
      )
  }
  
  return(result_tbl)
}

#' Helper function to look up category names
#'
#' @param category_ids Character vector of category IDs
#' @param platform Character string. "ios" or "android"
#' @return Character vector of category names
#' @export
lookup_category_names <- function(category_ids, platform = "ios") {
  # Load internal data
  st_category_data <- NULL
  data("st_category_data", envir = environment())
  
  # Filter for platform and match IDs
  categories <- st_category_data %>%
    filter(platform == !!platform) %>%
    filter(category_id %in% category_ids)
  
  # Return matched names in same order as input
  category_ids %>%
    map_chr(~ {
      matched <- categories %>%
        filter(category_id == .x) %>%
        pull(category_name)
      
      if (length(matched) > 0) {
        matched[1]
      } else {
        as.character(.x)  # Return ID if no match found
      }
    })
}