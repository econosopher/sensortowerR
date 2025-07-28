#' Get Top Publishers by Revenue or Downloads
#'
#' Retrieves top app publishers ranked by revenue or downloads for a specified
#' category, time period, and country. This function uses the
#' `/v1/{os}/top_and_trending/publishers` endpoint.
#'
#' @param measure Character. Metric to rank by: "revenue" or "units" (downloads).
#'   Defaults to "revenue".
#' @param os Character. Operating system: "ios", "android", or "unified". Required.
#' @param category Integer or character. Category ID to filter publishers.
#'   For iOS use numeric IDs (e.g., 6014 for Games), for Android use strings
#'   (e.g., "game"). Use 0 or "all" for all categories.
#' @param time_range Character. Time period: "day", "week", "month", "quarter",
#'   or "year". Defaults to "month".
#' @param comparison_attribute Character. Data type to return: "absolute"
#'   (total values), "delta" (growth), or "transformed_delta" (growth rate).
#'   Defaults to "absolute".
#' @param date Date or character. Start date in "YYYY-MM-DD" format. Required.
#'   **Important**: Must align with time_range boundaries:
#'   - `month`: Must be first day of month (e.g., 2025-06-01)
#'   - `week`: Must be Monday
#'   - `quarter`: Must be quarter start (Jan 1, Apr 1, Jul 1, Oct 1)
#'   - `year`: Must be January 1st
#'   - `day`: Any date allowed
#'   Function will error if date doesn't align. Defaults to 30 days ago.
#' @param end_date Date or character. Optional end date for aggregating multiple
#'   periods. If not provided with `time_range = "month"`, "quarter", or "year",
#'   it will be automatically set to the last day of the period. 
#'   **Important**: If provided, must align with time_range boundaries:
#'   - `month`: Must be last day of month (e.g., 2025-06-30, 2025-07-31)
#'   - `week`: Must be Sunday  
#'   - `quarter`: Must be quarter end (Mar 31, Jun 30, Sep 30, Dec 31)
#'   - `year`: Must be December 31st
#'   - `day`: Any date allowed
#'   Function will error if date doesn't align. Use `time_range = "day"`
#'   for custom date ranges.
#' @param country Character. Country or region code (e.g., "US", "GB", "WW" for worldwide). Required.
#' @param limit Integer. Number of publishers to return (1-100). Defaults to 25.
#' @param offset Integer. Number of publishers to skip for pagination. Defaults to 0.
#' @param device_type Character. For iOS: "iphone", "ipad", or "total".
#'   For unified: "total". Ignored for Android. Defaults to "total".
#' @param include_apps Logical. Whether to include each publisher's top apps
#'   in the response. Defaults to TRUE.
#' @param auth_token Character. Your Sensor Tower API authentication token.
#'   Defaults to the value stored in the `SENSORTOWER_AUTH_TOKEN` environment
#'   variable.
#'
#' @return A [tibble][tibble::tibble] containing top publishers with columns:
#'   - `publisher_id`: Unique publisher identifier
#'   - `publisher_name`: Publisher display name
#'   - `date`: Date of the metrics (as provided by API)
#'   - `date_start`: Actual start date of the period covered
#'   - `date_end`: Actual end date of the period covered
#'   - `units_absolute`: Total downloads for the period
#'   - `units_delta`: Download growth vs previous period
#'   - `units_transformed_delta`: Download growth rate
#'   - `revenue_absolute`: Total revenue in cents for the period
#'   - `revenue_delta`: Revenue growth vs previous period
#'   - `revenue_transformed_delta`: Revenue growth rate
#'   - `revenue_usd`: Total revenue in USD (revenue_absolute / 100)
#'   - `rank`: Publisher rank based on selected measure
#'   - `apps`: Nested tibble with top apps (if include_apps = TRUE)
#'
#' @section API Notes:
#'   - All revenue values are returned in cents from the API
#'   - The function adds a `revenue_usd` column for convenience
#'   - Growth metrics compare to the previous equivalent period
#'   - Worldwide data may have a 2-3 day lag vs country-specific data
#'   
#' @section Date Handling:
#'   When using `time_range = "month"`, "quarter", or "year":
#'   - Dates MUST align with period boundaries or the function will error
#'   - Example: For `time_range = "month"`, use `date = "2025-06-01"`, not `"2025-06-27"`
#'   - This prevents unexpected data ranges from the API
#'   - For custom date ranges (e.g., June 27 - July 26), use `time_range = "day"` and aggregate
#'
#' @examples
#' \dontrun{
#' # Get top 10 game publishers by revenue for last month
#' top_publishers <- st_top_publishers(
#'   measure = "revenue",
#'   category = 6014,  # Games category for iOS
#'   limit = 10
#' )
#'
#' # Get top publishers by downloads with growth metrics
#' growth_publishers <- st_top_publishers(
#'   measure = "units",
#'   comparison_attribute = "delta",
#'   time_range = "week",
#'   limit = 20
#' )
#'
#' # This will ERROR - dates don't align with month boundaries:
#' # publishers_custom <- st_top_publishers(
#' #   date = "2025-06-27",        # ERROR: Not start of month!
#' #   end_date = "2025-07-26",    # ERROR: Not end of month!
#' #   time_range = "month"
#' # )
#' 
#' # Correct way for full months (end_date auto-calculated):
#' publishers_month <- st_top_publishers(
#'   date = "2025-06-01",      # First day of June
#'   time_range = "month"      # end_date auto-set to 2025-06-30
#' )
#' 
#' # Or specify multiple months:
#' publishers_months <- st_top_publishers(
#'   date = "2025-06-01",      # First day of June
#'   end_date = "2025-07-31",  # Last day of July
#'   time_range = "month"
#' )
#' 
#' # For custom date ranges (e.g., June 27 - July 26), use daily:
#' daily_publishers <- purrr::map_df(
#'   seq(as.Date("2025-06-27"), as.Date("2025-07-26"), by = "day"),
#'   ~ st_top_publishers(date = .x, time_range = "day", limit = 50)
#' ) %>%
#'   group_by(publisher_id, publisher_name) %>%
#'   summarise(total_revenue = sum(revenue_usd))
#' }
#'
#' @import dplyr
#' @importFrom httr GET add_headers stop_for_status content http_status
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble as_tibble
#' @importFrom tidyr nest
#' @importFrom rlang abort warn %||% .data sym
#' @importFrom lubridate floor_date ceiling_date
#' @importFrom purrr map map_int
#' @export
st_top_publishers <- function(measure = "revenue",
                             os,
                             category = 0,
                             time_range = "month",
                             comparison_attribute = "absolute",
                             date,
                             end_date = NULL,
                             country,
                             limit = 25,
                             offset = 0,
                             device_type = "total",
                             include_apps = TRUE,
                             auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN")) {
  
  # Validate required parameters
  if (missing(os) || is.null(os)) {
    stop("'os' parameter is required. Specify one of: 'ios', 'android', 'unified'.")
  }
  
  if (missing(date) || is.null(date)) {
    stop("'date' parameter is required. Specify in YYYY-MM-DD format.")
  }
  
  if (missing(country) || is.null(country)) {
    stop("'country' parameter is required. Specify country code (e.g., 'US', 'GB', or 'WW' for worldwide).")
  }
  
  # Input validation
  measure <- match.arg(measure, c("revenue", "units"))
  os <- match.arg(os, c("ios", "android", "unified"))
  time_range <- match.arg(time_range, c("day", "week", "month", "quarter", "year"))
  comparison_attribute <- match.arg(comparison_attribute, 
                                   c("absolute", "delta", "transformed_delta"))
  
  # Display parameters being used
  message("\n=== Sensor Tower API Request ===")
  message(sprintf("  Endpoint: Top Publishers"))
  message(sprintf("  Measure: %s", measure))
  message(sprintf("  OS: %s", os))
  message(sprintf("  Country: %s", country))
  message(sprintf("  Time Range: %s", time_range))
  message(sprintf("  Date: %s", date))
  if (!is.null(end_date)) {
    message(sprintf("  End Date: %s", end_date))
  }
  message(sprintf("  Category: %s", category))
  message(sprintf("  Limit: %d", limit))
  message(sprintf("  Include Apps: %s", include_apps))
  message("================================\n")
  
  if (is.null(auth_token) || auth_token == "") {
    rlang::abort(
      paste(
        "Authentication token is required.",
        "Set SENSORTOWER_AUTH_TOKEN environment variable",
        "or pass via auth_token argument."
      )
    )
  }
  
  # Validate numeric inputs
  if (!is.numeric(limit) || limit < 1 || limit > 100) {
    rlang::abort("limit must be between 1 and 100")
  }
  
  if (!is.numeric(offset) || offset < 0) {
    rlang::abort("offset must be non-negative")
  }
  
  # Convert dates to proper format
  date <- as.Date(date)
  
  # Check if date aligns with time_range boundaries
  expected_date <- switch(time_range,
    day = date,
    week = floor_date(date, "week", week_start = 1),
    month = floor_date(date, "month"),
    quarter = floor_date(date, "quarter"),
    year = floor_date(date, "year")
  )
  
  # Error if date doesn't align with period boundaries
  if (time_range != "day" && date != expected_date) {
    rlang::abort(
      sprintf(
        "Date %s does not align with %s boundaries. For time_range='%s', date must be %s (start of %s). Use time_range='day' for custom date ranges.",
        format(date, "%Y-%m-%d"),
        time_range,
        time_range,
        format(expected_date, "%Y-%m-%d"),
        time_range
      )
    )
  }
  
  # Build API URL early for potential data availability check
  base_url <- "https://api.sensortower.com/v1"
  endpoint_path <- paste0(os, "/top_and_trending/publishers")
  url <- file.path(base_url, endpoint_path)
  
  # Auto-set end_date for period-based time_ranges if not provided
  if (is.null(end_date) && time_range != "day") {
    # Calculate theoretical end date
    theoretical_end <- switch(time_range,
      week = date + 6,  # Sunday of the same week
      month = ceiling_date(date, "month") - 1,  # Last day of the month
      quarter = ceiling_date(date, "quarter") - 1,  # Last day of the quarter
      year = ceiling_date(date, "year") - 1  # December 31st
    )
    
    # For current periods, we need to check actual data availability
    # The API typically has a 1-2 day lag
    if (theoretical_end >= Sys.Date() - 3) {
      # We're dealing with very recent data, need to check availability
      # Quick check: try yesterday first (most common case)
      test_date <- Sys.Date() - 1
      
      test_response <- tryCatch({
        httr::GET(
          url = url,
          query = list(
            auth_token = auth_token,
            comparison_attribute = "absolute",
            time_range = "day",
            measure = measure,
            category = as.character(category),
            date = format(test_date, "%Y-%m-%d"),
            limit = 1
          ),
          httr::add_headers("Accept" = "application/json")
        )
      }, error = function(e) NULL)
      
      # Check if yesterday has data
      if (!is.null(test_response) && httr::status_code(test_response) == 200) {
        test_content <- httr::content(test_response, "text", encoding = "UTF-8")
        if (nzchar(test_content) && test_content != "[]") {
          # Yesterday has data, use it
          latest_available <- test_date
        } else {
          # Yesterday is empty, try day before
          latest_available <- Sys.Date() - 2
        }
      } else {
        # API call failed, assume 2-day lag
        latest_available <- Sys.Date() - 2
      }
      
      # Use the minimum of theoretical end and latest available
      if (theoretical_end > latest_available) {
        end_date <- latest_available
        message(sprintf(
          "Auto-setting end_date to %s (latest available data)",
          format(end_date, "%Y-%m-%d")
        ))
      } else {
        end_date <- theoretical_end
        message(sprintf(
          "Auto-setting end_date to %s (last day of %s starting %s)",
          format(end_date, "%Y-%m-%d"),
          time_range,
          format(date, "%Y-%m-%d")
        ))
      }
    } else {
      # Historical data, use theoretical end
      end_date <- theoretical_end
      message(sprintf(
        "Auto-setting end_date to %s (last day of %s starting %s)",
        format(end_date, "%Y-%m-%d"),
        time_range,
        format(date, "%Y-%m-%d")
      ))
    }
  } else if (!is.null(end_date)) {
    # If end_date is provided, validate it
    end_date <- as.Date(end_date)
    
    # For non-day time ranges, check that end_date aligns with period boundaries
    if (time_range != "day") {
      expected_end_date <- switch(time_range,
        week = ceiling_date(end_date, "week", week_start = 1) - 1,
        month = ceiling_date(floor_date(end_date, "month"), "month") - 1,
        quarter = ceiling_date(floor_date(end_date, "quarter"), "quarter") - 1,
        year = ceiling_date(floor_date(end_date, "year"), "year") - 1
      )
      
      # Check if end_date is at period boundary
      if (end_date != expected_end_date) {
        rlang::abort(
          sprintf(
            "End date %s does not align with %s boundaries. For time_range='%s', end_date must be the last day of a %s. Use time_range='day' for custom date ranges.",
            format(end_date, "%Y-%m-%d"),
            time_range,
            time_range,
            time_range
          )
        )
      }
    }
  }
  
  # URL was already built earlier for data availability check
  
  # Build query parameters
  query_params <- list(
    auth_token = auth_token,
    comparison_attribute = comparison_attribute,
    time_range = time_range,
    measure = measure,
    category = as.character(category),
    date = format(date, "%Y-%m-%d"),
    limit = limit,
    offset = offset
  )
  
  # Add optional parameters
  if (!is.null(end_date)) {
    query_params$end_date <- format(end_date, "%Y-%m-%d")
  }
  
  if (!is.null(country) && country != "" && country != "WW") {
    query_params$country <- country
  }
  
  if (os %in% c("ios", "unified")) {
    query_params$device_type <- device_type
  }
  
  # Make API request
  response <- tryCatch(
    httr::GET(
      url = url,
      query = query_params,
      httr::add_headers("Accept" = "application/json")
    ),
    error = function(e) {
      rlang::abort(paste("HTTP request failed:", e$message))
    }
  )
  
  # Handle response
  parsed_data <- tryCatch(
    {
      httr::stop_for_status(
        response,
        task = paste("fetch top publishers for", measure, "in", os)
      )
      content_text <- httr::content(response, "text", encoding = "UTF-8")
      if (content_text == "") {
        rlang::warn("API returned an empty response body.")
        return(tibble::tibble())
      }
      jsonlite::fromJSON(content_text, flatten = TRUE)
    },
    error = function(e) {
      if (httr::http_error(response)) {
        status <- httr::http_status(response)
        error_msg <- paste0(
          "API request failed (HTTP ", status$status_code, "): ",
          status$reason
        )
        
        # Try to parse error message from response
        error_content <- httr::content(response, "text", encoding = "UTF-8")
        if (nzchar(error_content)) {
          tryCatch({
            error_json <- jsonlite::fromJSON(error_content)
            if (!is.null(error_json$error)) {
              error_msg <- paste0(error_msg, " - ", error_json$error)
            }
          }, error = function(e) {})
        }
        
        rlang::abort(error_msg)
      } else {
        rlang::abort(paste("Error processing API response:", e$message))
      }
    }
  )
  
  # Convert to tibble
  if (is.null(parsed_data) || length(parsed_data) == 0) {
    rlang::warn("No publishers found for the specified criteria.")
    return(tibble::tibble())
  }
  
  # Process the response
  publishers_df <- tibble::as_tibble(parsed_data)
  
  # Add date_start and date_end columns to clarify the actual period covered
  # This is crucial for understanding what data we're actually looking at
  # Use the validated date objects from earlier in the function
  actual_start_date <- date  # This was converted to Date earlier
  actual_end_date <- end_date %||% 
                     switch(time_range,
                       day = date,
                       week = date + 6,
                       month = ceiling_date(date, "month") - 1,
                       quarter = ceiling_date(date, "quarter") - 1,
                       year = ceiling_date(date, "year") - 1
                     )
  
  # For current periods, API might return partial data
  if (time_range != "day" && actual_end_date > Sys.Date()) {
    actual_end_date <- Sys.Date() - 1  # Yesterday
    message(sprintf("Note: Data covers %s to %s (partial %s)\n", 
                    actual_start_date, actual_end_date, time_range))
  }
  
  publishers_df <- publishers_df %>%
    mutate(
      date_start = format(actual_start_date, "%Y-%m-%d"),
      date_end = format(actual_end_date, "%Y-%m-%d"),
      # Add a warning column for YTD metrics
      ytd_warning = if (time_range == "year" && 
                       comparison_attribute %in% c("delta", "transformed_delta")) {
        "Use st_ytd_metrics() for accurate YTD metrics"
      } else {
        NA_character_
      }
    )
  
  # Add rank based on the measure used
  rank_column <- paste0(measure, "_absolute")
  if (rank_column %in% names(publishers_df)) {
    publishers_df <- publishers_df %>%
      arrange(desc(!!sym(rank_column))) %>%
      mutate(rank = row_number())
  }
  
  # Convert revenue from cents to dollars
  if ("revenue_absolute" %in% names(publishers_df)) {
    publishers_df <- publishers_df %>%
      mutate(revenue_usd = revenue_absolute / 100)
  }
  
  # Process nested apps data if present
  if ("apps" %in% names(publishers_df) && include_apps) {
    publishers_df <- publishers_df %>%
      mutate(apps = purrr::map(apps, ~{
        if (!is.null(.x) && length(.x) > 0) {
          app_df <- as_tibble(.x)
          if ("revenue_absolute" %in% names(app_df)) {
            app_df <- app_df %>%
              mutate(revenue_usd = revenue_absolute / 100)
          }
          app_df
        } else {
          tibble()
        }
      }))
  } else if (!include_apps && "apps" %in% names(publishers_df)) {
    publishers_df <- publishers_df %>%
      select(-apps)
  }
  
  # Reorder columns for better readability
  col_order <- c("rank", "publisher_id", "publisher_name", "date", 
                 "date_start", "date_end",
                 "revenue_absolute", "revenue_usd", "revenue_delta", 
                 "revenue_transformed_delta", "units_absolute", "units_delta",
                 "units_transformed_delta")
  
  if (include_apps) {
    col_order <- c(col_order, "apps")
  }
  
  # Keep only columns that exist
  col_order <- col_order[col_order %in% names(publishers_df)]
  
  # Add any remaining columns
  remaining_cols <- setdiff(names(publishers_df), col_order)
  col_order <- c(col_order, remaining_cols)
  
  publishers_df <- publishers_df %>%
    select(all_of(col_order))
  
  return(publishers_df)
}

#' Get Publisher Category Breakdown
#'
#' Fetches revenue breakdown by category for specified publishers.
#' This function makes multiple API calls to aggregate publisher performance
#' across different categories.
#'
#' @param publisher_ids Character vector. Publisher IDs to analyze.
#' @param categories Character vector. Category IDs to check. If NULL,
#'   uses major game categories.
#' @param time_range Character. Time period: "week", "month", "quarter".
#'   Defaults to "month".
#' @param date Date or character. Start date. Defaults to 30 days ago.
#' @param os Character. Operating system: "ios", "android", or "unified". Required.
#' @param country Character. Country or region code (e.g., "US", "GB", "WW" for worldwide). Required.
#' @param auth_token Character. Your Sensor Tower API authentication token.
#'
#' @return A tibble with publisher revenue broken down by category.
#'
#' @examples
#' \dontrun{
#' # Get category breakdown for top publishers
#' top_pubs <- st_top_publishers(limit = 5, country = "US")
#' breakdown <- st_publisher_category_breakdown(
#'   publisher_ids = top_pubs$publisher_id,
#'   country = "US"  # Match the country
#' )
#' }
#'
#' @export
st_publisher_category_breakdown <- function(publisher_ids,
                                          categories = NULL,
                                          time_range = "month",
                                          date = Sys.Date() - 30,
                                          os = "unified",
                                          country = "WW",
                                          auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN")) {
  
  # Display parameters being used
  message("\n=== Sensor Tower API Request ===")
  message(sprintf("  Endpoint: Publisher Category Breakdown"))
  message(sprintf("  Publishers: %d IDs provided", length(publisher_ids)))
  message(sprintf("  OS: %s", os))
  message(sprintf("  Country: %s", country))
  message(sprintf("  Time Range: %s", time_range))
  message(sprintf("  Date: %s", date))
  message("================================\n")
  
  # Default categories if not provided
  if (is.null(categories)) {
    if (os == "ios") {
      categories <- c(
        "6014",  # Games
        "7001",  # Action
        "7002",  # Adventure
        "7003",  # Arcade
        "7004",  # Board
        "7005",  # Card
        "7006",  # Casino
        "7014",  # Role Playing
        "7015",  # Simulation
        "7017",  # Strategy",
        "7019"   # Puzzle
      )
    } else if (os == "android") {
      categories <- c(
        "game",           # Games (general)
        "game_action",    # Action
        "game_adventure", # Adventure
        "game_arcade",    # Arcade
        "game_board",     # Board
        "game_card",      # Card
        "game_casino",    # Casino
        "game_role_playing", # Role Playing
        "game_simulation",   # Simulation
        "game_strategy",     # Strategy
        "game_puzzle"        # Puzzle
      )
    } else {
      # For unified, we'll use iOS categories as primary
      categories <- c("6014", "7001", "7002", "7003", "7004", 
                     "7005", "7006", "7014", "7015", "7017", "7019")
    }
  }
  
  # Collect results for each category
  all_results <- list()
  
  message(sprintf("Fetching data for %d publishers across %d categories...", 
                 length(publisher_ids), length(categories)))
  
  for (i in seq_along(categories)) {
    cat_id <- categories[i]
    
    # Fetch top publishers for this category
    cat_publishers <- tryCatch({
      st_top_publishers(
        measure = "revenue",
        os = os,
        category = cat_id,
        time_range = time_range,
        comparison_attribute = "absolute",
        date = date,
        country = country,  # Pass through country parameter
        limit = 100,  # Get more to ensure we find our publishers
        include_apps = FALSE,
        auth_token = auth_token
      )
    }, error = function(e) {
      warning(sprintf("Failed to fetch category %s: %s", cat_id, e$message))
      return(NULL)
    })
    
    if (!is.null(cat_publishers) && nrow(cat_publishers) > 0) {
      # Filter for our publishers of interest
      matching_pubs <- cat_publishers %>%
        filter(.data$publisher_id %in% publisher_ids) %>%
        mutate(category_id = cat_id)
      
      if (nrow(matching_pubs) > 0) {
        all_results[[i]] <- matching_pubs
      }
    }
    
    # Add small delay to avoid rate limiting
    Sys.sleep(0.5)
  }
  
  # Combine results
  if (length(all_results) > 0) {
    combined_results <- bind_rows(all_results)
    
    # Calculate percentages
    result_summary <- combined_results %>%
      group_by(.data$publisher_id, .data$publisher_name) %>%
      mutate(
        total_revenue = sum(.data$revenue_absolute, na.rm = TRUE),
        category_percentage = (.data$revenue_absolute / .data$total_revenue) * 100
      ) %>%
      ungroup() %>%
      select(.data$publisher_id, .data$publisher_name, .data$category_id, 
             .data$revenue_absolute, .data$revenue_usd, .data$category_percentage) %>%
      arrange(.data$publisher_name, desc(.data$revenue_absolute))
    
    return(result_summary)
  } else {
    warning("No data found for specified publishers in any category")
    return(tibble())
  }
}