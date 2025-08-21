#' Fetch Top Apps by Various Metrics
#'
#' Retrieves top apps from Sensor Tower based on revenue, downloads ("units"), 
#' or active user metrics (DAU, WAU, MAU). This unified function automatically 
#' selects the appropriate API endpoint based on the measure specified.
#'
#' @param measure Character string. Metric to measure. Must be one of:
#'   - **Revenue/Downloads**: "revenue" (default), "units" 
#'   - **Active Users**: "DAU", "WAU", "MAU"
#' @param os Character string. Operating System. Must be one of
#'   "ios", "android", or "unified". Required.
#' @param comparison_attribute Character string. Comparison attribute
#'   type. Must be one of "absolute", "delta", or "transformed_delta". 
#'   Defaults to "absolute".
#' @param time_range Character string. Time granularity. Must be one of
#'   "day", "week", "month", or "quarter". Defaults to "month".
#'   Note: "week" is not available when `measure` is "MAU".
#' @param date Character string or Date object. Start date for the
#'   query in "YYYY-MM-DD" format. Defaults to the start of the current month.
#' @param category Character string or numeric. The ID of the category to 
#'   filter by. **Required unless `custom_fields_filter_id` is provided**.
#'   Use `st_categories()` to find valid IDs.
#' @param regions Character vector or comma-separated string. Region
#'   codes (e.g., `"US"`, `c("US", "GB")`, `"WW"` for worldwide) to filter results. 
#'   Required.
#' @param end_date Optional. Character string or Date object. End date for the
#'   query in "YYYY-MM-DD" format, inclusive. Only used for revenue/downloads.
#' @param limit Optional. Integer. Maximum number of apps to return per call.
#'   Defaults to 20.
#' @param offset Optional. Integer. Number of apps to skip for pagination.
#' @param device_type Optional. Character string. For `os = "ios"` or
#'   `os = "unified"`: "iphone", "ipad", or "total". Defaults to `"total"`.
#' @param custom_fields_filter_id Optional. Character string. ID of a Sensor
#'   Tower custom field filter to apply.
#' @param custom_tags_mode Optional. Character string. Required if `os` is
#'   'unified' and `custom_fields_filter_id` is provided.
#' @param data_model Optional. Character string. The data model to use.
#'   Defaults to "DM_2025_Q2". Only used for active user metrics.
#' @param auth_token Optional. Character string. Your Sensor Tower API token.
#' @param base_url Optional. Character string. The base URL for the API.
#' @param enrich_response Optional. Logical. If `TRUE` (default), enriches
#'   the response with app metadata and custom metrics.
#' @param deduplicate_apps Optional. Logical. If `TRUE` (default), consolidates
#'   apps with the same name but different platform/regional SKUs into single rows
#'   with aggregated metrics. If `FALSE`, returns separate rows for each SKU.
#'
#' @return A [tibble][tibble::tibble] with top app data including enhanced
#'   custom metrics like downloads, revenue, retention rates, and more.
#'   For sales data (revenue/downloads), app names are automatically looked up
#'   using the app IDs since the sales endpoint doesn't provide app names natively.
#'   
#'   **Revenue Units**: Revenue values are standardized to base currency units (dollars,
#'   euros, etc.) for consistency across all sensortowerR functions. The function provides
#'   a `revenue` column in base units alongside the original `revenue_absolute` (in cents).
#'   
#'   **Data Cleaning**: Numeric metric values are automatically cleaned of special 
#'   characters (%, $, commas) and converted to proper numeric format for analysis.
#'   
#'   **App Deduplication**: By default, apps with the same name but different 
#'   platform/regional SKUs are consolidated into single rows with aggregated metrics
#'   (downloads/revenue summed, rates/percentages averaged).
#'
#' @section API Endpoints Used:
#'   - **All Measures**: `GET /v1/\{os\}/sales_report_estimates_comparison_attributes`
#'   - Note: DAU/WAU/MAU measures now use the sales endpoint with custom filters for correct sorting
#'
#' @section Enhanced Custom Metrics:
#'   The function extracts comprehensive custom metrics including:
#'   - Downloads: `downloads_180d_ww`, `downloads_90d_us`
#'   - Revenue: `revenue_180d_ww`, `revenue_90d_us`
#'   - Retention: `retention_1d_us`, `retention_7d_us`, `retention_30d_us`
#'   - Monetization: `rpd_alltime_us`, `arpu_90d_us`
#'   - Demographics: `male_share_us`, `female_share_us`
#'   - Platform: `ios_share_ww`, `android_share_ww`
#'
#' @examples
#' \dontrun{
#' # Top apps by revenue (default)
#' top_revenue <- st_top_charts(
#'   os = "ios",
#'   category = 6000,  # iOS Games
#'   regions = "WW"
#' )
#' 
#' # Top apps by downloads
#' top_downloads <- st_top_charts(
#'   os = "android",
#'   measure = "units", 
#'   category = 6000,
#'   regions = "US"
#' )
#' 
#' # Top apps by Daily Active Users with custom filter
#' # Custom filter URLs from Sensor Tower web interface can be used directly
#' # Extract the custom_fields_filter_id from the URL parameter 'uai'
#' top_word_puzzles <- st_top_charts(
#'   os = "unified",
#'   measure = "revenue",  # Use revenue but custom filter handles DAU sorting
#'   custom_fields_filter_id = "5a39e9681454d22f5a5e75ca",  # Word puzzle filter
#'   custom_tags_mode = "include_unified_apps",
#'   category = 7019,  # Puzzle category
#'   regions = "US",
#'   date = "2025-07-20",
#'   end_date = "2025-08-18"
#' )
#' 
#' # Custom time range and region
#' top_quarter <- st_top_charts(
#'   os = "ios",
#'   measure = "revenue",
#'   time_range = "quarter", 
#'   regions = "US",
#'   category = 6000
#' )
#' }
#'
#' @export
st_top_charts <- function(measure = "revenue",
                          os,
                          comparison_attribute = "absolute",
                          time_range = "month",
                          date = NULL,
                          category = NULL,
                          regions,
                          end_date = NULL,
                          limit = 20,
                          offset = NULL,
                          device_type = NULL,
                          custom_fields_filter_id = NULL,
                          custom_tags_mode = NULL,
                          data_model = "DM_2025_Q2",
                          auth_token = NULL,
                          base_url = "https://api.sensortower.com",
                          enrich_response = TRUE,
                          deduplicate_apps = TRUE) {
  
  # Validate required parameters
  if (missing(os) || is.null(os)) {
    stop("'os' parameter is required. Specify one of: 'ios', 'android', 'unified'.")
  }
  
  if (missing(regions) || is.null(regions) || length(regions) == 0) {
    stop("'regions' parameter is required. Specify country codes (e.g., 'US', 'GB', 'JP', or 'WW' for worldwide).")
  }
  
  # --- Input Validation ---
  measure <- match.arg(measure, c("revenue", "units", "DAU", "WAU", "MAU"))
  
  # Always use sales endpoint - it handles all measures including DAU
  # Determine which endpoint to use based on measure
  is_active_users <- toupper(measure) %in% c("DAU", "WAU", "MAU")
  is_sales <- !is_active_users
  
  # Validate category requirement - either category or custom_fields_filter_id must be provided
  if (is.null(category) && is.null(custom_fields_filter_id)) {
    rlang::abort("Either 'category' or 'custom_fields_filter_id' parameter is required. Use st_categories() to find valid IDs or provide a custom filter ID from the Sensor Tower web interface.")
  }
  
  # --- Handle Default Date ---
  if (is.null(date)) {
    date <- lubridate::floor_date(Sys.Date(), "month")
  }
  
  # --- Authentication ---
  auth_token_val <- auth_token %||% Sys.getenv("SENSORTOWER_AUTH_TOKEN")
  if (auth_token_val == "") {
    rlang::abort(
      "Authentication token not found. Please set it as an environment variable."
    )
  }
  
  # Handle default device_type
  if (is.null(device_type) && os %in% c("ios", "unified")) {
    device_type <- "total"
    if (os == "unified") {
      message("`device_type` is not specified for `os = 'unified'`. Defaulting to 'total'.")
    }
  }
  
  # Route to appropriate API endpoint based on measure type
  if (is_active_users) {
    # --- Input Validation for Active Users ---
    validate_inputs(
      os = os,
      comparison_attribute = comparison_attribute,
      time_range = time_range,
      measure = measure,
      date = date,
      category = category,
      regions = regions,
      limit = limit,
      offset = offset,
      device_type = device_type,
      custom_fields_filter_id = custom_fields_filter_id,
      custom_tags_mode = custom_tags_mode,
      data_model = data_model
    )

    # --- Prepare Query Parameters for Active Users ---
    query_params <- prepare_query_params_active_users(
      auth_token = auth_token_val,
      comparison_attribute = comparison_attribute,
      time_range = time_range,
      measure = measure,
      date = date,
      category = category,
      regions = regions,
      limit = limit,
      offset = offset,
      device_type = device_type,
      custom_fields_filter_id = custom_fields_filter_id,
      custom_tags_mode = custom_tags_mode,
      data_model = data_model,
      os = os
    )

    # --- Build and Perform Request for Active Users ---
    path <- c("v1", os, "top_and_trending", "active_users")
    
  } else {
    # --- Input Validation for Sales ---
    validate_inputs(
      os = os,
      comparison_attribute = comparison_attribute,
      time_range = time_range,
      measure = measure,
      date = date,
      category = category,
      regions = regions,
      end_date = end_date,
      limit = limit,
      offset = offset,
      device_type = device_type,
      custom_fields_filter_id = custom_fields_filter_id,
      custom_tags_mode = custom_tags_mode
    )

    # --- Prepare Query Parameters for Sales ---
    query_params <- prepare_query_params_sales(
      auth_token = auth_token_val,
      comparison_attribute = comparison_attribute,
      time_range = time_range,
      measure = measure,
      date = date,
      category = category,
      end_date = end_date,
      regions = regions,
      limit = limit,
      offset = offset,
      device_type = device_type,
      custom_fields_filter_id = custom_fields_filter_id,
      custom_tags_mode = custom_tags_mode,
      os = os
    )

    # --- Build and Perform Request for Sales ---
    path <- c("v1", os, "sales_report_estimates_comparison_attributes")
  }
  
  # --- Common Request Building and Processing ---
  req <- build_request(base_url, path, query_params)
  resp <- perform_request(req)
  
  # --- Process Response ---
  result <- process_response(resp, enrich_response)
  
  # --- Validate Data ---
  if (nrow(result) > 0) {
    result <- validate_top_charts_data(result, measure, regions)
  }
  
  # --- Deduplicate Apps ---
  # For unified OS, resolve true unified IDs and consolidate
  if (os == "unified" && "unified_app_name" %in% names(result) && nrow(result) > 1 && deduplicate_apps) {
    original_count <- nrow(result)
    
    # Get unique platform IDs and their corresponding names
    unique_ids <- unique(result$unified_app_id)
    # Create a mapping that handles duplicate keys by taking the first occurrence
    id_name_df <- result %>%
      dplyr::select(unified_app_id, unified_app_name) %>%
      dplyr::distinct(unified_app_id, .keep_all = TRUE)
    id_to_name <- stats::setNames(id_name_df$unified_app_name, id_name_df$unified_app_id)
    
    # Skip if all IDs are already hex format (true unified IDs)
    # Filter out NA values first
    non_na_ids <- unique_ids[!is.na(unique_ids)]
    non_hex_ids <- non_na_ids[!grepl("^[a-f0-9]{24}$", non_na_ids)]
    
    if (length(non_hex_ids) > 0) {
      message(sprintf("Resolving unified IDs for %d apps...", length(non_hex_ids)))
      
      # Get corresponding names for these IDs
      # Handle potential NA values in the mapping
      app_names <- character(length(non_hex_ids))
      for (i in seq_along(non_hex_ids)) {
        name <- id_to_name[non_hex_ids[i]]
        app_names[i] <- if (!is.null(name) && !is.na(name)) as.character(name) else NA_character_
      }
      
      # Use st_get_unified_mapping with app names for better resolution
      mapping_result <- tryCatch({
        st_get_unified_mapping(
          app_ids = non_hex_ids,
          app_names = app_names,
          os = "unified",
          auth_token = auth_token_val
        )
      }, error = function(e) {
        message("Warning: Error resolving unified IDs: ", e$message)
        NULL
      })
      
      if (!is.null(mapping_result) && nrow(mapping_result) > 0) {
        # Create mapping of platform IDs to true unified IDs
        id_mapping <- mapping_result %>%
          dplyr::filter(!is.na(.data$unified_app_id)) %>%
          dplyr::select(platform_id = .data$input_id, 
                       true_unified_id = .data$unified_app_id)
        
        if (nrow(id_mapping) > 0) {
          # Apply true unified ID mapping
          result <- result %>%
            dplyr::left_join(id_mapping, by = c("unified_app_id" = "platform_id")) %>%
            dplyr::mutate(
              .group_id = dplyr::coalesce(.data$true_unified_id, .data$unified_app_id)
            )
          
          # Group by true unified ID and aggregate metrics
          result <- deduplicate_by_group_id(result, ".group_id")
          
          # Update the unified_app_id to be the true unified ID where available
          if ("true_unified_id" %in% names(result)) {
            result <- result %>%
              dplyr::mutate(
                unified_app_id = dplyr::coalesce(.data$true_unified_id, .data$unified_app_id)
              ) %>%
              dplyr::select(-true_unified_id)
          }
          
          new_count <- nrow(result)
          if (original_count != new_count) {
            message(sprintf("Consolidated %d app entries into %d unique apps using unified IDs", 
                           original_count, new_count))
          }
        } else {
          message("Warning: Could not resolve unified IDs. Using name-based consolidation.")
          result <- deduplicate_apps_by_name(result)
        }
      } else {
        message("Using name-based consolidation as fallback.")
        result <- tryCatch({
          deduplicate_apps_by_name(result)
        }, error = function(e) {
          message("Error in deduplication: ", e$message)
          result  # Return original on error
        })
      }
    }
    # If all IDs are already hex format, no action needed
  } else if (deduplicate_apps && "unified_app_name" %in% names(result) && os != "unified") {
    # For non-unified OS, use simple name deduplication if requested
    result <- deduplicate_apps_by_name(result)
  }
  
  # --- Standardize Revenue Units ---
  result <- standardize_revenue_units(result, source = "top_charts")
  
  # --- Add platform information ---
  if (nrow(result) > 0) {
    result$platform <- os
    
    # Add app_id and app_id_type columns based on OS parameter
    if ("unified_app_id" %in% names(result)) {
      result$app_id <- result$unified_app_id
      result$app_id_type <- os  # The OS parameter determines the type
    }
    
    # --- Sort by the requested measure ---
    # After enrichment, the relevant columns are renamed
    if (is_active_users) {
      # For DAU/WAU/MAU, sort by the appropriate active user metric
      sort_col <- switch(
        toupper(measure),
        "DAU" = "dau_30d_us",
        "WAU" = "wau_4w_us", 
        "MAU" = "mau_month_us",
        NULL
      )
      # Fallback to users_absolute if enrichment didn't happen
      if (!is.null(sort_col) && sort_col %in% names(result)) {
        result <- result %>% dplyr::arrange(dplyr::desc(!!rlang::sym(sort_col)))
      } else if ("users_absolute" %in% names(result)) {
        result <- result %>% dplyr::arrange(dplyr::desc(users_absolute))
      }
    } else {
      # For sales measures, sort by the appropriate column
      sort_col <- switch(
        toupper(measure),
        "REVENUE" = if ("revenue_30d_ww" %in% names(result)) "revenue_30d_ww" else "revenue_absolute",
        "UNITS" = if ("downloads_30d_ww" %in% names(result)) "downloads_30d_ww" else "units_absolute",
        NULL
      )
      if (!is.null(sort_col) && sort_col %in% names(result)) {
        result <- result %>% dplyr::arrange(dplyr::desc(!!rlang::sym(sort_col)))
      }
    }
  }
  
  return(result)
} 