#' Year-over-Year Metrics Comparison
#'
#' Fetches metrics for the same date range across multiple years for 
#' year-over-year comparisons. Allows flexible date ranges and supports
#' all available metrics including revenue, downloads, and active users.
#'
#' @param os Character. Required. Operating system: "ios", "android", or "unified".
#'   This determines which platform's data is returned.
#' @param unified_app_id Character vector. Sensor Tower unified app ID(s). 
#'   Must be 24-character hex format (e.g., "5ba4585f539ce75b97db6bcb").
#' @param ios_app_id Character vector. iOS app ID(s) (e.g., "1234567890").
#' @param android_app_id Character vector. Android package name(s) (e.g., "com.example.app").
#' @param publisher_id Character vector. Publisher ID(s) (alternative to app IDs).
#' @param years Integer vector. Years to compare (e.g., c(2022, 2023, 2024)).
#'   If NULL, uses current year and previous year.
#' @param period_start Character string or Date. Start of the comparison period.
#'   Can be "MM-DD" format (e.g., "01-01" for Jan 1) or a full date.
#'   If a full date is provided, only the month and day are used.
#' @param period_end Character string or Date. End of the comparison period.  
#'   Can be "MM-DD" format (e.g., "03-31" for Mar 31) or a full date.
#'   If a full date is provided, only the month and day are used.
#' @param metrics Character vector. Metrics to fetch. Supports "revenue", "downloads", 
#'   "dau", "wau", and "mau". Default is both revenue and downloads.
#' @param countries Character vector. Country codes (e.g., "US", "GB", "JP"). Required.
#' @param cache_dir Character. Directory for caching API responses (optional).
#' @param auth_token Character string. Sensor Tower API token.
#' @param verbose Logical. Print progress messages.
#' @param granularity Character. Date granularity for the data (e.g., "daily", "monthly").
#' @param use_single_fetch Logical. If TRUE, uses a single API call to fetch all data.
#'   Defaults to TRUE for efficiency.
#'
#' @return A tibble in tidy/long format with columns:
#'   - `app_id`: The app ID used for fetching data
#'   - `app_id_type`: Type of app ID ("ios", "android", or "unified")
#'   - `entity_id`: App or publisher ID
#'   - `entity_name`: App or publisher name
#'   - `entity_type`: "app" or "publisher"
#'   - `year`: Year of the data
#'   - `date_start`: Start date of the period (YYYY-MM-DD)
#'   - `date_end`: End date of the period (YYYY-MM-DD)
#'   - `country`: Country code
#'   - `metric`: The metric name (e.g., "revenue", "downloads", "dau")
#'   - `value`: Metric value (units depend on metric type)
#'   - `yoy_change`: Year-over-year change (percentage)
#'   - `yoy_change_absolute`: Year-over-year change (absolute value)
#'
#' @details
#' This function is designed for year-over-year comparisons:
#' 
#' - **Flexible date ranges**: Compare any period (e.g., Q1, specific months, custom ranges)
#' - **Multiple years**: Compare across 2+ years in a single call
#' - **Smart date handling**: Automatically handles leap years and invalid dates
#' - **YoY calculations**: Includes both percentage and absolute change
#' - **Caching**: Reuses cached data to minimize API calls
#'
#' The function will apply the same calendar period (month/day range) to each
#' specified year, making it easy to compare seasonal trends, campaign periods,
#' or any custom date range across years.
#'
#' @examples
#' \dontrun{
#' # Compare Q1 performance across years
#' q1_comparison <- st_yoy_metrics(
#'   os = "ios",
#'   ios_app_id = "553834731",  # Candy Crush iOS
#'   years = c(2022, 2023, 2024),
#'   period_start = "01-01",
#'   period_end = "03-31",
#'   countries = "US",
#'   metrics = c("revenue", "downloads")
#' )
#'
#' # Compare holiday season (Nov-Dec) across years
#' holiday_comparison <- st_yoy_metrics(
#'   os = "unified",
#'   unified_app_id = "5ba4585f539ce75b97db6bcb",
#'   years = c(2021, 2022, 2023),
#'   period_start = "11-01",
#'   period_end = "12-31",
#'   countries = c("US", "GB", "JP"),
#'   metrics = c("revenue", "downloads", "dau")
#' )
#'
#' # Compare specific campaign period using full dates
#' campaign_comparison <- st_yoy_metrics(
#'   os = "android", 
#'   android_app_id = "com.king.candycrushsaga",
#'   years = NULL,  # Uses current and previous year
#'   period_start = as.Date("2024-02-14"),  # Valentine's campaign
#'   period_end = as.Date("2024-02-28"),
#'   countries = c("US", "GB", "JP"),
#'   metrics = c("revenue", "downloads", "dau", "wau")
#' )
#' }
#'
#' @importFrom dplyr %>% group_by summarise mutate select filter bind_rows arrange lag
#' @importFrom lubridate year month day
#' @importFrom tibble tibble
#' @export
st_yoy_metrics <- function(
  os,
  unified_app_id = NULL,
  ios_app_id = NULL,
  android_app_id = NULL,
  publisher_id = NULL,
  years = NULL,
  period_start,
  period_end,
  metrics = c("revenue", "downloads"),
  countries,
  cache_dir = NULL,
  auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN"),
  verbose = TRUE,
  granularity,
  use_single_fetch = TRUE
) {
  
  # Validate OS parameter
  if (missing(os) || is.null(os) || !os %in% c("ios", "android", "unified")) {
    rlang::abort("'os' parameter is required and must be one of: 'ios', 'android', or 'unified'")
  }
  
  # Validate required parameters
  if (missing(period_start) || is.null(period_start)) {
    rlang::abort("'period_start' is required")
  }
  
  if (missing(period_end) || is.null(period_end)) {
    rlang::abort("'period_end' is required")
  }
  
  if (missing(countries) || is.null(countries) || length(countries) == 0) {
    rlang::abort("'countries' parameter is required")
  }
  
  # Convert dates to MM-DD format if full dates provided
  if (inherits(period_start, "Date") || nchar(as.character(period_start)) == 10) {
    start_date <- as.Date(as.character(period_start))
    period_start <- format(start_date, "%m-%d")
  }
  
  if (inherits(period_end, "Date") || nchar(as.character(period_end)) == 10) {
    end_date <- as.Date(as.character(period_end))  
    period_end <- format(end_date, "%m-%d")
  }
  
  # Validate date formats
  if (!grepl("^\\d{2}-\\d{2}$", period_start)) {
    rlang::abort("period_start must be in MM-DD format (e.g., '02-01') or a Date object")
  }
  if (!grepl("^\\d{2}-\\d{2}$", period_end)) {
    rlang::abort("period_end must be in MM-DD format (e.g., '02-28') or a Date object")
  }
  
  # Default years to current and previous if not specified
  if (is.null(years)) {
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    years <- c(current_year - 1, current_year)
    if (verbose) {
      message("No years specified. Using: ", paste(years, collapse = ", "))
    }
  }
  
  # Ensure years are integers and sorted
  years <- sort(as.integer(years))
  
  # Create end_dates for each year
  end_dates <- character()
  for (year in years) {
    # Construct the end date for this year
    end_date_str <- paste(year, period_end, sep = "-")
    
    # Try to create the date, handling invalid dates like Feb 29 in non-leap years
    end_date <- tryCatch(
      as.Date(end_date_str),
      error = function(e) {
        # If date is invalid, use the last valid day of the month
        month <- as.integer(substr(period_end, 1, 2))
        if (month == 2 && substr(period_end, 4, 5) == "29") {
          # Special case for Feb 29 - use Feb 28 in non-leap years
          as.Date(paste(year, "02-28", sep = "-"))
        } else {
          # General case - use last day of month
          last_day <- lubridate::ceiling_date(
            as.Date(paste(year, month, "01", sep = "-")), 
            "month"
          ) - 1
          last_day
        }
      }
    )
    
    end_dates <- c(end_dates, as.character(end_date))
  }
  
  if (verbose) {
    message("\nFetching year-over-year metrics:")
    message("  Years: ", paste(years, collapse = ", "))
    message("  Period: ", period_start, " to ", period_end, " (each year)")
    message("  Metrics: ", paste(metrics, collapse = ", "))
    message("  Countries: ", paste(countries, collapse = ", "))
  }
  
  # Validate granularity explicitly
  if (missing(granularity) || is.null(granularity)) {
    rlang::abort("'granularity' parameter is required. Specify one of: 'daily', 'weekly', 'monthly', 'quarterly'.")
  }

  # Build unified app_list once (or set publisher mode)
  app_list <- NULL
  publisher_mode <- FALSE
  if (!is.null(unified_app_id)) {
    app_list <- unified_app_id
  } else if (!is.null(ios_app_id) && !is.null(android_app_id)) {
    # Prefer passing a unified ID to ensure both-platform pairing downstream
    unified_map <- tryCatch(
      st_get_unified_mapping(ios_app_id, os = "ios", auth_token = auth_token),
      error = function(e) NULL
    )
    if (!is.null(unified_map) && !is.null(unified_map$unified_app_id[1])) {
      app_list <- as.character(unified_map$unified_app_id[1])
    } else {
      # Fallback: pass both IDs; downstream will fetch per-platform separately
      app_list <- c(as.character(ios_app_id), as.character(android_app_id))
    }
  } else if (!is.null(ios_app_id) || !is.null(android_app_id)) {
    # If unified OS requested but platform IDs supplied, resolve to unified to capture all regional SKUs
    if (os == 'unified') {
      unified_try <- NULL
      if (!is.null(ios_app_id)) {
        unified_try <- tryCatch(st_get_unified_mapping(ios_app_id, os = 'ios', auth_token = auth_token), error = function(e) NULL)
      }
      if ((is.null(unified_try) || is.null(unified_try$unified_app_id[1])) && !is.null(android_app_id)) {
        unified_try <- tryCatch(st_get_unified_mapping(android_app_id, os = 'android', auth_token = auth_token), error = function(e) NULL)
      }
      if (!is.null(unified_try) && !is.null(unified_try$unified_app_id[1])) {
        app_list <- as.character(unified_try$unified_app_id[1])
      } else {
        # Fallback to passing both platform IDs
        ids <- c(as.character(ios_app_id %||% character()), as.character(android_app_id %||% character()))
        app_list <- ids[ids != '']
      }
    } else {
      # Non-unified OS: pass platform IDs through
      app_list <- if (!is.null(ios_app_id)) ios_app_id else android_app_id
    }
  } else if (!is.null(publisher_id)) {
    publisher_mode <- TRUE
    # Resolve publisher apps once and create a cross-platform app list
    apps_tbl <- tryCatch(st_publisher_apps(publisher_id, auth_token = auth_token), error = function(e) NULL)
    if (!is.null(apps_tbl) && nrow(apps_tbl) > 0) {
      # Prefer unified IDs; pass simple vector compatible with st_batch_metrics
      unified_ids <- if ("unified_app_id" %in% names(apps_tbl)) unique(na.omit(as.character(apps_tbl$unified_app_id))) else character(0)
      app_list <- unified_ids
    } else {
      app_list <- character(0)
    }
  }

  # Use the user-specified granularity; no auto-granularity
  chosen_granularity <- granularity

  # If we can fetch once, do a single spanning request; otherwise loop per year
  all_results <- list()
  if (use_single_fetch) {
    # Determine overall start/end
    min_year <- min(years)
    max_year <- max(years)
    all_start <- as.Date(paste(min_year, period_start, sep = "-"))
    all_end <- as.Date(paste(max_year, period_end, sep = "-"))
    # Adjust invalid dates (e.g., 02-29) to valid
    if (is.na(all_start)) {
      m <- as.integer(substr(period_start, 1, 2))
      all_start <- lubridate::floor_date(as.Date(paste(min_year, m, "01", sep = "-")), "month")
    }
    if (is.na(all_end)) {
      m <- as.integer(substr(period_end, 1, 2))
      all_end <- lubridate::ceiling_date(as.Date(paste(max_year, m, "01", sep = "-")), "month") - 1
    }

    fetched <- tryCatch({
      st_batch_metrics(
        os = os,
        app_list = app_list,
        metrics = metrics,
        date_range = list(start_date = all_start, end_date = all_end),
        countries = countries,
        granularity = chosen_granularity,
        parallel = FALSE,
        cache_dir = cache_dir,
        verbose = FALSE,
        auth_token = auth_token
      )
    }, error = function(e) {
      if (verbose) message("  Error fetching data: ", e$message)
      NULL
    })

    if (!is.null(fetched) && nrow(fetched) > 0) {
      fetched$date <- as.Date(fetched$date)
      # Cut per year windows and sum per app/country/metric
      per_year <- lapply(seq_along(years), function(i) {
        y <- years[i]
        ys <- tryCatch(as.Date(paste(y, period_start, sep = "-")), error = function(e) NA)
        ye <- tryCatch(as.Date(paste(y, period_end, sep = "-")), error = function(e) NA)
        if (is.na(ys)) {
          m <- as.integer(substr(period_start, 1, 2))
          ys <- lubridate::floor_date(as.Date(paste(y, m, "01", sep = "-")), "month")
        }
        if (is.na(ye)) {
          m <- as.integer(substr(period_end, 1, 2))
          ye <- lubridate::ceiling_date(as.Date(paste(y, m, "01", sep = "-")), "month") - 1
        }
        sub <- fetched %>% dplyr::filter(.data$date >= ys, .data$date <= ye)
        if (nrow(sub) == 0) return(NULL)
        sub %>%
          dplyr::group_by(.data$original_id, .data$app_name, .data$country, .data$metric) %>%
          dplyr::summarise(value = sum(.data$value, na.rm = TRUE), .groups = "drop") %>%
          dplyr::mutate(
            year = y,
            date_start = as.character(ys),
            date_end = as.character(ye),
            entity_id = .data$original_id,
            entity_name = .data$app_name,
            entity_type = if (publisher_mode) "publisher" else "app",
            app_id = .data$original_id,
            app_id_type = if (publisher_mode) "publisher" else os
          )
      })
      per_year <- per_year[!vapply(per_year, is.null, logical(1))]
      if (length(per_year) > 0) all_results[[1]] <- dplyr::bind_rows(per_year)
    }
  } else {
    # Per-year loop (with chosen granularity)
    for (i in seq_along(years)) {
      year <- years[i]
      start_date <- tryCatch(as.Date(paste(year, period_start, sep = "-")), error = function(e) NA)
      end_date <- tryCatch(as.Date(paste(year, period_end, sep = "-")), error = function(e) NA)
      if (is.na(start_date)) {
        m <- as.integer(substr(period_start, 1, 2))
        start_date <- lubridate::floor_date(as.Date(paste(year, m, "01", sep = "-")), "month")
      }
      if (is.na(end_date)) {
        m <- as.integer(substr(period_end, 1, 2))
        end_date <- lubridate::ceiling_date(as.Date(paste(year, m, "01", sep = "-")), "month") - 1
      }
      if (verbose) message(sprintf("Fetching data for %d: %s to %s", year, start_date, end_date))
      year_data <- tryCatch({
        st_batch_metrics(
          os = os,
          app_list = app_list,
          metrics = metrics,
          date_range = list(start_date = start_date, end_date = end_date),
          countries = countries,
          granularity = chosen_granularity,
          parallel = FALSE,
          cache_dir = cache_dir,
          verbose = FALSE,
          auth_token = auth_token
        )
      }, error = function(e) NULL)
      if (!is.null(year_data) && nrow(year_data) > 0) {
        year_summary <- year_data %>%
          dplyr::group_by(.data$original_id, .data$app_name, .data$country, .data$metric) %>%
          dplyr::summarise(value = sum(.data$value, na.rm = TRUE), .groups = "drop") %>%
          dplyr::mutate(year = year, date_start = as.character(start_date), date_end = as.character(end_date), entity_id = .data$original_id, entity_name = .data$app_name, entity_type = if (publisher_mode) "publisher" else "app", app_id = .data$original_id, app_id_type = if (publisher_mode) "publisher" else os)
        all_results[[length(all_results) + 1]] <- year_summary
      }
    }
  }

  # Combine all years
  ytd_data <- if (length(all_results) > 0) dplyr::bind_rows(all_results) else tibble::tibble()
  
  # If no data returned, return empty tibble with expected columns
  if (length(all_results) == 0 || nrow(ytd_data) == 0) {
    return(tibble::tibble(
      app_id = character(),
      app_id_type = character(),
      entity_id = character(),
      entity_name = character(),
      entity_type = character(),
      year = integer(),
      date_start = character(),
      date_end = character(),
      country = character(),
      metric = character(),
      value = numeric(),
      yoy_change = numeric(),
      yoy_change_absolute = numeric()
    ))
  }
  
  # Calculate year-over-year changes
  yoy_data <- ytd_data %>%
    # Ensure data is sorted by entity, country, metric, and year
    arrange(entity_id, country, metric, year) %>%
    # Group by entity, country, and metric to calculate YoY within each group
    group_by(entity_id, country, metric) %>%
    # Calculate YoY changes
    mutate(
      # Previous year's value
      prev_value = lag(.data$value, n = 1),
      # Year-over-year absolute change
      yoy_change_absolute = .data$value - .data$prev_value,
      # Year-over-year percentage change
      yoy_change = ifelse(
        is.na(.data$prev_value) | .data$prev_value == 0,
        NA_real_,
        ((.data$value - .data$prev_value) / .data$prev_value) * 100
      )
    ) %>%
    # Remove the temporary prev_value column
    select(-prev_value) %>%
    ungroup()
  
  if (verbose) {
    # Print summary of YoY changes
    summary_data <- yoy_data %>%
      filter(!is.na(.data$yoy_change)) %>%
      group_by(metric, year) %>%
      summarise(
        avg_yoy_change = mean(.data$yoy_change, na.rm = TRUE),
        .groups = "drop"
      )
    
    if (nrow(summary_data) > 0) {
      message("\nYear-over-Year Summary:")
      for (i in seq_len(nrow(summary_data))) {
        message(sprintf("  %s %d: %+.1f%% avg change", 
                       summary_data$metric[i], 
                       summary_data$year[i],
                       summary_data$avg_yoy_change[i]))
      }
    }
  }
  
  return(yoy_data)
}

#' Calculate year-over-year growth rates
#'
#' Helper function to calculate YoY growth rates from the output of st_yoy_metrics
#'
#' @param yoy_data Output from st_yoy_metrics
#' @param baseline_year The year to use as baseline (default: earliest year)
#'
#' @return A tibble with growth rates relative to baseline year
#' @export
calculate_yoy_growth <- function(yoy_data, baseline_year = NULL) {
  
  if (is.null(baseline_year)) {
    baseline_year <- min(yoy_data$year)
  }
  
  # Get baseline values
  baseline_data <- yoy_data %>%
    filter(.data$year == baseline_year) %>%
    select(entity_id, country, metric, baseline_value = value)
  
  # Calculate growth rates
  growth_data <- yoy_data %>%
    left_join(baseline_data, by = c("entity_id", "country", "metric")) %>%
    mutate(
      growth_from_baseline = ifelse(
        is.na(.data$baseline_value) | .data$baseline_value == 0,
        NA_real_,
        ((.data$value - .data$baseline_value) / .data$baseline_value) * 100
      ),
      growth_index = ifelse(
        is.na(.data$baseline_value) | .data$baseline_value == 0,
        NA_real_,
        (.data$value / .data$baseline_value) * 100
      )
    )
  
  return(growth_data)
}
