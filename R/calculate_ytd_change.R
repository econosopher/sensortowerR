#' Calculate Year-to-Date Change
#'
#' Calculates YTD change by summing monthly data instead of using the 
#' year endpoint which may include projections or different calculations.
#'
#' @param publisher_ids Character vector. Publisher IDs to calculate YTD for.
#' @param current_year Integer. Current year for YTD calculation.
#' @param measure Character. "revenue" or "units" (downloads).
#' @param country Character. Country code (e.g., "US", "WW").
#' @param end_month Integer. Last month to include (default is current month).
#' @param auth_token Character. Your Sensor Tower API authentication token.
#'
#' @return A tibble with publisher_id, publisher_name, ytd_current, ytd_previous, 
#'   and ytd_change_pct columns.
#'
#' @examples
#' \dontrun{
#' # Calculate YTD revenue change for top publishers
#' ytd_changes <- calculate_ytd_change(
#'   publisher_ids = c("pub1", "pub2"),
#'   current_year = 2025,
#'   measure = "revenue",
#'   country = "US"
#' )
#' }
#'
#' @export
calculate_ytd_change <- function(publisher_ids,
                                current_year = year(Sys.Date()),
                                measure = "revenue",
                                country = "WW",
                                end_month = month(Sys.Date()),
                                auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN")) {
  
  measure <- match.arg(measure, c("revenue", "units"))
  
  # Display what we're doing
  message("\n=== Calculating YTD Change ===")
  message(sprintf("  Method: Monthly aggregation (accurate)"))
  message(sprintf("  Measure: %s", measure))
  message(sprintf("  Country: %s", country))
  message(sprintf("  Period: Jan-%s for %d and %d", month.abb[end_month], current_year, current_year - 1))
  message(sprintf("  Publishers: %d", length(publisher_ids)))
  message("==============================\n")
  
  # Function to get monthly data for a year
  get_year_monthly <- function(year, publisher_ids) {
    monthly_data <- list()
    
    for (m in 1:end_month) {
      month_date <- as.Date(paste0(year, "-", sprintf("%02d", m), "-01"))
      
      month_pubs <- tryCatch({
        st_top_publishers(
          measure = measure,
          os = "unified",
          category = 6014,
          time_range = "month",
          date = month_date,
          country = country,
          limit = 100,  # Get enough to find our publishers
          include_apps = FALSE,
          auth_token = auth_token
        )
      }, error = function(e) {
        warning(sprintf("Failed to fetch %s %d: %s", month.abb[m], year, e$message))
        return(NULL)
      })
      
      if (!is.null(month_pubs) && nrow(month_pubs) > 0) {
        # Filter for our publishers
        month_filtered <- month_pubs %>%
          filter(publisher_id %in% publisher_ids) %>%
          mutate(month = m, year = year)
        
        if (nrow(month_filtered) > 0) {
          monthly_data[[m]] <- month_filtered
        }
      }
      
      Sys.sleep(0.3)  # Rate limiting
    }
    
    if (length(monthly_data) > 0) {
      bind_rows(monthly_data)
    } else {
      NULL
    }
  }
  
  # Get data for both years
  message(sprintf("Fetching %d data...", current_year))
  current_data <- get_year_monthly(current_year, publisher_ids)
  
  message(sprintf("\nFetching %d data...", current_year - 1))
  previous_data <- get_year_monthly(current_year - 1, publisher_ids)
  
  # Aggregate by publisher
  if (!is.null(current_data) && !is.null(previous_data)) {
    value_col <- if (measure == "revenue") "revenue_usd" else "units_absolute"
    
    current_totals <- current_data %>%
      group_by(publisher_id, publisher_name) %>%
      summarise(ytd_current = sum(!!sym(value_col), na.rm = TRUE), .groups = "drop")
    
    previous_totals <- previous_data %>%
      group_by(publisher_id, publisher_name) %>%
      summarise(ytd_previous = sum(!!sym(value_col), na.rm = TRUE), .groups = "drop")
    
    # Join and calculate change
    ytd_result <- current_totals %>%
      inner_join(previous_totals, by = c("publisher_id", "publisher_name")) %>%
      mutate(
        ytd_change_pct = ifelse(
          ytd_previous > 0,
          ((ytd_current - ytd_previous) / ytd_previous) * 100,
          NA_real_
        )
      )
    
    message(sprintf("\n✓ YTD calculation complete for %d publishers", nrow(ytd_result)))
    
    return(ytd_result)
  } else {
    warning("Failed to fetch sufficient data for YTD calculation")
    return(tibble())
  }
}