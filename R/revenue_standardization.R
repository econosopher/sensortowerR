#' Standardize Revenue Units Across sensortowerR
#'
#' Ensures revenue values have consistent units across all sensortowerR functions.
#' The Sensor Tower API returns revenue in cents, but for consistency with other
#' functions like st_sales_report, this converts to base currency units.
#'
#' @param data Data frame containing revenue columns
#' @param source Character string indicating the data source
#' @param target_unit Character string: "base" (dollars/euros/etc) or "cents"
#' @return Data frame with standardized revenue values
#' @keywords internal
#' 
#' @details
#' The Sensor Tower API inconsistently returns revenue values:
#' - Top Charts API: revenue in cents
#' - Sales Report API: revenue in base currency units
#' - Publishers API: revenue in cents (but converts to base units)
#' 
#' This function ensures consistency by converting all revenue to base units
#' by default, while preserving the original values for reference.
standardize_revenue_units <- function(data, source = "unknown", target_unit = "base") {
  if (nrow(data) == 0) return(data)
  
  # Define columns that are known to be in cents
  cents_columns <- c(
    "revenue_absolute",      # From top_charts
    "current_revenue_value", # From top_charts  
    "comparison_revenue_value", # From top_charts
    "revenue_cents",         # Generic cents column
    "iphone_revenue_cents",  # From sales_report (raw)
    "ipad_revenue_cents"     # From sales_report (raw)
  )
  
  # Process each cents column
  for (col in cents_columns) {
    if (col %in% names(data)) {
      if (target_unit == "base") {
        # Convert cents to base currency units (divide by 100)
        base_col <- gsub("_cents$|_absolute$|_value$", "", col)
        if (base_col == col) base_col <- paste0(col, "_base")
        
        data[[base_col]] <- data[[col]] / 100
        
        # Special case: create a standardized "revenue" column for common access
        if (col %in% c("revenue_absolute", "current_revenue_value")) {
          data$revenue <- data[[col]] / 100
          attr(data$revenue, "unit") <- "base_currency"
        }
      }
      
      # Always add unit metadata to original column
      attr(data[[col]], "unit") <- "cents"
    }
  }
  
  # Handle columns already in base units
  base_columns <- c(
    "total_revenue",    # From sales_report
    "iphone_revenue",   # From sales_report  
    "ipad_revenue",     # From sales_report
    "revenue_usd"       # From publishers
  )
  
  for (col in base_columns) {
    if (col %in% names(data)) {
      attr(data[[col]], "unit") <- "base_currency"
    }
  }
  
  # Add informative message for top_charts
  if (source == "top_charts" && "revenue" %in% names(data)) {
    message("Revenue units standardized: 'revenue' column contains base currency units (dollars/euros/etc).")
    message("Original cents values preserved in 'revenue_absolute' column.")
  }
  
  return(data)
}