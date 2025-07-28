#' Formatting Helper Functions
#'
#' @description
#' Functions for formatting numeric values in a human-readable way,
#' particularly useful for revenue, download counts, and percentages.
#' 
#' @name formatting_helpers
NULL

#' Format large numbers with K/M/B suffixes
#'
#' @param val Numeric value to format
#' @param digits Number of digits after decimal for millions/billions (default: 1)
#' @param prefix Optional prefix (e.g., "$" for currency)
#' @return Formatted string with appropriate suffix
#' @export
#' @examples
#' format_large_number(1234567)      # "1.2M"
#' format_large_number(1234567890)   # "1.2B"
#' format_large_number(1234567, prefix = "$") # "$1.2M"
format_large_number <- function(val, digits = 1, prefix = "") {
  if (is.na(val)) return("-")
  
  if (abs(val) >= 1e9) {
    paste0(prefix, formatC(val/1e9, format = "f", digits = digits), "B")
  } else if (abs(val) >= 1e6) {
    paste0(prefix, formatC(val/1e6, format = "f", digits = digits), "M")
  } else if (abs(val) >= 1e3) {
    paste0(prefix, formatC(val/1e3, format = "f", digits = 0), "K")
  } else {
    paste0(prefix, formatC(val, format = "f", digits = 0))
  }
}

#' Format currency values with appropriate suffixes
#'
#' @param val Numeric value to format as currency
#' @param digits Number of digits after decimal for millions/billions (default: 2)
#' @return Formatted currency string
#' @export
#' @examples
#' format_currency(1234567)      # "$1.23M"
#' format_currency(1234567890)   # "$1.23B"
#' format_currency(123)          # "$123"
format_currency <- function(val, digits = 2) {
  format_large_number(val, digits = digits, prefix = "$")
}

#' Format download counts with appropriate suffixes
#'
#' @param val Numeric value to format as downloads
#' @return Formatted download count string
#' @export
#' @examples
#' format_downloads(1234567)      # "1.2M"
#' format_downloads(1234567890)   # "1.2B"
format_downloads <- function(val) {
  format_large_number(val, digits = 1, prefix = "")
}

#' Format percentages
#'
#' @param val Numeric value to format as percentage (0-100 scale)
#' @param digits Number of decimal places (default: 1)
#' @return Formatted percentage string
#' @export
#' @examples
#' format_percent(23.456)    # "23.5%"
#' format_percent(0.234, digits = 2)    # "0.23%"
format_percent <- function(val, digits = 1) {
  if (is.na(val)) return("-")
  paste0(formatC(val, format = "f", digits = digits), "%")
}

#' Format retention rates
#'
#' @param val Numeric value as decimal (0-1 scale)
#' @param digits Number of decimal places (default: 1)
#' @return Formatted retention percentage string
#' @export
#' @examples
#' format_retention(0.234)    # "23.4%"
#' format_retention(0.85)     # "85.0%"
format_retention <- function(val, digits = 1) {
  if (is.na(val)) return("-")
  paste0(formatC(val * 100, format = "f", digits = digits), "%")
}

#' Format user counts (DAU/MAU/WAU)
#'
#' @param val Numeric value to format as user count
#' @return Formatted user count string
#' @export
#' @examples
#' format_users(1234567)      # "1.2M"
#' format_users(1234)         # "1.2K"
format_users <- function(val) {
  format_large_number(val, digits = 1, prefix = "")
}

#' Format ARPU (Average Revenue Per User)
#'
#' @param val Numeric ARPU value
#' @param digits Number of decimal places (default: 2)
#' @return Formatted ARPU string
#' @export
#' @examples
#' format_arpu(5.234)    # "$5.23"
#' format_arpu(0.99)     # "$0.99"
format_arpu <- function(val, digits = 2) {
  if (is.na(val)) return("-")
  paste0("$", formatC(val, format = "f", digits = digits))
}

#' Format market share as percentage
#'
#' @param val Numeric value as decimal (0-1 scale)
#' @param digits Number of decimal places (default: 1)
#' @return Formatted market share percentage string
#' @export
#' @examples
#' format_market_share(0.234)    # "23.4%"
#' format_market_share(0.05)     # "5.0%"
format_market_share <- function(val, digits = 1) {
  format_retention(val, digits = digits)
}

#' Create a vector of formatted values
#'
#' @param values Numeric vector to format
#' @param type Type of formatting: "currency", "downloads", "percent", "users"
#' @param ... Additional arguments passed to formatting function
#' @return Character vector of formatted values
#' @export
#' @examples
#' format_vector(c(1234567, 2345678), "currency")
#' format_vector(c(0.234, 0.456), "percent")
format_vector <- function(values, type = c("currency", "downloads", "percent", "users", "retention", "arpu"), ...) {
  type <- match.arg(type)
  
  format_fn <- switch(type,
    currency = format_currency,
    downloads = format_downloads,
    percent = format_percent,
    users = format_users,
    retention = format_retention,
    arpu = format_arpu
  )
  
  sapply(values, format_fn, ...)
}