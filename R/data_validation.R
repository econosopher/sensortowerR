#' Data validation functions for Sensor Tower API responses
#'
#' @description Functions to validate and clean data from API responses

#' Validate top charts data
#'
#' @param data Data frame from st_top_charts
#' @param measure The measure used (revenue, units, DAU, etc.)
#' @param regions The regions requested
#' @return Validated and potentially corrected data frame
#' @export
validate_top_charts_data <- function(data, measure, regions) {
  if (nrow(data) == 0) {
    return(data)
  }

  # Check for common column issues
  message("Validating data columns...")

  # 1. Handle app name column variations
  app_name_cols <- c("unified_app_name", "app_name", "app.name", "name")
  found_name_col <- intersect(app_name_cols, names(data))

  if (length(found_name_col) == 0) {
    message(
      "No app name column found. Available columns: ",
      paste(head(names(data), 10), collapse = ", ")
    )
  } else if (found_name_col[1] != "unified_app_name") {
    # Standardize to unified_app_name
    data <- data %>%
      dplyr::rename(unified_app_name = !!found_name_col[1])
    message(sprintf("Renamed '%s' to 'unified_app_name'", found_name_col[1]))
  }

  # 2. Enforce region-specific column availability (no fallbacks)
  requested_region <- tolower(regions[1])

  # Look for metric columns with 2-letter region suffixes
  metric_patterns <- c("revenue", "download", "retention", "dau", "mau", "wau", "rpd")

  for (pattern in metric_patterns) {
    cols <- grep(paste0(pattern, ".*_([a-z]{2})$"), names(data), value = TRUE)
    if (length(cols) > 0) {
      available_regions <- unique(gsub(".*_([a-z]{2})$", "\\1", cols))
      # If a specific region is requested, and region-specific metrics are missing,
      # fail loudly rather than implying WW values, but only when working with
      # active user measures where these columns are expected to be present.
      if (requested_region != "ww" && pattern %in% c("mau", "retention") &&
        toupper(measure) %in% c("DAU", "WAU", "MAU")) {
        if (!(requested_region %in% available_regions)) {
          rlang::abort(sprintf(
            "Region-specific '%s' metrics for '%s' are not available in the response. Available regions for '%s': %s",
            toupper(pattern), toupper(requested_region), toupper(pattern),
            ifelse(length(available_regions) > 0, paste(toupper(available_regions), collapse = ", "), "none")
          ), call. = FALSE)
        }
      }
    }
  }

  # 3. Validate numeric columns are actually numeric
  numeric_patterns <- c(
    "revenue", "download", "retention", "dau", "mau", "wau",
    "rpd", "arpu", "share", "rating", "age"
  )

  for (col in names(data)) {
    if (any(sapply(numeric_patterns, function(p) grepl(p, col, ignore.case = TRUE)))) {
      if (!is.numeric(data[[col]])) {
        # Try to convert to numeric
        cleaned <- clean_numeric_column(data[[col]])
        if (!all(is.na(cleaned))) {
          data[[col]] <- cleaned
          message(sprintf("Converted column '%s' to numeric", col))
        }
      }
    }
  }

  # 4. Check for essential columns based on measure
  essential_cols <- list(
    revenue = c("revenue_180d_ww", "revenue_30d_ww"),
    units = c("downloads_180d_ww", "downloads_30d_ww"),
    DAU = c("dau_30d_ww", "dau_30d_us"),
    MAU = c("mau_month_ww", "mau_month_us"),
    WAU = c("wau_4w_ww", "wau_4w_us")
  )

  if (measure %in% names(essential_cols)) {
    missing_all <- !any(essential_cols[[measure]] %in% names(data))
    if (missing_all) {
      # Only warn if we have NO revenue/download data at all
      # The API often doesn't return the 180d/30d metrics for standard sales calls
      has_any_data <- any(grepl(paste0(measure, "|absolute|value"), names(data), ignore.case = TRUE))
      if (!has_any_data) {
        warning(sprintf(
          "Expected columns for %s not found: %s. Available metric columns: %s",
          measure,
          paste(essential_cols[[measure]], collapse = ", "),
          paste(grep("(revenue|download|dau|mau|wau)", names(data),
            value = TRUE
          )[seq_len(min(5, length(grep("(revenue|download|dau|mau|wau)",
            names(data),
            value = TRUE
          ))))], collapse = ", ")
        ))
      }
    }
  }

  return(data)
}

#' Clean numeric column by removing special characters
#'
#' @param x Vector to clean
#' @return Numeric vector
#' @export
clean_numeric_column <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }

  # Convert to character first
  x <- as.character(x)

  # Remove common formatting characters
  x <- gsub("[$,%]", "", x)
  x <- gsub(",", "", x)

  # Handle percentages (keep as 0-100 not 0-1)
  x <- gsub("%", "", x)

  # Convert to numeric
  result <- suppressWarnings(as.numeric(x))

  # Don't convert percentages to decimals
  # (they should stay as 0-100)

  return(result)
}

#' Validate column exists with fallback options
#'
#' @param data Data frame
#' @param primary Primary column name
#' @param fallbacks Character vector of fallback column names
#' @param operation Description of operation for error message
#' @return Column name that exists, or stops with error
#' @export
require_column <- function(data, primary, fallbacks = NULL, operation = "operation") {
  all_options <- c(primary, fallbacks)
  found <- intersect(all_options, names(data))

  if (length(found) == 0) {
    available <- names(data)
    similar <- grep(gsub("_", ".*", primary), available, value = TRUE)

    error_msg <- sprintf("Required column '%s' not found for %s.", primary, operation)
    if (length(similar) > 0) {
      error_msg <- paste0(
        error_msg,
        sprintf(
          "\nDid you mean: %s?",
          paste(similar[seq_len(min(3, length(similar)))], collapse = ", ")
        )
      )
    }
    error_msg <- paste0(
      error_msg,
      sprintf(
        "\nAvailable columns: %s",
        paste(available[seq_len(min(10, length(available)))], collapse = ", ")
      )
    )

    rlang::abort(error_msg)
  }

  return(found[1])
}

#' Safe column selection with automatic fallbacks
#'
#' @param data Data frame
#' @param ... Column specifications (can be character vectors)
#' @return Data frame with selected columns
#' @export
select_robust <- function(data, ...) {
  cols <- list(...)
  selected <- c()

  for (col_spec in cols) {
    if (length(col_spec) == 1 && col_spec %in% names(data)) {
      selected <- c(selected, col_spec)
    } else {
      # Try each option
      found <- intersect(col_spec, names(data))
      if (length(found) > 0) {
        selected <- c(selected, found[1])
      }
    }
  }

  if (length(selected) == 0) {
    return(data)
  }

  return(dplyr::select(data, all_of(selected)))
}
