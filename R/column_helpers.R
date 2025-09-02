#' Column validation and mapping helpers
#' 
#' @description Helper functions to handle column name variations and missing columns

#' Get available columns matching a pattern
#' 
#' @param data Data frame to check
#' @param pattern Regular expression pattern to match
#' @param prefer Character vector of preferred column names (first match wins)
#' @return First matching column name or NULL
#' @export
find_column <- function(data, pattern, prefer = NULL) {
  cols <- names(data)
  
  # Check preferred columns first
  if (!is.null(prefer)) {
    for (pref in prefer) {
      if (pref %in% cols) return(pref)
    }
  }
  
  # Fall back to pattern matching
  matches <- grep(pattern, cols, value = TRUE, ignore.case = TRUE)
  if (length(matches) > 0) return(matches[1])
  
  return(NULL)
}

#' Select columns safely with fallbacks
#' 
#' @param data Data frame
#' @param columns Named list of column specifications
#' @return Data frame with selected/renamed columns
#' @export
select_columns_safe <- function(data, columns) {
  result <- data
  selected_cols <- c()
  
  for (new_name in names(columns)) {
    spec <- columns[[new_name]]
    
    # Handle different specification formats
    if (is.character(spec)) {
      # Simple column name or pattern
      if (spec %in% names(data)) {
        selected_cols[new_name] <- spec
      } else {
        # Try pattern matching
        found <- find_column(data, spec)
        if (!is.null(found)) {
          selected_cols[new_name] <- found
        }
      }
    } else if (is.list(spec)) {
      # Advanced specification with fallbacks
      found <- find_column(data, 
                          pattern = spec$pattern %||% "", 
                          prefer = spec$prefer)
      if (!is.null(found)) {
        selected_cols[new_name] <- found
      } else if (!is.null(spec$default)) {
        # Add column with default value
        result[[new_name]] <- spec$default
        selected_cols[new_name] <- new_name
      }
    }
  }
  
  # Select and rename columns
  if (length(selected_cols) > 0) {
    result <- result %>%
      dplyr::select(all_of(selected_cols)) %>%
      dplyr::rename(!!!stats::setNames(selected_cols, names(selected_cols)))
  }
  
  return(result)
}

#' Map region-specific columns intelligently
#' 
#' @param data Data frame with Sensor Tower data
#' @param requested_region Region requested (e.g., "US", "WW")
#' @return Data frame with mapped columns
#' @export
map_region_columns <- function(data, requested_region = "US") {
  # Common patterns for region-specific columns
  metric_patterns <- c("revenue", "download", "dau", "wau", "mau", 
                      "retention", "rpd", "arpu", "age")
  
  # Check what regions are actually available
  available_regions <- c()
  for (pattern in metric_patterns) {
    cols <- grep(paste0(pattern, ".*_(us|ww)$"), names(data), 
                value = TRUE, ignore.case = TRUE)
    if (length(cols) > 0) {
      regions <- unique(gsub(".*_([a-z]+)$", "\\1", cols))
      available_regions <- unique(c(available_regions, regions))
    }
  }
  
  message(sprintf("Available regions in data: %s", 
                  paste(toupper(available_regions), collapse = ", ")))
  
  # If requested region not available, use what we have
  actual_region <- tolower(requested_region)
  if (!actual_region %in% available_regions) {
    stop(sprintf("Requested region '%s' not found in data. Available regions: %s",
                 toupper(requested_region),
                 ifelse(length(available_regions) > 0, paste(toupper(available_regions), collapse = ", "), "none")),
         call. = FALSE)
  }
  
  return(data)
}

#' Validate required columns exist
#' 
#' @param data Data frame to check
#' @param required Character vector of required column names
#' @param context Context for error message
#' @return TRUE if all columns exist, otherwise stops with informative error
#' @export
validate_columns <- function(data, required, context = "data") {
  missing <- setdiff(required, names(data))
  
  if (length(missing) > 0) {
    available <- names(data)
    suggestions <- c()
    
    for (col in missing) {
      # Find similar column names
      pattern <- gsub("_", ".*", col)
      matches <- grep(pattern, available, value = TRUE, ignore.case = TRUE)
      if (length(matches) > 0) {
        suggestions <- c(suggestions, 
                        sprintf("  - '%s' not found, did you mean: %s?", 
                                col, paste(matches[1:min(3, length(matches))], 
                                         collapse = ", ")))
      } else {
        suggestions <- c(suggestions, sprintf("  - '%s' not found", col))
      }
    }
    
    stop(sprintf("Missing required columns in %s:\n%s\n\nAvailable columns: %s",
                 context,
                 paste(suggestions, collapse = "\n"),
                 paste(available[1:min(10, length(available))], collapse = ", ")),
         call. = FALSE)
  }
  
  return(TRUE)
}

#' Get column specification for common Sensor Tower metrics
#' 
#' @param metric_type Type of metric (revenue, downloads, retention, etc.)
#' @param time_period Time period (30d, 180d, alltime, etc.)
#' @param region Region (us, ww)
#' @return List of column specifications
#' @export
get_column_spec <- function(metric_type = NULL, time_period = NULL, region = NULL) {
  specs <- list()
  
  # App identification columns
  specs$app_name <- list(
    prefer = c("unified_app_name", "app_name", "name", "humanized_name"),
    pattern = "(app_)?name"
  )
  
  specs$app_id <- list(
    prefer = c("unified_app_id", "app_id", "id"),
    pattern = "(app_)?id"
  )
  
  # Build metric columns dynamically
  if (!is.null(metric_type)) {
    base_metrics <- list(
      revenue = c("revenue", "rev"),
      downloads = c("downloads", "units", "download"),
      retention = c("retention", "ret"),
      dau = c("dau", "daily.*active.*users"),
      mau = c("mau", "monthly.*active.*users"),
      wau = c("wau", "weekly.*active.*users")
    )
    
    for (type in metric_type) {
      if (type %in% names(base_metrics)) {
        patterns <- base_metrics[[type]]
        
        # Build column variations
        for (pattern in patterns) {
          if (!is.null(time_period)) {
            for (tp in time_period) {
              if (!is.null(region)) {
                for (r in region) {
                  col_name <- sprintf("%s_%s_%s", type, tp, r)
                  specs[[col_name]] <- list(
                    prefer = c(
                      sprintf("%s_%s_%s", pattern, tp, r),
                      sprintf("%s_%s_%s", pattern, tp, toupper(r))
                    ),
                    pattern = sprintf("%s.*%s.*%s", pattern, tp, r)
                  )
                }
              }
            }
          }
        }
      }
    }
  }
  
  return(specs)
}

#' Handle column mapping errors gracefully
#' 
#' @param expr Expression to evaluate
#' @param data Data frame being processed
#' @param context Context for error message
#' @return Result of expression or NULL with warning
#' @export
try_column_operation <- function(expr, data, context = "operation") {
  tryCatch({
    eval(expr)
  }, error = function(e) {
    # Extract column name from error if possible
    col_match <- regmatches(e$message, regexpr("`[^`]+`", e$message))
    if (length(col_match) > 0) {
      col_name <- gsub("`", "", col_match[1])
      warning(sprintf("Column '%s' not found in %s. Available columns: %s",
                      col_name, context,
                      paste(names(data)[1:min(5, length(names(data)))], 
                            collapse = ", ")),
              call. = FALSE)
    } else {
      warning(sprintf("Error in %s: %s", context, e$message), call. = FALSE)
    }
    return(NULL)
  })
}
