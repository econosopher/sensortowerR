#' Custom Filter Utilities
#' 
#' Internal functions for handling custom field filters across sensortowerR functions
#' 
#' @keywords internal
#' @name custom_filter_utils
NULL

#' Add Custom Filter Parameters to Query
#' 
#' Internal function to add custom field filter parameters to API query parameters.
#' Handles validation and OS-specific requirements.
#' 
#' @param query_params List. Existing query parameters
#' @param custom_fields_filter_id Character. Custom filter ID from Sensor Tower
#' @param custom_tags_mode Character. Tag mode for unified OS
#' @param os Character. Operating system (ios, android, unified)
#' @return Modified query_params list with custom filter parameters added
#' @keywords internal
add_custom_filter_params <- function(query_params, 
                                   custom_fields_filter_id = NULL,
                                   custom_tags_mode = NULL,
                                   os = NULL) {
  
  if (!is.null(custom_fields_filter_id)) {
    # Validate filter ID format
    if (!st_is_valid_filter_id(custom_fields_filter_id)) {
      stop("Invalid custom_fields_filter_id format. Expected 24-character hexadecimal string.",
           call. = FALSE)
    }
    
    # Add filter ID to query
    query_params$custom_fields_filter_id <- custom_fields_filter_id
    
    # Check if custom_tags_mode is required
    if (!is.null(os) && os == "unified") {
      if (is.null(custom_tags_mode)) {
        stop("custom_tags_mode is required when using custom_fields_filter_id with os='unified'. ",
             "Options: 'include', 'exclude', 'include_unified_apps'",
             call. = FALSE)
      }
      query_params$custom_tags_mode <- custom_tags_mode
    } else if (!is.null(custom_tags_mode) && !is.null(os) && os != "unified") {
      # Warn if custom_tags_mode provided for non-unified OS
      message("Note: custom_tags_mode is only used with os='unified'. Ignoring for os='", os, "'")
    }
  } else if (!is.null(custom_tags_mode)) {
    warning("custom_tags_mode provided without custom_fields_filter_id. Ignoring.",
            call. = FALSE)
  }
  
  # Remove NULL values
  query_params[!sapply(query_params, is.null)]
}

#' Validate Custom Filter Parameters
#' 
#' Checks if custom filter parameters are valid before making API call
#' 
#' @param custom_fields_filter_id Character. Custom filter ID
#' @param custom_tags_mode Character. Tag mode  
#' @param os Character. Operating system
#' @param require_category Logical. Whether category is required when no filter
#' @param category Category parameter value
#' @return NULL if valid, stops with error if invalid
#' @keywords internal
validate_custom_filter_params <- function(custom_fields_filter_id = NULL,
                                        custom_tags_mode = NULL,
                                        os = NULL,
                                        require_category = TRUE,
                                        category = NULL) {
  
  # Check if either category or custom filter is provided
  if (require_category && is.null(category) && is.null(custom_fields_filter_id)) {
    stop("Either 'category' or 'custom_fields_filter_id' parameter is required.",
         call. = FALSE)
  }
  
  # Validate filter ID if provided
  if (!is.null(custom_fields_filter_id)) {
    if (!st_is_valid_filter_id(custom_fields_filter_id)) {
      stop("Invalid custom_fields_filter_id format. Expected 24-character hexadecimal string.",
           call. = FALSE)
    }
    
    # Check custom_tags_mode requirement
    if (!is.null(os) && os == "unified" && is.null(custom_tags_mode)) {
      stop("custom_tags_mode is required when using custom_fields_filter_id with os='unified'. ",
           "Options: 'include', 'exclude', 'include_unified_apps'",
           call. = FALSE)
    }
  }
  
  # Validate custom_tags_mode values if provided
  if (!is.null(custom_tags_mode)) {
    valid_modes <- c("include", "exclude", "include_unified_apps")
    if (!custom_tags_mode %in% valid_modes) {
      stop("Invalid custom_tags_mode. Must be one of: ",
           paste(valid_modes, collapse = ", "),
           call. = FALSE)
    }
  }
  
  invisible(NULL)
}

#' Get Custom Filter Documentation
#' 
#' Returns standardized documentation for custom filter parameters
#' 
#' @return Character vector with roxygen2 parameter documentation
#' @keywords internal
get_custom_filter_docs <- function() {
  c(
    "#' @param custom_fields_filter_id Optional. Character string. ID of a Sensor",
    "#'   Tower custom field filter to apply. Use filter IDs from the web interface",
    "#'   at app.sensortower.com. When provided, this filter will be applied to the",
    "#'   results. The 'category' parameter becomes optional when using a custom filter.",
    "#' @param custom_tags_mode Optional. Character string. Required if `os` is",
    "#'   'unified' and `custom_fields_filter_id` is provided. Specifies how the",  
    "#'   custom filter applies to unified apps. Options: \"include\", \"exclude\",",
    "#'   \"include_unified_apps\". The \"include_unified_apps\" option includes all",
    "#'   platform versions when any version matches the filter."
  )
}

#' Extract Custom Filter from URL Parameters
#' 
#' Helper to extract custom filter parameters from parsed URL parameters
#' 
#' @param params List. Parameters from st_parse_web_url()
#' @return List with custom_fields_filter_id and custom_tags_mode (or NULLs)
#' @keywords internal
extract_custom_filter_params <- function(params) {
  list(
    custom_fields_filter_id = params$custom_fields_filter_id,
    custom_tags_mode = params$custom_tags_mode
  )
}

#' Create Custom Filter Example
#' 
#' Generates example code for using custom filters with a function
#' 
#' @param function_name Character. Name of the function
#' @param os Character. Example OS to use
#' @return Character string with example code
#' @keywords internal
create_custom_filter_example <- function(function_name, os = "ios") {
  if (os == "unified") {
    sprintf(
'# Using custom filter from web interface
filter_result <- %s(
  os = "unified",
  custom_fields_filter_id = "your_filter_id_here",
  custom_tags_mode = "include_unified_apps",
  # ... other parameters
)', function_name)
  } else {
    sprintf(
'# Using custom filter from web interface  
filter_result <- %s(
  os = "%s",
  custom_fields_filter_id = "your_filter_id_here",
  # ... other parameters
)', function_name, os)
  }
}