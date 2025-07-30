# Custom Filter Integration Strategy for sensortowerR

## Executive Summary

This document outlines a comprehensive strategy for enhancing custom field filter support across the sensortowerR package, making it easier for users to leverage Sensor Tower's web-based filter creation capabilities in their R workflows.

## Current State

### What's Implemented
- `st_top_charts()` supports `custom_fields_filter_id` parameter
- Basic validation that either `category` or `custom_fields_filter_id` is provided
- Support for `custom_tags_mode` when `os = "unified"`

### Gaps
- No filter validation functions
- Limited error handling for invalid filter IDs
- No helper functions for filter management
- Other potentially compatible functions don't support custom filters
- No documentation about which endpoints support filters

## Proposed Enhancements

### 1. Core Infrastructure

#### A. Filter Validation Function
```r
#' Validate Custom Field Filter ID
#' 
#' @param filter_id Character string. The filter ID to validate
#' @param test_call Logical. Whether to make a test API call
#' @return List with validation results
#' @export
st_validate_filter <- function(filter_id, test_call = FALSE) {
  # Check format
  valid_format <- grepl("^[a-f0-9]{24}$", filter_id)
  
  result <- list(
    filter_id = filter_id,
    valid_format = valid_format,
    tested = FALSE,
    test_result = NULL
  )
  
  if (test_call && valid_format) {
    # Make minimal API call to test
    test <- tryCatch(
      st_top_charts(
        os = "ios",
        custom_fields_filter_id = filter_id,
        regions = "US",
        limit = 1
      ),
      error = function(e) e
    )
    
    result$tested <- TRUE
    result$test_result <- !inherits(test, "error")
    if (inherits(test, "error")) {
      result$error_message <- test$message
    }
  }
  
  class(result) <- c("st_filter_validation", "list")
  result
}
```

#### B. Filter Management System
```r
# R/st_filter_manager.R

# Environment to store filter mappings
.filter_cache <- new.env(parent = emptyenv())

#' Register a custom filter
#' 
#' @param name Character string. Friendly name for the filter
#' @param filter_id Character string. The Sensor Tower filter ID
#' @param description Character string. Optional description
#' @export
st_register_filter <- function(name, filter_id, description = NULL) {
  if (!st_validate_filter(filter_id)$valid_format) {
    stop("Invalid filter ID format")
  }
  
  .filter_cache[[name]] <- list(
    id = filter_id,
    description = description,
    registered = Sys.time()
  )
  
  invisible(TRUE)
}

#' Get registered filter ID by name
#' 
#' @param name Character string. Filter name
#' @return Character string. Filter ID
#' @export
st_get_filter <- function(name) {
  if (!exists(name, envir = .filter_cache)) {
    stop("Filter '", name, "' not found. Use st_list_filters() to see available filters.")
  }
  .filter_cache[[name]]$id
}

#' List all registered filters
#' 
#' @return Data frame of registered filters
#' @export
st_list_filters <- function() {
  filters <- ls(envir = .filter_cache)
  if (length(filters) == 0) {
    return(data.frame(
      name = character(),
      filter_id = character(),
      description = character(),
      registered = character()
    ))
  }
  
  do.call(rbind, lapply(filters, function(name) {
    f <- .filter_cache[[name]]
    data.frame(
      name = name,
      filter_id = f$id,
      description = f$description %||% "",
      registered = f$registered
    )
  }))
}
```

### 2. Enhanced Error Handling

```r
# utils.R enhancement
handle_filter_error <- function(e, filter_id) {
  if (grepl("422", e$message)) {
    stop(
      "Invalid custom filter ID: ", filter_id, "\n",
      "This could mean:\n",
      "- The filter ID doesn't exist\n",
      "- The filter belongs to a different account\n",
      "- The filter has been deleted\n",
      "To get a valid filter ID, visit app.sensortower.com and create a filter",
      call. = FALSE
    )
  } else if (grepl("403", e$message)) {
    stop(
      "Access denied for filter ID: ", filter_id, "\n",
      "This filter may belong to a different Sensor Tower account",
      call. = FALSE
    )
  } else {
    stop(e)
  }
}
```

### 3. Extend Filter Support to Other Functions

#### A. Identify Compatible Functions
Based on API analysis, these functions could support custom filters:
- `st_sales_report()` - Uses sales_report_estimates endpoint
- `st_batch_metrics()` - Could pass filters through to underlying calls
- Future market analysis functions

#### B. Implementation Pattern
```r
# Add to function parameters
#' @param custom_fields_filter_id Optional. Custom filter ID from Sensor Tower
#' @param custom_tags_mode Optional. Required for unified OS with custom filter

# In function body
if (!is.null(custom_fields_filter_id)) {
  # Validate filter
  if (!grepl("^[a-f0-9]{24}$", custom_fields_filter_id)) {
    stop("Invalid custom_fields_filter_id format")
  }
  
  # Add to query parameters
  query_params$custom_fields_filter_id <- custom_fields_filter_id
  
  # Handle unified OS requirement
  if (os == "unified" && is.null(custom_tags_mode)) {
    stop("custom_tags_mode is required when using custom_fields_filter_id with unified OS")
  }
}
```

### 4. Configuration File Support

```r
#' Load filters from configuration file
#' 
#' @param file Path to YAML or JSON file with filter definitions
#' @export
st_load_filters <- function(file = "sensor_tower_filters.yml") {
  if (!file.exists(file)) {
    stop("Filter configuration file not found: ", file)
  }
  
  # Support both YAML and JSON
  if (grepl("\\.ya?ml$", file)) {
    filters <- yaml::read_yaml(file)
  } else if (grepl("\\.json$", file)) {
    filters <- jsonlite::fromJSON(file)
  } else {
    stop("Unsupported file format. Use .yml, .yaml, or .json")
  }
  
  # Register each filter
  for (name in names(filters)) {
    st_register_filter(
      name = name,
      filter_id = filters[[name]]$id,
      description = filters[[name]]$description
    )
  }
  
  message("Loaded ", length(filters), " filters from ", file)
  invisible(TRUE)
}
```

Example configuration file (sensor_tower_filters.yml):
```yaml
hypercasual_games:
  id: "687df26ac5a19ebcfe817d7f"
  description: "Hypercasual games from top publishers"

strategy_asia:
  id: "789ef34bc6b20fcade928e8a"
  description: "Strategy games popular in Asian markets"

premium_apps:
  id: "456ab78cd9e31gbcef103f2b"
  description: "Premium apps over $4.99"
```

### 5. User Workflow Enhancements

#### A. Interactive Filter Builder
```r
#' Build a Sensor Tower filter URL
#' 
#' @param base_url Base URL for Sensor Tower
#' @param ... Filter parameters
#' @export
st_build_filter_url <- function(
  base_url = "https://app.sensortower.com/top-charts",
  os = "unified",
  category = NULL,
  countries = NULL,
  ...
) {
  # Build URL with parameters
  params <- list(os = os, ...)
  if (!is.null(category)) params$category <- category
  if (!is.null(countries)) params$countries <- paste(countries, collapse = ",")
  
  url <- httr::modify_url(base_url, query = params)
  
  message("Visit this URL to create your filter:")
  message(url)
  message("\nAfter creating the filter, copy the custom_fields_filter_id from the URL")
  
  invisible(url)
}
```

#### B. Filter Testing Utility
```r
#' Test a custom filter
#' 
#' @param filter_id Filter ID to test
#' @param verbose Logical. Print detailed results
#' @export
st_test_filter <- function(filter_id, verbose = TRUE) {
  if (verbose) cat("Testing filter:", filter_id, "\n\n")
  
  # Test on different OS
  results <- list()
  
  for (os in c("ios", "android", "unified")) {
    if (verbose) cat("Testing", os, "... ")
    
    args <- list(
      os = os,
      custom_fields_filter_id = filter_id,
      regions = "US",
      limit = 5
    )
    
    if (os == "unified") {
      args$custom_tags_mode <- "include"
    }
    
    result <- tryCatch(
      do.call(st_top_charts, args),
      error = function(e) e
    )
    
    success <- !inherits(result, "error")
    results[[os]] <- success
    
    if (verbose) {
      cat(ifelse(success, "✅ Success", "❌ Failed"), "\n")
      if (!success) {
        cat("  Error:", result$message, "\n")
      } else if (is.data.frame(result)) {
        cat("  Returned", nrow(result), "apps\n")
      }
    }
  }
  
  invisible(results)
}
```

### 6. Documentation Strategy

#### A. Vignette: "Using Custom Filters"
Create a comprehensive vignette covering:
- What are custom filters
- How to create them in Sensor Tower
- Using filters in sensortowerR
- Managing multiple filters
- Best practices
- Troubleshooting

#### B. Function Documentation
Update all compatible functions with:
- Parameter documentation for custom_fields_filter_id
- Examples using custom filters
- Links to the vignette

#### C. README Section
Add prominent section about custom filter support with quick examples

### 7. Testing Framework

```r
# tests/testthat/test-custom-filters.R

test_that("Filter validation works correctly", {
  expect_true(st_validate_filter("687df26ac5a19ebcfe817d7f")$valid_format)
  expect_false(st_validate_filter("invalid")$valid_format)
  expect_false(st_validate_filter("687df26ac5a19ebcfe817d7")$valid_format)
})

test_that("Filter registration and retrieval work", {
  st_register_filter("test_filter", "687df26ac5a19ebcfe817d7f", "Test filter")
  expect_equal(st_get_filter("test_filter"), "687df26ac5a19ebcfe817d7f")
  expect_error(st_get_filter("nonexistent"))
})

test_that("Custom filter parameters work in API calls", {
  skip_if_no_auth()
  
  # Test with mock filter ID
  expect_error(
    st_top_charts(
      custom_fields_filter_id = "invalid_id",
      regions = "US"
    ),
    "Invalid custom_fields_filter_id format"
  )
})
```

## Implementation Phases

### Phase 1: Core Infrastructure (Week 1)
- Implement filter validation function
- Add filter management system
- Enhance error handling
- Create basic tests

### Phase 2: Function Integration (Week 2)
- Extend filter support to compatible functions
- Update parameter validation
- Add configuration file support
- Update documentation

### Phase 3: User Experience (Week 3)
- Create interactive utilities
- Write comprehensive vignette
- Add examples to README
- Create tutorial blog post

### Phase 4: Testing and Release (Week 4)
- Comprehensive testing
- User acceptance testing
- Documentation review
- Package release

## Success Metrics

1. **Adoption**: Number of users utilizing custom filters
2. **Error Reduction**: Decrease in filter-related support issues
3. **Efficiency**: Time saved by using pre-configured filters
4. **Coverage**: Percentage of compatible functions supporting filters

## Conclusion

This integration strategy will make sensortowerR the most comprehensive R interface for Sensor Tower's custom filter functionality, significantly improving user workflow efficiency and reducing the complexity of working with complex data filters.