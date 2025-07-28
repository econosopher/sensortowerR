# Validate README Examples
# This script checks that all functions mentioned in README exist and have correct signatures

library(sensortowerR)

cat("=== Validating sensortowerR README ===\n\n")

# Test 1: Check all core functions exist
cat("Test 1: Core Functions Availability\n")
core_functions <- c(
  "st_app_info",
  "st_publisher_apps", 
  "st_metrics",
  "st_top_charts",
  "st_game_summary",
  "st_category_rankings",
  "st_app_details",
  "st_top_publishers",
  "st_publisher_category_breakdown",
  "st_ytd_metrics",
  "st_gt_dashboard",
  "st_sales_report"
)

for (func in core_functions) {
  if (exists(func, where = "package:sensortowerR")) {
    cat(sprintf("  ✓ %s exists\n", func))
  } else {
    cat(sprintf("  ✗ %s NOT FOUND\n", func))
  }
}

# Test 2: Validate function signatures
cat("\nTest 2: Function Signatures\n")

# st_app_info
cat("\n  st_app_info:\n")
args_info <- formals(st_app_info)
expected_args <- c("term", "app_store", "entity_type", "limit", "auth_token", "return_all_fields")
cat(sprintf("    Expected args: %s\n", paste(expected_args, collapse = ", ")))
cat(sprintf("    Actual args: %s\n", paste(names(args_info), collapse = ", ")))

# st_metrics
cat("\n  st_metrics:\n")
args_metrics <- formals(st_metrics)
cat(sprintf("    Has 'app_id' parameter: %s\n", "app_id" %in% names(args_metrics)))
cat(sprintf("    Has 'unified_app_id' parameter (deprecated): %s\n", "unified_app_id" %in% names(args_metrics)))
cat(sprintf("    Has 'ios_app_id' parameter: %s\n", "ios_app_id" %in% names(args_metrics)))
cat(sprintf("    Has 'android_app_id' parameter: %s\n", "android_app_id" %in% names(args_metrics)))

# st_ytd_metrics
cat("\n  st_ytd_metrics:\n")
args_ytd <- formals(st_ytd_metrics)
cat(sprintf("    Has 'unified_app_id' parameter: %s\n", "unified_app_id" %in% names(args_ytd)))
cat(sprintf("    Has 'app_id' parameter (should be FALSE): %s\n", "app_id" %in% names(args_ytd)))
cat(sprintf("    Has 'years' parameter: %s\n", "years" %in% names(args_ytd)))
cat(sprintf("    Has 'period_start' parameter: %s\n", "period_start" %in% names(args_ytd)))
cat(sprintf("    Has 'period_end' parameter: %s\n", "period_end" %in% names(args_ytd)))

# st_top_charts
cat("\n  st_top_charts:\n")
args_charts <- formals(st_top_charts)
cat(sprintf("    Has 'measure' parameter: %s\n", "measure" %in% names(args_charts)))
cat(sprintf("    Default measure: %s\n", deparse(args_charts$measure)))
cat(sprintf("    Has 'category' parameter: %s\n", "category" %in% names(args_charts)))

# Test 3: Check for deprecated functions
cat("\nTest 3: Deprecated Functions Check\n")
deprecated_functions <- c(
  "calculate_ytd_change",  # Should not exist
  "st_top_sales"           # Replaced by st_top_charts
)

for (func in deprecated_functions) {
  if (exists(func, where = "package:sensortowerR")) {
    cat(sprintf("  ✗ %s still exists (should be removed)\n", func))
  } else {
    cat(sprintf("  ✓ %s correctly removed/deprecated\n", func))
  }
}

# Test 4: Validate example code snippets
cat("\nTest 4: Example Code Validation (syntax check only)\n")

# Example from README - st_metrics
cat("  Testing st_metrics example...\n")
tryCatch({
  # This will fail without auth token, but we're checking syntax
  expr <- quote(st_metrics(
    app_id = "1195621598",
    start_date = Sys.Date() - 30,
    end_date = Sys.Date() - 1
  ))
  cat("    ✓ st_metrics example syntax is valid\n")
}, error = function(e) {
  cat("    ✗ st_metrics example has syntax error\n")
})

# Example from README - st_ytd_metrics
cat("  Testing st_ytd_metrics example...\n")
tryCatch({
  expr <- quote(st_ytd_metrics(
    unified_app_id = "553834731",
    years = c(2023, 2024, 2025),
    cache_dir = ".cache/ytd"
  ))
  cat("    ✓ st_ytd_metrics example syntax is valid\n")
}, error = function(e) {
  cat("    ✗ st_ytd_metrics example has syntax error\n")
})

# Test 5: Check default values
cat("\nTest 5: Default Parameter Values\n")

# st_top_charts defaults
cat("  st_top_charts defaults:\n")
defaults_charts <- formals(st_top_charts)
cat(sprintf("    measure default: %s\n", deparse(defaults_charts$measure)))
cat(sprintf("    os default: %s\n", deparse(defaults_charts$os)))
cat(sprintf("    time_range default: %s\n", deparse(defaults_charts$time_range)))

# st_ytd_metrics defaults  
cat("\n  st_ytd_metrics defaults:\n")
defaults_ytd <- formals(st_ytd_metrics)
cat(sprintf("    metrics default: %s\n", deparse(defaults_ytd$metrics)))
cat(sprintf("    countries default: %s\n", deparse(defaults_ytd$countries)))
cat(sprintf("    verbose default: %s\n", deparse(defaults_ytd$verbose)))

# Test 6: Batch capabilities
cat("\nTest 6: Batch Request Capabilities\n")

# Check if functions accept vectors
cat("  Functions that should accept multiple IDs:\n")
batch_functions <- list(
  st_ytd_metrics = "unified_app_id",
  st_sales_report = "app_ids",
  st_app_details = "app_ids"
)

for (func_name in names(batch_functions)) {
  param_name <- batch_functions[[func_name]]
  if (exists(func_name, where = "package:sensortowerR")) {
    func_args <- formals(get(func_name))
    if (param_name %in% names(func_args)) {
      cat(sprintf("    ✓ %s has '%s' parameter\n", func_name, param_name))
    } else {
      cat(sprintf("    ✗ %s missing '%s' parameter\n", func_name, param_name))
    }
  }
}

# Test 7: Key features mentioned in README
cat("\nTest 7: Key Features Validation\n")

# Check if st_metrics has the auto-detection features
cat("  st_metrics features:\n")
metrics_args <- names(formals(st_metrics))
cat(sprintf("    Has date_granularity: %s\n", "date_granularity" %in% metrics_args))
cat(sprintf("    Has combine_platforms: %s\n", "combine_platforms" %in% metrics_args))
cat(sprintf("    Has verbose: %s\n", "verbose" %in% metrics_args))
cat(sprintf("    Has countries: %s\n", "countries" %in% metrics_args))

# Summary
cat("\n=== Validation Complete ===\n")
cat("Check the output above for any ✗ marks indicating issues.\n")
cat("All ✓ marks indicate the README accurately reflects the package.\n")