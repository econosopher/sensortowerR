#' Diagnose API Issues
#'
#' This function helps diagnose common API issues by testing various ID formats
#' and endpoints to determine the best approach for fetching data.
#'
#' @param app_id Character string. The app ID to diagnose (can be unified, iOS, or Android)
#' @param verbose Logical. Show detailed diagnostic output. Default is TRUE.
#' @param auth_token Character string. Your Sensor Tower API authentication token.
#'
#' @return A list with diagnostic results including:
#'   - `id_type`: Detected ID type
#'   - `platform_ids`: Resolved platform-specific IDs
#'   - `endpoint_results`: Results from testing various endpoints
#'   - `recommendations`: Suggested approach for this app
#'
#' @examples
#' \dontrun{
#' # Diagnose Star Trek Fleet Command
#' diagnosis <- st_api_diagnostics("5ba4585f539ce75b97db6bcb")
#' 
#' # Check iOS app
#' diagnosis <- st_api_diagnostics("1427744264")
#' }
#' 
#' @export
st_api_diagnostics <- function(app_id, 
                              verbose = TRUE,
                              auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN")) {
  
  # Validate auth token
  if (is.null(auth_token) || auth_token == "") {
    rlang::abort("Authentication token is required. Set SENSORTOWER_AUTH_TOKEN environment variable.")
  }
  
  results <- list(
    id_type = "unknown",
    platform_ids = list(ios = NULL, android = NULL),
    endpoint_results = list(),
    recommendations = character()
  )
  
  # Step 1: Detect ID type
  if (verbose) message("=== API DIAGNOSTICS FOR: ", app_id, " ===\n")
  
  if (grepl("^[a-f0-9]{24}$", app_id)) {
    results$id_type <- "sensor_tower_hex"
    if (verbose) message("Detected: Sensor Tower hex ID")
  } else if (grepl("^\\d+$", app_id)) {
    results$id_type <- "ios_numeric"
    if (verbose) message("Detected: iOS numeric ID")
    results$platform_ids$ios <- app_id
  } else if (grepl("^(com|net|org|io)\\.", app_id)) {
    results$id_type <- "android_package"
    if (verbose) message("Detected: Android package name")
    results$platform_ids$android <- app_id
  }
  
  # Step 2: Try to resolve platform IDs if needed
  if (results$id_type == "sensor_tower_hex") {
    if (verbose) message("\nResolving platform IDs...")
    
    lookup_result <- tryCatch({
      st_app_lookup(app_id, verbose = FALSE, auth_token = auth_token)
    }, error = function(e) NULL)
    
    if (!is.null(lookup_result)) {
      results$platform_ids$ios <- lookup_result$ios_app_id
      results$platform_ids$android <- lookup_result$android_app_id
      if (verbose) {
        message("  iOS ID: ", results$platform_ids$ios %||% "not found")
        message("  Android ID: ", results$platform_ids$android %||% "not found")
      }
    } else {
      if (verbose) message("  Could not resolve platform IDs")
    }
  }
  
  # Step 3: Test various endpoints
  if (verbose) message("\nTesting endpoints...")
  
  # Test unified endpoint
  test_endpoint <- function(endpoint_name, test_func) {
    if (verbose) message("  Testing ", endpoint_name, "... ", appendLF = FALSE)
    
    result <- tryCatch({
      data <- test_func()
      if (is.null(data) || nrow(data) == 0) {
        if (verbose) message("EMPTY")
        list(status = "empty", rows = 0)
      } else {
        if (verbose) message("SUCCESS (", nrow(data), " rows)")
        list(status = "success", rows = nrow(data))
      }
    }, error = function(e) {
      if (verbose) message("ERROR: ", e$message)
      list(status = "error", message = e$message)
    })
    
    return(result)
  }
  
  # Test sales report with detected OS
  os_type <- if (results$id_type == "ios_numeric") "ios" 
             else if (results$id_type == "android_package") "android" 
             else "ios"  # default
  
  # Determine the appropriate parameter based on os_type
  results$endpoint_results$sales_report <- test_endpoint(
    paste0(toupper(os_type), " Sales Report"),
    function() {
      if (os_type == "ios") {
        st_sales_report(
          ios_app_id = app_id,
          os = os_type,
          date_granularity = "monthly",
          start_date = format(Sys.Date() - 90, "%Y-%m-%d"),
          end_date = format(Sys.Date() - 1, "%Y-%m-%d"),
          countries = "US",
          auth_token = auth_token
        )
      } else {
        st_sales_report(
          android_app_id = app_id,
          os = os_type,
          date_granularity = "monthly",
          start_date = format(Sys.Date() - 90, "%Y-%m-%d"),
          end_date = format(Sys.Date() - 1, "%Y-%m-%d"),
          countries = "US",
          auth_token = auth_token
        )
      }
    }
  )

  # Test platform-specific if we have IDs
  if (!is.null(results$platform_ids$ios)) {
    results$endpoint_results$ios_sales <- test_endpoint(
      "iOS Sales Report",
      function() {
        st_sales_report(
          ios_app_id = results$platform_ids$ios,
          os = "ios",
          date_granularity = "monthly",
          start_date = format(Sys.Date() - 90, "%Y-%m-%d"),
          end_date = format(Sys.Date() - 1, "%Y-%m-%d"),
          countries = "US",
          auth_token = auth_token
        )
      }
    )
  }

  if (!is.null(results$platform_ids$android)) {
    results$endpoint_results$android_sales <- test_endpoint(
      "Android Sales Report",
      function() {
        st_sales_report(
          android_app_id = results$platform_ids$android,
          os = "android",
          date_granularity = "monthly",
          start_date = format(Sys.Date() - 90, "%Y-%m-%d"),
          end_date = format(Sys.Date() - 1, "%Y-%m-%d"),
          countries = "US",
          auth_token = auth_token
        )
      }
    )
  }

  # Test app details
  if (results$id_type %in% c("ios_numeric", "android_package")) {
    os <- ifelse(results$id_type == "ios_numeric", "ios", "android")
    results$endpoint_results$app_details <- test_endpoint(
      paste0(toupper(os), " App Details"),
      function() {
        st_app_details(
          app_ids = app_id,
          os = os,
          auth_token = auth_token
        )
      }
    )
  }
  
  # Step 4: Generate recommendations
  if (verbose) message("\nGenerating recommendations...")
  
  if (results$id_type == "sensor_tower_hex") {
    if (!is.null(results$platform_ids$ios) || !is.null(results$platform_ids$android)) {
      results$recommendations <- c(
        "Use platform-specific IDs for best results",
        sprintf("iOS: %s", results$platform_ids$ios %||% "not available"),
        sprintf("Android: %s", results$platform_ids$android %||% "not available"),
        "Unified endpoints may return empty data for this app"
      )
    } else {
      results$recommendations <- c(
        "Could not resolve platform IDs",
        "Try searching by app name using st_app_info()",
        "Unified endpoints unlikely to work with this ID"
      )
    }
  } else {
    results$recommendations <- c(
      sprintf("This is a %s ID", gsub("_", " ", results$id_type)),
      "Use directly with platform-specific endpoints",
      "For combined iOS+Android data, find the corresponding ID for the other platform"
    )
  }
  
  if (verbose) {
    message("\n=== RECOMMENDATIONS ===")
    for (rec in results$recommendations) {
      message("- ", rec)
    }
  }
  
  return(results)
}

# Helper operator
`%||%` <- function(x, y) if (is.null(x)) y else x