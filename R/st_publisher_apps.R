#' Get All Apps from a Publisher
#'
#' Retrieves a list of apps associated with a specified unified publisher ID
#' from the Sensor Tower API. Targets the `/v1/unified/publishers/apps` endpoint.
#'
#' @param publisher_id Character. The Unified Publisher ID from Sensor Tower for
#'   which to retrieve associated apps.
#' @param auth_token Character. Your Sensor Tower API authentication token.
#'   Defaults to the value stored in the `SENSORTOWER_AUTH_TOKEN` environment
#'   variable.
#'
#' @return A [tibble][tibble::tibble] containing details of the apps associated
#'   with the publisher. The exact columns depend on the API response but often
#'   include app IDs, names, platform, etc. Returns an empty tibble if the
#'   publisher ID is invalid, has no apps, or an error occurs.
#'
#' @section API Endpoint Used:
#'   - `GET /v1/unified/publishers/apps`
#'
#' @examples
#' \dontrun{
#' # Ensure SENSORTOWER_AUTH_TOKEN is set in your environment
#' # Sys.setenv(SENSORTOWER_AUTH_TOKEN = "your_secure_auth_token_here")
#'
#' # Define the publisher ID (use a real ID)
#' publisher_id <- "YOUR_PUBLISHER_ID_HERE"
#'
#' # Fetch the publisher's apps
#' apps_list <- st_publisher_apps(publisher_id = publisher_id)
#'
#' # View the results
#' print(apps_list)
#' }
#'
#' @import dplyr
#' @importFrom httr GET add_headers stop_for_status content http_status http_error
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble as_tibble
#' @importFrom rlang abort warn %||%
#' @export
st_publisher_apps <- function(publisher_id,
                                auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN")) {

  # --- Input Validation & Setup ---
  stopifnot(
      is.character(publisher_id) && length(publisher_id) == 1 && nzchar(publisher_id)
  )

  # Check auth token *after* default assignment
  if (is.null(auth_token) || auth_token == "") {
    rlang::abort("Authentication token is required. Set SENSORTOWER_AUTH_TOKEN environment variable or pass via auth_token argument.")
  }

  # Construct the API endpoint
  base_url <- "https://api.sensortower.com/v1"
  # Endpoint path seems slightly off in original, should likely not start with / if using file.path or paste0
  endpoint_path <- "unified/publishers/apps"
  url <- file.path(base_url, endpoint_path) # Use file.path for robustness

  # Build query parameters
  query_params <- list(
    # IMPORTANT: Verify API parameter name: is it 'unified_id' or 'publisher_id'? Using 'unified_id' based on original code.
    unified_id = publisher_id,
    auth_token = auth_token
  )

  # --- API Call ---
  response <- tryCatch({
      httr::GET(
        url = url,
        query = query_params,
        httr::add_headers("Accept" = "application/json") # Good practice
      )
  }, error = function(e) {
      rlang::abort(paste("HTTP request failed:", e$message))
  })

  # --- Response Handling ---
  # Use stop_for_status for standard HTTP errors (4xx, 5xx)
  # Wrap in tryCatch in case content parsing fails before we can check specific errors
  parsed_data <- tryCatch({
      httr::stop_for_status(response, task = paste("fetch apps for publisher ID", publisher_id))

      # Parse the response content
      content_text <- httr::content(response, "text", encoding = "UTF-8")
      if(content_text == "" || is.null(content_text)) {
          rlang::warn("API returned an empty response body.")
          return(NULL) # Indicate empty response
      }

      jsonlite::fromJSON(content_text, flatten = TRUE)

  }, error = function(e) {
      # Catch errors from stop_for_status or fromJSON
      # Check if it's an HTTP error to provide status
      http_err_info <- ""
      if(inherits(e, "http_error")) {
          status <- httr::http_status(e$response)
          http_err_info <- paste(" (HTTP Status:", status$status_code, status$reason, ")")
      }
      # Re-throw as an abort for consistency, include HTTP info if available
      rlang::abort(paste0("Error processing API response", http_err_info, ": ", e$message))
      # Return NULL or empty structure if abort is not desired here
      # return(NULL)
  })

  # --- Data Extraction & Processing ---
  if (is.null(parsed_data)) {
      # Handle case where response body was empty
      return(tibble::tibble())
  }

  # Check if 'apps' data is available and is a list or data frame
  if (!"apps" %in% names(parsed_data) || !(is.data.frame(parsed_data$apps) || is.list(parsed_data$apps))) {
    rlang::warn("API response did not contain an 'apps' list or data frame for the given publisher_id.")
    return(tibble::tibble())
  }

  # Extract the apps data
  apps_data <- parsed_data$apps

  # Handle empty list/dataframe case
  if (length(apps_data) == 0 || (is.data.frame(apps_data) && nrow(apps_data) == 0)) {
      message("No apps found associated with the publisher ID.")
      return(tibble::tibble())
  }

  # Ensure it's a tibble for consistency
  # If it's a list of lists, bind_rows; otherwise, convert df to tibble
  if (is.list(apps_data) && !is.data.frame(apps_data)) {
      result_tbl <- tryCatch({
          dplyr::bind_rows(apps_data)
      }, error = function(e) {
          rlang::warn("Could not bind rows from the 'apps' list. Check API response structure.")
          return(tibble::tibble()) # Return empty on failure
      })
      return(tibble::as_tibble(result_tbl))
  } else {
      # Already a data frame
      return(tibble::as_tibble(apps_data))
  }
}