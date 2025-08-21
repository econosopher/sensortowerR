#' Get All Apps from a Publisher
#'
#' Retrieves a list of apps associated with a specified unified publisher ID
#' from the Sensor Tower API. Targets the `/v1/unified/publishers/apps`
#' endpoint.
#'
#' @param unified_id Character. Unified ID to resolve apps for. May be either:
#'   - Unified Publisher ID (24-char hex)
#'   - Unified App ID (24-char hex) belonging to a publisher
#'   The API returns the unified publisher and all associated apps in both cases.
#' @param publisher_id Deprecated alias for `unified_id`.
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
#' @importFrom httr GET add_headers stop_for_status content http_status
#'   http_error
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble as_tibble
#' @importFrom rlang abort warn `%||%`
#' @export
st_publisher_apps <- function(unified_id = NULL,
                              publisher_id = NULL,
                              auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN")) {
  # --- Input Validation & Setup ---
  # Backward compatibility: support old signature
  if (is.null(unified_id) && !is.null(publisher_id)) unified_id <- publisher_id
  if (is.null(unified_id)) {
    rlang::abort("unified_id is required (24-char hex unified publisher or app id)")
  }
  stopifnot(is.character(unified_id), length(unified_id) == 1, nzchar(unified_id))
  # Basic format check: 24-char hex
  if (!grepl("^[a-f0-9]{24}$", unified_id)) {
    rlang::abort(paste0(
      "Invalid unified_id format: '", unified_id, "'. Expected 24-char hex (unified publisher or app id)."
    ))
  }

  if (is.null(auth_token) || auth_token == "") {
    rlang::abort(
      paste(
        "Authentication token is required.",
        "Set SENSORTOWER_AUTH_TOKEN environment variable",
        "or pass via auth_token argument."
      )
    )
  }

  base_url <- "https://api.sensortower.com/v1"
  endpoint_path <- "unified/publishers/apps"
  url <- file.path(base_url, endpoint_path)

  query_params <- list(
    unified_id = unified_id,
    auth_token = auth_token
  )

  # --- API Call ---
  response <- tryCatch(
    httr::GET(
      url = url,
      query = query_params,
      httr::add_headers("Accept" = "application/json")
    ),
    error = function(e) {
      rlang::abort(paste("HTTP request failed:", e$message))
    }
  )

  # --- Response Handling ---
  status_code <- tryCatch(httr::status_code(response), error = function(e) NA_integer_)
  if (is.na(status_code) || status_code >= 400) {
    body_text <- tryCatch(httr::content(response, "text", encoding = "UTF-8"), error = function(e) "")
    msg <- paste0("API request failed (HTTP ", status_code, ")")
    if (nzchar(body_text)) {
      # Try extracting structured error
      parsed_err <- tryCatch(jsonlite::fromJSON(body_text), error = function(e) NULL)
      if (!is.null(parsed_err) && !is.null(parsed_err$error)) {
        msg <- paste0(msg, ": ", parsed_err$error)
      }
    }
    rlang::abort(msg)
  }

  content_text <- httr::content(response, "text", encoding = "UTF-8")
  if (content_text == "") {
    rlang::warn("API returned an empty response body.")
    return(tibble::tibble())
  }
  parsed_data <- jsonlite::fromJSON(content_text, flatten = TRUE)

  # --- Data Extraction & Processing ---
  if (is.null(parsed_data) || !"apps" %in% names(parsed_data)) {
    rlang::warn(
      "API response did not contain an 'apps' list for the given unified_id."
    )
    return(tibble::tibble())
  }

  apps_data <- parsed_data$apps

  if (length(apps_data) == 0) {
    message("No apps found associated with the publisher ID.")
    return(tibble::tibble())
  }

  tibble::as_tibble(apps_data)
}