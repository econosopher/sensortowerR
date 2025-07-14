#' Get All Apps from a Publisher
#'
#' Retrieves a list of apps associated with a specified unified publisher ID
#' from the Sensor Tower API. Targets the `/v1/unified/publishers/apps`
#' endpoint.
#'
#' @param publisher_id Character. The Unified Publisher ID from Sensor Tower
#'   for which to retrieve associated apps.
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
st_publisher_apps <- function(publisher_id,
                              auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN")) {
  # --- Input Validation & Setup ---
  stopifnot(
    is.character(publisher_id) &&
      length(publisher_id) == 1 && nzchar(publisher_id)
  )

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
    unified_id = publisher_id,
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
  parsed_data <- tryCatch(
    {
      httr::stop_for_status(
        response,
        task = paste("fetch apps for publisher ID", publisher_id)
      )
      content_text <- httr::content(response, "text", encoding = "UTF-8")
      if (content_text == "") {
        rlang::warn("API returned an empty response body.")
        return(NULL)
      }
      jsonlite::fromJSON(content_text, flatten = TRUE)
    },
    error = function(e) {
      http_err_info <- ""
      if (inherits(e, "http_error")) {
        status <- httr::http_status(e$response)
        http_err_info <- paste0(
          " (HTTP Status: ", status$status_code, " ", status$reason, ")"
        )
      }
      rlang::abort(paste0("Error processing API response", http_err_info, ": ", e$message))
      return(NULL)
    }
  )

  # --- Data Extraction & Processing ---
  if (is.null(parsed_data) || !"apps" %in% names(parsed_data)) {
    rlang::warn(
      "API response did not contain an 'apps' list for the given publisher_id."
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