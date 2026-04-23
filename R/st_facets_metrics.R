#' Low-Level Access to Sensor Tower Facets Metrics
#'
#' Performs a GET request against Sensor Tower's `"/v1/facets/metrics"` route.
#' This helper stays intentionally low-level so package code can work with new
#' facets-based endpoints while higher-level wrappers are added incrementally.
#'
#' @param query Character vector of raw query fragments appended verbatim after
#'   encoded `params`. This is useful when Sensor Tower documents nested or
#'   repeated parameters that are awkward to express as a regular named R list.
#'   Example: `c("filters[app_ids][]=553834731")`.
#' @param params Named list of regular query parameters. Vector values are
#'   serialized as comma-separated strings to match Sensor Tower's
#'   `style=form, explode=false` usage in the OpenAPI spec.
#' @param auth_token Optional. Character string. Your Sensor Tower API token.
#'   Defaults to environment variable `SENSORTOWER_AUTH_TOKEN`.
#' @param host Character string. Which host to target: `"api"` (default) or
#'   `"app"`. The `app` host uses `https://app.sensortower.com/api`.
#' @param verbose Logical. If `TRUE`, prints the request URL with the auth token
#'   redacted.
#'
#' @return A parsed JSON response. Rectangular top-level responses are returned
#'   as a tibble; nested responses are returned as a named list.
#'
#' @details
#' As of March 17, 2026, the facets route is live and the retention contract is
#' validated against production. The machine-readable Sensor Tower docs remain
#' gated behind a signed-in web session, so this helper still provides a stable
#' escape hatch for raw or partially documented facets requests.
#'
#' @examples
#' \dontrun{
#' # Retention request using regular query parameters
#' response <- st_facets_metrics(
#'   params = list(
#'     facets = "retention",
#'     bundle = "retention_daily",
#'     breakdown = c("date", "app_id"),
#'     start_date = "2025-01-01",
#'     end_date = "2025-01-31",
#'     app_ids = "553834731"
#'   )
#' )
#' }
#'
#' @export
st_facets_metrics <- function(query = character(),
                              params = list(),
                              auth_token = NULL,
                              host = c("api", "app"),
                              verbose = FALSE) {
  if (!is.character(query)) {
    rlang::abort("`query` must be a character vector of raw query fragments.")
  }

  if (!is.list(params)) {
    rlang::abort("`params` must be a named list.")
  }

  if (length(params) > 0 && (is.null(names(params)) || any(!nzchar(names(params))))) {
    rlang::abort("`params` must be a named list.")
  }

  host <- match.arg(host)
  auth_token_val <- resolve_auth_token(
    auth_token,
    error_message = "Authentication token not found. Set SENSORTOWER_AUTH_TOKEN environment variable."
  )

  base_url <- switch(
    host,
    api = st_api_base_url(),
    app = "https://app.sensortower.com/api"
  )

  query <- trimws(query)
  query <- query[nzchar(query)]
  params <- params[!vapply(params, is.null, logical(1))]

  encoded_params <- if (length(params) == 0) {
    character()
  } else {
    stats::setNames(
      lapply(params, format_facets_query_value),
      names(params)
    )
  }

  encoded_params <- encoded_params[!vapply(encoded_params, is.null, logical(1))]
  encoded_query <- if (length(encoded_params) == 0) {
    character()
  } else {
    paste0(
      names(encoded_params),
      "=",
      vapply(
        encoded_params,
        utils::URLencode,
        reserved = TRUE,
        FUN.VALUE = character(1)
      )
    )
  }

  raw_query <- c(
    paste0("auth_token=", utils::URLencode(auth_token_val, reserved = TRUE)),
    encoded_query,
    query
  )

  url <- paste0(
    base_url,
    "/",
    st_endpoint_path("facets_metrics"),
    "?",
    paste(raw_query, collapse = "&")
  )

  if (verbose) {
    message("Requesting: ", sub(auth_token_val, "HIDDEN", url, fixed = TRUE))
  }

  req <- httr2::request(url) %>%
    httr2::req_headers("Accept" = "application/json") %>%
    httr2::req_timeout(30)

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      if (inherits(e, "httr2_http") && !is.null(e$resp)) {
        body <- tryCatch(httr2::resp_body_string(e$resp), error = function(...) "")
        status <- tryCatch(httr2::resp_status(e$resp), error = function(...) NA_integer_)

        rlang::abort(
          c(
            sprintf("Facets metrics request failed with status %s.", status),
            if (nzchar(body)) c("i" = body) else NULL
          ),
          parent = e
        )
      }

      rlang::abort(
        "An unexpected error occurred during the facets metrics request.",
        parent = e
      )
    }
  )

  body_raw <- httr2::resp_body_raw(resp)
  if (length(body_raw) == 0) {
    return(tibble::tibble())
  }

  parsed <- jsonlite::fromJSON(
    rawToChar(body_raw),
    simplifyVector = TRUE,
    flatten = TRUE
  )

  if (inherits(parsed, "data.frame")) {
    return(tibble::as_tibble(parsed))
  }

  parsed
}

format_facets_query_value <- function(value) {
  if (length(value) == 0 || all(is.na(value))) {
    return(NULL)
  }

  if (inherits(value, "Date")) {
    value <- format(value, "%Y-%m-%d")
  }

  if (is.logical(value)) {
    value <- tolower(as.character(value))
  } else {
    value <- as.character(value)
  }

  value <- trimws(value)
  value <- value[nzchar(value)]

  if (length(value) == 0) {
    return(NULL)
  }

  paste(value, collapse = ",")
}
