#' Fetch App Details
#'
#' Thin v1.0.0 facade for app metadata lookups. By default it dispatches to the
#' legacy detailed app metadata endpoint. When `fields` is supplied, it dispatches
#' to `st_app_enriched()` and optionally subsets to the requested fields.
#'
#' @param app_id Character scalar or vector of app identifiers.
#' @param os Operating system context. One of `"ios"`, `"android"`, or
#'   `"unified"`.
#' @param fields Optional character vector of enriched fields to keep. When
#'   supplied, the request is routed through `st_app_enriched()`.
#' @param auth_token Optional Sensor Tower API token.
#'
#' @return A tibble returned by the dispatched legacy implementation.
#'
#' @examples
#' \dontrun{
#' st_app("553834731", os = "ios")
#'
#' st_app(
#'   c("553834731", "com.supercell.clashofclans"),
#'   fields = c("revenue_90d_ww", "downloads_30d_ww")
#' )
#' }
#'
#' @export
st_app <- function(app_id,
                   os = "unified",
                   fields = NULL,
                   auth_token = NULL) {
  if (missing(app_id) || is.null(app_id) || !length(app_id)) {
    rlang::abort("`app_id` must be a non-empty character scalar or vector.")
  }

  app_id <- as.character(app_id)
  if (any(is.na(app_id) | !nzchar(app_id))) {
    rlang::abort("`app_id` entries must be non-empty strings.")
  }

  os <- normalize_os(os)
  auth_token <- get_auth_token(
    auth_token,
    error_message = paste(
      "Authentication token is required.",
      "Set SENSORTOWER_AUTH_TOKEN or pass `auth_token`."
    )
  )

  if (is.null(fields)) {
    resolved_ids <- .st_app_resolve_ids_for_os(
      app_id = app_id,
      os = os,
      auth_token = auth_token
    )

    return(
      st_app_details_impl(
        app_ids = resolved_ids,
        os = os,
        auth_token = auth_token
      )
    )
  }

  fields <- unique(as.character(fields))
  unified_ids <- .st_app_resolve_ids_for_os(
    app_id = app_id,
    os = "unified",
    auth_token = auth_token
  )

  enriched <- st_app_enriched(
    unified_app_ids = unified_ids,
    os = "unified",
    auth_token = auth_token
  )

  keep <- unique(c(
    "app_id", "unified_app_id", "app_name", "unified_app_name", "os", fields
  ))

  dplyr::select(enriched, dplyr::any_of(keep))
}

#' Search or Filter Apps
#'
#' Thin v1.0.0 facade for app discovery by query string or server-side filter.
#'
#' @param query Optional search string for app-name search.
#' @param filter Optional `st_filter` object or 24-character filter ID string.
#' @param os Operating system context for query searches.
#' @param country Two-letter country code used by the filtered-app workflow.
#' @param limit Positive integer row limit.
#' @param auth_token Optional Sensor Tower API token.
#'
#' @return A tibble returned by the dispatched legacy implementation.
#'
#' @examples
#' \dontrun{
#' st_apps(query = "Royal Match", os = "unified", limit = 25)
#'
#' filt <- st_filter(genre = "Puzzle")
#' st_apps(filter = filt, country = "US", limit = 50)
#' }
#'
#' @export
st_apps <- function(query = NULL,
                    filter = NULL,
                    os = "ios",
                    country = "US",
                    limit = 100,
                    auth_token = NULL) {
  if (is.null(query) && is.null(filter)) {
    rlang::abort("Provide either `query` or `filter`.")
  }
  if (!is.null(query) && !is.null(filter)) {
    rlang::abort("Provide only one of `query` or `filter`, not both.")
  }

  os <- normalize_os(os)
  country <- normalize_country(country)
  limit <- suppressWarnings(as.integer(limit[1]))
  if (is.na(limit) || limit < 1L) {
    rlang::abort("`limit` must be a positive integer.")
  }

  auth_token <- get_auth_token(
    auth_token,
    error_message = paste(
      "Authentication token is required.",
      "Set SENSORTOWER_AUTH_TOKEN or pass `auth_token`."
    )
  )

  if (!is.null(query)) {
    query <- as.character(query[1])
    if (!nzchar(query)) {
      rlang::abort("`query` must be a non-empty string when supplied.")
    }

    return(
      st_app_info_impl(
        term = query,
        app_store = os,
        entity_type = "app",
        limit = limit,
        auth_token = auth_token
      )
    )
  }

  filter_id <- .st_filter_resolve_id(filter)
  if (is.null(filter_id)) {
    rlang::abort("`filter` must be an `st_filter` object or a 24-character filter ID.")
  }

  if (os != "unified") {
    rlang::warn(
      "`st_apps(filter = ...)` uses the unified filtered-app workflow and ignores `os`."
    )
  }

  st_get_filtered_apps(
    filter_id = filter_id,
    regions = country,
    limit = limit,
    auth_token = auth_token
  )
}

.st_app_resolve_ids_for_os <- function(app_id, os, auth_token) {
  vapply(
    app_id,
    function(one_id) .st_app_resolve_single_id(one_id, os, auth_token),
    character(1)
  )
}

.st_app_resolve_single_id <- function(app_id, os, auth_token) {
  app_id <- as.character(app_id[1])

  if (os == "ios" && grepl("^\\d+$", app_id)) {
    return(app_id)
  }
  if (os == "android" && grepl("^(com|net|org|io|app|game)\\.", app_id)) {
    return(app_id)
  }
  if (os == "unified" && grepl("^[a-f0-9]{24}$", app_id)) {
    return(app_id)
  }

  resolved <- tryCatch(
    resolve_app_id(app_id, auth_token = auth_token, use_cache = TRUE, verbose = FALSE),
    error = function(e) NULL
  )

  if (is.null(resolved)) {
    rlang::abort(sprintf("Failed to resolve app ID '%s' for `os = '%s'`.", app_id, os))
  }

  resolved_id <- switch(
    os,
    ios = resolved$ios_app_id,
    android = resolved$android_app_id,
    unified = resolved$unified_app_id
  )

  if (is.null(resolved_id) || is.na(resolved_id) || !nzchar(as.character(resolved_id))) {
    rlang::abort(
      sprintf("Could not resolve a %s app ID for input '%s'.", os, app_id)
    )
  }

  as.character(resolved_id)
}
