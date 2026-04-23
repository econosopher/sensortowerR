#' Fetch Retention Metrics from Sensor Tower's Facets API
#'
#' Retrieves retention curves from Sensor Tower's new facets-based retention
#' endpoint. This wrapper targets `"/v1/facets/metrics?facets=retention"` and
#' returns a tidy tibble with the live response columns preserved.
#'
#' @param app_ids Character vector of platform-specific app IDs. May contain iOS
#'   numeric IDs or Android bundle IDs. Supply exactly one of `app_ids` or
#'   `unified_app_ids`.
#' @param unified_app_ids Character vector of Sensor Tower unified app IDs.
#'   Supply exactly one of `app_ids` or `unified_app_ids`.
#' @param bundle Character string. Retention bundle to request:
#'   `"retention_daily"`, `"retention_weekly"`, or `"retention_monthly"`.
#' @param breakdown Character vector of breakdown fields. Supported combinations
#'   are the ones documented by Sensor Tower:
#'   `"date"`, `"app_id"`, `"unified_app_id"`, `"date,app_id"`,
#'   `"date,unified_app_id"`, `"unified_app_id,app_id"`, and
#'   `"date,unified_app_id,app_id"`.
#' @param start_date Start date in `YYYY-MM-DD` format or as `Date`.
#' @param end_date End date in `YYYY-MM-DD` format or as `Date`.
#' @param regions Optional character vector of region codes. When omitted,
#'   Sensor Tower returns worldwide estimates.
#' @param auth_token Optional. Character string. Your Sensor Tower API token.
#'   Defaults to environment variable `SENSORTOWER_AUTH_TOKEN`.
#' @param verbose Logical. If `TRUE`, prints the request URL with the auth token
#'   redacted.
#'
#' @return A [tibble][tibble::tibble] with one row per requested breakdown
#'   combination. The response preserves Sensor Tower's live metric column names
#'   such as `est_retention_d1`, `est_retention_d14`, `est_retention_w52`, or
#'   `est_retention_m12`.
#'
#' @details
#' Validated against the live Sensor Tower API on March 17, 2026. The production
#' response currently returns daily retention columns including
#' `est_retention_d14` and `est_retention_d365`.
#'
#' @examples
#' \dontrun{
#' retention <- st_retention_facets(
#'   app_ids = "553834731",
#'   bundle = "retention_daily",
#'   breakdown = c("date", "app_id"),
#'   start_date = "2025-01-01",
#'   end_date = "2025-01-31"
#' )
#' }
#'
#' @seealso [st_retention()] for the legacy retention endpoint,
#'   [st_facets_metrics()] for raw facets access
#'
#' @export
st_retention_facets <- function(app_ids = NULL,
                                unified_app_ids = NULL,
                                bundle = c("retention_daily", "retention_weekly", "retention_monthly"),
                                breakdown = c("date", "app_id"),
                                start_date,
                                end_date,
                                regions = NULL,
                                auth_token = NULL,
                                verbose = FALSE) {
  bundle <- match.arg(bundle)
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  if (is.na(start_date) || is.na(end_date)) {
    rlang::abort("`start_date` and `end_date` must be valid dates.")
  }

  if (end_date < start_date) {
    rlang::abort("`end_date` must be on or after `start_date`.")
  }

  has_app_ids <- !is.null(app_ids) && length(app_ids) > 0
  has_unified_ids <- !is.null(unified_app_ids) && length(unified_app_ids) > 0

  if ((has_app_ids + has_unified_ids) != 1) {
    rlang::abort("Exactly one of `app_ids` or `unified_app_ids` must be supplied.")
  }

  breakdown <- normalize_retention_facets_breakdown(breakdown)

  params <- list(
    facets = "retention",
    bundle = bundle,
    breakdown = breakdown,
    start_date = start_date,
    end_date = end_date,
    regions = regions,
    app_ids = app_ids,
    unified_app_ids = unified_app_ids
  )

  response <- st_facets_metrics(
    params = params,
    auth_token = auth_token,
    verbose = verbose
  )

  result <- coerce_facets_data_tibble(response)

  if (nrow(result) == 0) {
    return(result)
  }

  if ("date" %in% names(result)) {
    result$date <- as.Date(result$date)
  }

  id_cols <- intersect(c("app_id", "unified_app_id"), names(result))
  for (col in id_cols) {
    result[[col]] <- as.character(result[[col]])
  }

  retention_cols <- grep("^est_retention_", names(result), value = TRUE)
  for (col in retention_cols) {
    result[[col]] <- as.numeric(result[[col]])
  }

  sort_cols <- intersect(c("date", "unified_app_id", "app_id"), names(result))
  if (length(sort_cols) > 0) {
    order_index <- do.call(order, c(result[sort_cols], list(na.last = TRUE)))
    result <- result[order_index, , drop = FALSE]
  }

  tibble::as_tibble(result)
}

normalize_retention_facets_breakdown <- function(breakdown) {
  breakdown <- trimws(tolower(as.character(breakdown)))
  breakdown <- unique(breakdown[nzchar(breakdown)])

  valid_breakdowns <- c("date", "app_id", "unified_app_id")
  invalid_breakdowns <- setdiff(breakdown, valid_breakdowns)

  if (length(breakdown) == 0) {
    rlang::abort("`breakdown` must contain at least one supported field.")
  }

  if (length(invalid_breakdowns) > 0) {
    rlang::abort(
      sprintf(
        "Unsupported `breakdown` field(s): %s.",
        paste(invalid_breakdowns, collapse = ", ")
      )
    )
  }

  ordered_breakdown <- c("date", "unified_app_id", "app_id")
  breakdown <- ordered_breakdown[ordered_breakdown %in% breakdown]

  valid_combinations <- c(
    "date",
    "app_id",
    "unified_app_id",
    "date,app_id",
    "date,unified_app_id",
    "unified_app_id,app_id",
    "date,unified_app_id,app_id"
  )

  breakdown_key <- paste(breakdown, collapse = ",")
  if (!breakdown_key %in% valid_combinations) {
    rlang::abort(
      sprintf(
        "Unsupported `breakdown` combination: %s.",
        breakdown_key
      )
    )
  }

  breakdown
}

coerce_facets_data_tibble <- function(response) {
  if (inherits(response, "data.frame")) {
    return(tibble::as_tibble(response))
  }

  if (!is.list(response) || is.null(response$data)) {
    return(tibble::tibble())
  }

  data <- response$data

  if (inherits(data, "data.frame")) {
    return(tibble::as_tibble(data))
  }

  data_json <- jsonlite::toJSON(data, auto_unbox = TRUE, null = "null")
  tibble::as_tibble(jsonlite::fromJSON(data_json, flatten = TRUE))
}
