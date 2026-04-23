# Process-local cache for standardized st_metrics() results
.st_metrics_cache <- new.env(parent = emptyenv())

#' Fetch Sensor Tower Metrics
#'
#' Thin v1.0.0 facade for sales/download metrics. The function validates a
#' standardized argument set, dispatches to the legacy implementation that best
#' matches the request, and then normalizes the result into a stable schema.
#'
#' @param app_id Character scalar or vector of app identifiers. Each entry can be
#'   a Sensor Tower unified app ID, an iOS numeric app ID, or an Android package
#'   name.
#' @param metrics Character vector of metrics to return. Supported values are
#'   `"revenue"` and `"downloads"`.
#' @param os Character scalar. One of `"ios"`, `"android"`, or `"unified"`.
#' @param countries Character vector of 2-letter country codes. Use `"WW"` for
#'   worldwide aggregates.
#' @param date_from,date_to Date bounds for the query. Accept `Date` objects or
#'   ISO date strings.
#' @param granularity Character scalar. One of `"daily"`, `"weekly"`,
#'   `"monthly"`, or `"quarterly"`.
#' @param revenue_unit Character scalar. `"dollars"` (default) returns revenue in
#'   base currency units. `"cents"` returns revenue in cents for compatibility
#'   with the legacy API surface.
#' @param shape Character scalar. `"long"` returns one row per metric observation.
#'   `"wide"` returns one row per app/date/country with separate metric columns.
#' @param cache Logical. If `TRUE`, use a process-local cache keyed on the
#'   normalized arguments.
#' @param auth_token Optional Sensor Tower API token. If `NULL`, falls back to
#'   `SENSORTOWER_AUTH_TOKEN`.
#'
#' @return
#' If `shape = "long"`, a tibble with columns:
#'   - `app_id`: identifier supplied to `st_metrics()`
#'   - `os`: normalized operating system
#'   - `country`: 2-letter country code
#'   - `date`: observation date
#'   - `metric`: one of `"revenue"` or `"downloads"`
#'   - `value`: metric value; revenue is in dollars by default and cents when
#'     `revenue_unit = "cents"`
#'
#' If `shape = "wide"`, a tibble with columns:
#'   - `app_id`
#'   - `os`
#'   - `country`
#'   - `date`
#'   - one numeric column per requested metric
#'
#' @examples
#' \dontrun{
#' st_metrics(
#'   app_id = "553834731",
#'   os = "ios",
#'   countries = "US",
#'   date_from = Sys.Date() - 30,
#'   date_to = Sys.Date() - 1
#' )
#'
#' st_metrics(
#'   app_id = c("553834731", "com.supercell.clashofclans"),
#'   os = "unified",
#'   shape = "wide",
#'   countries = c("US", "GB"),
#'   revenue_unit = "cents"
#' )
#' }
#'
#' @export
st_metrics <- function(app_id,
                       metrics = c("revenue", "downloads"),
                       os = "unified",
                       countries = "WW",
                       date_from = Sys.Date() - 90,
                       date_to = Sys.Date(),
                       granularity = "daily",
                       revenue_unit = c("dollars", "cents"),
                       shape = c("long", "wide"),
                       cache = TRUE,
                       auth_token = NULL) {
  if (missing(app_id) || is.null(app_id) || !length(app_id)) {
    rlang::abort("`app_id` must be a non-empty character scalar or vector.")
  }

  app_id <- as.character(app_id)
  if (any(is.na(app_id) | !nzchar(app_id))) {
    rlang::abort("`app_id` entries must be non-empty strings.")
  }

  os <- normalize_os(os)
  countries <- normalize_countries(countries)
  dates <- normalize_dates(date_from, date_to)
  granularity <- normalize_granularity(granularity)
  metrics <- normalize_metrics(metrics, allowed = c("revenue", "downloads"))
  revenue_unit <- match.arg(revenue_unit)
  shape <- match.arg(shape)
  cache <- isTRUE(cache)
  auth_token <- get_auth_token(
    auth_token,
    error_message = paste(
      "Authentication token is required.",
      "Set SENSORTOWER_AUTH_TOKEN or pass `auth_token`."
    )
  )

  cache_key <- .st_metrics_cache_key(
    app_id = app_id,
    metrics = metrics,
    os = os,
    countries = countries,
    date_from = dates$date_from,
    date_to = dates$date_to,
    granularity = granularity,
    revenue_unit = revenue_unit,
    shape = shape
  )

  if (cache && exists(cache_key, envir = .st_metrics_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .st_metrics_cache, inherits = FALSE))
  }

  raw_result <- if (length(app_id) == 1L) {
    .st_metrics_dispatch_single(
      app_id = app_id,
      os = os,
      countries = countries,
      date_from = dates$date_from,
      date_to = dates$date_to,
      granularity = granularity,
      auth_token = auth_token
    )
  } else {
    st_batch_metrics_impl(
      os = os,
      app_list = app_id,
      metrics = metrics,
      date_range = list(
        start_date = dates$date_from,
        end_date = dates$date_to
      ),
      countries = countries,
      granularity = granularity,
      parallel = FALSE,
      verbose = FALSE,
      auth_token = auth_token
    )
  }

  normalized <- .st_metrics_normalize_output(
    data = raw_result,
    requested_ids = app_id,
    os = os,
    metrics = metrics,
    revenue_unit = revenue_unit,
    shape = shape
  )

  if (cache) {
    assign(cache_key, normalized, envir = .st_metrics_cache)
  }

  normalized
}

.st_metrics_cache_key <- function(app_id,
                                  metrics,
                                  os,
                                  countries,
                                  date_from,
                                  date_to,
                                  granularity,
                                  revenue_unit,
                                  shape) {
  key_payload <- list(
    app_id = unname(app_id),
    metrics = unname(metrics),
    os = os,
    countries = unname(countries),
    date_from = format(date_from, "%Y-%m-%d"),
    date_to = format(date_to, "%Y-%m-%d"),
    granularity = granularity,
    revenue_unit = revenue_unit,
    shape = shape
  )

  openssl::sha1(jsonlite::toJSON(key_payload, auto_unbox = TRUE, null = "null"))
}

.st_metrics_dispatch_single <- function(app_id,
                                        os,
                                        countries,
                                        date_from,
                                        date_to,
                                        granularity,
                                        auth_token) {
  resolved_id <- .st_metrics_resolve_single_id(
    app_id = app_id,
    os = os,
    auth_token = auth_token
  )

  if (os == "unified") {
    return(
      st_unified_sales_report_impl(
        unified_app_id = resolved_id,
        countries = countries,
        start_date = date_from,
        end_date = date_to,
        date_granularity = granularity,
        auth_token = auth_token,
        verbose = FALSE
      )
    )
  }

  args <- list(
    os = os,
    countries = countries,
    start_date = date_from,
    end_date = date_to,
    date_granularity = granularity,
    auth_token = auth_token,
    auto_segment = TRUE,
    verbose = FALSE
  )

  if (os == "ios") {
    args$ios_app_id <- resolved_id
  } else {
    args$android_app_id <- resolved_id
  }

  do.call(st_sales_report_impl, args)
}

.st_metrics_resolve_single_id <- function(app_id, os, auth_token) {
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
    rlang::abort(
      sprintf(
        "Failed to resolve `app_id = '%s'` for `os = '%s'`.",
        app_id,
        os
      )
    )
  }

  resolved_id <- switch(
    os,
    ios = resolved$ios_app_id,
    android = resolved$android_app_id,
    unified = resolved$unified_app_id
  )

  if (is.null(resolved_id) || is.na(resolved_id) || !nzchar(as.character(resolved_id))) {
    rlang::abort(
      sprintf(
        "Could not resolve a %s app ID for input '%s'.",
        os,
        app_id
      )
    )
  }

  as.character(resolved_id)
}

.st_metrics_normalize_output <- function(data,
                                         requested_ids,
                                         os,
                                         metrics,
                                         revenue_unit,
                                         shape) {
  if (is.null(data) || !nrow(data)) {
    return(
      if (shape == "long") {
        tibble::tibble(
          app_id = character(),
          os = character(),
          country = character(),
          date = as.Date(character()),
          metric = character(),
          value = numeric()
        )
      } else {
        wide <- tibble::tibble(
          app_id = character(),
          os = character(),
          country = character(),
          date = as.Date(character())
        )
        for (metric_name in metrics) {
          wide[[metric_name]] <- numeric()
        }
        wide
      }
    )
  }

  if (all(c("metric", "value") %in% names(data))) {
    normalized_long <- tibble::as_tibble(data) %>%
      dplyr::transmute(
        app_id = as.character(dplyr::coalesce(.data$original_id, .data$app_id)),
        os = os,
        country = as.character(.data$country),
        date = as.Date(.data$date),
        metric = tolower(as.character(.data$metric)),
        value = as.numeric(.data$value)
      ) %>%
      dplyr::filter(.data$metric %in% metrics)
  } else {
    normalized_wide <- .st_metrics_coerce_wide(
      data = tibble::as_tibble(data),
      requested_id = requested_ids[1],
      os = os
    )

    normalized_long <- normalized_wide %>%
      tidyr::pivot_longer(
        cols = dplyr::any_of(metrics),
        names_to = "metric",
        values_to = "value"
      )
  }

  if (revenue_unit == "cents") {
    revenue_rows <- normalized_long$metric == "revenue"
    normalized_long$value[revenue_rows] <- normalized_long$value[revenue_rows] * 100
  }

  normalized_long <- normalized_long %>%
    dplyr::arrange(.data$app_id, .data$date, .data$country, .data$metric)

  if (shape == "long") {
    return(normalized_long)
  }

  wide <- normalized_long %>%
    tidyr::pivot_wider(names_from = "metric", values_from = "value") %>%
    dplyr::select("app_id", "os", "country", "date", dplyr::any_of(metrics)) %>%
    dplyr::arrange(.data$app_id, .data$date, .data$country)

  wide
}

.st_metrics_coerce_wide <- function(data, requested_id, os) {
  revenue <- if ("revenue" %in% names(data)) {
    data$revenue
  } else if ("total_revenue" %in% names(data)) {
    data$total_revenue
  } else {
    rep(NA_real_, nrow(data))
  }

  downloads <- if ("downloads" %in% names(data)) {
    data$downloads
  } else if ("total_downloads" %in% names(data)) {
    data$total_downloads
  } else {
    rep(NA_real_, nrow(data))
  }

  tibble::tibble(
    app_id = rep(as.character(requested_id), nrow(data)),
    os = rep(os, nrow(data)),
    country = as.character(data$country),
    date = as.Date(data$date),
    revenue = as.numeric(revenue),
    downloads = as.numeric(downloads)
  )
}
