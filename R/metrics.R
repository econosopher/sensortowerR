.st_metric_unit <- function(metric, os, revenue_unit) {
  ifelse(metric == "revenue", if (revenue_unit == "dollars") "USD" else "USD_cents",
    ifelse(metric == "downloads", "downloads", ifelse(os == "android", "users", ifelse(os == "ios", "device_users", "platform_users")))
  )
}
.st_sales_value <- function(raw, os, metric) {
  if (os == "unified") {
    return(.st_numeric(.st_column(raw, if (metric == "revenue") "unified_revenue" else "unified_units")))
  }
  if (os == "android") {
    return(.st_numeric(.st_column(raw, if (metric == "revenue") "r" else "u")))
  }
  cols <- if (metric == "revenue") c("ir", "ar") else c("iu", "au")
  .st_numeric(.st_column(raw, cols[1])) + .st_numeric(.st_column(raw, cols[2]))
}
.st_parse_metrics <- function(raw, os, ids, countries, dates, metrics, granularity, revenue_unit, usage = FALSE) {
  raw <- .st_records(raw)
  if (!nrow(raw)) {
    return(.st_metric_ptype())
  }
  base <- tibble::tibble(
    app_id = .st_ids(.st_column(raw, c("app_id", "aid"), TRUE)), os = os,
    country = as.character(.st_column(raw, c("country", "cc", "c"), TRUE)), date = .st_response_date(.st_column(raw, c("date", "d"), TRUE))
  )
  if (anyNA(base)) .st_abort("Metric response has missing keys.", "st_schema_error")
  if (any(!base$app_id %in% ids)) .st_abort("Metric response contains unrequested app IDs.", "st_schema_error")
  out <- purrr::map(metrics, function(metric) {
    if (usage) {
      cols <- switch(os,
        android = "users",
        ios = c("iphone_users", "ipad_users"),
        unified = c("android_users", "iphone_users", "ipad_users")
      )
      # Strict addition: unknown components keep the total unknown, never zero.
      value <- Reduce(`+`, purrr::map(cols, function(col) .st_numeric(.st_column(raw, col))))
      period <- switch(metric,
        dau = "day",
        wau = "week",
        mau = "month"
      )
    } else {
      value <- .st_sales_value(raw, os, metric)
      if (metric == "revenue" && revenue_unit == "dollars") value <- value / 100
      period <- switch(granularity,
        daily = "day",
        weekly = "week",
        monthly = "month",
        quarterly = "quarter"
      )
    }
    dplyr::mutate(base, metric = metric, value = value, unit = .st_metric_unit(metric, os, revenue_unit), period = period)
  }) |> purrr::list_rbind()
  # Active-user endpoints round to native periods. Keep a period only when its
  # start is inside the requested interval; never prorate or sum audiences.
  out <- out |> dplyr::filter(.data$country %in% countries, .data$date >= dates$start_date, .data$date <= dates$end_date)
  .st_unique(out, c("app_id", "os", "country", "date", "metric"))
}
.st_metric_failure <- function(ids, os, countries, metrics, revenue_unit) {
  tidyr::expand_grid(app_id = ids, os = os, country = countries, metric = metrics) |>
    dplyr::mutate(date = as.Date(NA), value = NA_real_, unit = .st_metric_unit(.data$metric, .env$os, .env$revenue_unit), period = NA_character_) |>
    .st_schema(.st_metric_ptype())
}
.st_metric_fetch <- function(keys, metrics, countries, dates, granularity, revenue_unit, errors, token) {
  output <- list()
  groups <- split(keys, keys$os)
  for (platform in names(groups)) {
    ids <- groups[[platform]]$app_id
    chunks <- split(ids, ceiling(seq_along(ids) / 100L))
    sales <- intersect(metrics, c("revenue", "downloads"))
    families <- c(if (length(sales)) list(sales), as.list(intersect(metrics, c("dau", "wau", "mau"))))
    for (chunk in chunks) {
      for (family in families) {
        usage <- family[1] %in% c("dau", "wau", "mau")
        path <- paste(platform, if (usage) "usage/active_users" else "sales_report_estimates", sep = "/")
        params <- c(
          list(app_ids = chunk, countries = countries), dates,
          if (usage) {
            list(time_period = switch(family[1],
              dau = "day",
              wau = "week",
              mau = "month"
            ))
          } else {
            list(date_granularity = granularity)
          }
        )
        label <- paste0(path, " [", paste(chunk, collapse = ","), "]")
        output[[length(output) + 1L]] <- .st_attempt(function() {
          raw <- .st_get(path, params, token)
          .st_parse_metrics(raw, platform, chunk, countries, dates, family, granularity, revenue_unit, usage)
        }, function() .st_metric_failure(chunk, platform, countries, family, revenue_unit), errors, label)
      }
    }
  }
  if (!length(output)) {
    return(.st_metric_ptype())
  }
  purrr::list_rbind(output)
}
.st_shape_metrics <- function(data, metrics, shape) {
  if (shape == "long") {
    return(data)
  }
  measures <- intersect(c("value", "unit", "period", "status", "error", "endpoint"), names(data))
  out <- tidyr::pivot_wider(data, names_from = "metric", values_from = dplyr::all_of(measures), names_glue = "{metric}_{.value}")
  names(out) <- sub("_value$", "", names(out))
  for (m in metrics) {
    if (!m %in% names(out)) out[[m]] <- rep(NA_real_, nrow(out))
    for (field in setdiff(measures, "value")) {
      n <- paste(m, field, sep = "_")
      if (!n %in% names(out)) out[[n]] <- rep(NA_character_, nrow(out))
    }
  }
  out
}
#' Fetch sales and active-user time series in a pipeline
#'
#' @param data A data frame with character `app_id` and `os` columns, or a
#'   character ID vector. User columns are retained; collisions get `.input`.
#' @param metrics Metrics to retrieve: revenue, downloads, dau, wau, mau.
#' @param os Platform for character IDs. Table inputs carry their own platform.
#' @param countries Country codes, or WW alone. Defaults to WW.
#' @param date_from,date_to Inclusive dates, as Date or YYYY-MM-DD strings.
#' @param granularity Sales period: daily, weekly, monthly, quarterly.
#' @param revenue_unit Revenue in dollars (USD) or cents.
#' @param shape Long (default) or wide output.
#' @param errors Abort by default; partial returns explicit error rows and warns.
#' @param cache Use the session cache, disabled by default.
#' @param cache_ttl Cache lifetime in seconds, default 300.
#' @param auth_token API token; defaults to SENSORTOWER_AUTH_TOKEN.
#' @return An ungrouped tibble. Long output contains app_id, os, country, date,
#'   metric, value, unit and period. Wide output includes per-metric unit/period
#'   columns. Partial mode also includes status, error and endpoint.
#' @details DAU, WAU and MAU always use their native day, week and month windows;
#'   granularity controls sales only. Dates label period starts; periods starting
#'   outside the requested interval are excluded. Audiences are never summed
#'   over time. iOS totals are device users; unified totals are platform users,
#'   not deduplicated people. Missing components make totals unknown.
#'   Unified sales use the unified endpoint including regional SKUs. No fuzzy
#'   name matching or platform fallback occurs. Use st_app(target_os = ...) to
#'   convert identifiers explicitly. HTTP/schema failures are never no-data.
#' @export
st_metrics <- function(data, date_from, date_to, metrics = c("revenue", "downloads"), os = NULL,
                       countries = "WW", granularity = "daily", revenue_unit = c("dollars", "cents"),
                       shape = c("long", "wide"), errors = c("abort", "partial"), cache = FALSE,
                       cache_ttl = 300, auth_token = NULL) {
  input <- .st_inputs(data, os)
  dates <- .st_dates(date_from, date_to)
  metrics <- .st_names(metrics, c("revenue", "downloads", "dau", "wau", "mau"), "metrics")
  countries <- .st_countries(countries)
  granularity <- .st_granularity(granularity)
  revenue_unit <- match.arg(revenue_unit)
  shape <- match.arg(shape)
  errors <- match.arg(errors)
  .st_flag(cache, "cache")
  .st_int(cache_ttl, "cache_ttl")
  if (!nrow(input)) {
    out <- .st_metric_ptype()
    if (errors == "partial") {
      out$status <- character()
      out$error <- character()
      out$endpoint <- character()
    }
    return(.st_attach(input, .st_shape_metrics(out, metrics, shape)))
  }
  keys <- dplyr::distinct(input, .data$app_id, .data$os)
  token <- .st_token(auth_token)
  result <- .st_cached(
    list(keys, metrics, countries, dates, granularity, revenue_unit, errors), token, cache, cache_ttl,
    function() .st_metric_fetch(keys, metrics, countries, dates, granularity, revenue_unit, errors, token)
  )
  result <- result |>
    .st_partial_warning() |>
    dplyr::arrange(.data$app_id, .data$os, .data$date, .data$country, .data$metric)
  .st_attach(input, .st_shape_metrics(result, metrics, shape))
}
#' Fetch market sales totals
#' @param categories Game category IDs, as character strings.
#' @inheritParams st_metrics
#' @return A tibble like st_metrics(), with category_id instead of app_id.
#' @export
st_market_metrics <- function(categories, os, date_from, date_to, countries = "WW", granularity = "monthly",
                              metrics = c("revenue", "downloads"), revenue_unit = c("dollars", "cents"),
                              shape = c("long", "wide"), auth_token = NULL) {
  os <- .st_choice(os, c("ios", "android"), "os")
  dates <- .st_dates(date_from, date_to)
  countries <- .st_countries(countries)
  granularity <- .st_granularity(granularity)
  metrics <- .st_names(metrics, c("revenue", "downloads"), "metrics")
  revenue_unit <- match.arg(revenue_unit)
  shape <- match.arg(shape)
  if (!is.character(categories) || anyNA(categories) || any(!nzchar(categories))) .st_abort("`categories` must be character IDs.")
  out <- .st_metric_ptype()
  if (length(categories)) {
    raw <- .st_records(.st_get(paste0(os, "/games_breakdown"), c(list(categories = unique(categories), countries = countries, date_granularity = granularity), dates), auth_token))
    if (nrow(raw)) {
      raw$app_id <- as.character(.st_column(raw, c("ca", "category_id"), TRUE))
      out <- .st_parse_metrics(raw, os, categories, countries, dates, metrics, granularity, revenue_unit)
    }
  }
  out <- .st_shape_metrics(out, metrics, shape)
  dplyr::rename(out, category_id = "app_id")
}
