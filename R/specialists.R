.st_country_results <- function(countries, id, os, errors, worker) {
  purrr::map(countries, function(country) {
    .st_attempt(function() worker(country), function() {
      tibble::tibble(app_id = id, os = os, country = country, date = as.Date(NA),
        metric = NA_character_, value = NA_real_, unit = NA_character_, period = NA_character_)
    }, errors, paste0("specialist [", id, ", ", country, "]"))
  })
}
.st_special <- function(input, worker, prototype, errors, endpoint) {
  keys <- dplyr::distinct(input, .data$app_id, .data$os)
  out <- purrr::map(seq_len(nrow(keys)), function(i) {
    id <- keys$app_id[i]
    os <- keys$os[i]
    .st_attempt(function() {
      x <- worker(id, os)
      if (!"app_id" %in% names(x)) x$app_id <- rep(id, nrow(x))
      if (nrow(x) && any(is.na(x$app_id) | as.character(x$app_id) != id)) .st_abort("Specialist response contains an unrequested ID.", "st_schema_error")
      x$os <- rep(os, nrow(x))
      .st_schema(x, prototype)
    }, function() {
      x <- vctrs::vec_init(prototype, 1L)
      x$app_id <- id
      x$os <- os
      x
    }, errors, paste0(endpoint, " [", id, "]"))
  }) |> purrr::list_rbind()
  out <- .st_schema(out, prototype)
  if (errors == "partial" && !"status" %in% names(out)) {
    out$status <- character()
    out$error <- character()
    out$endpoint <- character()
  }
  .st_attach(input, .st_partial_warning(out))
}
.st_facet_long <- function(raw, id, os, country, pattern, unit) {
  x <- .st_unwrap(raw, "data")
  if (!nrow(x)) {
    return(.st_metric_ptype())
  }
  id_field <- if (os == "unified") "unified_app_id" else "app_id"
  if (id_field %in% names(x)) {
    if (any(as.character(x[[id_field]]) != id, na.rm = TRUE)) .st_abort("Facets returned an unrequested ID.", "st_schema_error")
    if (id_field != "app_id") x[[id_field]] <- NULL
  }
  x$app_id <- rep(id, nrow(x))
  x$os <- rep(os, nrow(x))
  x$country <- as.character(.st_column(x, c("region", "country")))
  x$country[is.na(x$country)] <- country
  x$date <- .st_response_date(.st_column(x, "date"))
  cols <- grep(pattern, names(x), value = TRUE)
  if (!length(cols)) .st_abort("Facets response lacks expected metric fields.", "st_schema_error")
  for (n in cols) x[[n]] <- .st_numeric(x[[n]])
  x <- tidyr::pivot_longer(x, cols = dplyr::all_of(cols), names_to = "metric", values_to = "value")
  x$unit <- if (is.function(unit)) unit(x$metric) else rep(unit, nrow(x))
  x$period <- rep(NA_character_, nrow(x))
  .st_schema(x, .st_metric_ptype())
}
#' Low-level facets access
#' @param params Named query-parameter list. Vectors become comma-separated values.
#' @inheritParams st_metrics
#' @return A tibble of response records, preserving nested values as list-columns.
#'   Specialist wrappers provide typed metric schemas; this advanced interface
#'   intentionally preserves the provider's fields.
#' @export
st_facets <- function(params, auth_token = NULL) {
  if (!is.list(params) || !length(params) || is.null(names(params)) || any(!nzchar(names(params))) || anyDuplicated(names(params))) .st_abort("`params` must be a uniquely named non-empty list.")
  if (any(tolower(names(params)) %in% c("auth_token", "authorization"))) .st_abort("Pass credentials only through auth_token.")
  .st_unwrap(.st_get("facets/metrics", params, auth_token), "data")
}
#' Fetch retention curves
#' @inheritParams st_metrics
#' @param method facets (default) or legacy. Legacy requires store IDs.
#' @param bundle retention_daily, retention_weekly or retention_monthly.
#' @param granularity Legacy aggregation: all_time or quarterly.
#' @return A long tibble. Facets metrics retain provider horizon names; legacy
#'   curves include horizon (days since install). Retention units are fractions.
#' @export
st_retention <- function(data, date_from, date_to, os = NULL, countries = "WW", method = c("facets", "legacy"),
                         bundle = c("retention_daily", "retention_weekly", "retention_monthly"), granularity = "quarterly",
                         errors = c("abort", "partial"), auth_token = NULL) {
  input <- .st_inputs(data, os)
  dates <- .st_dates(date_from, date_to)
  countries <- .st_countries(countries)
  method <- match.arg(method)
  bundle <- match.arg(bundle)
  errors <- match.arg(errors)
  if (method == "legacy") {
    .st_choice(granularity, c("all_time", "quarterly"), "granularity")
    if (any(input$os == "unified")) .st_abort("Legacy retention needs store IDs; use st_app(target_os = 'ios' or 'android').")
  }
  proto <- .st_metric_ptype()
  proto$horizon <- double()
  .st_special(input, function(id, platform) {
    .st_country_results(countries, id, platform, errors, function(country) {
      if (method == "facets") {
        id_col <- if (platform == "unified") "unified_app_id" else "app_id"
        params <- c(list(facets = "retention", bundle = bundle, breakdown = c("date", id_col), regions = if (country != "WW") country), dates)
        params[[paste0(id_col, "s")]] <- id
        x <- .st_facet_long(st_facets(params, auth_token), id, platform, country, "^est_retention_", "fraction")
        x$horizon <- suppressWarnings(as.double(sub("^est_retention_[dwm]", "", x$metric)))
        x$period <- ifelse(grepl("_d", x$metric), "day", ifelse(grepl("_w", x$metric), "week", "month"))
        return(x)
      }
      raw <- .st_get(paste0(platform, "/usage/retention"), c(list(app_ids = id, country = country, date_granularity = granularity), dates), auth_token)
      if (is.list(raw) && is.null(names(raw)) && length(raw) == 1L && "app_data" %in% names(raw[[1]])) raw <- raw[[1]]
      x <- .st_records(raw, "app_data")
      if (!nrow(x)) {
        return(proto)
      }
      if (!"corrected_retention" %in% names(x)) .st_abort("Missing retention curve.", "st_schema_error")
      purrr::map(seq_len(nrow(x)), function(i) {
        values <- .st_numeric(unlist(x$corrected_retention[[i]]))
        tibble::tibble(
          app_id = as.character(x$app_id[i]), os = platform, country = country, date = .st_response_date(x$date[i]),
          metric = "retention", value = values, unit = "fraction", period = "day", horizon = as.double(seq_along(values))
        )
      }) |> purrr::list_rbind()
    }) |> purrr::list_rbind()
  }, proto, errors, paste0("retention/", method))
}
#' Fetch demographic estimates without averaging platforms
#' @inheritParams st_metrics
#' @param granularity all_time or quarterly.
#' @return A long tibble retaining source demographic names. Provider shares are
#'   labeled provider_share (no implicit rescaling); average ages use years.
#'   Store IDs are required; use st_app(target_os = ...) to expand unified IDs.
#' @export
st_demographics <- function(data, date_from, date_to, os = NULL, countries = "WW", granularity = "quarterly",
                            errors = c("abort", "partial"), auth_token = NULL) {
  input <- .st_inputs(data, os)
  dates <- .st_dates(date_from, date_to)
  countries <- .st_countries(countries)
  .st_choice(granularity, c("all_time", "quarterly"), "granularity")
  errors <- match.arg(errors)
  if (any(input$os == "unified")) .st_abort("Demographics needs store IDs; use st_app(target_os = 'ios' or 'android').")
  .st_special(input, function(id, platform) {
    .st_country_results(countries, id, platform, errors, function(country) {
      raw <- .st_get(paste0(platform, "/usage/demographics"), c(list(app_ids = id, country = country, date_granularity = granularity), dates), auth_token)
      x <- .st_records(raw, "app_data")
      for (col in intersect(c("grouped_normalized_demographics", "normalized_demographics"), names(x))) x <- tidyr::unnest_wider(x, dplyr::all_of(col), names_sep = ".")
      .st_facet_long(
        x, id, platform, country, "^(female|male|average_age|grouped_normalized_demographics|normalized_demographics)",
        function(metric) ifelse(grepl("average_age", metric), "years", "provider_share")
      )
    }) |> purrr::list_rbind()
  }, .st_metric_ptype(), errors, "usage/demographics")
}
#' Fetch session time series
#' @inheritParams st_metrics
#' @param metrics Session metrics: session_count, session_duration, time_spent,
#'   total_session_count, total_time_spent.
#' @param time_period Native averaging window: day, week or month.
#' @return A long tibble with explicit seconds, sessions and per-user units; period records the averaging window.
#' @export
st_sessions <- function(data, date_from, date_to, os = NULL, countries = "WW", granularity = "monthly", time_period = "week",
                        metrics = c("session_count", "session_duration", "time_spent"), errors = c("abort", "partial"), auth_token = NULL) {
  input <- .st_inputs(data, os)
  dates <- .st_dates(date_from, date_to)
  countries <- .st_countries(countries)
  granularity <- .st_granularity(granularity)
  .st_choice(time_period, c("day", "week", "month"), "time_period")
  metrics <- .st_names(metrics, c("session_count", "session_duration", "time_spent", "total_session_count", "total_time_spent"), "metrics")
  errors <- match.arg(errors)
  .st_special(input, function(id, platform) {
    .st_country_results(countries, id, platform, errors, function(country) {
      path <- if (platform == "unified") "apps/timeseries/unified_apps" else "apps/timeseries"
      raw <- .st_get(path, c(list(
        app_ids = id, regions = country, timeseries = metrics, time_period = time_period,
        date_granularity = granularity, breakdown = if (platform == "unified") "unified_app_id" else "app_id"
      ), dates), auth_token)
      apps <- .st_unwrap(raw, c("unified_apps", "apps"))
      if (!nrow(apps)) {
        return(.st_metric_ptype())
      }
      if (!"timeseries" %in% names(apps)) .st_abort("Missing session timeseries.", "st_schema_error")
      ids <- as.character(.st_column(apps, c("unified_app_id", "app_id"), TRUE))
      if (anyNA(ids) || any(ids != id)) .st_abort("Session response contains an unrequested ID.", "st_schema_error")
      x <- purrr::map(apps$timeseries, .st_records) |> purrr::list_rbind()
      if (!nrow(x)) {
        return(.st_metric_ptype())
      }
      for (n in metrics) if (!n %in% names(x)) x[[n]] <- rep(NA_real_, nrow(x))
      for (n in metrics) x[[n]] <- .st_numeric(x[[n]])
      x <- tidyr::pivot_longer(x, cols = dplyr::all_of(metrics), names_to = "metric", values_to = "value")
      x$app_id <- id
      x$os <- platform
      x$country <- country
      x$date <- .st_response_date(.st_column(x, "date", TRUE))
      x$unit <- ifelse(x$metric == "session_count", "sessions_per_user", ifelse(x$metric == "total_session_count", "sessions", ifelse(x$metric == "time_spent", "seconds_per_user", ifelse(x$metric == "session_duration", "seconds_per_session", "seconds"))))
      x$period <- time_period
      x
    }) |> purrr::list_rbind()
  }, .st_metric_ptype(), errors, "apps/timeseries")
}
#' Fetch rating and review metrics
#' @inheritParams st_metrics
#' @param bundle ratings_incremental or ratings_cumulative.
#' @param breakdown Provider breakdown. Ratings: app_id, app_id/date, region,
#'   region/date, app_version. Reviews: review_rating alone or with date, region,
#'   language or app_version. Each app is requested separately.
#' @param languages,review_keywords,review_sentiments,review_tags,search_terms Optional review filters.
#' @param rating_filters Optional star ratings (1 through 5).
#' @return A long tibble retaining breakdown columns. Counts use count, averages
#'   use stars and provider percentages use provider_percentage.
#' @export
st_ratings <- function(data, date_from, date_to, os = NULL, countries = "WW", granularity = "daily",
                       bundle = c("ratings_incremental", "ratings_cumulative"), breakdown = c("app_id", "date"),
                       errors = c("abort", "partial"), auth_token = NULL) {
  .st_rating_review(
    data, os, date_from, date_to, countries, granularity, "ratings", match.arg(bundle), breakdown,
    match.arg(errors), auth_token, list()
  )
}
#' @rdname st_ratings
#' @export
st_reviews <- function(data, date_from, date_to, os = NULL, countries = "WW", granularity = "daily",
                       breakdown = c("date", "review_rating"), languages = NULL, review_keywords = NULL, review_sentiments = NULL,
                       review_tags = NULL, search_terms = NULL, rating_filters = NULL, errors = c("abort", "partial"), auth_token = NULL) {
  if (!is.null(rating_filters) && (anyNA(rating_filters) || any(!rating_filters %in% 1:5))) .st_abort("rating_filters must contain stars 1 through 5.")
  .st_rating_review(
    data, os, date_from, date_to, countries, granularity, "reviews_by_rating", "reviews_by_rating", breakdown,
    match.arg(errors), auth_token, list(
      languages = languages, review_keywords = review_keywords, review_sentiments = review_sentiments,
      review_tags = review_tags, search_terms = search_terms, rating_filters = rating_filters
    )
  )
}
.st_rating_review <- function(data, os, date_from, date_to, countries, granularity, facet, bundle, breakdown, errors, auth_token, filters) {
  input <- .st_inputs(data, os)
  dates <- .st_dates(date_from, date_to)
  countries <- .st_countries(countries)
  .st_choice(granularity, c("daily", "weekly", "monthly"), "granularity")
  if (any(input$os == "unified")) .st_abort("Ratings and reviews require store IDs. Use st_app(target_os = ...).")
  valid <- if (facet == "ratings") {
    c("app_id", "app_id,date", "region", "date,region", "app_version")
  } else {
    c("review_rating", "date,review_rating", "region,review_rating", "language,review_rating", "app_version,review_rating")
  }
  if (!is.character(breakdown) || anyNA(breakdown) || !paste(sort(unique(breakdown)), collapse = ",") %in% valid) .st_abort("Unsupported facets breakdown.")
  if (!is.null(filters$languages) && any(input$os == "ios")) .st_abort("languages is supported for Android reviews only.")
  if (facet == "reviews_by_rating" && any(input$os == "android") && !identical(countries, "WW")) .st_abort("Android review facets use languages, not country filtering.")
  proto <- .st_metric_ptype()
  for (n in setdiff(breakdown, c("app_id", "date", "region"))) proto[[n]] <- if (n == "review_rating") double() else character()
  .st_special(input, function(id, platform) {
    .st_country_results(countries, id, platform, errors, function(country) {
      params <- c(list(
        facets = facet, bundle = bundle, breakdown = breakdown, app_ids = id,
        regions = if (country != "WW") country, date_granularity = if ("date" %in% breakdown) {
          switch(granularity,
            daily = "day",
            weekly = "week",
            monthly = "month"
          )
        }
      ), dates, filters)
      if (facet == "ratings") params$android_localized_estimates <- TRUE
      raw <- st_facets(params, auth_token)
      pattern <- if (facet == "ratings") "^rating_" else "^review_rating_(count|percentage|average)$"
      .st_facet_long(
        raw, id, platform, country, pattern,
        function(metric) ifelse(grepl("average", metric), "stars", ifelse(grepl("percentage", metric), "provider_percentage", "count"))
      )
    }) |> purrr::list_rbind()
  }, proto, errors, paste0("facets/", facet))
}
#' Fetch app tags
#' @inheritParams st_metrics
#' @param fields Custom/global field names to retrieve. Supply fields or field_categories.
#' @param field_categories Provider field categories to retrieve.
#' @return A tibble with app_id, os and a tags list-column, retaining raw fields.
#' @export
st_app_tags <- function(data, os = NULL, fields = NULL, field_categories = NULL, errors = c("abort", "partial"), auth_token = NULL) {
  input <- .st_inputs(data, os)
  errors <- match.arg(errors)
  if (any(input$os == "unified")) .st_abort("App tags require store IDs. Use st_app(target_os = ...).")
  if (is.null(fields) && is.null(field_categories)) .st_abort("Request fields or field_categories explicitly.")
  for (x in list(fields, field_categories)) if (!is.null(x) && (!is.character(x) || !length(x) || anyNA(x) || any(!nzchar(x)))) .st_abort("Tag fields and categories must be non-empty character vectors.")
  proto <- tibble::tibble(app_id = character(), os = character(), tags = list())
  .st_special(input, function(id, platform) {
    .st_unwrap(.st_get("app_tag/tags_for_apps", list(app_ids = id, `fields[]` = fields, `field_categories[]` = field_categories), auth_token), c("apps", "data"))
  }, proto, errors, "app_tag/tags_for_apps")
}
