#' Rank apps or publishers by an explicit measure
#' @param measure revenue, downloads, dau, wau or mau. Publisher rankings support
#'   revenue and downloads only.
#' @param entity app or publisher.
#' @param category Category ID, default overall (0).
#' @param comparison absolute, delta or transformed_delta.
#' @param limit Maximum entities per country.
#' @param filter Created filter or server ID; supported for app rankings.
#' @param tags Optional custom tag names to return as character columns for app
#'   sales rankings, such as `c("Game Genre", "Game Sub-genre")`. Values come
#'   from the app's aggregate tags, else its first platform entity carrying the
#'   tag; missing tags are NA. One unfiltered ranking then splits by genre.
#' @param details Retain source downloads and nested aggregate and entity tags for unified app sales rankings.
#' @inheritParams st_metrics
#' @return A tibble with entity ID, os, country, date, rank, measure, value and
#'   unit, plus one column per requested tag. Rankings are separate for each
#'   requested country.
#' @details Active rankings support weekly, monthly and quarterly periods; MAU
#'   does not support weekly. Active rankings cover one period beginning on
#'   date_from; date_to must be its last day. Sales may span multiple periods.
#' @export
st_rankings <- function(measure, os, date_from, date_to, countries = "WW", granularity = "monthly",
                        entity = c("app", "publisher"), category = "0", comparison = c("absolute", "delta", "transformed_delta"),
                        limit = 100, filter = NULL, tags = NULL, details = FALSE, auth_token = NULL) {
  measure <- .st_choice(measure, c("revenue", "downloads", "dau", "wau", "mau"), "measure")
  entity <- match.arg(entity)
  comparison <- match.arg(comparison)
  os <- .st_os(os)
  dates <- .st_dates(date_from, date_to)
  countries <- .st_countries(countries)
  granularity <- .st_granularity(granularity)
  category <- .st_string(category, "category")
  limit <- .st_int(limit, "limit")
  filters <- .st_filter_ids(filter)
  active <- measure %in% c("dau", "wau", "mau")
  .st_flag(details, "details")
  if (details && (os != "unified" || entity != "app" || active || comparison != "absolute")) .st_abort("details requires unified app sales rankings with absolute comparison.")
  if (entity == "publisher" && (active || !is.null(filter))) .st_abort("Publisher rankings support sales measures without custom filters.")
  if (!is.null(tags)) {
    if (!is.character(tags) || !length(tags) || anyNA(tags) || any(!nzchar(tags))) .st_abort("tags must be non-empty tag names.")
    if (entity == "publisher" || active) .st_abort("tags are supported for app sales rankings only.")
  }
  time_range <- switch(granularity,
    daily = "day",
    weekly = "week",
    monthly = "month",
    quarterly = "quarter"
  )
  start <- as.Date(lubridate::floor_date(dates$start_date, unit = time_range, week_start = 1))
  if (start != dates$start_date) .st_abort("Ranking date_from must be the start of its period (weeks start Monday).")
  if (active) {
    if (granularity == "daily" || (measure == "mau" && granularity == "weekly")) .st_abort("Unsupported active-user ranking period.")
    last <- as.Date(lubridate::ceiling_date(start, unit = time_range, change_on_boundary = TRUE)) - 1L
    if (dates$end_date != last) .st_abort("Active rankings require exactly one full native ranking period.")
  }
  path <- paste0(os, if (entity == "publisher") "/top_and_trending/publishers" else if (active) "/top_and_trending/active_users" else "/sales_report_estimates_comparison_attributes")
  api_measure <- if (active) toupper(measure) else if (measure == "downloads") "units" else "revenue"
  id_col <- paste0(entity, "_id")
  out <- purrr::map(countries, function(country) {
    results <- purrr::map(if (is.null(filters)) list(NULL) else as.list(filters), function(filter_id) {
      params <- list(
        measure = api_measure, comparison_attribute = comparison, time_range = time_range, date = dates$start_date,
        end_date = if (!active) dates$end_date, category = category, device_type = if (os %in% c("ios", "unified")) "total",
        custom_fields_filter_id = filter_id, custom_tags_mode = if (os == "unified" && !is.null(filter_id)) "include_unified_apps"
      )
      params[[if (entity == "publisher") "country" else "regions"]] <- country
      x <- .st_pages(path, params, auth_token, limit, if (entity == "publisher") 10 else 100, c("data", "publishers"))
      if (!nrow(x)) {
        empty <- tibble::tibble(!!id_col := character(), value = double())
        for (tag in tags) empty[[tag]] <- character()
        if (details) { empty$units_absolute <- double(); empty$aggregate_tags <- list(); empty$entities <- list() }
        return(empty)
      }
      values <- .st_numeric(.st_column(x, c(paste0(if (active) "users" else api_measure, "_", comparison), comparison, paste0("current_", api_measure, "_value")), TRUE))
      if (measure == "revenue" && comparison != "transformed_delta") values <- values / 100
      res <- tibble::tibble(!!id_col := .st_ids(.st_column(x, c(id_col, paste0("unified_", id_col)), TRUE)), value = values)
      for (tag in tags) res[[tag]] <- .st_row_tags(x, tag)
      if (details) {
        res$units_absolute <- .st_numeric(.st_column(x, "units_absolute"))
        for (field in c("aggregate_tags", "entities"))
          res[[field]] <- if (field %in% names(x)) x[[field]] else rep(list(NULL), nrow(x))
      }
      res
    }) |> purrr::list_rbind()
    results <- .st_unique(results, id_col) |>
      dplyr::arrange(dplyr::desc(.data$value), .data[[id_col]]) |>
      dplyr::slice_head(n = limit)
    results |> dplyr::mutate(
      os = os, country = country, date = dates$start_date, rank = dplyr::row_number(), measure = measure,
      unit = if (comparison == "transformed_delta") "ratio" else .st_metric_unit(measure, os, "dollars"), period = time_range
    )
  }) |> purrr::list_rbind()
  out
}
#' Rank unified apps by ad monetization
#' @param measure ad_revenue, ad_impressions or ad_impression_share (share of
#'   ad impressions among publishers).
#' @param countries Country codes, or WW alone (default). WW sends no region
#'   filter and ranks over the provider's ad-monetization regions, labeled
#'   `covered_regions`; coverage is not worldwide.
#' @param networks Optional ad networks: Admob, Applovin, Digital Turbine,
#'   InMobi, Meta Audience Network, Mintegral, Supersonic, Unity, Other.
#' @param categories,game_categories Optional iOS primary category IDs (such as
#'   6014) and iOS game category IDs (such as 7012), applied to unified apps.
#' @param limit Maximum apps per country.
#' @param filter Created filter or server ID.
#' @inheritParams st_metrics
#' @return A tibble like st_rankings(): app_id (unified), os, country, date,
#'   rank, measure, value, unit and period. Revenue is USD, impressions use
#'   impressions and share uses fraction. date is date_from; values cover the
#'   whole date_from to date_to range, so period is range.
#' @details Rankings are separate for each requested country. Estimates start on
#'   2024-01-01. Multiple filter IDs are unioned before ranking.
#' @export
st_ad_rankings <- function(measure, date_from, date_to, countries = "WW", networks = NULL, categories = NULL,
                           game_categories = NULL, limit = 100, filter = NULL, auth_token = NULL) {
  measure <- .st_choice(measure, c("ad_revenue", "ad_impressions", "ad_impression_share"), "measure")
  dates <- .st_dates(date_from, date_to)
  countries <- .st_countries(countries)
  if (!is.null(networks)) {
    networks <- .st_names(networks, c("Admob", "Applovin", "Digital Turbine", "InMobi", "Meta Audience Network", "Mintegral", "Supersonic", "Unity", "Other"), "networks")
  }
  for (x in list(categories, game_categories)) if (!is.null(x) && (!is.character(x) || !length(x) || anyNA(x) || any(!nzchar(x)))) .st_abort("Category IDs must be non-empty character vectors.")
  limit <- .st_int(limit, "limit")
  filters <- .st_filter_ids(filter)
  metric <- switch(measure,
    ad_revenue = "est_ad_monetization_revenue",
    ad_impressions = "est_ad_monetization_impressions",
    ad_impression_share = "est_ad_monetization_impression_share"
  )
  unit <- switch(measure,
    ad_revenue = "USD",
    ad_impressions = "impressions",
    ad_impression_share = "fraction"
  )
  purrr::map(countries, function(country) {
    results <- purrr::map(if (is.null(filters)) list(NULL) else as.list(filters), function(filter_id) {
      params <- c(list(
        bundle = if (measure == "ad_impression_share") "mobile_ad_monetization_impression_share" else "mobile_ad_monetization",
        breakdown = "unified_app_id", order_by = paste0(metric, ":desc"), regions = if (country != "WW") country,
        networks = networks, unified_app_primary_categories = categories, unified_app_game_categories = game_categories,
        custom_fields_filter_id = filter_id
      ), dates)
      x <- .st_pages("facets/metrics", params, auth_token, limit, 250L, "data")
      if (!nrow(x)) {
        return(tibble::tibble(app_id = character(), value = double()))
      }
      values <- .st_numeric(.st_column(x, metric, TRUE))
      if (measure == "ad_revenue") values <- values / 100
      tibble::tibble(app_id = .st_ids(.st_column(x, "unified_app_id", TRUE)), value = values)
    }) |> purrr::list_rbind()
    results <- .st_unique(results, "app_id") |>
      dplyr::arrange(dplyr::desc(.data$value), .data$app_id) |>
      dplyr::slice_head(n = limit)
    results |> dplyr::mutate(
      os = "unified", country = if (country == "WW") "covered_regions" else country, date = dates$start_date,
      rank = dplyr::row_number(), measure = measure, unit = unit, period = "range"
    )
  }) |> purrr::list_rbind()
}
#' Retrieve store chart positions
#' @param category Store category ID.
#' @param chart_type Provider store-chart identifier, such as topfreeapplications.
#' @param date Chart date.
#' @param limit Maximum entries per country, up to the provider's returned chart.
#' @inheritParams st_metrics
#' @return A tibble with app_id, os, country, date, rank, category_id, chart_type.
#' @export
st_charts <- function(os, category, chart_type, date, countries = "US", limit = 100, auth_token = NULL) {
  os <- .st_choice(os, c("ios", "android"), "os")
  .st_string(category, "category")
  .st_string(chart_type, "chart_type")
  date <- .st_date(date, "date")
  countries <- .st_countries(countries)
  limit <- .st_int(limit, "limit")
  purrr::map(countries, function(country) {
    raw <- .st_get(paste0(os, "/ranking"), list(category = category, chart_type = chart_type, date = date, country = country), auth_token)
    if ("ranking" %in% names(raw)) {
      ids <- .st_ids(unlist(raw$ranking))
    } else if (!length(raw)) {
      ids <- character()
    } else {
      ids <- as.character(.st_column(.st_records(raw), "app_id", TRUE))
    }
    ids <- utils::head(ids, limit)
    tibble::tibble(
      app_id = ids, os = rep(os, length(ids)), country = rep(country, length(ids)), date = rep(date, length(ids)),
      rank = seq_along(ids), category_id = rep(category, length(ids)), chart_type = rep(chart_type, length(ids))
    )
  }) |> purrr::list_rbind()
}

# One tag value per ranking row: the row's aggregate_tags, else the first platform
# entity whose custom_tags carry it. Returns a character vector (NA when absent).
.st_row_tags <- function(x, tag) {
  pick <- function(t) {
    v <- if (is.list(t) || is.data.frame(t)) t[[tag]] else NULL
    if (is.null(v) || !length(v) || is.na(v[[1]]) || !nzchar(as.character(v[[1]]))) NA_character_ else as.character(v[[1]])
  }
  agg <- if ("aggregate_tags" %in% names(x)) x$aggregate_tags else vector("list", nrow(x))
  ent <- if ("entities" %in% names(x)) x$entities else vector("list", nrow(x))
  vapply(seq_len(nrow(x)), function(i) {
    a <- if (is.data.frame(agg)) agg[i, , drop = FALSE] else agg[[i]]
    v <- pick(a)
    if (!is.na(v)) return(v)
    e <- ent[[i]]
    rows <- if (is.data.frame(e)) lapply(seq_len(nrow(e)), function(k) e$custom_tags[[k]] %||% (if (is.data.frame(e$custom_tags)) e$custom_tags[k, , drop = FALSE])) else lapply(e, function(z) z$custom_tags)
    for (ct in rows) { v <- pick(ct); if (!is.na(v)) return(v) }
    NA_character_
  }, character(1))
}
