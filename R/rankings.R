#' Rank apps or publishers by an explicit measure
#' @param measure revenue, downloads, dau, wau or mau. Publisher rankings support
#'   revenue and downloads only.
#' @param entity app or publisher.
#' @param category Category ID, default overall (0).
#' @param comparison absolute, delta or transformed_delta.
#' @param limit Maximum entities per country.
#' @param filter Created filter or server ID; supported for app rankings.
#' @inheritParams st_metrics
#' @return A tibble with entity ID, os, country, date, rank, measure, value and
#'   unit. Rankings are separate for each requested country.
#' @details Active rankings support weekly, monthly and quarterly periods; MAU
#'   does not support weekly. Active rankings cover one period beginning on
#'   date_from; date_to must be its last day. Sales may span multiple periods.
#' @export
st_rankings <- function(measure, os, date_from, date_to, countries = "WW", granularity = "monthly",
                        entity = c("app", "publisher"), category = "0", comparison = c("absolute", "delta", "transformed_delta"),
                        limit = 100, filter = NULL, auth_token = NULL) {
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
  if (entity == "publisher" && (active || !is.null(filter))) .st_abort("Publisher rankings support sales measures without custom filters.")
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
        return(tibble::tibble(!!id_col := character(), value = double()))
      }
      values <- .st_numeric(.st_column(x, c(paste0(if (active) "users" else api_measure, "_", comparison), comparison, paste0("current_", api_measure, "_value")), TRUE))
      if (measure == "revenue" && comparison != "transformed_delta") values <- values / 100
      tibble::tibble(!!id_col := .st_ids(.st_column(x, c(id_col, paste0("unified_", id_col)), TRUE)), value = values)
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
