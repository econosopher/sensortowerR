#' Fetch unified app platform components
#'
#' Retrieve provider-reported iPhone, iPad, Android and unified sales components
#' or weekly active-user components without treating missing values as zero.
#' Revenue is returned in USD cents; unit and user counts are counts.
#' @param data Character vector of unified app IDs.
#' @param date_from,date_to First and last date, inclusive.
#' @param type Either `sales` or `wau`.
#' @param granularity Sales period: daily, weekly, monthly or quarterly.
#' @param countries Country codes, or WW alone.
#' @inheritParams st_metrics
#' @return A tibble keyed by app_id, country and date, with platform component
#'   columns in provider units. Unreported components are NA.
#' @export
st_unified_breakdown <- function(data, date_from, date_to, type = c("sales", "wau"),
                                 granularity = "daily", countries = "WW", auth_token = NULL) {
  type <- match.arg(type)
  ids <- unique(.st_inputs(data, "unified")$app_id)
  dates <- .st_dates(date_from, date_to)
  countries <- .st_countries(countries)
  granularity <- .st_granularity(granularity)
  fields <- if (type == "sales") as.vector(outer(c("iphone", "ipad", "android", "unified"), c("revenue", "units"), paste, sep = "_")) else c("iphone_users", "ipad_users", "android_users")
  result <- lapply(split(ids, ceiling(seq_along(ids) / 100L)), function(chunk) {
    params <- c(list(app_ids = chunk, countries = countries), dates,
                if (type == "sales") list(date_granularity = granularity) else list(time_period = "week"))
    raw <- .st_records(.st_get(if (type == "sales") "unified/sales_report_estimates" else "unified/usage/active_users", params, auth_token))
    out <- tibble::tibble(app_id = .st_ids(.st_column(raw, "app_id", TRUE)),
                          country = as.character(.st_column(raw, "country", TRUE)),
                          date = .st_response_date(.st_column(raw, "date", TRUE)))
    if (anyNA(out) || any(!out$app_id %in% chunk)) .st_abort("Unified component response has invalid keys.", "st_schema_error")
    for (field in fields) out[[field]] <- .st_numeric(.st_column(raw, field))
    out
  })
  if (!length(result)) {
    out <- tibble::tibble(app_id = character(), country = character(), date = as.Date(character()))
    for (field in fields) out[[field]] <- double()
    return(out)
  }
  purrr::list_rbind(result)
}
