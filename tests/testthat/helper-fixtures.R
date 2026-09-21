# Every ordinary test uses synthetic responses. An unexpected request fails.
local_api <- function(handler, env = parent.frame()) {
  calls <- new.env(parent = emptyenv())
  calls$requests <- list()
  withr::local_envvar(c(SENSORTOWER_AUTH_TOKEN = "test-token"), .local_envir = env)
  testthat::local_mocked_bindings(.st_perform = function(req) {
    calls$requests[[length(calls$requests) + 1L]] <- req
    u <- httr2::url_parse(req$url)
    result <- handler(u$path, u$query, req)
    if (inherits(result, "httr2_response")) {
      return(result)
    }
    httr2::response(status_code = 200L, body = charToRaw(jsonlite::toJSON(result, auto_unbox = TRUE, null = "null", na = "null")), headers = list(`content-type` = "application/json"))
  }, .package = "sensortowerR", .env = env)
  calls
}
sales_rows <- function(ids = "a", country = "US", revenue = 12300, downloads = 25, date = "2026-01-01") {
  lapply(ids, function(id) list(app_id = id, country = country, date = date, unified_revenue = revenue, unified_units = downloads))
}
usage_rows <- function(ids = "a", country = "US", date = "2026-01-01", missing = FALSE) {
  lapply(ids, function(id) list(app_id = id, country = country, date = date, android_users = if (missing) NULL else 10, iphone_users = 20, ipad_users = 2))
}
metric_call <- function(data = "a", ..., os = "unified") st_metrics(data, "2026-01-01", "2026-01-31", os = os, countries = "US", ...)
empty_apps <- function() tibble::tibble(app_id = character(), os = character())
