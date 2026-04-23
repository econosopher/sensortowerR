#' Defunct Functions In sensortowerR
#'
#' Legacy entrypoints retained only as defunct stubs for the v1.0.0 transition.
#'
#' @name sensortowerR-defunct
#' @keywords internal
NULL

#' @rdname sensortowerR-defunct
#' @export
st_sales_report <- function(...) {
  .Defunct(
    "st_metrics",
    package = "sensortowerR",
    msg = paste(
      "`st_sales_report()` is defunct.",
      "Use `st_metrics()` (note: revenue is now in dollars by default;",
      "pass `revenue_unit = \"cents\"` for the old behavior).",
      "See `?st_metrics`."
    )
  )
}

#' @rdname sensortowerR-defunct
#' @export
st_unified_sales_report <- function(...) {
  .Defunct(
    "st_metrics",
    package = "sensortowerR",
    msg = "`st_unified_sales_report()` is defunct. Use `st_metrics()` with the default `os = \"unified\"`."
  )
}

#' @rdname sensortowerR-defunct
#' @export
st_batch_metrics <- function(...) {
  .Defunct(
    "st_metrics",
    package = "sensortowerR",
    msg = "`st_batch_metrics()` is defunct. Use `st_metrics()` with a vector `app_id`."
  )
}

#' @rdname sensortowerR-defunct
#' @export
st_smart_metrics <- function(...) {
  .Defunct(
    "st_metrics",
    package = "sensortowerR",
    msg = "`st_smart_metrics()` is defunct. Use `st_metrics()`; caching is enabled by default."
  )
}

#' @rdname sensortowerR-defunct
#' @export
st_top_charts <- function(...) {
  .Defunct(
    "st_rankings",
    package = "sensortowerR",
    msg = "`st_top_charts()` is defunct. Use `st_rankings(entity = \"app\")`."
  )
}

#' @rdname sensortowerR-defunct
#' @export
st_top_publishers <- function(...) {
  .Defunct(
    "st_rankings",
    package = "sensortowerR",
    msg = "`st_top_publishers()` is defunct. Use `st_rankings(entity = \"publisher\")`."
  )
}

#' @rdname sensortowerR-defunct
#' @export
st_category_rankings <- function(...) {
  .Defunct(
    "st_rankings",
    package = "sensortowerR",
    msg = "`st_category_rankings()` is defunct. Use `st_rankings(entity = \"category\")`."
  )
}

#' @rdname sensortowerR-defunct
#' @export
st_app_info <- function(...) {
  .Defunct(
    "st_apps",
    package = "sensortowerR",
    msg = "`st_app_info()` is defunct. Use `st_apps(query = ...)`."
  )
}

#' @rdname sensortowerR-defunct
#' @export
st_app_lookup <- function(...) {
  .Defunct(
    "st_app",
    package = "sensortowerR",
    msg = "`st_app_lookup()` is defunct. Use `st_app(app_id = ...)`."
  )
}

#' @rdname sensortowerR-defunct
#' @export
st_app_details <- function(...) {
  .Defunct(
    "st_app",
    package = "sensortowerR",
    msg = "`st_app_details()` is defunct. Use `st_app(app_id = ...)`."
  )
}

#' @rdname sensortowerR-defunct
#' @export
st_filter_by_date <- function(...) {
  .Defunct(
    "st_filter",
    package = "sensortowerR",
    msg = "`st_filter_by_date()` is defunct. Use `st_filter(date_from = ..., date_to = ...)`."
  )
}

#' @rdname sensortowerR-defunct
#' @export
st_filter_by_genre <- function(...) {
  .Defunct(
    "st_filter",
    package = "sensortowerR",
    msg = "`st_filter_by_genre()` is defunct. Use `st_filter(genre = ...)`."
  )
}

#' @rdname sensortowerR-defunct
#' @export
st_filter_by_monetization <- function(...) {
  .Defunct(
    "st_filter",
    package = "sensortowerR",
    msg = "`st_filter_by_monetization()` is defunct. Use `st_filter(monetization = ...)`."
  )
}

#' @rdname sensortowerR-defunct
#' @export
st_filter_by_publisher <- function(...) {
  .Defunct(
    "st_filter",
    package = "sensortowerR",
    msg = "`st_filter_by_publisher()` is defunct. Use `st_filter(publisher = ...)`."
  )
}

#' @rdname sensortowerR-defunct
#' @export
st_filter_by_sdk <- function(...) {
  .Defunct(
    "st_filter",
    package = "sensortowerR",
    msg = "`st_filter_by_sdk()` is defunct. Use `st_filter(sdk = ...)`."
  )
}

#' @rdname sensortowerR-defunct
#' @export
st_custom_fields_filter <- function(...) {
  .Defunct(
    "st_filter",
    package = "sensortowerR",
    msg = "`st_custom_fields_filter()` is defunct. Use `st_filter(custom_fields = ...)`."
  )
}

#' @rdname sensortowerR-defunct
#' @export
st_custom_fields_filter_by_id <- function(...) {
  .Defunct(
    "st_filter",
    package = "sensortowerR",
    msg = "`st_custom_fields_filter_by_id()` is defunct. Use `st_filter(filter_id = ...)`."
  )
}

#' @rdname sensortowerR-defunct
#' @export
st_combine_filters <- function(...) {
  .Defunct(
    package = "sensortowerR",
    msg = "`st_combine_filters()` is defunct. Use `c(st_filter(...), st_filter(...))`."
  )
}

#' @rdname sensortowerR-defunct
#' @export
st_create_simple_filter <- function(...) {
  .Defunct(
    "st_filter",
    package = "sensortowerR",
    msg = "`st_create_simple_filter()` is defunct. Use `st_filter(...)`."
  )
}

#' @rdname sensortowerR-defunct
#' @export
st_compare_filter_results <- function(...) {
  .Defunct(
    package = "sensortowerR",
    msg = paste(
      "`st_compare_filter_results()` is defunct.",
      "There is no direct replacement; use `st_apps(filter = ...)` and compare results manually."
    )
  )
}

#' @rdname sensortowerR-defunct
#' @export
st_generate_example_filter_ids <- function(...) {
  .Defunct(
    package = "sensortowerR",
    msg = paste(
      "`st_generate_example_filter_ids()` is defunct.",
      "There is no direct replacement; it was a diagnostic helper."
    )
  )
}
