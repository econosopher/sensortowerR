#' Fetch Rating Metrics from Sensor Tower's Facets API
#'
#' Retrieves rating metrics from Sensor Tower's new facets-based ratings
#' endpoint. This wrapper targets `"/v1/facets/metrics?facets=ratings"` and
#' returns a tidy tibble with the live response columns preserved.
#'
#' @param app_ids Character vector of iOS app IDs or Android package names.
#'   Sensor Tower documents a maximum of 1,000 IDs per request.
#' @param bundle Character string. Rating bundle to request:
#'   `"ratings_incremental"` or `"ratings_cumulative"`.
#' @param breakdown Breakdown fields or a comma-separated breakdown string.
#'   Supported combinations are `"app_id"`, `"app_id,date"`, `"region"`,
#'   `"region,date"`, and `"app_version"`.
#' @param start_date Start date in `YYYY-MM-DD` format or as `Date`.
#' @param end_date End date in `YYYY-MM-DD` format or as `Date`.
#' @param date_granularity Optional date granularity. Required when
#'   `breakdown` includes `date`.
#' @param regions Optional character vector of region codes.
#' @param android_localized_estimates Logical. Whether to apply Android country
#'   weighting. Defaults to `TRUE` to match the current documented default.
#' @param auth_token Optional. Character string. Your Sensor Tower API token.
#'   Defaults to environment variable `SENSORTOWER_AUTH_TOKEN`.
#' @param verbose Logical. If `TRUE`, prints the request URL with the auth token
#'   redacted.
#'
#' @return A [tibble][tibble::tibble] containing rating metrics such as
#'   `rating_average_incremental`, `rating_count_incremental`, or their
#'   cumulative counterparts.
#'
#' @details
#' Validated against the live Sensor Tower API on March 17, 2026.
#'
#' @examples
#' \dontrun{
#' ratings <- st_ratings_facets(
#'   app_ids = "553834731",
#'   bundle = "ratings_incremental",
#'   breakdown = c("app_id", "date"),
#'   start_date = "2024-01-01",
#'   end_date = "2024-01-07",
#'   date_granularity = "day"
#' )
#' }
#'
#' @seealso [st_facets_metrics()] for raw facets access
#'
#' @export
st_ratings_facets <- function(app_ids,
                              bundle = c("ratings_incremental", "ratings_cumulative"),
                              breakdown = c("app_id", "date"),
                              start_date,
                              end_date,
                              date_granularity = NULL,
                              regions = NULL,
                              android_localized_estimates = TRUE,
                              auth_token = NULL,
                              verbose = FALSE) {
  if (missing(app_ids) || is.null(app_ids) || length(app_ids) == 0) {
    rlang::abort("`app_ids` must contain at least one app ID.")
  }

  if (length(app_ids) > 1000) {
    rlang::abort("`app_ids` supports a maximum of 1,000 values per request.")
  }

  bundle <- match.arg(bundle)
  dates <- validate_facets_date_inputs(start_date, end_date)
  breakdown <- normalize_ratings_facets_breakdown(breakdown)
  date_granularity <- normalize_optional_facets_date_granularity(
    date_granularity = date_granularity,
    breakdown = breakdown
  )

  response <- st_facets_metrics(
    params = list(
      facets = "ratings",
      bundle = bundle,
      breakdown = breakdown,
      date_granularity = date_granularity,
      app_ids = app_ids,
      regions = regions,
      start_date = dates$start_date,
      end_date = dates$end_date,
      android_localized_estimates = android_localized_estimates
    ),
    auth_token = auth_token,
    verbose = verbose
  )

  result <- coerce_facets_data_tibble(response)

  if (nrow(result) == 0) {
    return(result)
  }

  result <- normalize_facets_result_columns(
    result = result,
    character_cols = c("app_id", "region", "app_version"),
    integer_cols = c(
      "rating_count_incremental",
      "rating_1_star_count_incremental",
      "rating_2_star_count_incremental",
      "rating_3_star_count_incremental",
      "rating_4_star_count_incremental",
      "rating_5_star_count_incremental",
      "rating_count_cumulative",
      "rating_1_star_count_cumulative",
      "rating_2_star_count_cumulative",
      "rating_3_star_count_cumulative",
      "rating_4_star_count_cumulative",
      "rating_5_star_count_cumulative"
    ),
    numeric_cols = c(
      "rating_average_incremental",
      "rating_average_cumulative"
    )
  )

  sort_facets_result(result, c("app_id", "region", "app_version", "date"))
}

#' Fetch Review Metrics by Rating from Sensor Tower's Facets API
#'
#' Retrieves review metrics broken down by star rating from Sensor Tower's new
#' facets-based review endpoint. This wrapper targets
#' `"/v1/facets/metrics?facets=reviews_by_rating"`.
#'
#' @param app_id Single iOS app ID or Android package name.
#' @param breakdown Breakdown fields or a comma-separated breakdown string.
#'   Supported combinations are `"review_rating"`, `"date,review_rating"`,
#'   `"region,review_rating"`, `"language,review_rating"`, and
#'   `"app_version,review_rating"`.
#' @param start_date Start date in `YYYY-MM-DD` format or as `Date`.
#' @param end_date End date in `YYYY-MM-DD` format or as `Date`.
#' @param date_granularity Optional date granularity. Required when
#'   `breakdown` includes `date`.
#' @param regions Optional character vector of iOS region codes.
#' @param languages Optional character vector of Android language codes.
#' @param review_keywords Optional character vector of review-keyword filters.
#' @param review_sentiments Optional character vector of sentiment filters.
#' @param review_tags Optional character vector of review-tag filters.
#' @param search_terms Optional character vector of content search terms.
#' @param rating_filters Optional character vector or integer vector of star
#'   filters (`1` through `5`).
#' @param auth_token Optional. Character string. Your Sensor Tower API token.
#'   Defaults to environment variable `SENSORTOWER_AUTH_TOKEN`.
#' @param verbose Logical. If `TRUE`, prints the request URL with the auth token
#'   redacted.
#'
#' @return A [tibble][tibble::tibble] containing fields such as
#'   `review_rating`, `review_rating_count`, `review_rating_percentage`, and
#'   `review_rating_average`.
#'
#' @details
#' Validated against the live Sensor Tower API on March 17, 2026.
#'
#' @examples
#' \dontrun{
#' reviews <- st_reviews_by_rating_facets(
#'   app_id = "553834731",
#'   breakdown = c("date", "review_rating"),
#'   start_date = "2024-01-01",
#'   end_date = "2024-01-07",
#'   date_granularity = "day",
#'   regions = "US"
#' )
#' }
#'
#' @seealso [st_facets_metrics()] for raw facets access
#'
#' @export
st_reviews_by_rating_facets <- function(app_id,
                                        breakdown = c("date", "review_rating"),
                                        start_date,
                                        end_date,
                                        date_granularity = NULL,
                                        regions = NULL,
                                        languages = NULL,
                                        review_keywords = NULL,
                                        review_sentiments = NULL,
                                        review_tags = NULL,
                                        search_terms = NULL,
                                        rating_filters = NULL,
                                        auth_token = NULL,
                                        verbose = FALSE) {
  if (missing(app_id) || is.null(app_id) || length(app_id) != 1 || !nzchar(trimws(as.character(app_id)))) {
    rlang::abort("`app_id` must be a single non-empty app ID.")
  }

  dates <- validate_facets_date_inputs(start_date, end_date)
  breakdown <- normalize_reviews_by_rating_facets_breakdown(breakdown)
  date_granularity <- normalize_optional_facets_date_granularity(
    date_granularity = date_granularity,
    breakdown = breakdown
  )

  response <- st_facets_metrics(
    params = list(
      facets = "reviews_by_rating",
      bundle = "reviews_by_rating",
      breakdown = breakdown,
      date_granularity = date_granularity,
      app_ids = app_id,
      regions = regions,
      languages = languages,
      start_date = dates$start_date,
      end_date = dates$end_date,
      review_keywords = review_keywords,
      review_sentiments = review_sentiments,
      review_tags = review_tags,
      search_terms = search_terms,
      rating_filters = rating_filters
    ),
    auth_token = auth_token,
    verbose = verbose
  )

  result <- coerce_facets_data_tibble(response)

  if (nrow(result) == 0) {
    return(result)
  }

  result <- normalize_facets_result_columns(
    result = result,
    character_cols = c("region", "language", "app_version"),
    integer_cols = c("review_rating", "review_rating_count"),
    numeric_cols = c("review_rating_percentage", "review_rating_average")
  )

  sort_facets_result(
    result,
    c("date", "region", "language", "app_version", "review_rating")
  )
}

validate_facets_date_inputs <- function(start_date, end_date) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  if (is.na(start_date) || is.na(end_date)) {
    rlang::abort("`start_date` and `end_date` must be valid dates.")
  }

  if (end_date < start_date) {
    rlang::abort("`end_date` must be on or after `start_date`.")
  }

  list(start_date = start_date, end_date = end_date)
}

parse_facets_breakdown_input <- function(breakdown) {
  if (is.null(breakdown)) {
    return(character())
  }

  breakdown <- as.character(breakdown)

  if (length(breakdown) == 1 && grepl(",", breakdown, fixed = TRUE)) {
    breakdown <- strsplit(breakdown, ",", fixed = TRUE)[[1]]
  }

  breakdown <- trimws(tolower(breakdown))
  unique(breakdown[nzchar(breakdown)])
}

normalize_optional_facets_date_granularity <- function(date_granularity, breakdown) {
  has_date_breakdown <- "date" %in% breakdown

  if (has_date_breakdown && (is.null(date_granularity) || !nzchar(trimws(as.character(date_granularity[1]))))) {
    rlang::abort("`date_granularity` is required when `breakdown` includes `date`.")
  }

  if (is.null(date_granularity) || !nzchar(trimws(as.character(date_granularity[1])))) {
    return(NULL)
  }

  match.arg(as.character(date_granularity[1]), c("day", "week", "month"))
}

normalize_ratings_facets_breakdown <- function(breakdown) {
  breakdown <- parse_facets_breakdown_input(breakdown)

  if (identical(sort(breakdown), "app_id")) {
    return("app_id")
  }

  if (identical(sort(breakdown), c("app_id", "date"))) {
    return(c("app_id", "date"))
  }

  if (identical(sort(breakdown), "region")) {
    return("region")
  }

  if (identical(sort(breakdown), c("date", "region"))) {
    return(c("region", "date"))
  }

  if (identical(sort(breakdown), "app_version")) {
    return("app_version")
  }

  breakdown_key <- paste(breakdown, collapse = ",")
  rlang::abort(
    sprintf(
      "Unsupported `breakdown` for ratings facets: %s.",
      if (nzchar(breakdown_key)) breakdown_key else "<blank>"
    )
  )
}

normalize_reviews_by_rating_facets_breakdown <- function(breakdown) {
  breakdown <- parse_facets_breakdown_input(breakdown)

  if (identical(sort(breakdown), "review_rating")) {
    return("review_rating")
  }

  if (identical(sort(breakdown), c("date", "review_rating"))) {
    return(c("date", "review_rating"))
  }

  if (identical(sort(breakdown), c("region", "review_rating"))) {
    return(c("region", "review_rating"))
  }

  if (identical(sort(breakdown), c("language", "review_rating"))) {
    return(c("language", "review_rating"))
  }

  if (identical(sort(breakdown), c("app_version", "review_rating"))) {
    return(c("app_version", "review_rating"))
  }

  breakdown_key <- paste(breakdown, collapse = ",")
  rlang::abort(
    sprintf(
      "Unsupported `breakdown` for reviews-by-rating facets: %s.",
      if (nzchar(breakdown_key)) breakdown_key else "<blank>"
    )
  )
}

normalize_facets_result_columns <- function(result,
                                            character_cols = character(),
                                            integer_cols = character(),
                                            numeric_cols = character()) {
  if ("date" %in% names(result)) {
    result$date <- as.Date(result$date)
  }

  for (col in intersect(character_cols, names(result))) {
    result[[col]] <- as.character(result[[col]])
  }

  for (col in intersect(integer_cols, names(result))) {
    result[[col]] <- as.integer(result[[col]])
  }

  for (col in intersect(numeric_cols, names(result))) {
    result[[col]] <- as.numeric(result[[col]])
  }

  tibble::as_tibble(result)
}

sort_facets_result <- function(result, sort_cols) {
  sort_cols <- intersect(sort_cols, names(result))

  if (length(sort_cols) == 0) {
    return(tibble::as_tibble(result))
  }

  order_index <- do.call(order, c(result[sort_cols], list(na.last = TRUE)))
  tibble::as_tibble(result[order_index, , drop = FALSE])
}
