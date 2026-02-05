#' Fetch Active User Metrics with a Tidy Long Output
#'
#' Lightweight wrapper around [st_batch_metrics()] focused only on active-user
#' metrics (`dau`, `wau`, `mau`). Returns a tidy long-format tibble that is
#' designed for straightforward tidyverse chaining.
#'
#' @param os Character. Required. Operating system: `"ios"`, `"android"`, or
#'   `"unified"`.
#' @param app_list List or data frame containing app information. Same formats
#'   accepted by [st_batch_metrics()].
#' @param metrics Character vector. Active-user metrics to return. Must be one
#'   or more of `"dau"`, `"wau"`, `"mau"`. Default is all three.
#' @param date_range List with `start_date` and `end_date`.
#' @param countries Character vector of country codes.
#' @param granularity Character granularity (`"daily"`, `"weekly"`,
#'   `"monthly"`, `"quarterly"`). Default `"monthly"`.
#' @param parallel Logical. Whether to use parallel processing.
#' @param verbose Logical. Whether to print progress messages.
#' @param auth_token Character string. Sensor Tower API token. Defaults to
#'   `SENSORTOWER_AUTH_TOKEN` environment variable.
#' @param max_cores Integer. Maximum cores for parallel processing.
#'
#' @return A tibble with columns including `original_id`, `app_name`, `app_id`,
#'   `app_id_type`, `date`, `country`, `metric`, and `value`.
#' @export
st_active_users <- function(os,
                            app_list,
                            metrics = c("dau", "wau", "mau"),
                            date_range = list(
                              start_date = Sys.Date() - 90,
                              end_date = Sys.Date() - 1
                            ),
                            countries,
                            granularity = "monthly",
                            parallel = FALSE,
                            verbose = TRUE,
                            auth_token = NULL,
                            max_cores = 2) {
  if (missing(os) || is.null(os)) {
    rlang::abort("'os' parameter is required. Specify one of: 'ios', 'android', 'unified'.")
  }

  if (missing(app_list) || is.null(app_list)) {
    rlang::abort("'app_list' parameter is required.")
  }

  if (missing(countries) || is.null(countries) || length(countries) == 0) {
    rlang::abort("'countries' parameter is required.")
  }

  metrics <- tolower(as.character(metrics))
  valid_metrics <- c("dau", "wau", "mau")
  invalid_metrics <- setdiff(metrics, valid_metrics)
  if (length(invalid_metrics) > 0) {
    rlang::abort(
      sprintf(
        "Invalid metrics: %s. `metrics` must be one or more of: dau, wau, mau.",
        paste(invalid_metrics, collapse = ", ")
      )
    )
  }

  auth_token_val <- resolve_auth_token(
    auth_token,
    error_message = "Authentication token is required. Set SENSORTOWER_AUTH_TOKEN environment variable."
  )

  results <- st_batch_metrics(
    os = os,
    app_list = app_list,
    metrics = metrics,
    date_range = date_range,
    countries = countries,
    granularity = granularity,
    parallel = parallel,
    verbose = verbose,
    auth_token = auth_token_val,
    max_cores = max_cores
  )

  if (nrow(results) == 0) {
    return(results)
  }

  results %>%
    dplyr::filter(.data$metric %in% metrics) %>%
    dplyr::arrange(.data$original_id, .data$metric, .data$date, .data$country)
}
