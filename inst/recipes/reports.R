# Source with source(system.file("recipes", "reports.R", package = "sensortowerR"))
# These examples operate on already-fetched tables and never call the API.
recipe_yoy <- function(data) {
  keys <- c("app_id", "os", "country", "date", "metric", "unit", "period")
  if (!all(c(keys, "value") %in% names(data))) stop("Use long st_metrics output.")
  if (anyDuplicated(data[keys])) stop("Resolve duplicate observations before calculating growth.")
  prior <- data |>
    dplyr::select(dplyr::all_of(c(keys, "value"))) |>
    dplyr::mutate(date = lubridate::`%m+%`(.data$date, lubridate::years(1))) |>
    dplyr::rename(prior_value = "value")
  dplyr::left_join(data, prior, by = keys, relationship = "one-to-one") |>
    dplyr::mutate(yoy_growth = dplyr::if_else(is.na(.data$prior_value) | .data$prior_value == 0,
      NA_real_, .data$value / .data$prior_value - 1))
}
recipe_portfolio <- function(data) {
  keys <- c("app_id", "os", "country", "date", "metric")
  if (any(!data$metric %in% c("revenue", "downloads"))) stop("Portfolio totals are for sales only; audiences overlap.")
  if (anyDuplicated(data[keys])) stop("Resolve duplicate observations before totaling a portfolio.")
  data |>
    dplyr::group_by(.data$os, .data$country, .data$date, .data$metric, .data$unit, .data$period) |>
    dplyr::summarise(value = if (anyNA(.data$value)) NA_real_ else sum(.data$value), apps = dplyr::n_distinct(.data$app_id), .groups = "drop")
}
recipe_plot <- function(data) {
  ggplot2::ggplot(data, ggplot2::aes(x = .data$date, y = .data$value, colour = .data$app_id)) +
    ggplot2::geom_line() + ggplot2::facet_grid(metric ~ country, scales = "free_y") +
    ggplot2::scale_y_continuous(labels = scales::label_number()) + ggplot2::theme_minimal()
}
recipe_dashboard <- function(data) {
  gt::gt(data) |> gt::fmt_number(columns = "value", decimals = 2)
}
