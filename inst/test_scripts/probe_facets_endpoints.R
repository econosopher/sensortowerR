suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(tibble)
  library(dplyr)
})

resolve_token <- function() {
  token <- Sys.getenv("SENSORTOWER_AUTH_TOKEN")

  if (!nzchar(token)) {
    stop("Set SENSORTOWER_AUTH_TOKEN before running this script.", call. = FALSE)
  }

  token
}

probe_endpoint <- function(label, request) {
  response <- httr2::request(request$url) %>%
    httr2::req_headers(!!!request$headers) %>%
    httr2::req_timeout(30) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  body <- tryCatch(httr2::resp_body_string(response), error = function(...) "")
  parsed <- tryCatch(jsonlite::fromJSON(body, simplifyVector = FALSE), error = function(...) NULL)

  tibble::tibble(
    label = label,
    status = httr2::resp_status(response),
    body_preview = substr(gsub("[[:space:]]+", " ", body), 1, 180),
    top_level_keys = if (is.list(parsed)) paste(names(parsed), collapse = ", ") else NA_character_
  )
}

token <- resolve_token()

legacy_retention_url <- paste0(
  "https://api.sensortower.com/v1/ios/usage/retention",
  "?auth_token=", utils::URLencode(token, reserved = TRUE),
  "&app_ids=553834731",
  "&country=US",
  "&date_granularity=all_time",
  "&start_date=2015-10-01",
  "&end_date=", format(Sys.Date() - 1, "%Y-%m-%d")
)

facets_probe_url <- paste0(
  "https://api.sensortower.com/v1/facets/metrics",
  "?auth_token=", utils::URLencode(token, reserved = TRUE),
  "&facets=retention",
  "&bundle=retention_daily",
  "&breakdown=date,app_id",
  "&start_date=2025-01-01",
  "&end_date=2025-01-31",
  "&app_ids=553834731"
)

ratings_probe_url <- paste0(
  "https://api.sensortower.com/v1/facets/metrics",
  "?auth_token=", utils::URLencode(token, reserved = TRUE),
  "&facets=ratings",
  "&bundle=ratings_incremental",
  "&breakdown=app_id,date",
  "&date_granularity=day",
  "&app_ids=553834731",
  "&start_date=2024-01-01",
  "&end_date=2024-01-07"
)

reviews_by_rating_probe_url <- paste0(
  "https://api.sensortower.com/v1/facets/metrics",
  "?auth_token=", utils::URLencode(token, reserved = TRUE),
  "&facets=reviews_by_rating",
  "&bundle=reviews_by_rating",
  "&breakdown=date,review_rating",
  "&date_granularity=day",
  "&app_ids=553834731",
  "&regions=US",
  "&start_date=2024-01-01",
  "&end_date=2024-01-07"
)

results <- dplyr::bind_rows(
  probe_endpoint(
    "legacy_retention",
    list(
      url = legacy_retention_url,
      headers = list(Accept = "application/json")
    )
  ),
  probe_endpoint(
    "facets_retention",
    list(
      url = facets_probe_url,
      headers = list(Accept = "application/json")
    )
  ),
  probe_endpoint(
    "facets_ratings",
    list(
      url = ratings_probe_url,
      headers = list(Accept = "application/json")
    )
  ),
  probe_endpoint(
    "facets_reviews_by_rating",
    list(
      url = reviews_by_rating_probe_url,
      headers = list(Accept = "application/json")
    )
  ),
  probe_endpoint(
    "docs_json",
    list(
      url = "https://app.sensortower.com/api/docs/app_analysis.json",
      headers = list(Accept = "application/json")
    )
  )
)

print(results)
