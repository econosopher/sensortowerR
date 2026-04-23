#' Fetch Sensor Tower Rankings
#'
#' Thin v1.0.0 facade that standardizes the app, publisher, and category ranking
#' entrypoints while dispatching to the existing implementations.
#'
#' @param entity Ranking entity to fetch: `"app"`, `"publisher"`, or
#'   `"category"`.
#' @param os Operating system. One of `"ios"`, `"android"`, or `"unified"`.
#' @param category Optional category identifier forwarded to the legacy ranking
#'   implementation.
#' @param country Two-letter country code.
#' @param chart_type Chart type for category rankings. Ignored for app and
#'   publisher rankings.
#' @param date Ranking date. Accepts a `Date` object or ISO date string.
#' @param limit Positive integer row limit.
#' @param filter Optional `st_filter` object or 24-character filter ID string.
#' @param auth_token Optional Sensor Tower API token.
#'
#' @return A tibble with standardized columns `rank`, `id`, `name`, `os`,
#'   `category`, `country`, and `date`, plus any entity-specific columns returned
#'   by the dispatched implementation.
#'
#' @examples
#' \dontrun{
#' st_rankings(entity = "app", os = "ios", category = 6014, country = "US")
#'
#' st_rankings(
#'   entity = "category",
#'   os = "android",
#'   category = "game",
#'   chart_type = "topgrossing",
#'   country = "GB"
#' )
#' }
#'
#' @export
st_rankings <- function(entity = c("app", "publisher", "category"),
                        os = "ios",
                        category = NULL,
                        country = "US",
                        chart_type = "topfreeapplications",
                        date = Sys.Date() - 1,
                        limit = 100,
                        filter = NULL,
                        auth_token = NULL) {
  entity <- match.arg(entity)
  os <- normalize_os(os)
  country <- normalize_country(country)
  auth_token <- get_auth_token(
    auth_token,
    error_message = paste(
      "Authentication token is required.",
      "Set SENSORTOWER_AUTH_TOKEN or pass `auth_token`."
    )
  )

  ranking_date <- tryCatch(as.Date(date), error = function(e) NA)
  if (is.na(ranking_date)) {
    rlang::abort("`date` must be a Date or ISO date string (YYYY-MM-DD).")
  }

  limit <- suppressWarnings(as.integer(limit[1]))
  if (is.na(limit) || limit < 1L) {
    rlang::abort("`limit` must be a positive integer.")
  }

  filter_id <- .st_filter_resolve_id(filter)
  custom_tags_mode <- if (!is.null(filter_id) && os == "unified") {
    "include_unified_apps"
  } else {
    NULL
  }

  raw <- switch(
    entity,
    app = st_top_charts_impl(
      measure = "revenue",
      os = os,
      time_range = "day",
      date = ranking_date,
      end_date = ranking_date,
      category = if (!is.null(category)) category else if (!is.null(filter_id)) 0 else NULL,
      regions = country,
      limit = limit,
      custom_fields_filter_id = filter_id,
      custom_tags_mode = custom_tags_mode,
      auth_token = auth_token
    ),
    publisher = {
      if (!is.null(filter_id)) {
        rlang::abort("`filter` is not supported for `entity = 'publisher'`.")
      }
      .st_rankings_fetch_publishers(
        os = os,
        category = category %||% 0,
        country = country,
        date = ranking_date,
        limit = limit,
        auth_token = auth_token
      )
    },
    category = st_category_rankings_impl(
      os = os,
      category = category,
      chart_type = chart_type,
      country = country,
      date = ranking_date,
      limit = limit,
      custom_fields_filter_id = filter_id,
      custom_tags_mode = custom_tags_mode,
      auth_token = auth_token
    )
  )

  .st_rankings_standardize(
    data = raw,
    entity = entity,
    os = os,
    category = category,
    country = country,
    date = ranking_date
  )
}

.st_rankings_fetch_publishers <- function(os,
                                          category,
                                          country,
                                          date,
                                          limit,
                                          auth_token) {
  page_size <- 10L
  offsets <- seq.int(0L, max(limit - 1L, 0L), by = page_size)

  pages <- lapply(offsets, function(offset) {
    st_top_publishers_impl(
      measure = "revenue",
      os = os,
      category = category,
      time_range = "day",
      comparison_attribute = "absolute",
      date = date,
      country = country,
      limit = min(page_size, limit - offset),
      offset = offset,
      include_apps = FALSE,
      auth_token = auth_token
    )
  })

  dplyr::bind_rows(pages)
}

.st_rankings_standardize <- function(data, entity, os, category, country, date) {
  if (is.null(data) || !nrow(data)) {
    return(
      tibble::tibble(
        rank = integer(),
        id = character(),
        name = character(),
        os = character(),
        category = character(),
        country = character(),
        date = as.Date(character())
      )
    )
  }

  data <- tibble::as_tibble(data)

  id <- switch(
    entity,
    app = dplyr::coalesce(
      .st_chr_col(data, "unified_app_id"),
      .st_chr_col(data, "app_id")
    ),
    publisher = dplyr::coalesce(
      .st_chr_col(data, "publisher_id"),
      .st_chr_col(data, "unified_publisher_id"),
      .st_chr_col(data, "app_id")
    ),
    category = dplyr::coalesce(
      .st_chr_col(data, "app_id"),
      .st_chr_col(data, "unified_app_id")
    )
  )

  name <- switch(
    entity,
    app = dplyr::coalesce(
      .st_chr_col(data, "app_name"),
      .st_chr_col(data, "unified_app_name"),
      .st_chr_col(data, "name")
    ),
    publisher = dplyr::coalesce(
      .st_chr_col(data, "publisher_name"),
      .st_chr_col(data, "unified_publisher_name"),
      .st_chr_col(data, "name")
    ),
    category = dplyr::coalesce(
      .st_chr_col(data, "app_name"),
      .st_chr_col(data, "unified_app_name"),
      .st_chr_col(data, "name")
    )
  )

  standardized <- tibble::tibble(
    rank = if ("rank" %in% names(data)) as.integer(data$rank) else seq_len(nrow(data)),
    id = id,
    name = name,
    os = rep(os, nrow(data)),
    category = rep(as.character(category %||% NA), nrow(data)),
    country = rep(country, nrow(data)),
    date = rep(as.Date(date), nrow(data))
  )

  drop_cols <- c(
    "rank", "app_id", "unified_app_id", "name", "app_name", "unified_app_name",
    "publisher_id", "publisher_name", "unified_publisher_id",
    "unified_publisher_name", "os", "category", "country", "date"
  )

  extras <- data[, setdiff(names(data), drop_cols), drop = FALSE]
  dplyr::bind_cols(standardized, extras)
}

.st_chr_col <- function(data, column) {
  if (!column %in% names(data)) {
    return(rep(NA_character_, nrow(data)))
  }

  as.character(data[[column]])
}
