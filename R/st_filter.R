.st_filter_new <- function(criteria, combine, filter_id, custom_fields = NULL) {
  structure(
    list(
      criteria = criteria,
      combine = combine,
      filter_id = filter_id,
      custom_fields = custom_fields
    ),
    class = "st_filter"
  )
}

#' Create a Sensor Tower Filter Object
#'
#' Thin v1.0.0 facade for building or wrapping Sensor Tower custom-field filter
#' IDs. When criteria are supplied, the function translates them into the legacy
#' custom-fields request structure and creates a server-side filter ID using the
#' existing implementation.
#'
#' @param date_from,date_to Optional date bounds for a release-date criterion.
#' @param genre Optional character vector of game genres.
#' @param monetization Optional monetization criteria. Accept either a character
#'   vector such as `c("free", "iap")` or a named list like
#'   `list(free = TRUE, iap = TRUE)`.
#' @param publisher Optional character vector of publisher names.
#' @param sdk Optional character vector of SDK names.
#' @param custom_fields Optional raw custom-fields specification. Can be a
#'   list-of-lists already matching the legacy request format or a named list of
#'   field names and values.
#' @param filter_id Optional existing 24-character Sensor Tower filter ID to
#'   wrap.
#' @param combine Logical operator metadata for multi-criterion filters: `"and"`
#'   or `"or"`.
#' @param auth_token Optional Sensor Tower API token.
#' @param x An `st_filter` object (S3 method argument).
#' @param ... Additional arguments. For `c.st_filter()`, further `st_filter`
#'   objects to combine; ignored by `print.st_filter()`, `format.st_filter()`,
#'   and `as.character.st_filter()`.
#' @param recursive Unused; present for S3 method-signature compatibility with
#'   [base::c()].
#'
#' @return An object of class `st_filter`, implemented as a list with elements:
#'   - `criteria`: normalized criteria supplied to `st_filter()`
#'   - `combine`: `"and"` or `"or"`
#'   - `filter_id`: the wrapped or created server-side filter ID
#'
#' @examples
#' \dontrun{
#' st_filter(genre = c("Puzzle", "Strategy"))
#'
#' st_filter(
#'   date_from = "2024-01-01",
#'   date_to = "2024-12-31",
#'   monetization = c("free", "iap")
#' )
#'
#' existing <- st_filter(filter_id = "687df26ac5a19ebcfe817d7f")
#' as.character(existing)
#' }
#'
#' @export
st_filter <- function(date_from = NULL,
                      date_to = NULL,
                      genre = NULL,
                      monetization = NULL,
                      publisher = NULL,
                      sdk = NULL,
                      custom_fields = NULL,
                      filter_id = NULL,
                      combine = c("and", "or"),
                      auth_token = NULL) {
  combine <- match.arg(combine)
  criteria_present <- !all(vapply(
    list(date_from, date_to, genre, monetization, publisher, sdk, custom_fields),
    is.null,
    logical(1)
  ))

  if (!criteria_present) {
    if (is.null(filter_id)) {
      rlang::abort("Supply at least one filter criterion or an existing `filter_id`.")
    }
    if (!st_is_valid_filter_id(filter_id)) {
      rlang::abort("`filter_id` must be a 24-character hexadecimal Sensor Tower filter ID.")
    }

    return(
      .st_filter_new(
        criteria = list(),
        combine = combine,
        filter_id = as.character(filter_id[1]),
        custom_fields = NULL
      )
    )
  }

  if (!is.null(filter_id)) {
    rlang::abort(
      "When criteria are supplied, omit `filter_id`; `st_filter()` will create a new server-side filter."
    )
  }

  auth_token <- get_auth_token(
    auth_token,
    error_message = paste(
      "Authentication token is required to create a filter.",
      "Set SENSORTOWER_AUTH_TOKEN or pass `auth_token`."
    )
  )

  bounds <- .st_filter_normalize_bounds(date_from, date_to)
  built_custom_fields <- .st_filter_build_custom_fields(
    date_from = bounds$date_from,
    date_to = bounds$date_to,
    genre = genre,
    monetization = monetization,
    publisher = publisher,
    sdk = sdk,
    custom_fields = custom_fields
  )

  created_filter_id <- st_custom_fields_filter_impl(
    custom_fields = built_custom_fields,
    auth_token = auth_token
  )

  criteria <- list(
    date_from = bounds$date_from,
    date_to = bounds$date_to,
    genre = if (!is.null(genre)) unique(as.character(genre)) else NULL,
    monetization = monetization,
    publisher = if (!is.null(publisher)) unique(as.character(publisher)) else NULL,
    sdk = if (!is.null(sdk)) unique(as.character(sdk)) else NULL,
    custom_fields = custom_fields
  )
  criteria <- criteria[!vapply(criteria, is.null, logical(1))]

  .st_filter_new(
    criteria = criteria,
    combine = combine,
    filter_id = created_filter_id,
    custom_fields = built_custom_fields
  )
}

#' @rdname st_filter
#' @export
print.st_filter <- function(x, ...) {
  cat(format(x, ...), "\n", sep = "")
  invisible(x)
}

#' @rdname st_filter
#' @export
format.st_filter <- function(x, ...) {
  criteria_names <- names(x$criteria)
  criteria_text <- if (length(criteria_names)) {
    paste(criteria_names, collapse = ", ")
  } else {
    "none"
  }

  sprintf(
    "<st_filter> id=%s combine=%s criteria=%s",
    x$filter_id %||% "NA",
    x$combine %||% "and",
    criteria_text
  )
}

#' @rdname st_filter
#' @export
as.character.st_filter <- function(x, ...) {
  x$filter_id %||% NA_character_
}

#' @rdname st_filter
#' @export
c.st_filter <- function(..., recursive = FALSE, combine = c("and", "or")) {
  filters <- list(...)
  combine <- match.arg(combine)

  if (!length(filters)) {
    return(.st_filter_new(criteria = list(), combine = combine, filter_id = NULL))
  }
  if (!all(vapply(filters, inherits, logical(1), what = "st_filter"))) {
    rlang::abort("`c.st_filter()` only accepts `st_filter` objects.")
  }

  auth_token <- get_auth_token(
    NULL,
    error_message = paste(
      "Authentication token is required to combine filters.",
      "Set SENSORTOWER_AUTH_TOKEN or pass `auth_token` when creating the filters."
    )
  )

  custom_fields <- unlist(
    lapply(filters, .st_filter_extract_custom_fields, auth_token = auth_token),
    recursive = FALSE
  )

  if (!length(custom_fields)) {
    rlang::abort("Could not extract any criteria to combine from the supplied filters.")
  }

  created_filter_id <- st_custom_fields_filter_impl(
    custom_fields = custom_fields,
    auth_token = auth_token
  )

  .st_filter_new(
    criteria = lapply(filters, `[[`, "criteria"),
    combine = combine,
    filter_id = created_filter_id,
    custom_fields = custom_fields
  )
}

.st_filter_resolve_id <- function(filter) {
  if (is.null(filter)) {
    return(NULL)
  }

  if (inherits(filter, "st_filter")) {
    filter_id <- filter$filter_id %||% NA_character_
    if (is.na(filter_id) || !nzchar(filter_id)) {
      rlang::abort("`st_filter` object does not contain a server-side `filter_id`.")
    }
    return(filter_id)
  }

  if (is.character(filter) && length(filter) == 1L && st_is_valid_filter_id(filter)) {
    return(as.character(filter[1]))
  }

  rlang::abort("`filter` must be `NULL`, an `st_filter` object, or a 24-character filter ID.")
}

.st_filter_normalize_bounds <- function(date_from, date_to) {
  if (is.null(date_from) && is.null(date_to)) {
    return(list(date_from = NULL, date_to = NULL))
  }

  if (is.null(date_from)) {
    date_from <- date_to
  }
  if (is.null(date_to)) {
    date_to <- date_from
  }

  normalize_dates(date_from, date_to)
}

.st_filter_build_custom_fields <- function(date_from,
                                           date_to,
                                           genre,
                                           monetization,
                                           publisher,
                                           sdk,
                                           custom_fields) {
  fields <- list()

  if (!is.null(date_from) || !is.null(date_to)) {
    values <- if (!is.null(date_from) && !is.null(date_to)) {
      list(paste(format(date_from, "%Y-%m-%d"), format(date_to, "%Y-%m-%d"), sep = " to "))
    } else if (!is.null(date_from)) {
      list(paste("after", format(date_from, "%Y-%m-%d")))
    } else {
      list(paste("before", format(date_to, "%Y-%m-%d")))
    }

    fields <- append(fields, list(list(
      name = "Release Date (US)",
      values = values,
      global = TRUE,
      exclude = FALSE
    )))
  }

  if (!is.null(genre)) {
    fields <- append(fields, list(list(
      name = "Game Genre",
      values = as.list(unique(as.character(genre))),
      global = TRUE,
      exclude = FALSE
    )))
  }

  fields <- append(fields, .st_filter_build_monetization_fields(monetization))

  if (!is.null(publisher)) {
    fields <- append(fields, list(list(
      name = "Publisher",
      values = as.list(unique(as.character(publisher))),
      global = TRUE,
      exclude = FALSE
    )))
  }

  if (!is.null(sdk)) {
    sdk <- unique(as.character(sdk))
    fields <- append(fields, lapply(sdk, function(one_sdk) {
      list(
        name = paste0("SDK: ", one_sdk),
        values = list(),
        global = TRUE,
        exclude = FALSE,
        true = TRUE
      )
    }))
  }

  fields <- append(fields, .st_filter_normalize_custom_fields(custom_fields))

  if (!length(fields)) {
    rlang::abort("No valid filter criteria were supplied.")
  }

  fields
}

.st_filter_build_monetization_fields <- function(monetization) {
  if (is.null(monetization)) {
    return(list())
  }

  field_map <- list(
    free = "Free",
    iap = "In-App Purchases",
    ads = "Contains Ads",
    subscription = "In-App Subscription"
  )

  if (is.character(monetization)) {
    keys <- tolower(unique(as.character(monetization)))
    bad <- setdiff(keys, names(field_map))
    if (length(bad)) {
      rlang::abort(
        sprintf(
          "Unknown `monetization` value(s): %s.",
          paste(bad, collapse = ", ")
        )
      )
    }
    values <- stats::setNames(rep(list(TRUE), length(keys)), keys)
  } else if (is.list(monetization)) {
    values <- monetization
  } else {
    rlang::abort("`monetization` must be a character vector or named list.")
  }

  keys <- intersect(names(values), names(field_map))
  if (!length(keys)) {
    rlang::abort(
      "`monetization` must include one or more of: free, iap, ads, subscription."
    )
  }

  lapply(keys, function(key) {
    list(
      name = field_map[[key]],
      global = TRUE,
      true = isTRUE(values[[key]])
    )
  })
}

.st_filter_normalize_custom_fields <- function(custom_fields) {
  if (is.null(custom_fields)) {
    return(list())
  }

  if (!is.list(custom_fields)) {
    rlang::abort("`custom_fields` must be a list.")
  }

  if (length(custom_fields) == 0) {
    return(list())
  }

  if (all(vapply(custom_fields, function(x) is.list(x) && !is.null(x$name), logical(1)))) {
    return(custom_fields)
  }

  if (is.null(names(custom_fields)) || any(!nzchar(names(custom_fields)))) {
    rlang::abort(
      "`custom_fields` must be either a legacy list-of-lists or a named list of field values."
    )
  }

  lapply(names(custom_fields), function(field_name) {
    value <- custom_fields[[field_name]]
    field <- list(
      name = field_name,
      global = TRUE,
      exclude = FALSE
    )

    if (is.logical(value) && length(value) == 1L) {
      field$true <- isTRUE(value)
    } else {
      field$values <- as.list(value)
    }

    field
  })
}

.st_filter_extract_custom_fields <- function(filter, auth_token) {
  if (!is.null(filter$custom_fields)) {
    return(filter$custom_fields)
  }

  if (is.null(filter$filter_id)) {
    return(list())
  }

  details <- st_custom_fields_filter_by_id_impl(
    id = filter$filter_id,
    auth_token = auth_token
  )

  .st_filter_details_to_custom_fields(details)
}

.st_filter_details_to_custom_fields <- function(details) {
  custom_fields <- details$custom_fields
  if (is.null(custom_fields) || !nrow(custom_fields)) {
    return(list())
  }

  lapply(seq_len(nrow(custom_fields)), function(i) {
    field <- list(
      name = custom_fields$name[i],
      global = if ("global" %in% names(custom_fields)) isTRUE(custom_fields$global[i]) else TRUE,
      exclude = if ("exclude" %in% names(custom_fields)) isTRUE(custom_fields$exclude[i]) else FALSE
    )

    if ("values" %in% names(custom_fields)) {
      field$values <- custom_fields$values[[i]]
    }
    if ("true" %in% names(custom_fields) && !is.na(custom_fields$true[i])) {
      field$true <- custom_fields$true[i]
    }

    field
  })
}
