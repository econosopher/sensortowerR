#' Retrieve app metadata in batches
#'
#' Retrieve provider metadata for explicitly identified apps without name-based
#' matching. Unified rows retain the provider's nested regional store mappings.
#' @param data Character vector of IDs in the selected namespace.
#' @param os One of unified, ios, or android.
#' @param country Optional two-letter store country for iOS or Android metadata.
#' @inheritParams st_metrics
#' @return A tibble of provider metadata records, retaining nested mappings.
#'   The returned app ID and canonical store ID (`canonical_app_id`, when
#'   present) are character columns. Returned app IDs are checked against the
#'   requested IDs. Other fields use atomic vectors for a single scalar JSON
#'   type and list columns for mixed or nested values. Missing values are `NA`
#'   in atomic columns and `NULL` in list columns.
#' @export
st_app_metadata <- function(data, os, country = NULL, auth_token = NULL) {
  os <- .st_os(os)
  ids <- unique(.st_inputs(data, os)$app_id)
  if (!is.null(country)) {
    if (os == "unified") .st_abort("country is available for store metadata only.")
    country <- .st_countries(country)
    if (length(country) != 1L || country == "WW") .st_abort("Provide one store country.")
  }
  key <- if (os == "unified") "unified_app_id" else "app_id"
  id_columns <- c(key, "canonical_app_id")
  pages <- lapply(split(ids, ceiling(seq_along(ids) / 100L)), function(chunk) {
    params <- list(app_ids = chunk)
    if (os == "unified") params$app_id_type <- "unified"
    if (!is.null(country)) params$country <- country
    .st_metadata_rows(.st_get(paste0(os, "/apps"), params, auth_token), key, id_columns, chunk)
  })
  # Infer field types across all pages so a later page can introduce a different JSON type.
  rows <- unlist(pages, recursive = FALSE, use.names = FALSE)
  if (!length(rows)) return(tibble::tibble())
  fields <- unique(unlist(lapply(rows, names), use.names = FALSE))
  columns <- lapply(fields, function(field) {
    values <- lapply(rows, `[[`, field)
    if (field %in% id_columns) return(vapply(values, function(value) value %||% NA_character_, character(1)))
    .st_metadata_column(values)
  })
  names(columns) <- fields
  tibble::as_tibble(columns)
}

.st_metadata_rows <- function(response, key, id_columns, requested) {
  if (!is.list(response) || !"apps" %in% names(response)) .st_abort("Response lacks `apps`.", "st_schema_error")
  rows <- response[["apps"]]
  if (is.null(rows)) return(list())
  if (!is.list(rows)) .st_abort("Expected JSON records.", "st_schema_error")
  if (length(rows) && !is.null(names(rows))) rows <- list(rows)
  lapply(rows, function(row) {
    if (!is.list(row) || is.null(names(row)) || anyDuplicated(names(row))) .st_abort("Expected named JSON records.", "st_schema_error")
    if (!key %in% names(row)) .st_abort(paste0("Response lacks ", key, "."), "st_schema_error")
    for (field in intersect(id_columns, names(row))) {
      value <- row[[field]]
      if (is.null(value)) next
      if (length(value) != 1L || !(is.numeric(value) || is.character(value))) {
        .st_abort(paste0("Expected a scalar ID in `", field, "`."), "st_schema_error")
      }
      row[[field]] <- .st_ids(value)
    }
    if (is.null(row[[key]]) || !row[[key]] %in% requested) .st_abort("Metadata response contains an unrequested ID.", "st_schema_error")
    row
  })
}

.st_metadata_column <- function(values) {
  present <- !vapply(values, is.null, logical(1))
  observed <- values[present]
  column <- rep(NA, length(values))
  if (!length(observed)) return(column)
  if (!all(vapply(observed, function(value) is.atomic(value) && length(value) == 1L, logical(1)))) return(values)
  types <- vapply(observed, function(value) if (is.numeric(value)) "number" else typeof(value), character(1))
  if (length(unique(types)) > 1L) return(values)
  column[present] <- unlist(observed, use.names = FALSE)
  column
}
