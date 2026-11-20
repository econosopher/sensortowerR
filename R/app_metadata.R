#' Retrieve app metadata in batches
#'
#' Retrieve provider metadata for explicitly identified apps without name-based
#' matching. Unified rows retain the provider's nested regional store mappings.
#' @param data Character vector of IDs in the selected namespace.
#' @param os One of unified, ios, or android.
#' @param country Optional two-letter store country for iOS or Android metadata.
#' @inheritParams st_metrics
#' @return A tibble of provider metadata records, retaining nested mappings.
#'   Returned IDs are checked against the requested IDs.
#' @export
st_app_metadata <- function(data, os, country = NULL, auth_token = NULL) {
  os <- .st_os(os)
  ids <- unique(.st_inputs(data, os)$app_id)
  if (!is.null(country)) {
    if (os == "unified") .st_abort("country is available for store metadata only.")
    country <- .st_countries(country)
    if (length(country) != 1L || country == "WW") .st_abort("Provide one store country.")
  }
  pages <- lapply(split(ids, ceiling(seq_along(ids) / 100L)), function(chunk) {
    params <- list(app_ids = chunk)
    if (os == "unified") params$app_id_type <- "unified"
    if (!is.null(country)) params$country <- country
    x <- .st_records(.st_get(paste0(os, "/apps"), params, auth_token), "apps")
    key <- if (os == "unified") "unified_app_id" else "app_id"
    if (nrow(x)) {
      values <- .st_ids(.st_column(x, key, TRUE))
      if (anyNA(values) || any(!values %in% chunk)) .st_abort("Metadata response contains an unrequested ID.", "st_schema_error")
      x[[key]] <- values
    }
    x
  })
  if (!length(pages)) return(tibble::tibble())
  purrr::list_rbind(pages)
}
