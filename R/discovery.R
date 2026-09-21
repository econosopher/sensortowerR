.st_entities <- function(raw, os, entity = "app") {
  x <- .st_unwrap(raw, c("data", if (entity == "app") "apps" else "publishers"))
  proto <- if (entity == "app") .st_app_ptype() else .st_publisher_ptype()
  if (!nrow(x)) {
    return(proto)
  }
  if ("entity_type" %in% names(x)) {
    keep <- grepl(if (entity == "app") "(^|_)app$" else "(^|_)publisher$", x$entity_type)
    x <- x[!is.na(keep) & keep, , drop = FALSE]
  }
  id <- paste0(entity, "_id")
  name <- paste0(entity, "_name")
  x[[id]] <- .st_ids(.st_column(x, c(if (os == "unified") paste0("unified_", id), id, if (entity == "publisher") "app_id", paste0("unified_", id)), TRUE))
  x[[name]] <- as.character(.st_column(x, c(if (os == "unified") paste0("unified_", name), name, "name", paste0("unified_", name))))
  x$os <- rep(os, nrow(x))
  if ("entity_type" %in% names(x)) {
    x$os[grepl("^unified_", x$entity_type)] <- "unified"
    x$os[grepl("^(ios|itunes)_", x$entity_type)] <- "ios"
    x$os[grepl("^android_", x$entity_type)] <- "android"
  }
  if (anyNA(x[[id]]) || any(!nzchar(x[[id]]))) .st_abort("Entity response has missing IDs.", "st_schema_error")
  x <- dplyr::select(x, -dplyr::any_of(c("name", paste0("unified_", id), paste0("unified_", name))))
  .st_schema(x, proto)
}
#' Find apps or publishers
#' @param query Search term, or NULL when using filter.
#' @param filter A created st_filter object or server filter ID.
#' @param os Platform to search, default unified.
#' @param limit Maximum results. Search supports at most 250; filtered discovery
#'   follows cursor pages up to the requested limit.
#' @inheritParams st_metrics
#' @return An ungrouped tibble with app_id, os, app_name or publisher_id, os,
#'   publisher_name. Additional source metadata is retained.
#' @export
st_apps <- function(query = NULL, filter = NULL, os = "unified", limit = 100, auth_token = NULL) {
  os <- .st_os(os)
  limit <- .st_int(limit, "limit")
  if (is.null(query) == is.null(filter)) .st_abort("Supply exactly one of `query` or `filter`.")
  if (!is.null(query)) {
    .st_string(query, "query")
    .st_int(limit, "limit", max = 250)
    return(.st_entities(.st_get(paste0(os, "/search_entities"), list(term = query, entity_type = "app", limit = limit), auth_token), os))
  }
  ids <- .st_filter_ids(filter)
  pages <- purrr::map(ids, function(id) .st_filter_apps(id, os, limit, auth_token))
  out <- purrr::list_rbind(pages) |> dplyr::distinct(.data$app_id, .data$os, .keep_all = TRUE)
  dplyr::slice_head(out, n = limit)
}
#' @rdname st_apps
#' @export
st_publishers <- function(query, os = "unified", limit = 100, auth_token = NULL) {
  .st_string(query, "query")
  os <- .st_os(os)
  limit <- .st_int(limit, "limit", max = 250)
  .st_entities(.st_get(paste0(os, "/search_entities"), list(term = query, entity_type = "publisher", limit = limit), auth_token), os, "publisher")
}
.st_filter_apps <- function(id, os, limit, token) {
  pages <- list()
  cursor <- NULL
  seen <- character()
  count <- 0L
  repeat {
    raw <- .st_get("app_tag/apps", list(app_id_type = if (os == "ios") "itunes" else os, custom_fields_filter_id = id, last_known_id = cursor), token)
    x <- if (is.list(raw) && "app_ids" %in% names(raw)) tibble::tibble(app_id = as.character(unlist(raw$app_ids))) else .st_unwrap(raw, c("apps", "data"))
    if (nrow(x)) x <- .st_entities(x, os) else x <- .st_app_ptype()
    pages[[length(pages) + 1L]] <- x
    count <- count + nrow(x)
    next_cursor <- raw$last_known_id %||% raw$next_cursor
    if (is.null(next_cursor) || !length(next_cursor) || identical(next_cursor, "") || !nrow(x) || count >= limit) break
    next_cursor <- as.character(next_cursor)
    if (next_cursor %in% seen) .st_abort("Filter pagination repeated a cursor.", "st_schema_error")
    seen <- c(seen, next_cursor)
    cursor <- next_cursor
  }
  purrr::list_rbind(pages) |> dplyr::slice_head(n = limit)
}
#' Retrieve metadata and explicitly convert app IDs
#' @inheritParams st_metrics
#' @param fields Optional names of metadata fields to retain, in addition to IDs
#'   and app_name. Missing fields are returned as missing values.
#' @param target_os Optional target ID namespace. Unified conversion uses the
#'   provider's mapping; conversion to a store expands all mapped regional SKUs.
#' @return A tibble with app metadata and input columns. Conversions include
#'   input_app_id and input_os. No name-based ID matching is performed.
#' @export
st_app <- function(data, os = NULL, fields = NULL, target_os = NULL, errors = c("abort", "partial"), auth_token = NULL) {
  input <- .st_inputs(data, os)
  errors <- match.arg(errors)
  if (!is.null(target_os)) target_os <- .st_os(target_os)
  if (!is.null(fields) && (!is.character(fields) || anyNA(fields))) .st_abort("`fields` must be character column names.")
  converted <- !is.null(target_os)
  if (converted && any(c("input_app_id", "input_os") %in% names(input))) .st_abort("Rename existing input_app_id/input_os columns before conversion.")
  keys <- dplyr::distinct(input, .data$app_id, .data$os)
  out <- purrr::map(seq_len(nrow(keys)), function(i) {
    id <- keys$app_id[i]
    platform <- keys$os[i]
    target <- target_os %||% platform
    path <- paste0(if (target != platform) "unified" else platform, "/apps")
    .st_attempt(function() {
      params <- list(app_ids = id)
      if (startsWith(path, "unified")) params$app_id_type <- if (platform == "ios") "itunes" else platform
      raw <- .st_get(path, params, auth_token)
      x <- .st_entities(raw, if (startsWith(path, "unified")) "unified" else platform)
      if (target != "unified" && target != platform && nrow(x)) {
        col <- if (target == "ios") "itunes_apps" else "android_apps"
        if (!col %in% names(x)) .st_abort("Unified metadata lacks store mappings.", "st_schema_error")
        x <- purrr::map(x[[col]], function(v) .st_entities(v, target)) |> purrr::list_rbind()
      }
      if (!converted && nrow(x) && any(x$app_id != id)) .st_abort("Metadata response contains an unrequested ID.", "st_schema_error")
      if (converted) {
        x$input_app_id <- rep(id, nrow(x))
        x$input_os <- rep(platform, nrow(x))
      }
      x
    }, function() {
      x <- tibble::tibble(app_id = if (converted) NA_character_ else id, os = target, app_name = NA_character_)
      if (converted) {
        x$input_app_id <- id
        x$input_os <- platform
      }
      x
    }, errors, paste0(path, " [", id, "]"))
  }) |> purrr::list_rbind()
  out <- .st_schema(out, .st_app_ptype())
  if (errors == "partial" && !"status" %in% names(out)) {
    out$status <- character()
    out$error <- character()
    out$endpoint <- character()
  }
  if (!is.null(fields)) {
    for (n in setdiff(fields, names(out))) out[[n]] <- rep(NA_character_, nrow(out))
    out <- dplyr::select(out, dplyr::any_of(unique(c(names(.st_app_ptype()), fields, "input_app_id", "input_os", "status", "error", "endpoint"))))
  }
  if (converted) {
    input <- dplyr::rename(input, input_app_id = "app_id", input_os = "os")
    if (!"input_app_id" %in% names(out)) {
      out$input_app_id <- character()
      out$input_os <- character()
    }
  }
  out <- .st_partial_warning(out)
  .st_attach(input, out, if (converted) c("input_app_id", "input_os") else c("app_id", "os"))
}
#' Retrieve a publisher's apps
#' @param data A publisher tibble with publisher_id and os, or character IDs.
#' @param limit Maximum apps per publisher (default 10000).
#' @inheritParams st_metrics
#' @return An app tibble retaining publisher context. Related publishers are not
#'   silently included and names are never used to merge IDs.
#' @export
st_publisher_apps <- function(data, os = NULL, limit = 10000, errors = c("abort", "partial"), auth_token = NULL) {
  input <- .st_inputs(data, os, "publisher_id")
  limit <- .st_int(limit, "limit")
  errors <- match.arg(errors)
  keys <- dplyr::distinct(input, .data$publisher_id, .data$os)
  out <- purrr::map(seq_len(nrow(keys)), function(i) {
    id <- keys$publisher_id[i]
    platform <- keys$os[i]
    path <- if (platform == "unified") "unified/publishers/apps" else paste0(platform, "/publisher/publisher_apps")
    .st_attempt(function() {
      raw <- if (platform == "unified") {
        .st_get(path, list(unified_id = id), auth_token)
      } else {
        .st_pages(path, list(publisher_id = id), auth_token, limit, 100, c("apps", "data"))
      }
      x <- .st_entities(raw, platform) |> dplyr::slice_head(n = limit)
      x$publisher_id <- rep(id, nrow(x))
      x
    }, function() tibble::tibble(publisher_id = id, os = platform, app_id = NA_character_, app_name = NA_character_), errors, paste0(path, " [", id, "]"))
  }) |> purrr::list_rbind()
  out <- .st_schema(out, tibble::tibble(publisher_id = character(), app_id = character(), os = character(), app_name = character()))
  if (errors == "partial" && !"status" %in% names(out)) {
    out$status <- character()
    out$error <- character()
    out$endpoint <- character()
  }
  .st_attach(input, .st_partial_warning(out), c("publisher_id", "os"))
}
