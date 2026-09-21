.st_cache <- new.env(parent = emptyenv())
.st_token <- function(auth_token) .st_string(auth_token %||% Sys.getenv("SENSORTOWER_AUTH_TOKEN"), "auth_token (or SENSORTOWER_AUTH_TOKEN)")
.st_hash <- function(x) as.character(openssl::sha256(serialize(x, NULL)))
.st_query <- function(params) {
  purrr::imap(purrr::compact(params), function(x, name) {
    if (anyNA(x)) .st_abort("Query parameters cannot contain missing values.")
    if (inherits(x, "Date")) x <- format(x, "%Y-%m-%d")
    if (is.logical(x)) x <- tolower(as.character(x))
    if (endsWith(name, "[]")) as.character(x) else paste(x, collapse = ",")
  })
}
.st_request <- function(path, params = list(), auth_token = NULL, method = "GET", body = NULL) {
  token <- .st_token(auth_token)
  req <- httr2::request("https://api.sensortower.com") |>
    httr2::req_url_path_append("v1", path) |>
    httr2::req_url_query(!!!.st_query(params), .multi = "explode") |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_user_agent("sensortowerR/2.0.0") |>
    httr2::req_timeout(30)
  if (method == "GET") {
    req <- req |>
      httr2::req_url_query(auth_token = token) |>
      httr2::req_retry(
        max_tries = 3, max_seconds = 60, retry_on_failure = TRUE,
        is_transient = function(resp) httr2::resp_status(resp) %in% c(429L, 500L, 502L, 503L, 504L)
      )
  } else {
    req <- req |>
      httr2::req_auth_bearer_token(token) |>
      httr2::req_method(method) |>
      httr2::req_body_json(body)
  }
  req
}
.st_perform <- function(req) httr2::req_perform(req)
.st_get <- function(path, params = list(), auth_token = NULL, method = "GET", body = NULL) {
  req <- .st_request(path, params, auth_token, method, body)
  resp <- tryCatch(.st_perform(req), error = function(e) {
    response <- e$resp %||% e$response
    status <- if (inherits(response, "httr2_response")) httr2::resp_status(response) else NA_integer_
    # Do not chain raw conditions: curl errors, URLs and response bodies may echo credentials.
    .st_abort(paste0("Request to ", path, " failed", if (!is.na(status)) paste0(" (HTTP ", status, ")"), "."), "st_http_error", status = status)
  })
  status <- httr2::resp_status(resp)
  if (status >= 400L) .st_abort(paste0("Request to ", path, " failed (HTTP ", status, ")."), "st_http_error", status = status)
  raw <- resp$body
  if (is.null(raw)) raw <- raw()
  if (!length(raw)) {
    if (status == 204L) {
      return(list())
    }
    .st_abort(paste0("Empty response body from ", path, "."), "st_schema_error")
  }
  out <- tryCatch(jsonlite::fromJSON(rawToChar(raw), simplifyVector = FALSE), error = function(e) .st_abort(paste0("Invalid JSON from ", path, "."), "st_schema_error"))
  if (is.null(out) || !is.list(out) || identical(trimws(rawToChar(raw)), "{}")) .st_abort(paste0("Unexpected response from ", path, "."), "st_schema_error")
  if (any(c("error", "errors") %in% names(out))) .st_abort(paste0("API reported an error in a successful response from ", path, "."), "st_schema_error")
  out
}
.st_cached <- function(key, token, cache, ttl, fun) {
  .st_flag(cache, "cache")
  .st_int(ttl, "cache_ttl")
  key <- .st_hash(list(key, credential = .st_hash(token)))
  if (cache && exists(key, .st_cache, inherits = FALSE)) {
    entry <- get(key, .st_cache)
    if (as.numeric(difftime(Sys.time(), entry$time, units = "secs")) < min(ttl, entry$ttl)) {
      return(entry$data)
    }
    rm(list = key, envir = .st_cache)
  }
  out <- fun()
  if (cache && !("status" %in% names(out) && any(out$status == "error"))) assign(key, list(time = Sys.time(), ttl = ttl, data = out), .st_cache)
  out
}
#' Inspect or clear the session cache
#' @return `st_cache_info()` returns entry counts and expiry times in a tibble.
#'   `st_cache_clear()` invisibly returns the number of removed entries.
#' @export
st_cache_info <- function() {
  keys <- ls(.st_cache)
  purrr::map(keys, function(key) {
    x <- get(key, .st_cache)
    tibble::tibble(key = key, created_at = x$time, expires_at = x$time + x$ttl, rows = nrow(x$data))
  }) |> purrr::list_rbind(ptype = tibble::tibble(key = character(), created_at = as.POSIXct(character()), expires_at = as.POSIXct(character()), rows = integer()))
}
#' @rdname st_cache_info
#' @export
st_cache_clear <- function() {
  keys <- ls(.st_cache)
  rm(list = keys, envir = .st_cache)
  invisible(length(keys))
}
.st_pages <- function(path, params, auth_token, limit, page_size, envelope = NULL) {
  chunks <- list()
  offset <- 0L
  seen <- character()
  repeat {
    n <- min(page_size, limit - offset)
    raw <- .st_get(path, c(params, list(limit = n, offset = offset)), auth_token)
    page <- .st_unwrap(raw, envelope %||% character())
    fingerprint <- .st_hash(page)
    if (nrow(page) && fingerprint %in% seen) .st_abort("API pagination repeated a page.", "st_schema_error")
    seen <- c(seen, fingerprint)
    chunks[[length(chunks) + 1L]] <- page
    offset <- offset + nrow(page)
    if (nrow(page) < n || offset >= limit) break
  }
  out <- purrr::list_rbind(chunks)
  dplyr::slice_head(out, n = limit)
}
