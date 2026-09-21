#' Parse or build a Sensor Tower web URL
#' @param url An https app.sensortower.com URL.
#' @param params Named query parameters; credentials are forbidden.
#' @param path Web path, default /top-charts.
#' @return st_parse_url() returns one row per parameter (name, value).
#'   st_build_url() returns a URL string. Neither function makes requests.
#' @export
st_parse_url <- function(url) {
  .st_string(url, "url")
  if (!grepl("^https://app\\.sensortower\\.com(/|$)", url)) .st_abort("Expected an HTTPS app.sensortower.com URL.")
  if (!grepl("?", url, fixed = TRUE)) {
    return(tibble::tibble(name = character(), value = character()))
  }
  query <- sub("^[^?]*\\?", "", sub("#.*$", "", url))
  if (!nzchar(query)) {
    return(tibble::tibble(name = character(), value = character()))
  }
  parts <- strsplit(query, "&", fixed = TRUE)[[1]]
  out <- tibble::tibble(
    name = vapply(parts, function(x) utils::URLdecode(sub("=.*$", "", x)), character(1), USE.NAMES = FALSE),
    value = vapply(parts, function(x) utils::URLdecode(if (grepl("=", x, fixed = TRUE)) sub("^[^=]*=", "", x) else ""), character(1), USE.NAMES = FALSE)
  )
  dplyr::filter(out, !tolower(.data$name) %in% c("auth_token", "authorization", "token", "api_key"))
}
#' @rdname st_parse_url
#' @export
st_build_url <- function(params = list(), path = "/top-charts") {
  .st_string(path, "path")
  if (!startsWith(path, "/") || grepl("[?#]", path)) .st_abort("`path` must be an absolute web path without query or fragment.")
  if (!is.list(params) || (length(params) && (is.null(names(params)) || any(!nzchar(names(params)))))) .st_abort("`params` must be named.")
  if (any(tolower(names(params)) %in% c("auth_token", "authorization", "token", "api_key"))) .st_abort("Credentials must not appear in web URLs.")
  req <- httr2::request(paste0("https://app.sensortower.com", path)) |> httr2::req_url_query(!!!.st_query(params))
  req$url
}
#' Inspect package and credential availability without network requests
#' @return A one-row tibble. Credential contents are never returned.
#' @export
st_diagnostics <- function() {
  tibble::tibble(
    package_version = as.character(utils::packageVersion("sensortowerR")),
    r_version = as.character(getRversion()), token_available = nzchar(Sys.getenv("SENSORTOWER_AUTH_TOKEN")), cache_entries = length(ls(.st_cache))
  )
}
