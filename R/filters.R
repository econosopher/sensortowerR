#' Construct a local filter, then explicitly create it on the server
#' @param genre,publisher,sdk Character field values.
#' @param date_from,date_to Optional release-date bounds for Release Date (US).
#' @param monetization One or more of free, iap, ads, subscription.
#' @param custom_fields Named list of field values, or a list of explicit field
#'   records with name, values, global and exclude.
#' @param filter_id Existing server filter ID(s), mutually exclusive with criteria.
#' @param combine Combine field predicates using and or or.
#' @return st_filter() returns a local st_filter object without making requests.
#'   st_filter_create() returns the same object with verified server IDs. AND
#'   sends all fields in one request; OR creates one filter per field. Consumers
#'   union OR results by ID. Values inside a field use the provider's semantics.
#' @examples
#' # Construction is local. Only st_filter_create() writes to the server.
#' st_filter(genre = "RPG", publisher = "Supercell", combine = "and")
#' st_filter(genre = "RPG", publisher = "Supercell", combine = "or")
#' @export
st_filter <- function(genre = NULL, monetization = NULL, publisher = NULL, sdk = NULL,
                      custom_fields = NULL, filter_id = NULL, date_from = NULL, date_to = NULL, combine = c("and", "or")) {
  combine <- match.arg(combine)
  fields <- list()
  add <- function(name, values, global = TRUE, exclude = FALSE) {
    if (!is.atomic(values) || !length(values) || anyNA(values)) .st_abort("Filter field values must be non-empty atomic vectors without missing values.")
    values <- if (is.logical(values)) tolower(as.character(values)) else as.character(values)
    if (any(!nzchar(values))) .st_abort("Filter field values cannot be blank.")
    list(name = .st_string(name, "field name"), values = as.list(unique(values)), global = .st_flag(global, "global"), exclude = .st_flag(exclude, "exclude"))
  }
  if (!is.null(date_from) || !is.null(date_to)) {
    from <- if (!is.null(date_from)) .st_date(date_from, "date_from")
    to <- if (!is.null(date_to)) .st_date(date_to, "date_to")
    if (!is.null(from) && !is.null(to) && from > to) .st_abort("Release date_from must be on or before date_to.")
    value <- if (is.null(from)) paste("before", to) else if (is.null(to)) paste("after", from) else paste(from, "to", to)
    fields <- c(fields, list(add("Release Date (US)", value)))
  }
  if (!is.null(genre)) fields <- c(fields, list(add("Game Genre", genre)))
  if (!is.null(publisher)) fields <- c(fields, list(add("Publisher", publisher)))
  if (!is.null(monetization)) {
    map <- c(free = "Free", iap = "In-App Purchases", ads = "Contains Ads", subscription = "In-App Subscription")
    monetization <- .st_names(monetization, names(map), "monetization")
    fields <- c(fields, purrr::map(monetization, function(x) add(map[[x]], TRUE)))
  }
  if (!is.null(sdk)) fields <- c(fields, purrr::map(sdk, function(x) add(paste0("SDK: ", x), TRUE)))
  if (!is.null(custom_fields)) {
    if (!is.list(custom_fields)) .st_abort("`custom_fields` must be a list.")
    if (!is.null(names(custom_fields))) {
      fields <- c(fields, purrr::imap(custom_fields, function(value, name) add(name, value)))
    } else {
      fields <- c(fields, purrr::map(custom_fields, function(x) add(x$name, unlist(x$values), x$global %||% TRUE, x$exclude %||% FALSE)))
    }
  }
  if (!is.null(filter_id)) {
    if (length(fields)) .st_abort("Supply filter_id or criteria, not both.")
    if (!is.character(filter_id) || !length(filter_id) || anyNA(filter_id) || any(!grepl("^[a-fA-F0-9]{24}$", filter_id))) .st_abort("Invalid server filter ID.")
    if (length(filter_id) > 1L && combine != "or") .st_abort("Multiple existing IDs can only be combined with or.")
  } else if (!length(fields)) .st_abort("Supply filter criteria or an existing filter_id.")
  structure(list(fields = fields, combine = combine, filter_ids = filter_id), class = "st_filter")
}
#' @rdname st_filter
#' @param filter A local st_filter object.
#' @inheritParams st_metrics
#' @export
st_filter_create <- function(filter, auth_token = NULL) {
  if (!inherits(filter, "st_filter")) .st_abort("`filter` must be created by st_filter().")
  if (length(filter$filter_ids)) {
    return(filter)
  }
  bodies <- if (filter$combine == "and") list(filter$fields) else purrr::map(filter$fields, function(x) list(x))
  filter$filter_ids <- purrr::map_chr(bodies, function(fields) {
    raw <- .st_get("custom_fields_filter", auth_token = auth_token, method = "POST", body = list(custom_fields = fields))
    id <- raw$custom_fields_filter_id
    if (!is.character(id) || length(id) != 1L || is.na(id) || !grepl("^[a-fA-F0-9]{24}$", id)) .st_abort("Server did not return a valid filter ID.", "st_schema_error")
    id
  })
  filter
}
.st_filter_ids <- function(filter) {
  if (is.null(filter)) {
    return(NULL)
  }
  if (is.character(filter)) filter <- st_filter(filter_id = filter)
  if (!inherits(filter, "st_filter")) .st_abort("Expected a st_filter object or server ID.")
  if (!length(filter$filter_ids)) .st_abort("Local filter has no server ID. Call st_filter_create() explicitly first.")
  filter$filter_ids
}
#' @rdname st_filter
#' @param x An st_filter object.
#' @param ... Unused.
#' @export
print.st_filter <- function(x, ...) {
  cat("<st_filter>", x$combine, "|", length(x$fields), "fields |", length(x$filter_ids), "server IDs\n")
  invisible(x)
}
#' Discover custom field values
#' @param query Optional field-name search term.
#' @inheritParams st_metrics
#' @return A tibble with name, global and a values list-column.
#' @export
st_fields <- function(query = NULL, auth_token = NULL) {
  if (!is.null(query)) .st_string(query, "query")
  x <- .st_records(.st_get("custom_fields_filter/fields_values", list(term = query), auth_token), "custom_fields")
  .st_schema(x, tibble::tibble(name = character(), global = logical(), values = list()))
}
#' List the bundled platform categories
#' @param os Optional ios or android filter. NULL returns both.
#' @return A tibble with os, category_id and category_name. This is a bundled
#'   reference snapshot, not a live guarantee of provider category coverage.
#' @export
st_categories <- function(os = NULL) {
  x <- tibble::as_tibble(st_category_data)
  names(x)[names(x) == "platform"] <- "os"
  x$category_id <- as.character(x$category_id)
  if (!is.null(os)) {
    os <- .st_choice(os, c("ios", "android"), "os")
    x <- dplyr::filter(x, .data$os == .env$os)
  }
  x
}

#' Read a server filter definition
#' @param filter A created st_filter or existing server ID.
#' @inheritParams st_metrics
#' @return A tibble of custom field predicates with filter_id provenance.
#' @export
st_filter_read <- function(filter, auth_token = NULL) {
  ids <- .st_filter_ids(filter)
  if (!length(ids)) .st_abort("Supply a server filter ID.")
  out <- purrr::map(ids, function(id) {
    x <- .st_records(.st_get(paste0("custom_fields_filter/", id), auth_token = auth_token), "custom_fields")
    x$filter_id <- rep(id, nrow(x))
    x
  }) |> purrr::list_rbind()
  .st_schema(out, tibble::tibble(filter_id = character(), name = character(), global = logical(), values = list()))
}
