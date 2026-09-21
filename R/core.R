#' @importFrom rlang .data .env %||% :=
NULL

.st_abort <- function(message, class = "st_input_error", ...) {
  rlang::abort(message, class = c(class, "sensortower_error"), ...)
}
.st_string <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    .st_abort(paste0("`", name, "` must be a non-empty string."))
  }
  x
}
.st_choice <- function(x, choices, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !x %in% choices) {
    .st_abort(paste0("`", name, "` must be one of: ", paste(choices, collapse = ", "), "."))
  }
  x
}
.st_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) .st_abort(paste0("`", name, "` must be TRUE or FALSE."))
  x
}
.st_int <- function(x, name, min = 1, max = Inf) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < min || x > max || x != floor(x)) {
    .st_abort(paste0("`", name, "` must be an integer between ", min, " and ", max, "."))
  }
  as.integer(x)
}
.st_os <- function(os) .st_choice(os, c("ios", "android", "unified"), "os")
.st_countries <- function(countries) {
  if (!is.character(countries) || !length(countries) || anyNA(countries) || any(!grepl("^[A-Z]{2}$", countries))) {
    .st_abort("`countries` must contain uppercase two-letter codes or WW.")
  }
  if ("WW" %in% countries && length(unique(countries)) > 1L) {
    .st_abort("Request WW separately from individual countries to avoid overlapping totals.")
  }
  unique(countries)
}
.st_date <- function(x, name) {
  if (length(x) != 1L || anyNA(x) || !(inherits(x, "Date") || (is.character(x) && grepl("^\\d{4}-\\d{2}-\\d{2}$", x)))) {
    .st_abort(paste0("`", name, "` must be one Date or YYYY-MM-DD string."))
  }
  out <- tryCatch(as.Date(x), error = function(e) as.Date(NA))
  if (is.na(out) || (is.character(x) && format(out, "%Y-%m-%d") != x)) .st_abort(paste0("Invalid `", name, "`."))
  out
}
.st_dates <- function(date_from, date_to) {
  a <- .st_date(date_from, "date_from")
  b <- .st_date(date_to, "date_to")
  if (a > b) .st_abort("`date_from` must be on or before `date_to`.")
  list(start_date = a, end_date = b)
}
.st_granularity <- function(x) .st_choice(x, c("daily", "weekly", "monthly", "quarterly"), "granularity")
.st_names <- function(x, allowed, name) {
  if (!is.character(x) || !length(x) || anyNA(x) || any(!x %in% allowed)) {
    .st_abort(paste0("`", name, "` must contain only: ", paste(allowed, collapse = ", "), "."))
  }
  unique(x)
}
.st_inputs <- function(data, os = NULL, id = "app_id") {
  if (is.data.frame(data)) {
    out <- tibble::as_tibble(dplyr::ungroup(data))
    if (!all(c(id, "os") %in% names(out))) .st_abort(paste0("Table input requires `", id, "` and `os` columns."))
    if (!is.null(os) && any(out$os != .st_os(os), na.rm = TRUE)) .st_abort("`os` conflicts with the input table; convert IDs explicitly with st_app(target_os = ...).")
  } else {
    if (!is.character(data)) .st_abort("Input must be a data frame or character ID vector.")
    if (is.null(os)) .st_abort("Character ID input requires `os`.")
    out <- tibble::tibble(!!id := data, os = rep(.st_os(os), length(data)))
  }
  if (!is.character(out[[id]]) || anyNA(out[[id]]) || any(!nzchar(trimws(out[[id]])))) .st_abort(paste0("`", id, "` must contain non-empty character IDs."))
  if (!is.character(out$os) || anyNA(out$os) || any(!out$os %in% c("ios", "android", "unified"))) .st_abort("Invalid `os` column.")
  out
}
.st_attach <- function(input, result, keys = c("app_id", "os")) {
  context <- setdiff(names(input), keys)
  used <- union(names(input), names(result))
  for (n in intersect(context, names(result))) {
    new <- paste0(n, ".input")
    while (new %in% used) new <- paste0(new, ".input")
    names(input)[names(input) == n] <- new
    used <- c(used, new)
  }
  dplyr::inner_join(input, result, by = keys, relationship = "many-to-many") |>
    dplyr::ungroup() |>
    tibble::as_tibble()
}
.st_records <- function(x, envelope = NULL) {
  if (!is.null(envelope)) {
    if (!is.list(x) || !envelope %in% names(x)) .st_abort(paste0("Response lacks `", envelope, "`."), "st_schema_error")
    x <- x[[envelope]]
  }
  if (is.data.frame(x)) {
    return(tibble::as_tibble(x))
  }
  if (is.null(x) || (is.list(x) && !length(x))) {
    return(tibble::tibble())
  }
  if (!is.list(x)) .st_abort("Expected JSON records.", "st_schema_error")
  if (!is.null(names(x))) x <- list(x)
  purrr::map(x, function(row) {
    if (!is.list(row) || is.null(names(row)) || anyDuplicated(names(row))) .st_abort("Expected named JSON records.", "st_schema_error")
    values <- purrr::map(row, function(v) {
      if (is.null(v)) NA else if (is.atomic(v) && length(v) == 1L) v else list(v)
    })
    tibble::as_tibble(values)
  }) |> purrr::list_rbind()
}
.st_unwrap <- function(x, envelopes) {
  for (n in envelopes) if (is.list(x) && n %in% names(x)) {
    return(.st_records(x, n))
  }
  .st_records(x)
}
.st_column <- function(x, candidates, required = FALSE) {
  found <- intersect(candidates, names(x))
  if (!length(found)) {
    if (required && nrow(x)) .st_abort(paste0("Response lacks ", paste(candidates, collapse = "/"), "."), "st_schema_error")
    return(rep(NA_character_, nrow(x)))
  }
  x[[found[[1]]]]
}
.st_numeric <- function(x) {
  if (is.list(x)) x <- vapply(x, function(v) if (is.null(v) || !length(v)) NA_character_ else as.character(v), character(1))
  out <- suppressWarnings(as.double(x))
  if (any(!is.na(x) & (is.na(out) | !is.finite(out)))) .st_abort("Non-numeric metric value in response.", "st_schema_error")
  out
}
.st_response_date <- function(x) {
  out <- suppressWarnings(as.Date(substr(as.character(x), 1, 10), format = "%Y-%m-%d"))
  if (any(!is.na(x) & is.na(out))) .st_abort("Invalid response date.", "st_schema_error")
  out
}
.st_schema <- function(x, prototype) {
  for (n in names(prototype)) {
    if (!n %in% names(x)) {
      x[[n]] <- vctrs::vec_init(prototype[[n]], nrow(x))
    } else if (inherits(prototype[[n]], "Date")) {
      x[[n]] <- .st_response_date(x[[n]])
    } else if (is.character(prototype[[n]])) {
      x[[n]] <- as.character(x[[n]])
    } else if (is.double(prototype[[n]])) {
      x[[n]] <- .st_numeric(x[[n]])
    } else {
      x[[n]] <- vctrs::vec_cast(x[[n]], prototype[[n]])
    }
  }
  dplyr::select(x, dplyr::all_of(names(prototype)), dplyr::everything())
}
.st_metric_ptype <- function() tibble::tibble(app_id = character(), os = character(), country = character(), date = as.Date(character()), metric = character(), value = double(), unit = character(), period = character())
.st_app_ptype <- function() tibble::tibble(app_id = character(), os = character(), app_name = character())
.st_publisher_ptype <- function() tibble::tibble(publisher_id = character(), os = character(), publisher_name = character())
.st_unique <- function(data, keys) {
  data <- dplyr::distinct(data)
  if (anyDuplicated(data[keys])) .st_abort("Response contains conflicting duplicate observations.", "st_schema_error")
  data
}
.st_attempt <- function(fun, failure, errors, endpoint) {
  tryCatch(
    {
      out <- fun()
      if (errors == "partial" && !"status" %in% names(out)) {
        out$status <- rep("ok", nrow(out))
        out$error <- rep(NA_character_, nrow(out))
        out$endpoint <- rep(endpoint, nrow(out))
      }
      out
    },
    error = function(e) {
      while (!inherits(e, "sensortower_error") && !is.null(e$parent)) e <- e$parent
      if (!inherits(e, "sensortower_error")) stop(e)
      if (errors == "abort") .st_abort(paste0(endpoint, ": ", conditionMessage(e)), class(e)[1], status = e$status %||% NA_integer_)
      out <- failure()
      out$status <- rep("error", nrow(out))
      out$error <- rep(conditionMessage(e), nrow(out))
      out$endpoint <- rep(endpoint, nrow(out))
      out
    }
  )
}
.st_partial_warning <- function(data) {
  if ("status" %in% names(data) && any(data$status == "error")) rlang::warn("Some requests failed; inspect status, error, and endpoint columns. Missing values are not zero.", class = "st_partial_warning")
  data
}

.st_ids <- function(x) {
  if (is.numeric(x)) {
    if (any(!is.na(x) & (!is.finite(x) | x != floor(x)))) .st_abort("Response IDs must be whole numbers or strings.", "st_schema_error")
    out <- format(x, scientific = FALSE, trim = TRUE, digits = 22)
    out[is.na(x)] <- NA_character_
    return(out)
  }
  as.character(x)
}
