# --- Utility Functions for sensortowerR ---

# This file contains helper functions used by the main data-fetching functions
# in the package. They handle tasks like input validation, query parameter
# preparation, and API request execution.

#' @importFrom rlang abort
#' @importFrom httr2 request req_user_agent req_url_path_append req_url_query
#'   req_error req_perform resp_status resp_body_raw resp_check_status
#'   resp_body_string
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @importFrom utils URLencode
#' @importFrom tidyr unnest
#' @importFrom dplyr rename all_of
#'
# --- Input Validation ---

validate_inputs <- function(os,
                            comparison_attribute,
                            time_range,
                            measure,
                            date,
                            category,
                            regions,
                            end_date = NULL,
                            limit = 25,
                            offset = NULL,
                            device_type = NULL,
                            custom_fields_filter_id = NULL,
                            custom_tags_mode = NULL,
                            data_model = NULL) {
  # Validation checks for common parameters
  stopifnot(
    "`os` must be one of 'ios', 'android', or 'unified'" =
      os %in% c("ios", "android", "unified"),
    "`comparison_attribute` must be one of 'absolute', 'delta', 'transformed_delta'" =
      comparison_attribute %in% c("absolute", "delta", "transformed_delta"),
    "`time_range` must be a non-empty string" =
      is.character(time_range) && nzchar(time_range),
    "`measure` must be a non-empty string" =
      is.character(measure) && nzchar(measure),
    "`date` must be provided" = !is.null(date),
    "Either `category` or `custom_fields_filter_id` must be provided" =
      !is.null(category) || !is.null(custom_fields_filter_id),
    "`regions` must be provided" = !is.null(regions),
    "`limit` must be a positive integer" =
      is.numeric(limit) && limit > 0 && limit == round(limit)
  )

  # Specific validations
  if (os %in% c("ios", "unified") && is.null(device_type)) {
    message("`device_type` is not specified for `os = '", os, "'`. Defaulting to 'total'.")
    device_type <- "total"
  }

  if (!is.null(custom_fields_filter_id) && os == "unified" && is.null(custom_tags_mode)) {
    rlang::abort("`custom_tags_mode` must be provided when `os` is 'unified' and `custom_fields_filter_id` is used.")
  }
}

# --- Query Parameter Preparation ---

prepare_query_params_sales <- function(auth_token,
                                       comparison_attribute,
                                       time_range,
                                       measure,
                                       date,
                                       category,
                                       end_date,
                                       regions,
                                       limit,
                                       offset,
                                       device_type,
                                       custom_fields_filter_id,
                                       custom_tags_mode,
                                       os) {
  params <- list(
    auth_token = auth_token,
    comparison_attribute = comparison_attribute,
    time_range = time_range,
    measure = measure,
    date = as.character(date),
    category = category,
    end_date = if (!is.null(end_date)) as.character(end_date) else NULL,
    regions = paste(regions, collapse = ","),
    limit = limit,
    offset = offset,
    device_type = if (os %in% c("ios", "unified")) device_type else NULL,
    custom_fields_filter_id = custom_fields_filter_id,
    custom_tags_mode = if (os == "unified") custom_tags_mode else NULL
  )
  # Remove NULLs
  params[!sapply(params, is.null)]
}

prepare_query_params_active_users <- function(auth_token,
                                              comparison_attribute,
                                              time_range,
                                              measure,
                                              date,
                                              category,
                                              regions,
                                              limit,
                                              offset,
                                              device_type,
                                              custom_fields_filter_id,
                                              custom_tags_mode,
                                              data_model,
                                              os) {
  params <- list(
    auth_token = auth_token,
    comparison_attribute = comparison_attribute,
    time_range = time_range,
    measure = measure,
    date = as.character(date),
    category = category,
    regions = paste(regions, collapse = ","),
    limit = limit,
    offset = offset,
    device_type = if (os %in% c("ios", "unified")) device_type else NULL,
    custom_fields_filter_id = custom_fields_filter_id,
    custom_tags_mode = if (os == "unified") custom_tags_mode else NULL,
    data_model = data_model
  )
  # Remove NULLs
  params[!sapply(params, is.null)]
}


# --- API Request Building and Performance ---

build_request <- function(base_url, path_segments, query_params) {
  req <- httr2::request(base_url) %>%
    httr2::req_user_agent("sensortowerR (https://github.com/ge-data-solutions/sensortowerR)")

  for (segment in path_segments) {
    req <- req %>% httr2::req_url_path_append(segment)
  }

  req %>% httr2::req_url_query(!!!query_params)
}


perform_request <- function(req) {
  tryCatch(
    {
      resp <- httr2::req_perform(req)
      httr2::resp_check_status(resp) # Check for HTTP errors
      resp
    },
    httr2_error = function(e) {
      status <- httr2::resp_status(e$resp)
      body <- httr2::resp_body_string(e$resp)
      rlang::abort(
        message = sprintf("API request failed with status %d.", status),
        body = body,
        parent = e
      )
    },
    error = function(e) {
      rlang::abort("An unexpected error occurred during the API request.", parent = e)
    }
  )
}

# --- Response Processing ---

process_response <- function(resp, enrich_response = TRUE) {
  body_raw <- httr2::resp_body_raw(resp)
  if (length(body_raw) == 0) {
    return(tibble::tibble())
  }

  body_text <- rawToChar(body_raw)
  result <- jsonlite::fromJSON(body_text, flatten = TRUE)

  if (length(result) == 0 || nrow(result) == 0) {
    return(tibble::tibble())
  }

  result_tbl <- tibble::as_tibble(result)

  # Enrich with app names if requested and possible
  if (enrich_response && "entities" %in% names(result_tbl) && is.list(result_tbl$entities)) {
    # Proactively coerce app_id to character in the nested data frames
    # to prevent type errors during the unnest operation. The API can return
    # a mix of integer and character IDs, which vctrs cannot combine.
    result_tbl$entities <- lapply(result_tbl$entities, function(df) {
      if (!is.null(df) && "app_id" %in% names(df)) {
        df$app_id <- as.character(df$app_id)
      }
      df
    })

    result_tbl <- tidyr::unnest(result_tbl, dplyr::all_of("entities"))

    if ("name" %in% names(result_tbl) && !"app.name" %in% names(result_tbl)) {
      result_tbl <- dplyr::rename(result_tbl, app.name = "name")
    }
  }

  return(result_tbl)
} 