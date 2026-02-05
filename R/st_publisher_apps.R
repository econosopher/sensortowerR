#' Get All Apps from a Publisher
#'
#' Retrieves a list of apps associated with a specified unified publisher ID
#' from the Sensor Tower API. Targets the `/v1/unified/publishers/apps`
#' endpoint.
#'
#' @param unified_id Character. Unified ID to resolve apps for. May be either:
#'   - Unified Publisher ID (24-char hex)
#'   - Unified App ID (24-char hex) belonging to a publisher
#'   The API returns the unified publisher and all associated apps in both cases.
#' @param publisher_id Deprecated alias for `unified_id`.
#' @param aggregate_related Logical. If TRUE, ensures each app's unified_app_id
#'   is the canonical ID that aggregates ALL regional SKUs. This solves the problem
#'   where games like "Watcher of Realms" are published under multiple regional
#'   publishers (Moonton, Vizta Games, Skystone Games, etc.) and may return
#'   different unified_app_ids. When TRUE, the function looks up each app by name
#'   to find the true unified_app_id that combines all regional versions.
#'   Defaults to FALSE for backwards compatibility.
#' @param auth_token Character. Your Sensor Tower API authentication token.
#'   Defaults to the value stored in the `SENSORTOWER_AUTH_TOKEN` environment
#'   variable.
#' @param verbose Logical. If TRUE, prints progress messages during aggregation.
#'   Defaults to TRUE.
#'
#' @return A [tibble][tibble::tibble] containing details of the apps associated
#'   with the publisher. The exact columns depend on the API response but often
#'   include app IDs, names, platform, etc. Returns an empty tibble if the
#'   publisher ID is invalid, has no apps, or an error occurs.
#'
#' @section Solving Regional Publisher Issues:
#' Many publishers have regional subsidiaries or partners that publish the same
#' game under different app IDs in different regions. For example, Moonton's
#' "Watcher of Realms" is published by Moonton in some regions, Vizta Games in
#' others, and Skystone Games in others.
#'
#' When `aggregate_related = TRUE`, this function ensures you get the unified_app_id
#' that represents the FULL game across all regional publishers, which is required
#' for accurate revenue/download aggregation via `st_unified_sales_report()`.
#'
#' @section API Endpoint Used:
#'   - `GET /v1/unified/publishers/apps`
#'
#' @examples
#' \dontrun{
#' # Ensure SENSORTOWER_AUTH_TOKEN is set in your environment
#' # Sys.setenv(SENSORTOWER_AUTH_TOKEN = "your_secure_auth_token_here")
#'
#' # Basic usage - get publisher's apps
#' apps_list <- st_publisher_apps(unified_id = "647eb849d9d91f31a54f1792")
#'
#' # With regional SKU aggregation - ensures canonical unified_app_ids
#' apps_list <- st_publisher_apps(
#'   unified_id = "647eb849d9d91f31a54f1792",
#'   aggregate_related = TRUE
#' )
#'
#' # Then use with st_unified_sales_report() for accurate data
#' sales <- st_unified_sales_report(
#'   unified_app_id = apps_list$unified_app_id,
#'   countries = "WW",
#'   start_date = "2024-01-01",
#'   end_date = "2024-12-31",
#'   date_granularity = "monthly"
#' )
#' }
#'
#' @import dplyr
#' @importFrom httr GET add_headers stop_for_status content http_status
#'   http_error
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble as_tibble
#' @importFrom rlang abort warn `%||%`
#' @export
st_publisher_apps <- function(unified_id = NULL,
                              publisher_id = NULL,
                              aggregate_related = FALSE,
                              auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN"),
                              verbose = TRUE) {
  # --- Input Validation & Setup ---
  # Backward compatibility: support old signature
  if (is.null(unified_id) && !is.null(publisher_id)) unified_id <- publisher_id
  if (is.null(unified_id)) {
    rlang::abort("unified_id is required (24-char hex unified publisher or app id)")
  }
  if (!is.character(unified_id)) {
    rlang::abort("unified_id must be a character string")
  }
  if (length(unified_id) != 1) {
    rlang::abort("unified_id must be a single value, not a vector")
  }
  if (!nzchar(unified_id)) {
    rlang::abort("unified_id cannot be an empty string")
  }
  # Basic format check: 24-char hex
  if (!grepl("^[a-f0-9]{24}$", unified_id)) {
    rlang::abort(paste0(
      "Invalid unified_id format: '", unified_id, "'. Expected 24-char hex (unified publisher or app id)."
    ))
  }

  auth_token <- resolve_auth_token(
    auth_token,
    error_message = paste(
      "Authentication token is required.",
      "Set SENSORTOWER_AUTH_TOKEN environment variable",
      "or pass via auth_token argument."
    )
  )

  url <- paste0(st_api_base_url(), "/", st_endpoint_path("unified_publishers_apps"))

  query_params <- list(
    unified_id = unified_id,
    auth_token = auth_token
  )

  # --- API Call ---
  response <- tryCatch(
    httr::GET(
      url = url,
      query = query_params,
      httr::add_headers("Accept" = "application/json")
    ),
    error = function(e) {
      rlang::abort(paste("HTTP request failed:", e$message))
    }
  )

  # --- Response Handling ---
  status_code <- tryCatch(httr::status_code(response), error = function(e) NA_integer_)
  if (is.na(status_code) || status_code >= 400) {
    body_text <- tryCatch(httr::content(response, "text", encoding = "UTF-8"), error = function(e) "")
    msg <- paste0("API request failed (HTTP ", status_code, ")")
    if (nzchar(body_text)) {
      # Try extracting structured error
      parsed_err <- tryCatch(jsonlite::fromJSON(body_text), error = function(e) NULL)
      if (!is.null(parsed_err) && !is.null(parsed_err$error)) {
        msg <- paste0(msg, ": ", parsed_err$error)
      }
    }
    rlang::abort(msg)
  }

  content_text <- httr::content(response, "text", encoding = "UTF-8")
  if (content_text == "") {
    rlang::warn("API returned an empty response body.")
    return(tibble::tibble())
  }
  parsed_data <- jsonlite::fromJSON(content_text, flatten = TRUE)

  # --- Data Extraction & Processing ---
  if (is.null(parsed_data) || !"apps" %in% names(parsed_data)) {
    rlang::warn(
      "API response did not contain an 'apps' list for the given unified_id."
    )
    return(tibble::tibble())
  }

  apps_data <- parsed_data$apps

  if (length(apps_data) == 0) {
    message("No apps found associated with the publisher ID.")
    return(tibble::tibble())
  }

  result <- tibble::as_tibble(apps_data)


  # --- Aggregate Related SKUs ---
  if (aggregate_related && nrow(result) > 0) {
    if (verbose) {
      message("Resolving canonical unified_app_ids for ", nrow(result), " apps...")
    }

    # Get app names - handle different possible column names
    name_col <- intersect(c("unified_app_name", "name", "app_name"), names(result))[1]
    id_col <- intersect(c("unified_app_id", "app_id"), names(result))[1]

    if (is.na(name_col) || is.na(id_col)) {
      rlang::warn("Could not find name/id columns for aggregation. Returning raw results.")
      return(result)
    }

    # Look up each app by name to get canonical unified_app_id
    canonical_ids <- list()
    for (i in seq_len(nrow(result))) {
      app_name <- result[[name_col]][i]
      original_id <- result[[id_col]][i]

      # Search for the app by name
      search_result <- tryCatch({
        st_app_info(
          term = app_name,
          app_store = "unified",
          entity_type = "app",
          limit = 5,
          auth_token = auth_token,
          return_all_fields = FALSE
        )
      }, error = function(e) {
        if (verbose) message("  Warning: Could not search for '", app_name, "': ", e$message)
        NULL
      })

      # Find best match - prefer exact name match
      canonical_id <- original_id
      if (!is.null(search_result) && nrow(search_result) > 0) {
        # Try exact match first
        exact_match <- search_result %>%
          dplyr::filter(tolower(.data$unified_app_name) == tolower(app_name))

        if (nrow(exact_match) > 0) {
          canonical_id <- exact_match$unified_app_id[1]
        } else {
          # Use first result as best match
          canonical_id <- search_result$unified_app_id[1]
        }

        if (canonical_id != original_id && verbose) {
          message("  '", app_name, "': ", original_id, " -> ", canonical_id)
        }
      }

      canonical_ids[[i]] <- list(
        original_id = original_id,
        canonical_id = canonical_id,
        app_name = app_name
      )

      Sys.sleep(0.2)  # Rate limiting
    }

    # Update result with canonical IDs and preserve mapping
    canonical_df <- dplyr::bind_rows(canonical_ids)

    # Store original ID before replacing
    result$original_unified_app_id <- result[[id_col]]
    result[[id_col]] <- canonical_df$canonical_id

    # Add canonical_id column for apps that were remapped
    result$canonical_id <- ifelse(
      result$original_unified_app_id != result[[id_col]],
      result[[id_col]],
      NA_character_
    )

    # Deduplicate by canonical unified_app_id (keep first occurrence with name)
    n_before <- nrow(result)
    result <- result %>%
      dplyr::distinct(dplyr::across(dplyr::all_of(id_col)), .keep_all = TRUE)
    n_after <- nrow(result)

    if (verbose && n_before != n_after) {
      message("Deduplicated: ", n_before, " -> ", n_after, " unique apps")
    }

    # Create a canonical ID -> name lookup for use by other functions
    # This includes ALL canonical IDs (including those from remapped apps)
    name_lookup <- canonical_df %>%
      dplyr::select(unified_app_id = canonical_id, app_name) %>%
      dplyr::distinct(unified_app_id, .keep_all = TRUE)

    attr(result, "canonical_name_lookup") <- name_lookup

    if (verbose) {
      message("Aggregation complete. ", nrow(result), " apps with canonical IDs.")
    }
  }

  return(result)
}


#' Get App Names from Publisher Apps Result
#'
#' Helper function to create a name lookup table from the result of
#' `st_publisher_apps()`. This handles canonical ID mapping automatically,
#' so you can join sales data (which uses canonical IDs) back to app names.
#'
#' @param apps_df A tibble returned by `st_publisher_apps()`.
#' @param include_canonical Logical. If TRUE, includes mappings for canonical IDs
#'   that were resolved during aggregation. Defaults to TRUE.
#'
#' @return A tibble with columns `unified_app_id` and `app_name` suitable for
#'   joining with sales data or other API results.
#'
#' @examples
#' \dontrun{
#' # Get apps with canonical ID resolution
#' apps <- st_publisher_apps("647eb849d9d91f31a54f1792", aggregate_related = TRUE)
#'
#' # Get name lookup table
#' name_lookup <- st_get_app_names(apps)
#'
#' # Use with sales data
#' sales <- st_unified_sales_report(apps$unified_app_id, ...)
#' sales_with_names <- sales %>%
#'   left_join(name_lookup, by = "unified_app_id")
#' }
#'
#' @export
st_get_app_names <- function(apps_df, include_canonical = TRUE) {
  if (!is.data.frame(apps_df)) {
    rlang::abort("apps_df must be a data frame from st_publisher_apps()")
  }

  # Determine column names
  id_col <- intersect(c("unified_app_id", "app_id"), names(apps_df))[1]
  name_col <- intersect(c("unified_app_name", "name", "app_name"), names(apps_df))[1]


  if (is.na(id_col) || is.na(name_col)) {
    rlang::abort("Could not find unified_app_id and app_name columns in apps_df")
  }

  # Start with the main mapping
  name_lookup <- apps_df %>%
    dplyr::select(
      unified_app_id = dplyr::all_of(id_col),
      app_name = dplyr::all_of(name_col)
    ) %>%
    dplyr::distinct()

  # Add canonical ID mappings if available
  if (include_canonical) {
    canonical_lookup <- attr(apps_df, "canonical_name_lookup")
    if (!is.null(canonical_lookup) && nrow(canonical_lookup) > 0) {
      name_lookup <- dplyr::bind_rows(name_lookup, canonical_lookup) %>%
        dplyr::distinct(unified_app_id, .keep_all = TRUE)
    }
  }

  return(name_lookup)
}
