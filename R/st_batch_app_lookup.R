#' Batch Look up app information
#'
#' This function takes a vector of unified app IDs and returns a single
#' data frame containing the corresponding unified_app_id, app_name,
#' platform-specific IDs (ios_app_id, android_app_id), and icon_url.
#' It handles chunking requests to the st_app_details endpoint (limit 100).
#'
#' @param app_ids Character vector of unified app IDs.
#' @param auth_token Character string. Your Sensor Tower API authentication token.
#' @param verbose Logical. Whether to show progress messages. Default is FALSE.
#'
#' @return A \code{tibble} with columns:
#'   \itemize{
#'     \item \code{unified_app_id}: The Sensor Tower unified app ID
#'     \item \code{app_name}: The app's display name
#'     \item \code{ios_app_id}: iOS app ID if found
#'     \item \code{android_app_id}: Android app ID if found
#'     \item \code{icon_url}: URL of the app icon
#'   }
#'   Returns empty tibble if no apps can be found or details cannot be fetched.
#'
#' @examples
#' \dontrun{
#' # Look up multiple games
#' apps <- st_batch_app_lookup(c("5ba4585f539ce75b97db6bcb", "5d10d6bfecb4db5f93902f23"))
#' }
#'
#' @importFrom dplyr select mutate left_join bind_rows distinct coalesce rename transmute
#' @importFrom purrr map_df
#' @importFrom tibble tibble
#' @export
st_batch_app_lookup <- function(app_ids,
                                auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN"),
                                verbose = FALSE) {
    if (length(app_ids) == 0) {
        return(tibble::tibble(
            unified_app_id = character(),
            app_name = character(),
            ios_app_id = character(),
            android_app_id = character(),
            icon_url = character()
        ))
    }

    app_ids <- unique(app_ids[!is.na(app_ids) & nzchar(app_ids)])

    if (length(app_ids) == 0) {
        return(tibble::tibble(
            unified_app_id = character(),
            app_name = character(),
            ios_app_id = character(),
            android_app_id = character(),
            icon_url = character()
        ))
    }

    auth_token <- resolve_auth_token(
        auth_token,
        error_message = "Authentication token is required. Set SENSORTOWER_AUTH_TOKEN environment variable."
    )

    # Helper to chunk requests to st_app_details
    chunk_vector <- function(x, size) {
        split(x, ceiling(seq_along(x) / size))
    }

    id_chunks <- chunk_vector(app_ids, 100)

    if (verbose) message("Fetching details for ", length(app_ids), " unified apps in ", length(id_chunks), " batches.")

    details_list <- lapply(id_chunks, function(chunk) {
        tryCatch(
            {
                st_app_details(
                    app_ids = chunk,
                    os = "unified",
                    include_developer_contacts = FALSE,
                    auth_token = auth_token
                )
            },
            error = function(e) {
                if (verbose) message("Batch detail fetch failed: ", e$message)
                tibble::tibble()
            }
        )
    })

    combined_details <- dplyr::bind_rows(details_list)

    if (nrow(combined_details) == 0) {
        return(tibble::tibble(
            unified_app_id = character(),
            app_name = character(),
            ios_app_id = character(),
            android_app_id = character(),
            icon_url = character()
        ))
    }

    # Ensure all desired columns exist
    res <- combined_details
    if (!"app_id" %in% names(res)) res$app_id <- NA_character_
    if (!"app_name" %in% names(res)) res$app_name <- NA_character_
    if (!"ios_app_id" %in% names(res)) res$ios_app_id <- NA_character_
    if (!"android_app_id" %in% names(res)) res$android_app_id <- NA_character_
    if (!"icon_url" %in% names(res)) res$icon_url <- NA_character_

    res <- res %>%
        dplyr::transmute(
            unified_app_id = as.character(.data$app_id),
            app_name = as.character(.data$app_name),
            ios_app_id = as.character(.data$ios_app_id),
            android_app_id = as.character(.data$android_app_id),
            icon_url = as.character(.data$icon_url)
        )

    return(res)
}
