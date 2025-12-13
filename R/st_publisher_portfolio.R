#' Publisher Portfolio Analysis
#'
#' Fetches comprehensive portfolio data for a publisher including revenue,
#' downloads, MAU, and rankings. Returns a tidy data frame ready for
#' visualization or GT table creation.
#'
#' @param publisher Character. Publisher name to search for (e.g., "Lilith Games",
#'   "Supercell", "King"). The function will search for the publisher and use
#'   the first match.
#' @param publisher_id Character. Optional. If provided, skips the publisher
#'   search and uses this unified_publisher_id directly.
#' @param start_date Date or character. Start date for metrics (default: "2023-01-01").
#' @param end_date Date or character. End date for metrics (default: last day of
#'   previous month).
#' @param countries Character. Countries for metrics (default: "WW" for worldwide).
#' @param metrics Character vector. Which metrics to fetch. Options: "revenue",
#'   "downloads", "mau". Default: all three.
#' @param include_rankings Logical. Whether to fetch subgenre rankings from top
#'   charts. Default: TRUE.
#' @param include_portfolio_total Logical. Whether to add a portfolio total row.
#'   Default: TRUE.
#' @param granularity Character. How to aggregate the data: "yearly" (default),
#'   "quarterly", or "monthly".
#' @param min_revenue Numeric. Minimum revenue threshold to include an app.
#'   Default: 100000 (apps with at least $100K in any year).
#' @param auth_token Character. Sensor Tower API token. Defaults to
#'   SENSORTOWER_AUTH_TOKEN environment variable
#' @param verbose Logical. Print progress messages. Default: TRUE.
#' @param use_cache Logical. Use cached data if available. Default: TRUE.
#' @param cache_dir Character. Directory for cached data. Default: "data/".
#'
#' @return A tibble with portfolio data including:
#'   - app_name: Game name
#'   - subgenre: Game sub-genre
#'   - subgenre_rank: Rank within sub-genre
#'   - revenue_\{year\}: Revenue by year
#'   - downloads_\{year\}: Downloads by year
#'   - mau_\{year\}: Average MAU by year (if requested)
#'   - revenue_yoy, downloads_yoy, mau_yoy: Year-over-year growth percentages
#'
#' @examples
#' \dontrun{
#' # Simple usage - just provide publisher name
#' lilith_portfolio <- st_publisher_portfolio("Lilith Games")
#'
#' # Piped workflow
#' library(dplyr)
#'
#' "Supercell" %>%
#'   st_publisher_portfolio(
#'     start_date = "2023-01-01",
#'     metrics = c("revenue", "downloads")
#'   ) %>%
#'   filter(revenue_2024 > 1000000) %>%
#'   arrange(desc(revenue_2024))
#'
#' # Custom date range and countries
#' portfolio <- st_publisher_portfolio(
#'   publisher = "King",
#'   start_date = "2022-01-01",
#'   end_date = "2024-12-31",
#'   countries = c("US", "GB", "DE"),
#'   metrics = c("revenue", "downloads", "mau"),
#'   include_rankings = TRUE
#' )
#' }
#'
#' @import dplyr
#' @import tidyr
#' @importFrom lubridate year month floor_date days
#' @importFrom glue glue
#' @importFrom utils tail
#' @export
st_publisher_portfolio <- function(publisher = NULL,
                                    publisher_id = NULL,
                                    start_date = "2023-01-01",
                                    end_date = NULL,
                                    countries = "WW",
                                    metrics = c("revenue", "downloads", "mau"),
                                    include_rankings = TRUE,
                                    include_portfolio_total = TRUE,
                                    granularity = "yearly",
                                    min_revenue = 100000,
                                    auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN"),
                                    verbose = TRUE,
                                    use_cache = TRUE,
                                    cache_dir = "data/") {


  # --- Input Validation ---
  if (is.null(publisher) && is.null(publisher_id)) {
    rlang::abort("Either 'publisher' (name) or 'publisher_id' must be provided")
  }

  metrics <- tolower(metrics)
  valid_metrics <- c("revenue", "downloads", "mau")
  if (!all(metrics %in% valid_metrics)) {
    rlang::abort(glue::glue(
      "Invalid metrics: {paste(setdiff(metrics, valid_metrics), collapse = ', ')}. ",
      "Valid options: {paste(valid_metrics, collapse = ', ')}"
    ))
}

  granularity <- match.arg(granularity, c("yearly", "quarterly", "monthly"))

  # Default end_date to last complete month
  if (is.null(end_date)) {
    end_date <- lubridate::floor_date(Sys.Date(), "month") - lubridate::days(1)
  }
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  # Create cache directory if needed
  if (use_cache && !dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # --- Step 1: Find Publisher ---
  if (is.null(publisher_id)) {
    if (verbose) message("Step 1: Finding publisher '", publisher, "'...")

    publisher_search <- st_app_info(
      term = publisher,
      entity_type = "publisher",
      auth_token = auth_token
    )

    if (is.null(publisher_search) || nrow(publisher_search) == 0) {
      rlang::abort(glue::glue("Publisher '{publisher}' not found"))
    }

    publisher_id <- publisher_search$unified_publisher_id[1]
    publisher_name <- publisher_search$unified_publisher_name[1]

    if (verbose) message("  Found: ", publisher_name, " (", publisher_id, ")")
  } else {
    publisher_name <- publisher %||% "Unknown Publisher"
  }

  # --- Step 2: Get Publisher Apps ---
  if (verbose) message("\nStep 2: Fetching publisher apps...")

  cache_file_apps <- file.path(cache_dir, paste0(publisher_id, "_apps.rds"))

  if (use_cache && file.exists(cache_file_apps)) {
    if (verbose) message("  Loading cached apps...")
    apps <- readRDS(cache_file_apps)
  } else {
    apps <- st_publisher_apps(
      unified_id = publisher_id,
      aggregate_related = TRUE,
      auth_token = auth_token,
      verbose = verbose
    )

    if (use_cache && nrow(apps) > 0) {
      saveRDS(apps, cache_file_apps)
    }
  }

  if (nrow(apps) == 0) {
    rlang::warn("No apps found for publisher")
    return(tibble::tibble())
  }

  if (verbose) message("  Found ", nrow(apps), " apps")

  app_ids <- apps$unified_app_id

  # --- Step 3: Fetch Sales Data (Revenue & Downloads) ---
  sales_data <- NULL
  if (any(c("revenue", "downloads") %in% metrics)) {
    if (verbose) message("\nStep 3: Fetching sales data...")

    cache_file_sales <- file.path(
      cache_dir,
      paste0(publisher_id, "_sales_", format(start_date, "%Y%m%d"), "_",
             format(end_date, "%Y%m%d"), ".rds")
    )

    if (use_cache && file.exists(cache_file_sales)) {
      if (verbose) message("  Loading cached sales data...")
      sales_data <- readRDS(cache_file_sales)
    } else {
      # Fetch in batches
      batch_size <- 10
      n_batches <- ceiling(length(app_ids) / batch_size)
      sales_list <- list()

      for (i in seq_len(n_batches)) {
        start_idx <- (i - 1) * batch_size + 1
        end_idx <- min(i * batch_size, length(app_ids))
        batch_ids <- app_ids[start_idx:end_idx]

        if (verbose) message(glue::glue("  Batch {i}/{n_batches}: apps {start_idx}-{end_idx}"))

        result <- tryCatch({
          st_unified_sales_report(
            unified_app_id = batch_ids,
            countries = countries,
            start_date = start_date,
            end_date = end_date,
            date_granularity = "monthly",
            auth_token = auth_token,
            verbose = FALSE
          )
        }, error = function(e) {
          if (verbose) message("    Warning: ", e$message)
          NULL
        })

        if (!is.null(result) && nrow(result) > 0) {
          sales_list[[i]] <- result
        }

        Sys.sleep(0.3)  # Rate limiting
      }

      if (length(sales_list) > 0) {
        sales_data <- dplyr::bind_rows(sales_list)
        if (use_cache) saveRDS(sales_data, cache_file_sales)
      }
    }

    if (!is.null(sales_data)) {
      if (verbose) message("  Sales data: ", nrow(sales_data), " records")
    }
  }

  # --- Step 4: Fetch MAU Data ---
  mau_data <- NULL
  if ("mau" %in% metrics) {
    if (verbose) message("\nStep 4: Fetching MAU data...")

    cache_file_mau <- file.path(
      cache_dir,
      paste0(publisher_id, "_mau_", format(start_date, "%Y%m%d"), "_",
             format(end_date, "%Y%m%d"), ".rds")
    )

    if (use_cache && file.exists(cache_file_mau)) {
      if (verbose) message("  Loading cached MAU data...")
      mau_data <- readRDS(cache_file_mau)
    } else {
      mau_data <- tryCatch({
        st_batch_metrics(
          os = "unified",
          app_list = app_ids,
          metrics = "mau",
          date_range = list(start_date = start_date, end_date = end_date),
          countries = countries,
          granularity = "monthly",
          auth_token = auth_token
        )
      }, error = function(e) {
        if (verbose) message("  Warning: Could not fetch MAU: ", e$message)
        NULL
      })

      if (!is.null(mau_data) && nrow(mau_data) > 0 && use_cache) {
        saveRDS(mau_data, cache_file_mau)
      }
    }

    if (!is.null(mau_data)) {
      if (verbose) message("  MAU data: ", nrow(mau_data), " records")
    }
  }

  # --- Step 5: Fetch Rankings ---
  rank_data <- NULL
  if (include_rankings) {
    if (verbose) message("\nStep 5: Fetching rankings data...")

    cache_file_ranks <- file.path(cache_dir, "top_charts_rankings.rds")

    if (use_cache && file.exists(cache_file_ranks)) {
      file_age <- difftime(Sys.time(), file.info(cache_file_ranks)$mtime, units = "days")
      if (file_age < 1) {
        if (verbose) message("  Loading cached rankings...")
        rank_data <- readRDS(cache_file_ranks)
      }
    }

    if (is.null(rank_data)) {
      rank_data <- tryCatch({
        top_games <- st_top_charts(
          measure = "revenue",
          os = "unified",
          category = "6014",  # Games
          regions = "WW",
          time_range = "month",
          date = format(lubridate::floor_date(Sys.Date(), "month") - lubridate::days(1), "%Y-%m-%d"),
          limit = 1500,
          auth_token = auth_token
        )

        top_games %>%
          dplyr::mutate(global_rank = dplyr::row_number()) %>%
          dplyr::group_by(`aggregate_tags.Game Sub-genre`) %>%
          dplyr::mutate(subgenre_rank = dplyr::row_number()) %>%
          dplyr::ungroup() %>%
          dplyr::select(
            unified_app_id, unified_app_name, global_rank, subgenre_rank,
            subgenre = `aggregate_tags.Game Sub-genre`
          )
      }, error = function(e) {
        if (verbose) message("  Warning: Could not fetch rankings: ", e$message)
        NULL
      })

      if (!is.null(rank_data) && use_cache) {
        saveRDS(rank_data, cache_file_ranks)
      }
    }

    if (!is.null(rank_data)) {
      if (verbose) message("  Rankings data: ", nrow(rank_data), " apps")
    }
  }

  # --- Step 6: Build Portfolio Summary ---
  if (verbose) message("\nStep 6: Building portfolio summary...")

  # Get app name mapping
  app_names <- apps %>%
    dplyr::select(dplyr::any_of(c("unified_app_id", "unified_app_name"))) %>%
    dplyr::distinct()

  # If rank_data has more complete names, use those
  if (!is.null(rank_data)) {
    app_names <- dplyr::bind_rows(
      app_names,
      rank_data %>% dplyr::select(unified_app_id, unified_app_name)
    ) %>%
      dplyr::distinct(unified_app_id, .keep_all = TRUE)
  }

  # Determine current year and month for YTD filtering
  current_year <- lubridate::year(Sys.Date())
  current_month <- lubridate::month(Sys.Date()) - 1  # Last complete month
  if (current_month == 0) {
    current_month <- 12
    current_year <- current_year - 1
  }

  # Process sales data
  if (!is.null(sales_data) && nrow(sales_data) > 0) {
    sales_summary <- sales_data %>%
      dplyr::left_join(app_names, by = "unified_app_id") %>%
      dplyr::mutate(
        Year = lubridate::year(date),
        Month = lubridate::month(date)
      ) %>%
      # YTD filter: include full years for past years, YTD for current year
      dplyr::filter(Month <= current_month | Year < current_year) %>%
      dplyr::group_by(unified_app_name, Year) %>%
      dplyr::summarise(
        revenue = sum(revenue, na.rm = TRUE),
        downloads = sum(downloads, na.rm = TRUE),
        .groups = "drop"
      )

    # Pivot to wide format
    portfolio_data <- sales_summary %>%
      tidyr::pivot_wider(
        names_from = Year,
        values_from = c(revenue, downloads),
        names_sep = "_",
        values_fill = 0
      )
  } else {
    portfolio_data <- app_names %>%
      dplyr::select(unified_app_name) %>%
      dplyr::distinct()
  }

  # Add MAU data
  if (!is.null(mau_data) && nrow(mau_data) > 0) {
    mau_summary <- mau_data %>%
      dplyr::left_join(app_names, by = c("original_id" = "unified_app_id")) %>%
      dplyr::mutate(
        Year = lubridate::year(date),
        Month = lubridate::month(date)
      ) %>%
      dplyr::filter(Month <= current_month | Year < current_year) %>%
      dplyr::group_by(unified_app_name, Year) %>%
      dplyr::summarise(
        mau = mean(value, na.rm = TRUE),
        .groups = "drop"
      )

    mau_wide <- mau_summary %>%
      tidyr::pivot_wider(
        names_from = Year,
        values_from = mau,
        names_prefix = "mau_",
        values_fill = 0
      )

    portfolio_data <- portfolio_data %>%
      dplyr::left_join(mau_wide, by = "unified_app_name")
  }

  # Add rankings
  if (!is.null(rank_data)) {
    portfolio_data <- portfolio_data %>%
      dplyr::left_join(
        rank_data %>%
          dplyr::select(unified_app_name, subgenre_rank, subgenre) %>%
          dplyr::distinct(unified_app_name, .keep_all = TRUE),
        by = "unified_app_name"
      )
  }

  # Filter by minimum revenue
  years_in_data <- grep("^revenue_\\d{4}$", names(portfolio_data), value = TRUE)
  if (length(years_in_data) > 0) {
    portfolio_data <- portfolio_data %>%
      dplyr::filter(
        dplyr::if_any(dplyr::all_of(years_in_data), ~ . >= min_revenue)
      ) %>%
      dplyr::arrange(dplyr::desc(!!rlang::sym(tail(years_in_data, 1))))
  }

  # Calculate YoY growth
  if (length(years_in_data) >= 2) {
    latest_year <- max(as.numeric(gsub("revenue_", "", years_in_data)))
    prev_year <- latest_year - 1

    rev_latest <- paste0("revenue_", latest_year)
    rev_prev <- paste0("revenue_", prev_year)
    dl_latest <- paste0("downloads_", latest_year)
    dl_prev <- paste0("downloads_", prev_year)
    mau_latest <- paste0("mau_", latest_year)
    mau_prev <- paste0("mau_", prev_year)

    portfolio_data <- portfolio_data %>%
      dplyr::mutate(
        revenue_yoy = dplyr::if_else(
          !!rlang::sym(rev_prev) > 0,
          round((!!rlang::sym(rev_latest) - !!rlang::sym(rev_prev)) /
                  !!rlang::sym(rev_prev) * 100, 0),
          NA_real_
        )
      )

    if (dl_prev %in% names(portfolio_data) && dl_latest %in% names(portfolio_data)) {
      portfolio_data <- portfolio_data %>%
        dplyr::mutate(
          downloads_yoy = dplyr::if_else(
            !!rlang::sym(dl_prev) > 0,
            round((!!rlang::sym(dl_latest) - !!rlang::sym(dl_prev)) /
                    !!rlang::sym(dl_prev) * 100, 0),
            NA_real_
          )
        )
    }

    if (mau_prev %in% names(portfolio_data) && mau_latest %in% names(portfolio_data)) {
      portfolio_data <- portfolio_data %>%
        dplyr::mutate(
          mau_yoy = dplyr::if_else(
            !!rlang::sym(mau_prev) > 0,
            round((!!rlang::sym(mau_latest) - !!rlang::sym(mau_prev)) /
                    !!rlang::sym(mau_prev) * 100, 0),
            NA_real_
          )
        )
    }
  }

  # Add rank column
  portfolio_data <- portfolio_data %>%
    dplyr::mutate(rank = dplyr::row_number())

  # Add portfolio total row
  if (include_portfolio_total && nrow(portfolio_data) > 0) {
    numeric_cols <- names(portfolio_data)[sapply(portfolio_data, is.numeric)]
    numeric_cols <- setdiff(numeric_cols, c("rank", "subgenre_rank"))

    total_row <- portfolio_data %>%
      dplyr::summarise(
        dplyr::across(dplyr::all_of(numeric_cols), ~ sum(., na.rm = TRUE))
      ) %>%
      dplyr::mutate(
        unified_app_name = "PORTFOLIO TOTAL",
        rank = NA_integer_,
        subgenre = NA_character_,
        subgenre_rank = NA_integer_
      )

    # Recalculate YoY for total
    if ("revenue_yoy" %in% names(total_row) && length(years_in_data) >= 2) {
      latest_year <- max(as.numeric(gsub("revenue_", "", years_in_data)))
      prev_year <- latest_year - 1
      rev_latest <- paste0("revenue_", latest_year)
      rev_prev <- paste0("revenue_", prev_year)

      total_row <- total_row %>%
        dplyr::mutate(
          revenue_yoy = round((!!rlang::sym(rev_latest) - !!rlang::sym(rev_prev)) /
                                !!rlang::sym(rev_prev) * 100, 0)
        )

      if ("downloads_yoy" %in% names(total_row)) {
        dl_latest <- paste0("downloads_", latest_year)
        dl_prev <- paste0("downloads_", prev_year)
        total_row <- total_row %>%
          dplyr::mutate(
            downloads_yoy = round((!!rlang::sym(dl_latest) - !!rlang::sym(dl_prev)) /
                                    !!rlang::sym(dl_prev) * 100, 0)
          )
      }

      if ("mau_yoy" %in% names(total_row)) {
        mau_latest <- paste0("mau_", latest_year)
        mau_prev <- paste0("mau_", prev_year)
        if (mau_latest %in% names(total_row) && mau_prev %in% names(total_row)) {
          total_row <- total_row %>%
            dplyr::mutate(
              mau_yoy = round((!!rlang::sym(mau_latest) - !!rlang::sym(mau_prev)) /
                                !!rlang::sym(mau_prev) * 100, 0)
            )
        }
      }
    }

    portfolio_data <- dplyr::bind_rows(total_row, portfolio_data)
  }

  # Rename columns for cleaner output
  portfolio_data <- portfolio_data %>%
    dplyr::rename(app_name = unified_app_name)

  if (verbose) message("\nPortfolio summary complete: ", nrow(portfolio_data), " apps")

  # Add metadata as attributes
  attr(portfolio_data, "publisher") <- publisher_name

  attr(portfolio_data, "publisher_id") <- publisher_id
  attr(portfolio_data, "date_range") <- list(start = start_date, end = end_date)
  attr(portfolio_data, "countries") <- countries

  return(portfolio_data)
}
