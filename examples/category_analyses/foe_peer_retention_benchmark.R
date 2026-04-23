# Forge of Empires cohort retention benchmark
# Run from the SensorTowerR repo root.

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("The `pkgload` package is required to run this script.", call. = FALSE)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(tibble)
  library(tidyr)
})

if (!exists("target_app_name", inherits = FALSE)) {
  target_app_name <- "Forge of Empires: Build a City"
}
if (!exists("ranking_region", inherits = FALSE)) {
  ranking_region <- "US"
}
if (!exists("lookback_days", inherits = FALSE)) {
  lookback_days <- 90
}
if (!exists("benchmark_specs", inherits = FALSE)) {
  benchmark_specs <- list(
    empire_building = list(
      cohort_name = "Empire Building",
      field_name = "Game Theme",
      field_values = "Empire Building",
      field_global = TRUE
    ),
    build_and_battle = list(
      cohort_name = "Build & Battle",
      field_name = "DoF Sub-genre",
      field_values = "Build & Battle",
      field_global = FALSE
    )
  )
}
if (!exists("output_dir", inherits = FALSE)) {
  output_dir <- "outputs"
}
if (!exists("combined_output_path", inherits = FALSE)) {
  combined_output_path <- file.path(output_dir, "foe_peer_retention_combined.csv")
}

project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(project_root, "DESCRIPTION"))) {
  stop("Run this script from the SensorTowerR repo root.", call. = FALSE)
}

pkgload::load_all(".", quiet = TRUE)

abort_script <- function(message_text) {
  stop(message_text, call. = FALSE)
}

inform <- function(message_text) {
  cat(message_text, "\n", sep = "")
}

null_fallback <- function(x, fallback) {
  if (is.null(x) || length(x) == 0) {
    return(fallback)
  }

  x
}

normalize_scalar_character <- function(x, field_name) {
  if (is.null(x) || length(x) != 1 || is.na(x) || !nzchar(trimws(x))) {
    abort_script(sprintf("`%s` must be a single non-empty string.", field_name))
  }

  trimws(as.character(x))
}

normalize_character_vector <- function(x, field_name) {
  values <- trimws(as.character(x))
  values <- unique(values[!is.na(values) & nzchar(values)])

  if (length(values) == 0) {
    abort_script(sprintf("`%s` must contain at least one non-empty value.", field_name))
  }

  values
}

normalize_flag <- function(x, field_name) {
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    abort_script(sprintf("`%s` must be a single TRUE/FALSE value.", field_name))
  }

  x
}

resolve_output_path <- function(path, project_root) {
  if (grepl("^/", path)) {
    return(path)
  }

  file.path(project_root, path)
}

format_pct <- function(x) {
  ifelse(is.na(x), NA_character_, sprintf("%.1f%%", x * 100))
}

format_currency <- function(x) {
  ifelse(
    is.na(x),
    NA_character_,
    paste0("$", format(round(x, 0), big.mark = ",", scientific = FALSE))
  )
}

build_date_window <- function(lookback_days) {
  if (!is.numeric(lookback_days) || length(lookback_days) != 1 || is.na(lookback_days)) {
    abort_script("`lookback_days` must be a single numeric value.")
  }

  lookback_days <- as.integer(lookback_days)
  if (lookback_days < 1) {
    abort_script("`lookback_days` must be at least 1.")
  }

  ranking_end_date <- Sys.Date() - 1L
  ranking_start_date <- ranking_end_date - (lookback_days - 1L)

  list(
    ranking_start_date = ranking_start_date,
    ranking_end_date = ranking_end_date
  )
}

normalize_benchmark_spec <- function(spec, spec_name) {
  if (!is.list(spec)) {
    abort_script(sprintf("`benchmark_specs[[\"%s\"]]` must be a list.", spec_name))
  }

  cohort_id <- normalize_scalar_character(
    null_fallback(spec$cohort_id, spec_name),
    sprintf("benchmark_specs[[\"%s\"]]$cohort_id", spec_name)
  )
  cohort_name <- normalize_scalar_character(
    null_fallback(spec$cohort_name, str_replace_all(spec_name, "_", " ") %>% str_to_title()),
    sprintf("benchmark_specs[[\"%s\"]]$cohort_name", spec_name)
  )

  field_name_raw <- null_fallback(spec$field_name, spec$name)
  field_values_raw <- null_fallback(spec$field_values, spec$values)
  field_global_raw <- null_fallback(spec$field_global, spec$global)

  field_name <- normalize_scalar_character(
    field_name_raw,
    sprintf("benchmark_specs[[\"%s\"]]$field_name", spec_name)
  )
  field_values <- normalize_character_vector(
    field_values_raw,
    sprintf("benchmark_specs[[\"%s\"]]$field_values", spec_name)
  )
  field_global <- normalize_flag(
    field_global_raw,
    sprintf("benchmark_specs[[\"%s\"]]$field_global", spec_name)
  )

  list(
    cohort_id = cohort_id,
    cohort_name = cohort_name,
    cohort_label = paste(field_name, "=", paste(field_values, collapse = " | ")),
    field_name = field_name,
    field_values = field_values,
    field_global = field_global
  )
}

normalize_benchmark_specs <- function(benchmark_specs) {
  if (!is.list(benchmark_specs) || length(benchmark_specs) == 0) {
    abort_script("`benchmark_specs` must be a non-empty list of cohort definitions.")
  }

  purrr::imap(benchmark_specs, normalize_benchmark_spec)
}

pick_target_app <- function(app_name) {
  candidates <- suppressMessages(
    st_app_info(app_name, return_all_fields = FALSE, limit = 20)
  )

  if (nrow(candidates) == 0) {
    abort_script(sprintf("No app matches were found for `%s`.", app_name))
  }

  exact_match <- candidates %>%
    filter(tolower(unified_app_name) == tolower(app_name))

  if (nrow(exact_match) > 0) {
    return(slice_head(exact_match, n = 1))
  }

  partial_match <- candidates %>%
    filter(str_detect(tolower(unified_app_name), fixed(tolower(app_name))))

  if (nrow(partial_match) > 0) {
    return(slice_head(partial_match, n = 1))
  }

  slice_head(candidates, n = 1)
}

extract_enriched_tag <- function(enriched_tbl, field_name) {
  if (!field_name %in% names(enriched_tbl)) {
    return(NA_character_)
  }

  value <- enriched_tbl[[field_name]][1]
  if (is.null(value) || length(value) == 0 || is.na(value) || !nzchar(trimws(value))) {
    return(NA_character_)
  }

  trimws(as.character(value))
}

detect_target_taxonomy <- function(unified_app_id) {
  enriched <- suppressMessages(
    st_app_enriched(unified_app_id)
  )

  if (nrow(enriched) == 0) {
    abort_script("Target app enrichment returned no rows.")
  }

  tibble(
    detected_game_genre = extract_enriched_tag(enriched, "aggregate_tags.Game Genre"),
    detected_game_sub_genre = extract_enriched_tag(enriched, "aggregate_tags.Game Sub-genre"),
    detected_game_theme = extract_enriched_tag(enriched, "aggregate_tags.Game Theme"),
    detected_dof_sub_genre = extract_enriched_tag(enriched, "aggregate_tags.DoF Sub-genre"),
    detected_bg_sub_genre = extract_enriched_tag(enriched, "aggregate_tags.BG -Sub-genre"),
    detected_storefront_primary = extract_enriched_tag(enriched, "aggregate_tags.Storefront Game Subcategory"),
    detected_storefront_secondary = extract_enriched_tag(enriched, "aggregate_tags.Storefront Game Subcategory (Secondary)")
  )
}

coerce_filter_fields_tbl <- function(filter_details) {
  custom_fields <- filter_details$custom_fields

  if (is.data.frame(custom_fields)) {
    return(tibble::as_tibble(custom_fields))
  }

  if (!is.list(custom_fields) || length(custom_fields) == 0) {
    return(tibble(name = character(), global = logical(), values = list()))
  }

  tibble(
    name = map_chr(custom_fields, ~ null_fallback(.x$name, NA_character_)),
    global = map_lgl(custom_fields, ~ isTRUE(.x$global)),
    values = map(custom_fields, ~ as.character(null_fallback(.x$values, character())))
  )
}

validate_filter_details <- function(filter_id, benchmark_spec) {
  details <- tryCatch(
    suppressMessages(st_custom_fields_filter_by_id(filter_id)),
    error = function(error) {
      abort_script(
        sprintf(
          "Could not validate the `%s` filter (`%s`): %s",
          benchmark_spec$cohort_name,
          benchmark_spec$cohort_label,
          error$message
        )
      )
    }
  )

  custom_fields_tbl <- coerce_filter_fields_tbl(details)

  matching_field <- custom_fields_tbl %>%
    filter(name == benchmark_spec$field_name)

  if (nrow(matching_field) == 0) {
    abort_script(
      sprintf(
        "The created filter for `%s` did not retain field `%s`.",
        benchmark_spec$cohort_name,
        benchmark_spec$field_name
      )
    )
  }

  if (!any(matching_field$global == benchmark_spec$field_global)) {
    abort_script(
      sprintf(
        paste(
          "The created filter for `%s` did not preserve the expected global flag.",
          "Expected `%s = %s`."
        ),
        benchmark_spec$cohort_name,
        benchmark_spec$field_name,
        benchmark_spec$field_global
      )
    )
  }

  matching_values <- matching_field %>%
    filter(global == benchmark_spec$field_global) %>%
    pull(values) %>%
    unlist(use.names = FALSE) %>%
    as.character()

  missing_values <- setdiff(benchmark_spec$field_values, matching_values)
  if (length(missing_values) > 0) {
    abort_script(
      sprintf(
        paste(
          "The created filter for `%s` did not retain the expected values:",
          "%s."
        ),
        benchmark_spec$cohort_name,
        paste(missing_values, collapse = ", ")
      )
    )
  }

  invisible(filter_id)
}

create_validated_custom_field_filter <- function(benchmark_spec) {
  filter_id <- suppressWarnings(
    suppressMessages(
      st_custom_fields_filter(
        custom_fields = list(
          list(
            exclude = FALSE,
            global = benchmark_spec$field_global,
            name = benchmark_spec$field_name,
            values = as.list(benchmark_spec$field_values)
          )
        )
      )
    )
  )

  validate_filter_details(filter_id, benchmark_spec)
  filter_id
}

fetch_ranked_cohort <- function(filter_id,
                                ranking_region,
                                ranking_start_date,
                                ranking_end_date,
                                stop_when,
                                page_size = 200L,
                                max_pages = 15L) {
  pages <- list()
  offset <- 0L

  repeat {
    page_number <- length(pages) + 1L
    page <- suppressMessages(
      st_top_charts(
        os = "unified",
        measure = "revenue",
        custom_fields_filter_id = filter_id,
        custom_tags_mode = "include_unified_apps",
        category = 0,
        regions = ranking_region,
        date = ranking_start_date,
        end_date = ranking_end_date,
        time_range = "day",
        limit = page_size,
        offset = offset
      )
    )

    if (nrow(page) == 0) {
      break
    }

    pages[[page_number]] <- page

    combined <- bind_rows(pages) %>%
      distinct(unified_app_id, .keep_all = TRUE) %>%
      mutate(rank = row_number())

    if (stop_when(combined)) {
      return(combined)
    }

    if (nrow(page) < page_size || page_number >= max_pages) {
      return(combined)
    }

    offset <- offset + page_size
  }

  bind_rows(pages) %>%
    distinct(unified_app_id, .keep_all = TRUE) %>%
    mutate(rank = row_number())
}

cohort_stop_condition <- function(target_unified_app_id) {
  force(target_unified_app_id)

  function(cohort) {
    target_rank <- cohort %>%
      filter(unified_app_id == target_unified_app_id) %>%
      pull(rank)

    length(target_rank) == 1 &&
      !is.na(target_rank) &&
      any(cohort$rank == 10) &&
      any(cohort$rank > target_rank)
  }
}

select_cohort_slots <- function(cohort, target_unified_app_id, cohort_name) {
  target_row <- cohort %>%
    filter(unified_app_id == target_unified_app_id) %>%
    slice_head(n = 1)

  if (nrow(target_row) == 0) {
    abort_script(sprintf("The target app was not found in the `%s` cohort.", cohort_name))
  }

  cohort_leader <- cohort %>%
    filter(rank == 1) %>%
    slice_head(n = 1)

  if (nrow(cohort_leader) == 0) {
    abort_script(sprintf("The `%s` cohort returned no ranked apps.", cohort_name))
  }

  cohort_rank_10 <- cohort %>%
    filter(rank == 10) %>%
    slice_head(n = 1)

  if (nrow(cohort_rank_10) == 0) {
    abort_script(sprintf("The `%s` cohort has fewer than 10 ranked apps.", cohort_name))
  }

  below_target <- cohort %>%
    filter(rank > target_row$rank[1]) %>%
    slice_head(n = 1)

  if (nrow(below_target) == 0) {
    abort_script(sprintf("There is no lower-ranked peer below FoE in the `%s` cohort.", cohort_name))
  }

  bind_rows(
    target_row %>%
      mutate(slot = "target_app"),
    cohort_leader %>%
      mutate(slot = "cohort_leader"),
    below_target %>%
      mutate(slot = "below_target"),
    cohort_rank_10 %>%
      mutate(slot = "cohort_rank_10")
  ) %>%
    transmute(
      slot,
      unified_app_id,
      game = unified_app_name,
      revenue,
      cohort_rank = rank
    )
}

resolve_platform_ids <- function(unified_app_ids) {
  map_dfr(unique(unified_app_ids), function(unified_app_id) {
    lookup <- tryCatch(
      suppressMessages(st_app_lookup(unified_app_id)),
      error = function(error) {
        warning(
          sprintf("Could not resolve platform IDs for unified app `%s`: %s", unified_app_id, error$message),
          call. = FALSE
        )
        NULL
      }
    )

    if (is.null(lookup)) {
      return(
        tibble(
          unified_app_id = unified_app_id,
          ios_app_id = NA_character_,
          android_app_id = NA_character_,
          game = NA_character_
        )
      )
    }

    tibble(
      unified_app_id = lookup$unified_app_id,
      ios_app_id = null_fallback(lookup$ios_app_id, NA_character_),
      android_app_id = null_fallback(lookup$android_app_id, NA_character_),
      game = null_fallback(lookup$app_name, NA_character_)
    )
  })
}

build_platform_id_table <- function(selected_slots, id_lookup) {
  slot_ids <- selected_slots %>%
    select(slot, unified_app_id, game) %>%
    left_join(id_lookup, by = "unified_app_id", suffix = c("", "_lookup")) %>%
    mutate(game = coalesce(game, game_lookup)) %>%
    select(-game_lookup)

  missing_all_ids <- slot_ids %>%
    filter(is.na(ios_app_id) & is.na(android_app_id))

  if (nrow(missing_all_ids) > 0) {
    abort_script(
      sprintf(
        "No platform IDs were found for: %s.",
        paste(missing_all_ids$game, collapse = ", ")
      )
    )
  }

  missing_partial_ids <- slot_ids %>%
    filter(is.na(ios_app_id) | is.na(android_app_id)) %>%
    distinct(game)

  if (nrow(missing_partial_ids) > 0) {
    warning(
      sprintf(
        paste(
          "Some selected games are missing one mobile platform ID and will only",
          "return the available platform: %s."
        ),
        paste(missing_partial_ids$game, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  slot_ids %>%
    pivot_longer(
      cols = c(ios_app_id, android_app_id),
      names_to = "platform",
      values_to = "app_id"
    ) %>%
    mutate(platform = recode(platform, ios_app_id = "ios", android_app_id = "android")) %>%
    filter(!is.na(app_id), nzchar(app_id))
}

fetch_retention_snapshot <- function(platform_ids_tbl,
                                     ranking_region,
                                     ranking_start_date,
                                     ranking_end_date) {
  retention_regions <- if (toupper(ranking_region) == "WW") NULL else ranking_region
  requested_app_ids <- unique(platform_ids_tbl$app_id)

  retention <- suppressMessages(
    st_retention_facets(
      app_ids = requested_app_ids,
      bundle = "retention_daily",
      breakdown = c("date", "app_id"),
      start_date = ranking_start_date,
      end_date = ranking_end_date,
      regions = retention_regions
    )
  )

  if (nrow(retention) == 0) {
    abort_script("The retention facets pull returned no rows.")
  }

  available_app_ids <- unique(retention$app_id)
  missing_app_ids <- setdiff(requested_app_ids, available_app_ids)
  if (length(missing_app_ids) > 0) {
    warning(
      sprintf(
        "Retention data was missing for app IDs: %s. Those platform rows will be omitted.",
        paste(missing_app_ids, collapse = ", ")
      )
    ,
      call. = FALSE
    )
  }

  available_platform_ids_tbl <- platform_ids_tbl %>%
    filter(app_id %in% available_app_ids)

  missing_slot_games <- anti_join(
    platform_ids_tbl %>%
      distinct(slot, game),
    available_platform_ids_tbl %>%
      distinct(slot, game),
    by = c("slot", "game")
  )

  if (nrow(missing_slot_games) > 0) {
    abort_script(
      sprintf(
        paste(
          "Retention data was missing for every available platform ID for:",
          "%s."
        ),
        paste(missing_slot_games$game, collapse = ", ")
      )
    )
  }

  partial_missing_platforms <- anti_join(
    platform_ids_tbl %>%
      distinct(game, platform, app_id),
    available_platform_ids_tbl %>%
      distinct(game, platform, app_id),
    by = c("game", "platform", "app_id")
  )

  if (nrow(partial_missing_platforms) > 0) {
    warning(
      sprintf(
        paste(
          "Retention data was missing for some platform rows and they will be omitted:",
          "%s."
        ),
        paste(
          paste0(partial_missing_platforms$game, " (", partial_missing_platforms$platform, ")"),
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  latest_common_date <- retention %>%
    distinct(app_id, date) %>%
    count(date, name = "available_app_ids") %>%
    filter(available_app_ids == length(unique(available_platform_ids_tbl$app_id))) %>%
    summarise(latest_common_date = max(date)) %>%
    pull(latest_common_date)

  if (length(latest_common_date) != 1 || is.na(latest_common_date)) {
    abort_script("No common retention cohort date exists across the requested platform app IDs.")
  }

  retention_cols <- paste0("est_retention_d", 1:7)

  snapshot <- retention %>%
    filter(date == latest_common_date) %>%
    select(date, app_id, all_of(retention_cols))

  snapshot_app_ids <- unique(snapshot$app_id)
  missing_latest_app_ids <- setdiff(unique(available_platform_ids_tbl$app_id), snapshot_app_ids)

  if (length(missing_latest_app_ids) > 0) {
    abort_script(
      sprintf(
        "The latest common retention date still dropped app IDs: %s.",
        paste(missing_latest_app_ids, collapse = ", ")
      )
    )
  }

  list(
    platform_ids_tbl = available_platform_ids_tbl,
    retention_snapshot = snapshot %>%
      rename(retention_date = date) %>%
      rename_with(
        .fn = ~ str_replace(.x, "^est_retention_", "") %>%
          paste0("_retention"),
        .cols = starts_with("est_retention_")
      )
  )
}

assemble_output <- function(selected_slots,
                            platform_ids_tbl,
                            retention_snapshot,
                            benchmark_spec,
                            ranking_region,
                            ranking_start_date,
                            ranking_end_date) {
  cohort_value_label <- paste(benchmark_spec$field_values, collapse = " | ")
  cohort_scope_label <- if (benchmark_spec$field_global) {
    "global_custom_field"
  } else {
    "organization_custom_field"
  }

  selected_slots %>%
    select(slot, unified_app_id, game, cohort_rank) %>%
    left_join(
      platform_ids_tbl %>%
        select(slot, unified_app_id, game, platform, app_id),
      by = c("slot", "unified_app_id", "game")
    ) %>%
    left_join(retention_snapshot, by = "app_id") %>%
    mutate(
      cohort_id = benchmark_spec$cohort_id,
      cohort_name = benchmark_spec$cohort_name,
      cohort_label = benchmark_spec$cohort_label,
      cohort_field = benchmark_spec$field_name,
      cohort_value = cohort_value_label,
      cohort_global = benchmark_spec$field_global,
      cohort_scope_label = cohort_scope_label,
      ranking_measure = "revenue",
      leader_scope = "top_in_cohort",
      ranking_region = ranking_region,
      ranking_start_date = ranking_start_date,
      ranking_end_date = ranking_end_date
    ) %>%
    select(
      slot,
      cohort_id,
      cohort_name,
      cohort_label,
      cohort_field,
      cohort_value,
      cohort_global,
      cohort_scope_label,
      ranking_measure,
      leader_scope,
      ranking_region,
      ranking_start_date,
      ranking_end_date,
      retention_date,
      unified_app_id,
      game,
      platform,
      cohort_rank,
      d1_retention,
      d2_retention,
      d3_retention,
      d4_retention,
      d5_retention,
      d6_retention,
      d7_retention
    ) %>%
    arrange(
      factor(slot, levels = c("target_app", "cohort_leader", "below_target", "cohort_rank_10")),
      factor(platform, levels = c("ios", "android"))
    )
}

print_cohort_summary <- function(benchmark_spec, final_table, output_path) {
  retention_cols <- paste0("d", 1:7, "_retention")

  printable_table <- final_table %>%
    mutate(across(all_of(retention_cols), format_pct))

  inform(sprintf("Completed cohort: %s", benchmark_spec$cohort_label))
  inform(sprintf("Output written to: %s", output_path))
  inform("Selected slots:")
  print(
    final_table %>%
      distinct(slot, game, cohort_rank) %>%
      arrange(match(slot, c("target_app", "cohort_leader", "below_target", "cohort_rank_10"))),
    n = Inf
  )
  inform("Retention snapshot:")
  print(printable_table, n = nrow(printable_table), width = Inf)
  inform("")
}

run_cohort_benchmark <- function(benchmark_spec,
                                 target_row,
                                 ranking_region,
                                 ranking_start_date,
                                 ranking_end_date,
                                 output_dir) {
  inform(sprintf("Creating filter for %s...", benchmark_spec$cohort_label))
  filter_id <- create_validated_custom_field_filter(benchmark_spec)

  inform(sprintf("Fetching ranked cohort for %s...", benchmark_spec$cohort_name))
  ranked_cohort <- fetch_ranked_cohort(
    filter_id = filter_id,
    ranking_region = ranking_region,
    ranking_start_date = ranking_start_date,
    ranking_end_date = ranking_end_date,
    stop_when = cohort_stop_condition(target_row$unified_app_id[1])
  )

  selected_slots <- select_cohort_slots(
    cohort = ranked_cohort,
    target_unified_app_id = target_row$unified_app_id[1],
    cohort_name = benchmark_spec$cohort_name
  )

  platform_id_lookup <- resolve_platform_ids(selected_slots$unified_app_id)
  platform_ids_tbl <- build_platform_id_table(selected_slots, platform_id_lookup)

  inform(sprintf("Fetching retention facets for %s...", benchmark_spec$cohort_name))
  retention_result <- fetch_retention_snapshot(
    platform_ids_tbl = platform_ids_tbl,
    ranking_region = ranking_region,
    ranking_start_date = ranking_start_date,
    ranking_end_date = ranking_end_date
  )
  platform_ids_tbl <- retention_result$platform_ids_tbl
  retention_snapshot <- retention_result$retention_snapshot

  final_table <- assemble_output(
    selected_slots = selected_slots,
    platform_ids_tbl = platform_ids_tbl,
    retention_snapshot = retention_snapshot,
    benchmark_spec = benchmark_spec,
    ranking_region = ranking_region,
    ranking_start_date = ranking_start_date,
    ranking_end_date = ranking_end_date
  )

  output_path <- file.path(
    output_dir,
    paste0("foe_peer_retention_", benchmark_spec$cohort_id, ".csv")
  )

  write_csv(final_table, output_path)
  print_cohort_summary(benchmark_spec, final_table, output_path)

  list(
    benchmark_spec = benchmark_spec,
    benchmark_tbl = final_table,
    output_path = output_path
  )
}

print_overall_summary <- function(target_row,
                                  target_taxonomy,
                                  results,
                                  combined_output_path) {
  taxonomy_summary <- target_taxonomy %>%
    pivot_longer(everything(), names_to = "field", values_to = "value") %>%
    filter(!is.na(value), nzchar(value))

  inform("FoE cohort retention benchmark complete.")
  inform(sprintf("Target app: %s", target_row$unified_app_name[1]))
  inform("Detected FoE taxonomy tags:")
  print(taxonomy_summary, n = nrow(taxonomy_summary))
  inform("")
  inform("Per-cohort CSV outputs:")
  print(
    tibble(
      cohort = map_chr(results, ~ .x$benchmark_spec$cohort_label),
      output_path = map_chr(results, "output_path")
    ),
    n = length(results)
  )
  inform(sprintf("Combined output written to: %s", combined_output_path))
}

ranking_region <- toupper(normalize_scalar_character(ranking_region, "ranking_region"))
target_app_name <- normalize_scalar_character(target_app_name, "target_app_name")
benchmark_specs <- normalize_benchmark_specs(benchmark_specs)
date_window <- build_date_window(lookback_days)
ranking_start_date <- date_window$ranking_start_date
ranking_end_date <- date_window$ranking_end_date
output_dir <- resolve_output_path(output_dir, project_root)
combined_output_path <- resolve_output_path(combined_output_path, project_root)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(combined_output_path), recursive = TRUE, showWarnings = FALSE)

inform("Resolving target app...")
target_row <- pick_target_app(target_app_name)
target_taxonomy <- detect_target_taxonomy(target_row$unified_app_id[1])

results <- map(
  benchmark_specs,
  run_cohort_benchmark,
  target_row = target_row,
  ranking_region = ranking_region,
  ranking_start_date = ranking_start_date,
  ranking_end_date = ranking_end_date,
  output_dir = output_dir
)

combined_table <- bind_rows(map(results, "benchmark_tbl")) %>%
  arrange(
    factor(cohort_id, levels = map_chr(benchmark_specs, "cohort_id")),
    factor(slot, levels = c("target_app", "cohort_leader", "below_target", "cohort_rank_10")),
    factor(platform, levels = c("ios", "android"))
  )

write_csv(combined_table, combined_output_path)

print_overall_summary(
  target_row = target_row,
  target_taxonomy = target_taxonomy,
  results = results,
  combined_output_path = combined_output_path
)

invisible(combined_table)
