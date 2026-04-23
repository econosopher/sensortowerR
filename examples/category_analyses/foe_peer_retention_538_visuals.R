# FoE cohort-retention visuals
# Run from the SensorTowerR repo root.

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("The `pkgload` package is required to run this script.", call. = FALSE)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(glue)
  library(gt)
  library(purrr)
  library(readr)
  library(scales)
  library(stringr)
  library(tidyr)
})

if (!exists("benchmark_csv", inherits = FALSE)) {
  benchmark_csv <- file.path("outputs", "foe_peer_retention_combined.csv")
}
if (!exists("output_dir", inherits = FALSE)) {
  output_dir <- "outputs"
}
if (!exists("render_separate_outputs", inherits = FALSE)) {
  render_separate_outputs <- TRUE
}
if (!exists("render_combined_output", inherits = FALSE)) {
  render_combined_output <- FALSE
}
if (!exists("render_actual_vs_st_chart", inherits = FALSE)) {
  render_actual_vs_st_chart <- TRUE
}
if (!exists("line_chart_path", inherits = FALSE)) {
  line_chart_path <- NULL
}
if (!exists("table_png_path", inherits = FALSE)) {
  table_png_path <- NULL
}
if (!exists("table_html_path", inherits = FALSE)) {
  table_html_path <- NULL
}
if (!exists("actual_csv_path", inherits = FALSE)) {
  actual_csv_path <- "/Users/phillip/Library/CloudStorage/GoogleDrive-pblack@gameeconomistconsulting.com/My Drive/Client Folders/InnoGames/Shared InnoGames/FoE Data/FoE Session Data Feb-Mar 2026.csv"
}
if (!exists("actual_vs_st_chart_path", inherits = FALSE)) {
  actual_vs_st_chart_path <- NULL
}
if (!exists("actual_vs_st_simple_chart_path", inherits = FALSE)) {
  actual_vs_st_simple_chart_path <- NULL
}

project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(project_root, "DESCRIPTION"))) {
  stop("Run this script from the SensorTowerR repo root.", call. = FALSE)
}

resolve_output_path <- function(path, project_root) {
  if (is.null(path)) {
    return(NULL)
  }

  if (grepl("^/", path)) {
    return(path)
  }

  file.path(project_root, path)
}

ensure_benchmark_exists <- function(csv_path) {
  if (file.exists(csv_path)) {
    return(invisible(csv_path))
  }

  benchmark_script <- file.path(
    project_root,
    "examples",
    "category_analyses",
    "foe_peer_retention_benchmark.R"
  )

  if (!file.exists(benchmark_script)) {
    stop("Benchmark CSV is missing and the benchmark script could not be found.", call. = FALSE)
  }

  sys.source(benchmark_script, envir = new.env(parent = globalenv()))

  if (!file.exists(csv_path)) {
    stop("Benchmark script ran but did not produce the expected CSV.", call. = FALSE)
  }
}

ensure_package_loaded <- function() {
  pkgload::load_all(project_root, quiet = TRUE)
}

slot_order <- c("target_app", "cohort_leader", "below_target", "cohort_rank_10")
checkpoint_days <- c(1L, 2L, 3L, 7L)
checkpoint_day_columns <- paste0("d", checkpoint_days, "_retention")
slot_labels <- c(
  target_app = "Forge of Empires",
  cohort_leader = "Cohort Leader",
  below_target = "Next Below FoE",
  cohort_rank_10 = "Cohort #10"
)
slot_palette <- c(
  target_app = "#FF5A1F",
  cohort_leader = "#5F6B7A",
  below_target = "#8A97A5",
  cohort_rank_10 = "#B9C2CB"
)
slot_linewidths <- c(
  target_app = 1.45,
  cohort_leader = 1.00,
  below_target = 1.00,
  cohort_rank_10 = 1.00
)
slot_point_sizes <- c(
  target_app = 2.5,
  cohort_leader = 1.8,
  below_target = 1.8,
  cohort_rank_10 = 1.8
)
foe_compare_palette <- c(
  foe_actual = "#FF5A1F",
  foe_st_us = "#CC3D00",
  foe_st_ww = "#FF9A73"
)
foe_compare_linewidths <- c(
  foe_actual = 1.65,
  foe_st_us = 1.35,
  foe_st_ww = 1.20
)
foe_compare_point_sizes <- c(
  foe_actual = 2.7,
  foe_st_us = 2.2,
  foe_st_ww = 2.0
)

theme_538_retention <- function() {
  theme_minimal(base_family = "Helvetica") +
    theme(
      plot.background = element_rect(fill = "#F7F7F7", color = NA),
      panel.background = element_rect(fill = "#F7F7F7", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "#D9D9D9", linewidth = 0.3),
      panel.grid.major.y = element_line(color = "#E5E5E5", linewidth = 0.3),
      strip.background = element_rect(fill = "#EAEAEA", color = NA),
      strip.text = element_text(face = "bold", color = "#222222"),
      axis.title = element_text(color = "#222222"),
      axis.text = element_text(color = "#444444"),
      plot.title = element_text(face = "bold", size = 16, color = "#222222", hjust = 0),
      plot.subtitle = element_text(size = 10.5, color = "#6B6B6B", hjust = 0),
      plot.caption = element_text(size = 7.6, color = "#7A7A7A", hjust = 0, lineheight = 1.1),
      plot.caption.position = "plot",
      legend.position = "bottom",
      legend.box = "vertical",
      legend.title = element_blank(),
      legend.text = element_text(color = "#333333", size = 8.4),
      legend.key.width = grid::unit(18, "pt"),
      plot.margin = margin(t = 10, r = 15, b = 34, l = 10)
    )
}

format_platform <- function(x) {
  recode(x, ios = "iOS", android = "Android")
}

format_slot_label <- function(slot, game) {
  case_when(
    slot == "target_app" ~ "Forge of Empires",
    TRUE ~ glue("{recode(slot, !!!slot_labels)} ({game})")
  )
}

format_cohort_display <- function(cohort_field, cohort_value) {
  glue("{cohort_field}\n{cohort_value}")
}

format_cohort_short_label <- function(cohort_field, cohort_value) {
  case_when(
    cohort_field == "Game Theme" ~ "Empire Building",
    cohort_field == "DoF Sub-genre" ~ "Build & Battle",
    TRUE ~ cohort_value
  )
}

compress_source_note <- function(lines) {
  paste(lines[nzchar(lines)], collapse = "\n")
}

format_peer_source_note <- function(benchmark_tbl) {
  cohort_summary <- benchmark_tbl %>%
    distinct(cohort_field, cohort_value, retention_date) %>%
    mutate(cohort_text = glue("{cohort_field} = {cohort_value} ({retention_date})")) %>%
    pull(cohort_text) %>%
    paste(collapse = " | ")

  ranking_days <- benchmark_tbl %>%
    summarise(days = as.integer(max(ranking_end_date) - min(ranking_start_date)) + 1L) %>%
    pull(days)

  compress_source_note(c(
    "Source: Sensor Tower via sensortowerR",
    glue("US mobile estimates; peers ranked by {ranking_days}-day US revenue"),
    glue("Cohorts and latest common retention dates: {cohort_summary}")
  ))
}

format_actual_peer_source_note <- function(foe_us_date, foe_ww_date, actual_csv_path) {
  actual_file_label <- basename(actual_csv_path)

  compress_source_note(c(
    "Source: Sensor Tower via sensortowerR and InnoGames actual session data",
    glue("Peers are Sensor Tower US mobile estimates; FoE includes Sensor Tower US ({foe_us_date}) and WW ({foe_ww_date})"),
    glue("The first-party FoE line is `% players reached session` from `{actual_file_label}` and should be read as session progression, not day retention")
  ))
}

format_actual_only_source_note <- function(foe_us_date, foe_ww_date, actual_csv_path) {
  actual_file_label <- basename(actual_csv_path)

  compress_source_note(c(
    "Source: Sensor Tower via sensortowerR and InnoGames actual session data",
    glue("Forge of Empires includes Sensor Tower US ({foe_us_date}) and WW ({foe_ww_date}) mobile estimates"),
    glue("The first-party FoE line is `% players reached session` from `{actual_file_label}` and should be read as session progression, not day retention")
  ))
}

format_snapshot_table_source_note <- function(actual_csv_path) {
  compress_source_note(c(
    "Source: Sensor Tower via sensortowerR",
    "Peer rows, including `Forge of Empires (Sensor Tower estimate)`, are Sensor Tower US mobile estimates",
    "First-party FoE session-progression data is shown separately in the actual-vs-Sensor Tower charts, not in this D1-D7 snapshot table"
  ))
}

build_cohort_metadata <- function(benchmark_tbl) {
  benchmark_tbl %>%
    distinct(cohort_id, cohort_name, cohort_field, cohort_value) %>%
    mutate(
      cohort_display = format_cohort_display(cohort_field, cohort_value),
      cohort_short_label = format_cohort_short_label(cohort_field, cohort_value)
    )
}

build_peer_series_metadata <- function(chart_tbl, combined = FALSE) {
  chart_tbl %>%
    distinct(cohort_id, cohort_short_label, slot, game) %>%
    arrange(match(slot, slot_order), cohort_short_label, game) %>%
    mutate(
      series_key = if_else(
        slot == "target_app" & !combined,
        "foe_target",
        paste(cohort_id, slot, sep = "__")
      ),
      series_label = case_when(
        slot == "target_app" & !combined ~ "Forge of Empires",
        combined ~ glue("{cohort_short_label}: {format_slot_label(slot, game)}"),
        TRUE ~ format_slot_label(slot, game)
      ) %>% str_wrap(width = if_else(combined, 34L, 42L))
    )
}

prepare_peer_chart_tbl <- function(benchmark_tbl, combined = FALSE) {
  cohort_metadata <- build_cohort_metadata(benchmark_tbl)

  benchmark_tbl %>%
    left_join(cohort_metadata, by = c("cohort_id", "cohort_name", "cohort_field", "cohort_value")) %>%
    mutate(
      slot = factor(slot, levels = slot_order),
      platform_display = factor(format_platform(platform), levels = c("iOS", "Android"))
    ) %>%
    pivot_longer(
      cols = starts_with("d"),
      names_to = "retention_day",
      values_to = "retention"
    ) %>%
    mutate(day_number = readr::parse_number(retention_day))
}

build_peer_scales <- function(chart_tbl, combined = FALSE) {
  series_tbl <- build_peer_series_metadata(chart_tbl, combined = combined)

  list(
    color_values = setNames(slot_palette[series_tbl$slot], series_tbl$series_key),
    linewidth_values = setNames(slot_linewidths[series_tbl$slot], series_tbl$series_key),
    size_values = setNames(slot_point_sizes[series_tbl$slot], series_tbl$series_key),
    label_values = setNames(series_tbl$series_label, series_tbl$series_key)
  )
}

attach_peer_series_keys <- function(chart_tbl, combined = FALSE) {
  chart_tbl %>%
    left_join(
      build_peer_series_metadata(chart_tbl, combined = combined) %>%
        select(cohort_id, slot, game, series_key),
      by = c("cohort_id", "slot", "game")
    )
}

build_single_cohort_line_chart <- function(benchmark_tbl) {
  chart_tbl <- prepare_peer_chart_tbl(benchmark_tbl, combined = FALSE) %>%
    attach_peer_series_keys(combined = FALSE)
  series_scales <- build_peer_scales(chart_tbl, combined = FALSE)
  ranking_region <- unique(benchmark_tbl$ranking_region)
  cohort_name <- unique(benchmark_tbl$cohort_name)
  cohort_label <- unique(benchmark_tbl$cohort_label)
  retention_date <- unique(benchmark_tbl$retention_date)

  ggplot(chart_tbl, aes(day_number, retention, color = series_key, group = interaction(series_key, platform_display))) +
    geom_line(aes(linewidth = series_key)) +
    geom_point(aes(size = series_key)) +
    facet_wrap(~ platform_display) +
    scale_color_manual(values = series_scales$color_values, labels = series_scales$label_values) +
    scale_linewidth_manual(values = series_scales$linewidth_values, guide = "none") +
    scale_size_manual(values = series_scales$size_values, guide = "none") +
    scale_x_continuous(
      breaks = 1:7,
      labels = paste0("D", 1:7),
      expand = expansion(mult = c(0.02, 0.03))
    ) +
    scale_y_continuous(
      labels = label_percent(accuracy = 1),
      expand = expansion(mult = c(0.02, 0.08))
    ) +
    labs(
      title = glue("Forge of Empires retention against the {cohort_name} peer set"),
      subtitle = glue("{ranking_region} revenue-ranked cohort over the last 90 days | {cohort_label} | Latest common retention cohort: {retention_date}"),
      x = NULL,
      y = "Retention Rate",
      caption = format_peer_source_note(benchmark_tbl)
    ) +
    guides(color = guide_legend(ncol = 2, byrow = TRUE, override.aes = list(linewidth = 1.3, size = 2.3))) +
    theme_538_retention()
}

build_combined_line_chart <- function(benchmark_tbl) {
  chart_tbl <- prepare_peer_chart_tbl(benchmark_tbl, combined = TRUE) %>%
    attach_peer_series_keys(combined = TRUE)
  series_scales <- build_peer_scales(chart_tbl, combined = TRUE)
  ranking_region <- unique(benchmark_tbl$ranking_region)
  cohort_metadata <- build_cohort_metadata(benchmark_tbl)

  ggplot(chart_tbl, aes(day_number, retention, color = series_key, group = interaction(series_key, cohort_id, platform_display))) +
    geom_line(aes(linewidth = series_key)) +
    geom_point(aes(size = series_key)) +
    facet_grid(
      factor(cohort_display, levels = cohort_metadata$cohort_display) ~ platform_display
    ) +
    scale_color_manual(values = series_scales$color_values, labels = series_scales$label_values) +
    scale_linewidth_manual(values = series_scales$linewidth_values, guide = "none") +
    scale_size_manual(values = series_scales$size_values, guide = "none") +
    scale_x_continuous(
      breaks = 1:7,
      labels = paste0("D", 1:7),
      expand = expansion(mult = c(0.02, 0.03))
    ) +
    scale_y_continuous(
      labels = label_percent(accuracy = 1),
      expand = expansion(mult = c(0.02, 0.08))
    ) +
    labs(
      title = "Forge of Empires retention across selected peer cohorts",
      subtitle = glue("{ranking_region} revenue-ranked cohorts over the last 90 days | Rows split by cohort and columns split mobile platform"),
      x = NULL,
      y = "Retention Rate",
      caption = format_peer_source_note(benchmark_tbl)
    ) +
    guides(color = guide_legend(ncol = 3, byrow = TRUE, override.aes = list(linewidth = 1.2, size = 2.1))) +
    theme_538_retention()
}

build_generic_retention_gt <- function(benchmark_tbl) {
  single_cohort <- dplyr::n_distinct(benchmark_tbl$cohort_id) == 1
  column_labels <- list(
    slot_label = "Slot",
    game = "Game",
    platform_display = "Platform",
    d1_retention = "D1",
    d2_retention = "D2",
    d3_retention = "D3",
    d4_retention = "D4",
    d5_retention = "D5",
    d6_retention = "D6",
    d7_retention = "D7"
  )

  table_tbl <- benchmark_tbl %>%
    mutate(
      slot = factor(slot, levels = slot_order),
      slot_label = format_slot_label(as.character(slot), game),
      platform_display = format_platform(platform)
    ) %>%
    select(
      cohort_name,
      slot,
      slot_label,
      game,
      platform_display,
      d1_retention,
      d2_retention,
      d3_retention,
      d4_retention,
      d5_retention,
      d6_retention,
      d7_retention
    ) %>%
    arrange(cohort_name, slot, platform_display)

  if (single_cohort) {
    table_tbl <- table_tbl %>% select(-cohort_name)
  } else {
    column_labels <- c(list(cohort_name = "Cohort"), column_labels)
  }

  max_retention <- table_tbl %>%
    select(starts_with("d")) %>%
    unlist(use.names = FALSE) %>%
    max(na.rm = TRUE)

  if (!is.finite(max_retention)) {
    max_retention <- 1
  }

  gt_table <- gt(table_tbl) %>%
    tab_header(
      title = if (single_cohort) "FoE Cohort Retention Snapshot" else "FoE Cohort Retention Comparison",
      subtitle = if (single_cohort) {
        "D1-D7 retention by game and mobile platform"
      } else {
        "D1-D7 retention by cohort, game, and mobile platform"
      }
    )

  do.call(cols_label, c(list(.data = gt_table), column_labels)) %>%
    fmt_percent(columns = starts_with("d"), decimals = 1) %>%
    sub_missing(everything(), missing_text = "—") %>%
    data_color(
      columns = starts_with("d"),
      method = "numeric",
      palette = c("#F7F7F7", "#77C3EC", "#008FD5"),
      domain = c(0, max_retention)
    ) %>%
    tab_spanner(label = "Retention Curve", columns = starts_with("d")) %>%
    tab_style(
      style = list(cell_fill(color = "#FFF1EB"), cell_text(weight = "bold")),
      locations = cells_body(rows = slot_label == "Forge of Empires")
    ) %>%
    opt_row_striping() %>%
    tab_source_note(source_note = md(format_peer_source_note(benchmark_tbl))) %>%
    tab_options(
      table.background.color = "#F7F7F7",
      heading.background.color = "#F7F7F7",
      column_labels.background.color = "#EBEBEB",
      table.font.color = "#222222",
      source_notes.font.size = px(11),
      data_row.padding = px(6)
    )
}

build_single_cohort_retention_gt <- function(benchmark_tbl, actual_csv_path) {
  cohort_name <- unique(benchmark_tbl$cohort_name)
  cohort_label <- unique(benchmark_tbl$cohort_label)
  retention_date <- unique(benchmark_tbl$retention_date)
  row_group_order <- c("Peer Snapshot • iOS", "Peer Snapshot • Android")

  peer_tbl <- benchmark_tbl %>%
    distinct(slot, game) %>%
    crossing(platform_display = factor(c("iOS", "Android"), levels = c("iOS", "Android"))) %>%
    left_join(
      benchmark_tbl %>%
        mutate(platform_display = factor(format_platform(platform), levels = c("iOS", "Android"))) %>%
        select(
          slot,
          game,
          platform_display,
          d1_retention,
          d2_retention,
          d3_retention,
          d4_retention,
          d5_retention,
          d6_retention,
          d7_retention
        ),
      by = c("slot", "game", "platform_display")
    ) %>%
    mutate(
      slot = factor(slot, levels = slot_order),
      row_group = factor(glue("Peer Snapshot • {platform_display}"), levels = row_group_order),
      row_label = case_when(
        as.character(slot) == "target_app" ~ "Forge of Empires (Sensor Tower estimate)",
        TRUE ~ format_slot_label(as.character(slot), game)
      ),
      row_order = case_when(
        as.character(slot) == "target_app" ~ 1L,
        as.character(slot) == "cohort_leader" ~ 2L,
        as.character(slot) == "below_target" ~ 3L,
        as.character(slot) == "cohort_rank_10" ~ 4L,
        TRUE ~ 99L
      )
    ) %>%
    arrange(platform_display, row_order, slot) %>%
    select(
      row_group,
      row_label,
      row_order,
      d1_retention,
      d2_retention,
      d3_retention,
      d4_retention,
      d5_retention,
      d6_retention,
      d7_retention
    )

  table_tbl <- peer_tbl %>%
    arrange(row_group, row_order) %>%
    select(-row_order)

  max_retention <- table_tbl %>%
    select(starts_with("d")) %>%
    unlist(use.names = FALSE) %>%
    max(na.rm = TRUE)

  if (!is.finite(max_retention)) {
    max_retention <- 1
  }

  gt(table_tbl, rowname_col = "row_label", groupname_col = "row_group") %>%
    tab_header(
      title = glue("Forge of Empires retention snapshot: {cohort_name}"),
      subtitle = glue("{cohort_label} | Latest common Sensor Tower cohort: {retention_date} | Peer snapshot shows D1-D7 Sensor Tower estimates")
    ) %>%
    cols_label(
      d1_retention = "D1",
      d2_retention = "D2",
      d3_retention = "D3",
      d4_retention = "D4",
      d5_retention = "D5",
      d6_retention = "D6",
      d7_retention = "D7"
    ) %>%
    cols_align(align = "center", columns = starts_with("d")) %>%
    fmt_percent(columns = starts_with("d"), decimals = 1) %>%
    sub_missing(everything(), missing_text = "—") %>%
    data_color(
      columns = starts_with("d"),
      method = "numeric",
      palette = c("#F7F7F7", "#77C3EC", "#008FD5"),
      domain = c(0, max_retention)
    ) %>%
    tab_spanner(label = "Retention Values", columns = starts_with("d")) %>%
    tab_style(
      style = list(cell_fill(color = "#FFF1EB"), cell_text(weight = "bold")),
      locations = cells_stub(rows = row_label == "Forge of Empires (Sensor Tower estimate)")
    ) %>%
    opt_row_striping() %>%
    tab_source_note(source_note = md(format_snapshot_table_source_note(actual_csv_path))) %>%
    tab_options(
      table.background.color = "#F7F7F7",
      heading.background.color = "#F7F7F7",
      column_labels.background.color = "#EBEBEB",
      table.font.color = "#222222",
      source_notes.font.size = px(11),
      data_row.padding = px(6)
    )
}

build_retention_gt <- function(benchmark_tbl, actual_csv_path) {
  if (dplyr::n_distinct(benchmark_tbl$cohort_id) == 1) {
    return(build_single_cohort_retention_gt(benchmark_tbl, actual_csv_path))
  }

  build_generic_retention_gt(benchmark_tbl)
}

load_actual_session_data <- function(actual_csv_path) {
  if (is.null(actual_csv_path) || !file.exists(actual_csv_path)) {
    stop("Actual FoE CSV could not be found at the configured `actual_csv_path`.", call. = FALSE)
  }

  stage_actual_csv_path <- function(source_path) {
    target_dir <- "/private/tmp/codex_preview"
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
    escape_applescript_string <- function(x) {
      gsub('"', '\\"', x, fixed = TRUE)
    }

    shell_script <- c(
      "osascript <<'APPLESCRIPT'",
      sprintf('set sourceItem to POSIX file "%s"', escape_applescript_string(source_path)),
      sprintf('set targetFolder to POSIX file "%s"', escape_applescript_string(paste0(target_dir, "/"))),
      'tell application "Finder"',
      '  set copiedFile to duplicate sourceItem to targetFolder with replacing',
      '  POSIX path of (copiedFile as alias)',
      'end tell',
      'APPLESCRIPT'
    )

    staged_path <- tryCatch(
      system2(
        "sh",
        args = c("-lc", shQuote(paste(shell_script, collapse = "\n"))),
        stdout = TRUE,
        stderr = TRUE
      ),
      error = function(e) character()
    )

    staged_path <- staged_path[nzchar(trimws(staged_path))]
    if (length(staged_path) == 0) {
      stop(
        sprintf("Actual FoE CSV exists but could not be staged from `%s`.", source_path),
        call. = FALSE
      )
    }

    trimws(staged_path[[length(staged_path)]])
  }

  safe_read_actual_csv <- function(path) {
    tryCatch(
      read_csv(path, show_col_types = FALSE),
      error = function(e) {
        staged_path <- stage_actual_csv_path(path)
        read_csv(staged_path, show_col_types = FALSE)
      }
    )
  }

  actual_tbl <- safe_read_actual_csv(actual_csv_path)
  required_cols <- c("platform", "session_number", "pct_players_reached_session")
  missing_cols <- setdiff(required_cols, names(actual_tbl))

  if (length(missing_cols) > 0) {
    stop(
      sprintf("Actual FoE CSV is missing required columns: %s.", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  actual_tbl %>%
    mutate(platform = tolower(platform)) %>%
    filter(platform %in% c("ios", "android"))
}

resolve_target_platform_ids <- function(benchmark_tbl) {
  ensure_package_loaded()

  target_unified_app_id <- benchmark_tbl %>%
    filter(slot == "target_app") %>%
    pull(unified_app_id) %>%
    unique()

  if (length(target_unified_app_id) != 1) {
    stop("Could not resolve a single target unified app id from the benchmark CSV.", call. = FALSE)
  }

  lookup <- suppressMessages(st_app_lookup(target_unified_app_id))

  tibble(
    platform = c("ios", "android"),
    app_id = c(lookup$ios_app_id, lookup$android_app_id)
  ) %>%
    filter(!is.na(app_id), nzchar(app_id))
}

fetch_foe_retention_curve <- function(platform_ids_tbl,
                                      ranking_start_date,
                                      ranking_end_date,
                                      region_code,
                                      source_series) {
  ensure_package_loaded()

  retention_regions <- if (toupper(region_code) == "WW") NULL else region_code
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

  latest_common_date <- retention %>%
    distinct(app_id, date) %>%
    count(date, name = "available_app_ids") %>%
    filter(available_app_ids == length(requested_app_ids)) %>%
    summarise(latest_common_date = max(date)) %>%
    pull(latest_common_date)

  if (length(latest_common_date) != 1 || is.na(latest_common_date)) {
    stop(sprintf("No common FoE retention cohort date exists for region `%s`.", region_code), call. = FALSE)
  }

  retention %>%
    filter(date == latest_common_date) %>%
    select(date, app_id, starts_with("est_retention_d")) %>%
    rename(retention_date = date) %>%
    left_join(platform_ids_tbl, by = "app_id") %>%
    pivot_longer(
      cols = starts_with("est_retention_d"),
      names_to = "period",
      values_to = "retention_rate"
    ) %>%
    transmute(
      platform,
      platform_display = factor(format_platform(platform), levels = c("iOS", "Android")),
      step_number = readr::parse_number(period),
      retention_rate = as.numeric(retention_rate),
      source_series = source_series,
      retention_date = retention_date
    ) %>%
    filter(step_number >= 1, step_number <= 7)
}

build_foe_reference_curves <- function(benchmark_tbl, actual_csv_path) {
  ranking_start_date <- min(benchmark_tbl$ranking_start_date)
  ranking_end_date <- max(benchmark_tbl$ranking_end_date)
  target_platform_ids <- resolve_target_platform_ids(benchmark_tbl)

  actual_tbl <- load_actual_session_data(actual_csv_path) %>%
    filter(session_number >= 2, session_number <= 8) %>%
    transmute(
      platform,
      platform_display = factor(format_platform(platform), levels = c("iOS", "Android")),
      step_number = session_number - 1L,
      retention_rate = pct_players_reached_session / 100,
      source_series = "foe_actual",
      source_label = "Forge of Empires (First-party session progression)",
      retention_date = as.Date(NA)
    )

  foe_us_tbl <- fetch_foe_retention_curve(
    platform_ids_tbl = target_platform_ids,
    ranking_start_date = ranking_start_date,
    ranking_end_date = ranking_end_date,
    region_code = "US",
    source_series = "foe_st_us"
  ) %>%
    mutate(source_label = "Forge of Empires (Sensor Tower US)")

  foe_ww_tbl <- fetch_foe_retention_curve(
    platform_ids_tbl = target_platform_ids,
    ranking_start_date = ranking_start_date,
    ranking_end_date = ranking_end_date,
    region_code = "WW",
    source_series = "foe_st_ww"
  ) %>%
    mutate(source_label = "Forge of Empires (Sensor Tower WW)")

  list(
    reference_tbl = bind_rows(actual_tbl, foe_us_tbl, foe_ww_tbl),
    foe_us_date = foe_us_tbl %>% distinct(retention_date) %>% pull(retention_date),
    foe_ww_date = foe_ww_tbl %>% distinct(retention_date) %>% pull(retention_date)
  )
}

build_foe_checkpoint_gt_tbl <- function(reference_tbl) {
  row_order <- c("Actual", "Sensor Tower US", "Sensor Tower WW")

  checkpoint_tbl <- reference_tbl %>%
    filter(step_number %in% checkpoint_days) %>%
    mutate(
      platform_display = factor(platform_display, levels = c("iOS", "Android")),
      row_label = recode(
        source_series,
        foe_actual = "Actual",
        foe_st_us = "Sensor Tower US",
        foe_st_ww = "Sensor Tower WW"
      ),
      source_order = factor(row_label, levels = row_order),
      retention_day = paste0("d", step_number, "_retention")
    ) %>%
    select(platform_display, row_label, source_order, retention_day, retention_rate) %>%
    pivot_wider(names_from = retention_day, values_from = retention_rate)

  for (retention_col in paste0("d", 1:7, "_retention")) {
    if (!retention_col %in% names(checkpoint_tbl)) {
      checkpoint_tbl[[retention_col]] <- NA_real_
    }
  }

  checkpoint_tbl %>%
    select(
      platform_display,
      row_label,
      source_order,
      d1_retention,
      d2_retention,
      d3_retention,
      d4_retention,
      d5_retention,
      d6_retention,
      d7_retention
    )
}

prepare_actual_vs_st_plus_peers_tbl <- function(benchmark_tbl, actual_csv_path) {
  cohort_metadata <- build_cohort_metadata(benchmark_tbl)
  foe_reference <- build_foe_reference_curves(benchmark_tbl, actual_csv_path)

  foe_reference_tbl <- foe_reference$reference_tbl %>%
    crossing(
      cohort_metadata %>%
        select(cohort_id, cohort_display, cohort_short_label)
    )

  peer_tbl <- benchmark_tbl %>%
    filter(slot != "target_app") %>%
    left_join(cohort_metadata, by = c("cohort_id", "cohort_name", "cohort_field", "cohort_value")) %>%
    pivot_longer(
      cols = starts_with("d"),
      names_to = "period",
      values_to = "retention_rate"
    ) %>%
    mutate(
      step_number = readr::parse_number(period),
      platform_display = factor(format_platform(platform), levels = c("iOS", "Android")),
      source_series = paste(cohort_id, slot, sep = "__"),
      source_label = glue("{cohort_short_label}: {format_slot_label(slot, game)}") %>% str_wrap(width = 34)
    ) %>%
    select(
      cohort_id,
      cohort_display,
      cohort_short_label,
      platform,
      platform_display,
      step_number,
      retention_rate,
      source_series,
      source_label
    )

  bind_rows(
    foe_reference_tbl %>%
      select(cohort_id, cohort_display, cohort_short_label, platform, platform_display, step_number, retention_rate, source_series, source_label),
    peer_tbl
  )
}

build_actual_vs_st_plus_peers_scales <- function(chart_tbl) {
  foe_tbl <- tibble(
    source_series = c("foe_actual", "foe_st_us", "foe_st_ww"),
    source_label = c(
      "Forge of Empires (First-party session progression)",
      "Forge of Empires (Sensor Tower US)",
      "Forge of Empires (Sensor Tower WW)"
    )
  )

  peer_tbl <- chart_tbl %>%
    filter(!source_series %in% foe_tbl$source_series) %>%
    distinct(source_series, source_label) %>%
    arrange(source_label)

  all_series <- bind_rows(foe_tbl, peer_tbl)
  peer_slots <- chart_tbl %>%
    filter(!source_series %in% foe_tbl$source_series) %>%
    distinct(source_series) %>%
    mutate(slot = stringr::str_split_fixed(source_series, "__", 2)[, 2])

  peer_colors <- setNames(slot_palette[peer_slots$slot], peer_slots$source_series)
  peer_linewidths <- setNames(slot_linewidths[peer_slots$slot], peer_slots$source_series)
  peer_point_sizes <- setNames(slot_point_sizes[peer_slots$slot], peer_slots$source_series)

  list(
    color_values = c(foe_compare_palette, peer_colors),
    linewidth_values = c(foe_compare_linewidths, peer_linewidths),
    size_values = c(foe_compare_point_sizes, peer_point_sizes),
    label_values = setNames(all_series$source_label, all_series$source_series)
  )
}

build_actual_vs_st_chart <- function(benchmark_tbl, actual_csv_path) {
  chart_tbl <- prepare_actual_vs_st_plus_peers_tbl(benchmark_tbl, actual_csv_path)
  scale_values <- build_actual_vs_st_plus_peers_scales(chart_tbl)
  cohort_metadata <- build_cohort_metadata(benchmark_tbl)
  foe_reference <- build_foe_reference_curves(benchmark_tbl, actual_csv_path)

  ggplot(chart_tbl, aes(step_number, retention_rate, color = source_series, group = interaction(source_series, cohort_id, platform_display))) +
    geom_line(aes(linewidth = source_series)) +
    geom_point(aes(size = source_series)) +
    facet_grid(
      factor(cohort_display, levels = cohort_metadata$cohort_display) ~ platform_display
    ) +
    scale_color_manual(values = scale_values$color_values, labels = scale_values$label_values) +
    scale_linewidth_manual(values = scale_values$linewidth_values, guide = "none") +
    scale_size_manual(values = scale_values$size_values, guide = "none") +
    scale_x_continuous(
      breaks = 1:7,
      labels = paste0("D", 1:7, " / S", 2:8),
      expand = expansion(mult = c(0.02, 0.03))
    ) +
    scale_y_continuous(
      labels = label_percent(accuracy = 1),
      expand = expansion(mult = c(0.02, 0.08))
    ) +
    labs(
      title = "Forge of Empires first-party progression versus Sensor Tower and peer curves",
      subtitle = "Rows split by cohort and columns split mobile platform; peers are Sensor Tower US only; FoE first-party line is session progression",
      x = NULL,
      y = "Rate",
      caption = format_actual_peer_source_note(
        foe_reference$foe_us_date,
        foe_reference$foe_ww_date,
        actual_csv_path
      )
    ) +
    guides(color = guide_legend(ncol = 3, byrow = TRUE, override.aes = list(linewidth = 1.2, size = 2.0))) +
    theme_538_retention()
}

build_actual_vs_st_simple_chart <- function(benchmark_tbl, actual_csv_path) {
  foe_reference <- build_foe_reference_curves(benchmark_tbl, actual_csv_path)

  chart_tbl <- foe_reference$reference_tbl %>%
    mutate(
      source_series = factor(
        source_series,
        levels = c("foe_actual", "foe_st_us", "foe_st_ww")
      ),
      source_label = recode(
        source_series,
        foe_actual = "Forge of Empires (First-party session progression)",
        foe_st_us = "Forge of Empires (Sensor Tower US)",
        foe_st_ww = "Forge of Empires (Sensor Tower WW)"
      ),
      platform_display = factor(platform_display, levels = c("iOS", "Android"))
    )

  ggplot(chart_tbl, aes(step_number, retention_rate, color = source_series, group = interaction(source_series, platform_display))) +
    geom_line(aes(linewidth = source_series)) +
    geom_point(aes(size = source_series)) +
    facet_wrap(~ platform_display) +
    scale_color_manual(
      values = foe_compare_palette,
      labels = c(
        foe_actual = "Forge of Empires (First-party session progression)",
        foe_st_us = "Forge of Empires (Sensor Tower US)",
        foe_st_ww = "Forge of Empires (Sensor Tower WW)"
      )
    ) +
    scale_linewidth_manual(values = foe_compare_linewidths, guide = "none") +
    scale_size_manual(values = foe_compare_point_sizes, guide = "none") +
    scale_x_continuous(
      breaks = 1:7,
      labels = paste0("D", 1:7, " / S", 2:8),
      expand = expansion(mult = c(0.02, 0.03))
    ) +
    scale_y_continuous(
      labels = label_percent(accuracy = 1),
      expand = expansion(mult = c(0.02, 0.08))
    ) +
    labs(
      title = "Forge of Empires first-party progression versus Sensor Tower estimates",
      subtitle = "FoE only | iOS and Android | first-party session progression compared against Sensor Tower US and WW",
      x = NULL,
      y = "Rate",
      caption = format_actual_only_source_note(
        foe_reference$foe_us_date,
        foe_reference$foe_ww_date,
        actual_csv_path
      )
    ) +
    guides(color = guide_legend(ncol = 3, byrow = TRUE, override.aes = list(linewidth = 1.2, size = 2.0))) +
    theme_538_retention()
}

render_artifacts <- function(benchmark_tbl,
                             line_chart_path,
                             table_png_path,
                             table_html_path,
                             actual_csv_path,
                             combined = FALSE) {
  dir.create(dirname(line_chart_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(table_png_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(table_html_path), recursive = TRUE, showWarnings = FALSE)

  line_chart <- if (combined) {
    build_combined_line_chart(benchmark_tbl)
  } else {
    build_single_cohort_line_chart(benchmark_tbl)
  }

  ggsave(
    filename = line_chart_path,
    plot = line_chart,
    width = if (combined) 18 else 13,
    height = if (combined) 14 else 8.5,
    dpi = 200,
    bg = "#F7F7F7"
  )

  gt_table <- build_retention_gt(benchmark_tbl, actual_csv_path)
  gtsave(gt_table, filename = table_html_path)
  gtsave(gt_table, filename = table_png_path)

  cat("Saved line chart to: ", line_chart_path, "\n", sep = "")
  cat("Saved table HTML to: ", table_html_path, "\n", sep = "")
  cat("Saved table PNG to: ", table_png_path, "\n", sep = "")
}

benchmark_csv <- resolve_output_path(benchmark_csv, project_root)
output_dir <- resolve_output_path(output_dir, project_root)
line_chart_path <- resolve_output_path(line_chart_path, project_root)
table_png_path <- resolve_output_path(table_png_path, project_root)
table_html_path <- resolve_output_path(table_html_path, project_root)
actual_vs_st_chart_path <- resolve_output_path(actual_vs_st_chart_path, project_root)
actual_vs_st_simple_chart_path <- resolve_output_path(actual_vs_st_simple_chart_path, project_root)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

ensure_benchmark_exists(benchmark_csv)

benchmark_tbl <- read_csv(benchmark_csv, show_col_types = FALSE)

if (nrow(benchmark_tbl) == 0) {
  stop("Benchmark CSV is empty.", call. = FALSE)
}

cohort_ids <- unique(benchmark_tbl$cohort_id)

if (length(cohort_ids) == 1) {
  cohort_id <- cohort_ids[[1]]
  line_chart_path <- if (is.null(line_chart_path)) {
    file.path(output_dir, paste0("foe_peer_retention_", cohort_id, "_lines.png"))
  } else {
    line_chart_path
  }
  table_png_path <- if (is.null(table_png_path)) {
    file.path(output_dir, paste0("foe_peer_retention_", cohort_id, "_table.png"))
  } else {
    table_png_path
  }
  table_html_path <- if (is.null(table_html_path)) {
    file.path(output_dir, paste0("foe_peer_retention_", cohort_id, "_table.html"))
  } else {
    table_html_path
  }

  render_artifacts(
    benchmark_tbl = benchmark_tbl,
    line_chart_path = line_chart_path,
    table_png_path = table_png_path,
    table_html_path = table_html_path,
    actual_csv_path = actual_csv_path,
    combined = FALSE
  )
} else {
  if (isTRUE(render_separate_outputs)) {
    benchmark_tbl %>%
      group_split(cohort_id, .keep = TRUE) %>%
      walk(function(cohort_tbl) {
        cohort_id <- unique(cohort_tbl$cohort_id)

        render_artifacts(
          benchmark_tbl = cohort_tbl,
          line_chart_path = file.path(output_dir, paste0("foe_peer_retention_", cohort_id, "_lines.png")),
          table_png_path = file.path(output_dir, paste0("foe_peer_retention_", cohort_id, "_table.png")),
          table_html_path = file.path(output_dir, paste0("foe_peer_retention_", cohort_id, "_table.html")),
          actual_csv_path = actual_csv_path,
          combined = FALSE
        )
      })
  }

  if (isTRUE(render_combined_output)) {
    combined_line_chart_path <- if (is.null(line_chart_path)) {
      file.path(output_dir, "foe_peer_retention_combined_lines.png")
    } else {
      line_chart_path
    }
    combined_table_png_path <- if (is.null(table_png_path)) {
      file.path(output_dir, "foe_peer_retention_combined_table.png")
    } else {
      table_png_path
    }
    combined_table_html_path <- if (is.null(table_html_path)) {
      file.path(output_dir, "foe_peer_retention_combined_table.html")
    } else {
      table_html_path
    }

    render_artifacts(
      benchmark_tbl = benchmark_tbl,
      line_chart_path = combined_line_chart_path,
      table_png_path = combined_table_png_path,
      table_html_path = combined_table_html_path,
      actual_csv_path = actual_csv_path,
      combined = TRUE
    )
  }
}

if (isTRUE(render_actual_vs_st_chart)) {
  actual_vs_st_chart_path <- if (is.null(actual_vs_st_chart_path)) {
    file.path(output_dir, "foe_actual_vs_sensor_tower_lines.png")
  } else {
    actual_vs_st_chart_path
  }

  actual_vs_st_chart <- build_actual_vs_st_chart(
    benchmark_tbl = benchmark_tbl,
    actual_csv_path = actual_csv_path
  )

  ggsave(
    filename = actual_vs_st_chart_path,
    plot = actual_vs_st_chart,
    width = 18,
    height = 14,
    dpi = 200,
    bg = "#F7F7F7"
  )

  cat("Saved actual-vs-ST chart to: ", actual_vs_st_chart_path, "\n", sep = "")

  actual_vs_st_simple_chart_path <- if (is.null(actual_vs_st_simple_chart_path)) {
    file.path(output_dir, "foe_actual_vs_sensor_tower_simple_lines.png")
  } else {
    actual_vs_st_simple_chart_path
  }

  actual_vs_st_simple_chart <- build_actual_vs_st_simple_chart(
    benchmark_tbl = benchmark_tbl,
    actual_csv_path = actual_csv_path
  )

  ggsave(
    filename = actual_vs_st_simple_chart_path,
    plot = actual_vs_st_simple_chart,
    width = 13,
    height = 8.5,
    dpi = 200,
    bg = "#F7F7F7"
  )

  cat("Saved FoE-only actual-vs-ST chart to: ", actual_vs_st_simple_chart_path, "\n", sep = "")
}
