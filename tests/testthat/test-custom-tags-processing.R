ns <- getNamespace("sensortowerR")

local_clean_cache <- function() {
  env <- ns$.sensortowerR_env
  env$id_cache <- list()
  rm(list = ls(envir = ns$.app_name_cache), envir = ns$.app_name_cache)
}

test_that("extract_custom_metrics handles aggregate and custom tag columns", {
  df <- tibble::tibble(
    `aggregate_tags.Last 30 Days Average DAU (US)` = "1,234",
    `aggregate_tags.Last 30 Days Average DAU (WW)` = "5,678",
    `custom_tags.Day 1 Retention (Last Quarter, US)` = "45%",
    `custom_tags.Day 1 Retention (Last Quarter, WW)` = "55%",
    `entities.custom_tags.Game Genre` = "Strategy"
  )

  processed <- ns$extract_custom_metrics(df)
  processed <- ns$clean_numeric_values(processed)

  expect_equal(processed$dau_30d_us, 1234)
  expect_equal(processed$dau_30d_ww, 5678)
  expect_equal(processed$retention_1d_us, 0.45)
  expect_equal(processed$retention_1d_ww, 0.55)
  expect_equal(processed$game_genre, "Strategy")
})

test_that("extract_custom_metrics maps custom tags when aggregate tags missing", {
  df <- tibble::tibble(
    `custom_tags.Last 30 Days Average DAU (US)` = "9,876",
    `custom_tags.Last 30 Days Average DAU (WW)` = "12,345",
    `custom_tags.Day 7 Retention (Last Quarter, US)` = "32%",
    `custom_tags.Day 7 Retention (Last Quarter, WW)` = "38%"
  )

  processed <- ns$extract_custom_metrics(df)
  processed <- ns$clean_numeric_values(processed)

  expect_equal(processed$dau_30d_us, 9876)
  expect_equal(processed$dau_30d_ww, 12345)
  expect_equal(processed$retention_7d_us, 0.32)
  expect_equal(processed$retention_7d_ww, 0.38)
})

test_that("lookup_app_names_by_id prefers cache metadata", {
  on.exit(local_clean_cache(), add = TRUE)

  ns <- getNamespace("sensortowerR")
  ns$cache_id_mapping(
    input_id = "123",
    ios_id = "123",
    android_id = "com.example.app",
    unified_id = "abc123abc123abc123abc123",
    app_name = "Example App"
  )

  df <- tibble::tibble(
    `entities.app_id` = "123",
    unified_app_name = NA_character_,
    unified_app_id = NA_character_
  )

  result <- ns$lookup_app_names_by_id(df)
  expect_equal(result$unified_app_name, "Example App")
  expect_equal(result$unified_app_id, "abc123abc123abc123abc123")
})
