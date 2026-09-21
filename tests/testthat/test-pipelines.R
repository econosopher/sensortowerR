test_that("search, filter, metadata and metrics compose with both pipes", {
  calls <- local_api(function(path, q, req) {
    switch(path,
      `/v1/unified/search_entities` = list(list(app_id = "a", name = "Clash of Clans", entity_type = "unified_app"), list(app_id = "b", name = "Clash Royale", entity_type = "unified_app")),
      `/v1/unified/apps` = list(apps = list(list(unified_app_id = q$app_ids, name = "Clash of Clans"))),
      `/v1/unified/sales_report_estimates` = sales_rows(q$app_ids),
      `/v1/unified/usage/active_users` = usage_rows(q$app_ids),
      stop("Unexpected endpoint")
    )
  })
  apps <- st_apps("Clash") |>
    dplyr::filter(.data$app_name == "Clash of Clans") |>
    dplyr::mutate(cohort = "launch")
  out <- apps |>
    st_app() |>
    st_metrics("2026-01-01", "2026-01-31", countries = "US", metrics = c("revenue", "downloads", "mau"))
  expect_equal(nrow(out), 3L)
  expect_equal(unique(out$app_id), "a")
  expect_equal(unique(out$app_name), "Clash of Clans")
  expect_equal(unique(out$cohort), "launch")
  expect_equal(out$value[out$metric == "revenue"], 123)
  expect_equal(out$value[out$metric == "mau"], 32)
  expect_equal(out$unit[out$metric == "mau"], "platform_users")
  expect_false(dplyr::is_grouped_df(out))
  `%>%` <- dplyr::`%>%`
  expect_equal(apps %>% st_metrics("2026-01-01", "2026-01-31", countries = "US"), st_metrics(apps, "2026-01-01", "2026-01-31", countries = "US"))
  expect_true(all(vapply(calls$requests, function(req) httr2::url_parse(req$url)$hostname == "api.sensortower.com", logical(1))))
})

test_that("publisher search and portfolio retrieval feed metrics", {
  local_api(function(path, q, req) {
    switch(path,
      `/v1/unified/search_entities` = list(list(publisher_id = "pub", name = "Supercell", entity_type = "unified_publisher")),
      `/v1/unified/publishers/apps` = list(apps = list(list(unified_app_id = "a", name = "A"))),
      `/v1/unified/sales_report_estimates` = sales_rows(),
      stop("Unexpected endpoint")
    )
  })
  out <- st_publishers("Supercell") |>
    st_publisher_apps() |>
    st_metrics("2026-01-01", "2026-01-31", countries = "US")
  expect_equal(unique(out$publisher_id), "pub")
  expect_equal(unique(out$publisher_name), "Supercell")
  expect_equal(unique(out$app_name), "A")
})

test_that("single, vector and tibble calls share normalization and deduplicate requests", {
  calls <- local_api(function(path, q, req) sales_rows(strsplit(q$app_ids, ",", fixed = TRUE)[[1]]))
  a <- metric_call("a")
  b <- metric_call("b")
  both <- metric_call(c("a", "b"))
  expect_equal(both, dplyr::bind_rows(a, b))
  input <- tibble::tibble(app_id = c("a", "a", "b"), os = "unified", cohort = c("one", "two", "three"), date = as.Date("2020-01-01"), date.input = "keep")
  n <- length(calls$requests)
  out <- metric_call(input, os = NULL)
  expect_length(calls$requests, n + 1L)
  expect_equal(nrow(out), 6L)
  expect_equal(out$cohort, rep(input$cohort, each = 2))
  expect_true(all(c("date", "date.input", "date.input.input") %in% names(out)))
  expect_equal(unique(out$date.input), "keep")
  expect_equal(unique(out$date.input.input), as.Date("2020-01-01"))
})

test_that("metadata conversion expands all regional SKUs, never name-matches", {
  calls <- local_api(function(path, q, req) {
    expect_equal(path, "/v1/unified/apps")
    list(apps = list(list(unified_app_id = "unified", name = "Game", itunes_apps = list(list(app_id = "1", name = "Game US"), list(app_id = "2", name = "Game JP")))))
  })
  out <- st_app("unified", os = "unified", target_os = "ios")
  expect_equal(out$app_id, c("1", "2"))
  expect_equal(out$os, c("ios", "ios"))
  expect_equal(out$input_app_id, rep("unified", 2))
  mapped <- st_app("1", os = "ios", target_os = "unified")
  expect_equal(mapped$app_id, "unified")
  expect_equal(mapped$input_app_id, "1")
  expect_equal(httr2::url_parse(calls$requests[[2]]$url)$query$app_id_type, "itunes")
})

test_that("empty data is typed and makes no requests", {
  local_api(function(...) stop("No request allowed"))
  withr::local_envvar(c(SENSORTOWER_AUTH_TOKEN = ""))
  out <- metric_call(empty_apps(), os = NULL)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0L)
  expect_type(out$value, "double")
  expect_s3_class(out$date, "Date")
  expect_named(st_metrics(empty_apps(), "2026-01-01", "2026-01-31", shape = "wide"), c("app_id", "os", "country", "date", "revenue", "revenue_unit", "revenue_period", "downloads", "downloads_unit", "downloads_period"))
  expect_equal(nrow(st_app(empty_apps())), 0L)
  expect_equal(nrow(st_publisher_apps(tibble::tibble(publisher_id = character(), os = character()))), 0L)
  expect_equal(nrow(st_retention(empty_apps(), "2026-01-01", "2026-01-31")), 0L)
})

test_that("store metadata retains its store ID when unified_app_id is also present", {
  local_api(function(...) list(apps = list(list(app_id = 529479190, unified_app_id = "unified", name = "Store name"))))
  out <- st_app("529479190", os = "ios")
  expect_equal(out$app_id, "529479190"); expect_equal(out$os, "ios")
})

test_that("numeric provider IDs never become scientific notation", {
  local_api(function(...) list(apps = list(list(app_id = 7000000000, name = "Game"))))
  expect_equal(st_app("7000000000", os = "ios")$app_id, "7000000000")
})
