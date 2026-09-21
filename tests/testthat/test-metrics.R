test_that("missing metrics and components remain NA in both shapes", {
  local_api(function(path, q, req) if (grepl("active_users", path)) usage_rows(missing = TRUE) else sales_rows(revenue = NULL))
  long <- metric_call(metrics = c("revenue", "downloads", "mau"))
  expect_true(is.na(long$value[long$metric == "revenue"]))
  expect_true(is.na(long$value[long$metric == "mau"]))
  expect_equal(long$value[long$metric == "downloads"], 25)
  wide <- metric_call(shape = "wide")
  expect_true(is.na(wide$revenue))
  expect_equal(wide$downloads, 25)
  raw <- tibble::tibble(app_id = "a", os = "unified", country = "US", date = as.Date("2026-01-01"), metric = "downloads", value = 1, unit = "downloads", period = "day")
  expect_true(is.na(sensortowerR:::.st_shape_metrics(raw, c("revenue", "downloads"), "wide")$revenue))
})

test_that("duplicate conflict protection survives the rewrite", {
  local_api(function(...) c(sales_rows(), sales_rows()))
  expect_equal(nrow(metric_call()), 2L)
  local_api(function(...) c(sales_rows(), sales_rows(revenue = 999)))
  expect_error(metric_call(), class = "st_schema_error")
})

test_that("unified sales never fall back to incomplete platform totals", {
  calls <- local_api(function(path, q, req) {
    expect_equal(path, "/v1/unified/sales_report_estimates")
    httr2::response(403L, body = charToRaw('{"error":"denied"}'))
  })
  expect_error(metric_call(c("a", "b")), class = "st_http_error")
  expect_length(calls$requests, 1L)
  expect_warning(partial <- metric_call(c("a", "b"), errors = "partial"), class = "st_partial_warning")
  expect_equal(nrow(partial), 4L)
  expect_true(all(partial$status == "error"))
  expect_true(all(is.na(partial$value)))
  expect_true(all(is.na(partial$date)))
  expect_true(all(grepl("403", partial$error)))
})

test_that("partial mode retains successful endpoints with visible failures", {
  local_api(function(path, q, req) if (grepl("active_users", path)) httr2::response(403L, body = charToRaw("[]")) else sales_rows())
  expect_error(metric_call(metrics = c("revenue", "mau")), class = "st_http_error")
  expect_warning(out <- metric_call(metrics = c("revenue", "mau"), errors = "partial"), class = "st_partial_warning")
  expect_equal(out$status[out$metric == "revenue"], "ok")
  expect_equal(out$status[out$metric == "mau"], "error")
})

test_that("platform units and native audience windows are explicit", {
  calls <- local_api(function(path, q, req) {
    if (grepl("active_users", path)) {
      return(usage_rows(date = if (q$time_period == "week") "2026-01-05" else "2026-01-01"))
    }
    if (grepl("/ios/", path)) {
      return(list(list(aid = 1, cc = "US", d = "2026-01-01T00:00:00Z", ir = 100, ar = 50, iu = 10, au = 2)))
    }
    if (grepl("/android/", path)) {
      return(list(list(aid = "com.a", c = "US", d = "2026-01-01", r = 123, u = 4)))
    }
    sales_rows()
  })
  ios <- metric_call("1", os = "ios")
  expect_equal(ios$value[ios$metric == "revenue"], 1.5)
  expect_equal(ios$value[ios$metric == "downloads"], 12)
  android <- metric_call("com.a", os = "android", revenue_unit = "cents")
  expect_equal(android$value[android$metric == "revenue"], 123)
  expect_equal(android$unit[android$metric == "revenue"], "USD_cents")
  actives <- metric_call(metrics = c("dau", "wau", "mau"), granularity = "monthly")
  expect_equal(actives$period[match(c("dau", "wau", "mau"), actives$metric)], c("day", "week", "month"))
  expect_equal(vapply(utils::tail(calls$requests, 3), function(r) httr2::url_parse(r$url)$query$time_period, character(1)), c("day", "week", "month"))
})

test_that("date and country bounds do not silently include extra observations", {
  local_api(function(...) c(sales_rows(date = "2025-12-31"), sales_rows(date = "2026-02-01"), sales_rows(country = "JP"), sales_rows()))
  expect_equal(nrow(metric_call()), 2L)
  expect_error(st_metrics("a", "2026-01-01", "2026-01-31", os = "unified", countries = c("WW", "US")), "WW separately")
  expect_error(metric_call(os = NULL), "requires `os`")
  expect_error(st_metrics("a", "2026-02-31", "2026-03-31", os = "unified"), "Invalid")
  expect_error(st_metrics("a", "2026-02-01", "2026-01-01", os = "unified"), "before")
  expect_error(metric_call(data.frame(app_id = "a"), os = NULL), "columns")
})

test_that("market output converts cents and retains categories", {
  local_api(function(path, q, req) {
    expect_equal(path, "/v1/ios/games_breakdown")
    list(list(ca = "7001", cc = "US", d = "2026-01-01", iu = 4, au = 1, ir = 500, ar = 100))
  })
  out <- st_market_metrics("7001", "ios", "2026-01-01", "2026-01-31", countries = "US")
  expect_equal(out$value[out$metric == "revenue"], 6)
  expect_equal(unique(out$category_id), "7001")
})

test_that("multi-row active-user responses preserve all observations", {
  local_api(function(path, q, req) c(usage_rows(date = "2026-01-01"), usage_rows(date = "2026-01-02")))
  out <- metric_call(metrics = "dau")
  expect_equal(out$value, c(32, 32))
  expect_equal(out$unit, rep("platform_users", 2))
})

test_that("large requests chunk without changing results", {
  calls <- local_api(function(path, q, req) {
    ids <- strsplit(q$app_ids, ",", fixed = TRUE)[[1]]
    expect_lte(length(ids), 100L)
    sales_rows(ids)
  })
  ids <- sprintf("id%03d", 1:205)
  out <- metric_call(ids)
  expect_equal(nrow(out), 410L)
  expect_length(calls$requests, 3L)
  expect_setequal(out$app_id, ids)
})
