ad_rows <- function(n = 1, date = "2026-01-01", ...) {
  lapply(seq_len(n), function(i) c(list(unified_app_id = "a", date = date, est_ad_monetization_impressions = 4500 + i, est_ad_monetization_revenue = 45000), list(...)))
}

test_that("ad monetization requests unified time series and labels units", {
  calls <- local_api(function(path, q, req) {
    expect_equal(path, "/v1/facets/metrics")
    expect_equal(q$bundle, "mobile_ad_monetization_estimates")
    expect_equal(q$breakdown, "unified_app_id,date")
    expect_equal(q$date_granularity, "month")
    expect_equal(q$unified_app_ids, "a")
    expect_null(q$regions)
    expect_null(q$facets)
    list(data = list(
      list(unified_app_id = "a", date = "2026-01-01", est_ad_monetization_impressions = 4500, est_ad_monetization_revenue = 45000),
      list(unified_app_id = "a", date = "2026-02-01", est_ad_monetization_impressions = NULL, est_ad_monetization_revenue = 25000)
    ))
  })
  out <- st_ad_monetization(tibble::tibble(app_id = "a", os = "unified", cohort = "x"), "2026-01-01", "2026-02-28")
  rev <- out[out$metric == "est_ad_monetization_revenue", ]
  imp <- out[out$metric == "est_ad_monetization_impressions", ]
  expect_equal(rev$value, c(450, 250))
  expect_equal(unique(rev$unit), "USD")
  expect_equal(imp$value, c(4500, NA))
  expect_equal(unique(imp$unit), "impressions")
  expect_equal(unique(out$country), "covered_regions")
  expect_equal(unique(out$period), "month")
  expect_equal(unique(out$cohort), "x")
  cents <- st_ad_monetization("a", "2026-01-01", "2026-02-28", os = "unified", revenue_unit = "cents")
  expect_equal(cents$value[cents$metric == "est_ad_monetization_revenue"], c(45000, 25000))
  expect_equal(unique(cents$unit[cents$metric == "est_ad_monetization_revenue"]), "USD_cents")
})

test_that("ad monetization retrieves every 200-row page", {
  calls <- local_api(function(path, q, req) {
    expect_equal(q$limit, "200")
    rows <- if (q$offset == "0") ad_rows(200) else ad_rows(1, date = "2026-02-01")
    if (q$breakdown == "unified_app_id,network,date") rows <- Map(function(r, i) c(r, network = paste0("n", i)), rows, seq_along(rows))
    list(data = rows)
  })
  out <- st_ad_monetization("a", "2026-01-01", "2026-02-28", os = "unified", breakdown = "network")
  expect_length(calls$requests, 2L)
  expect_equal(nrow(out), 402L)
  expect_equal(httr2::url_parse(calls$requests[[2]]$url)$query$offset, "200")
  expect_error(st_ad_monetization("a", "2026-01-01", "2026-02-28", os = "unified"), "conflicting duplicate")
})

test_that("ad monetization breakdowns keep network and region dimensions", {
  calls <- local_api(function(path, q, req) {
    if (q$breakdown == "unified_app_id,region,date") {
      return(list(data = list(ad_rows(1, region = "US")[[1]], ad_rows(1, region = "JP")[[1]])))
    }
    expect_equal(q$breakdown, "network,region,date")
    expect_equal(q$regions, "US")
    expect_equal(q$date_granularity, "quarter")
    list(data = list(
      list(network = "Unity", region = "US", date = "2026-01-01", est_ad_monetization_impressions = 1000, est_ad_monetization_revenue = 10000),
      list(network = "Admob", region = "US", date = "2026-01-01", est_ad_monetization_impressions = 1500, est_ad_monetization_revenue = 15000)
    ))
  })
  regions <- st_ad_monetization("a", "2026-01-01", "2026-01-31", os = "unified", breakdown = "region")
  expect_setequal(regions$country, c("US", "JP"))
  expect_false("region" %in% names(regions))
  both <- st_ad_monetization("a", "2026-01-01", "2026-03-31", os = "unified", countries = "US", granularity = "quarterly", breakdown = c("region", "network"))
  expect_equal(both$network[both$metric == "est_ad_monetization_revenue"], c("Unity", "Admob"))
  expect_equal(unique(both$app_id), "a")
  expect_equal(unique(both$country), "US")
})

test_that("ad monetization empty results, invalid inputs and partial failures", {
  calls <- local_api(function(path, q, req) {
    if (identical(q$regions, "JP")) return(httr2::response(403L, body = charToRaw("[]")))
    list(data = list(), meta = list(limit = 200, offset = 0))
  })
  empty <- st_ad_monetization("a", "2026-01-01", "2026-01-31", os = "unified")
  expect_equal(nrow(empty), 0L)
  expect_named(empty, names(.st_metric_ptype()), ignore.order = TRUE)
  n <- length(calls$requests)
  expect_error(st_ad_monetization("1", "2026-01-01", "2026-01-31", os = "ios"), "unified IDs")
  expect_error(st_ad_monetization("a", "2026-01-01", "2026-01-31", os = "unified", granularity = "daily"), "granularity")
  expect_error(st_ad_monetization("a", "2026-01-01", "2026-01-31", os = "unified", breakdown = "app_id"), "breakdown")
  expect_equal(nrow(st_ad_monetization(empty_apps(), "2026-01-01", "2026-01-31")), 0L)
  expect_length(calls$requests, n)
  expect_warning(out <- st_ad_monetization("a", "2026-01-01", "2026-01-31", os = "unified", countries = c("US", "JP"), errors = "partial"), class = "st_partial_warning")
  expect_equal(out$status, "error")
  expect_equal(out$country, "JP")
})

test_that("ad rankings use the top-apps bundle, convert cents and page by 250", {
  calls <- local_api(function(path, q, req) {
    expect_equal(q$breakdown, "unified_app_id")
    if (q$bundle == "mobile_ad_monetization_impression_share") {
      expect_equal(q$order_by, "est_ad_monetization_impression_share:desc")
      return(list(data = list(list(unified_app_id = "b", est_ad_monetization_impression_share = 0.083))))
    }
    expect_equal(q$order_by, "est_ad_monetization_revenue:desc")
    expect_equal(q$networks, "Unity,Digital Turbine")
    expect_equal(q$unified_app_game_categories, "7012")
    if (q$offset == "0") {
      expect_equal(q$limit, "250")
      rows <- lapply(seq_len(250), function(i) list(unified_app_id = paste0("app", i), est_ad_monetization_impressions = 1, est_ad_monetization_revenue = 1000 * i))
      return(list(data = rows, meta = list(limit = 250, offset = 0, total_count = 251)))
    }
    list(data = list(list(unified_app_id = "last", est_ad_monetization_impressions = 1, est_ad_monetization_revenue = 542000)))
  })
  out <- st_ad_rankings("ad_revenue", "2026-01-01", "2026-01-31", networks = c("Unity", "Digital Turbine"), game_categories = "7012", limit = 300)
  expect_length(calls$requests, 2L)
  expect_equal(nrow(out), 251L)
  expect_equal(out$app_id[1:2], c("last", "app250"))
  expect_equal(out$value[1], 5420)
  expect_equal(out$rank[1:2], 1:2)
  expect_equal(unique(out$unit), "USD")
  expect_equal(unique(out$country), "covered_regions")
  expect_equal(unique(out$period), "range")
  share <- st_ad_rankings("ad_impression_share", "2026-01-01", "2026-01-31", countries = "US")
  expect_equal(share$value, 0.083)
  expect_equal(share$unit, "fraction")
  expect_equal(share$country, "US")
  expect_equal(httr2::url_parse(calls$requests[[3]]$url)$query$regions, "US")
  expect_error(st_ad_rankings("revenue", "2026-01-01", "2026-01-31"), "measure")
  expect_error(st_ad_rankings("ad_revenue", "2026-01-01", "2026-01-31", networks = "Google"), "networks")
})
