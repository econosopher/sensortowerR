test_that("facets retention preserves horizons and identity", {
  local_api(function(path, q, req) {
    expect_equal(q$facets, "retention")
    expect_equal(q$unified_app_ids, "a")
    list(data = list(list(unified_app_id = "a", date = "2026-01-01", est_retention_d1 = 0.7, est_retention_d14 = 0.2)))
  })
  out <- st_retention(tibble::tibble(app_id = "a", os = "unified", cohort = "x"), "2026-01-01", "2026-01-31")
  expect_equal(out$horizon, c(1, 14))
  expect_equal(out$value, c(0.7, 0.2))
  expect_equal(out$cohort, c("x", "x"))
})

test_that("legacy retention and demographics keep their source scales", {
  local_api(function(path, q, req) {
    if (grepl("retention", path)) {
      return(list(app_data = list(list(app_id = "1", date = "2026-01-01", corrected_retention = list(0.8, 0.6)))))
    }
    list(app_data = list(list(
      app_id = "1", date = "2026-01-01", female = 0.6, male = 0.4, average_age_total = 31,
      grouped_normalized_demographics = list(female_18 = 0.2)
    )))
  })
  out <- st_retention("1", "2026-01-01", "2026-03-31", os = "ios", method = "legacy")
  expect_equal(out$value, c(0.8, 0.6))
  expect_equal(out$horizon, c(1, 2))
  demographics <- st_demographics("1", "2026-01-01", "2026-03-31", os = "ios")
  expect_equal(demographics$value[demographics$metric == "average_age_total"], 31)
  expect_equal(demographics$unit[demographics$metric == "female"], "provider_share")
  expect_error(st_demographics("a", "2026-01-01", "2026-03-31", os = "unified"), "store IDs")
})

test_that("sessions, ratings, reviews and tags return composable typed tables", {
  local_api(function(path, q, req) {
    if (grepl("timeseries", path)) {
      return(list(apps = list(list(app_id = "1", timeseries = list(list(date = "2026-01-01", session_count = 4, session_duration = 10))))))
    }
    if (grepl("tags_for_apps", path)) {
      return(list(apps = list(list(app_id = "1", tags = list(list(name = "Genre", value = "RPG"))))))
    }
    if (q$facets == "ratings") {
      return(list(data = list(list(app_id = "1", date = "2026-01-01", rating_average_incremental = 4.5, rating_count_incremental = 100))))
    }
    list(data = list(list(date = "2026-01-01", review_rating = 5, review_rating_count = 7, review_rating_percentage = 70)))
  })
  sessions <- st_sessions("1", "2026-01-01", "2026-01-31", os = "ios")
  expect_equal(sessions$value[sessions$metric == "session_count"], 4)
  expect_true(is.na(sessions$value[sessions$metric == "time_spent"]))
  ratings <- st_ratings("1", "2026-01-01", "2026-01-31", os = "ios")
  expect_equal(ratings$unit[ratings$metric == "rating_average_incremental"], "stars")
  reviews <- st_reviews("1", "2026-01-01", "2026-01-31", os = "ios")
  expect_equal(unique(reviews$review_rating), 5)
  expect_type(st_app_tags("1", os = "ios", fields = "Game Genre")$tags, "list")
})

test_that("specialist error modes and empty input are consistent", {
  calls <- local_api(function(...) httr2::response(403L, body = charToRaw("[]")))
  expect_error(st_retention("a", "2026-01-01", "2026-01-31", os = "unified"), class = "st_http_error")
  expect_warning(out <- st_retention("a", "2026-01-01", "2026-01-31", os = "unified", errors = "partial"), class = "st_partial_warning")
  expect_equal(out$app_id, "a")
  expect_equal(out$status, "error")
  n <- length(calls$requests)
  expect_equal(nrow(st_sessions(empty_apps(), "2026-01-01", "2026-01-31")), 0L)
  expect_equal(nrow(st_ratings(empty_apps(), "2026-01-01", "2026-01-31")), 0L)
  expect_length(calls$requests, n)
})

test_that("current demographic age bands are retained", {
  local_api(function(...) {
    list(app_data = list(list(
      app_id = "1", date = "2026-01-01", female = 0.4,
      normalized_demographics = list(female_18 = 0.1, male_25 = 0.2)
    )))
  })
  out <- st_demographics("1", "2026-01-01", "2026-03-31", os = "ios")
  expect_setequal(out$metric, c("female", "normalized_demographics.female_18", "normalized_demographics.male_25"))
})

test_that("tags fail early without a field selection", {
  local_api(function(...) stop("No request allowed"))
  expect_error(st_app_tags("1", os = "ios"), "fields or field_categories")
})

test_that("partial specialist results preserve successful countries", {
  local_api(function(path,q,req) {
    if(q$regions == "JP") return(httr2::response(403L,body=charToRaw('[]')))
    list(data=list(list(unified_app_id="a",date="2026-01-01",est_retention_d1=0.7)))
  })
  expect_warning(out <- st_retention("a","2026-01-01","2026-01-31",os="unified",countries=c("US","JP"),errors="partial"),class="st_partial_warning")
  expect_equal(out$status[out$country=="US"],"ok")
  expect_equal(out$value[out$country=="US"],0.7)
  expect_equal(out$status[out$country=="JP"],"error")
})
