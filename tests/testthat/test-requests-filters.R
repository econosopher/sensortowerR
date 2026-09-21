test_that("rankings honor measure and charts use their own endpoint", {
  calls <- local_api(function(path, q, req) {
    if (grepl("/ranking$", path)) {
      return(list(ranking = list("a", "b")))
    }
    expect_equal(q$measure, "units")
    list(list(app_id = "a", units_absolute = 3))
  })
  out <- st_rankings("downloads", "ios", "2026-01-01", "2026-01-31", countries = "US")
  expect_equal(out$value, 3)
  expect_error(st_rankings("downloads", "ios", "2026-01-01", "2026-01-31", chart_type = "topfreeapplications"), "unused argument")
  charts <- st_charts("ios", "6014", "topfreeapplications", "2026-01-01")
  expect_equal(charts$app_id, c("a", "b"))
  expect_equal(charts$rank, 1:2)
  expect_equal(httr2::url_parse(calls$requests[[2]]$url)$query$chart_type, "topfreeapplications")
})

test_that("active rankings reject unsupported periods and request native measure", {
  local_api(function(path, q, req) {
    expect_equal(path, "/v1/unified/top_and_trending/active_users")
    expect_equal(q$measure, "MAU")
    list(list(app_id = "a", users_absolute = 10))
  })
  expect_equal(st_rankings("mau", "unified", "2026-01-01", "2026-01-31")$value, 10)
  expect_error(st_rankings("mau", "unified", "2026-01-05", "2026-01-11", granularity = "weekly"), "Unsupported")
  expect_error(st_rankings("mau", "unified", "2026-01-01", "2026-02-28"), "one full")
})

test_that("filter construction is local; AND and OR have different verified payloads", {
  calls <- local_api(function(path, q, req) {
    expect_equal(path, "/v1/custom_fields_filter")
    expect_equal(req$method, "POST")
    expect_null(req$policies$retry_max_tries)
    list(custom_fields_filter_id = sprintf("%024d", length(calls$requests)))
  })
  a <- st_filter(genre = "RPG", publisher = "P", combine = "and")
  b <- st_filter(genre = "RPG", publisher = "P", combine = "or")
  expect_length(calls$requests, 0L)
  expect_error(st_apps(filter = a), "st_filter_create")
  a <- st_filter_create(a)
  b <- st_filter_create(b)
  expect_length(a$filter_ids, 1L)
  expect_length(b$filter_ids, 2L)
  expect_length(calls$requests[[1]]$body$data$custom_fields, 2L)
  expect_length(calls$requests[[2]]$body$data$custom_fields, 1L)
  expect_length(calls$requests[[3]]$body$data$custom_fields, 1L)
  expect_equal(st_filter_create(a), a)
  expect_length(calls$requests, 3L)
})

test_that("failed filter creation never fabricates an ID or retries a POST", {
  calls <- local_api(function(...) httr2::response(422L, body = charToRaw('{"error":"invalid"}')))
  expect_error(st_filter_create(st_filter(monetization = "iap")), class = "st_http_error")
  expect_length(calls$requests, 1L)
})

test_that("OR discovery unions IDs and cursor pagination detects repetition", {
  ids <- c(strrep("a", 24), strrep("b", 24))
  local_api(function(path, q, req) {
    list(app_ids = if (q$custom_fields_filter_id == ids[1]) list("1", "2") else list("2", "3"))
  })
  expect_equal(st_apps(filter = st_filter(filter_id = ids, combine = "or"))$app_id, c("1", "2", "3"))
  local_api(function(...) list(app_ids = list("1"), last_known_id = "same"))
  expect_error(st_apps(filter = ids[1]), "repeated a cursor")
})

test_that("publisher offset pagination uses bounded pages", {
  calls <- local_api(function(path, q, req) {
    count <- if (q$offset == "0") 100L else 1L
    list(apps = lapply(seq_len(count), function(i) list(app_id = as.character(as.integer(q$offset) + i), name = "App")))
  })
  out <- st_publisher_apps("pub", os = "ios")
  expect_equal(nrow(out), 101L)
  expect_length(calls$requests, 2L)
  expect_equal(httr2::url_parse(calls$requests[[2]]$url)$query$offset, "100")
})

test_that("schema and request errors redact secrets and retain status", {
  secret <- "do-not-expose-secret"
  local_api(function(...) httr2::response(401L, body = charToRaw(paste0('{"error":"', secret, '"}'))))
  err <- tryCatch(metric_call(auth_token = secret), error = identity)
  expect_s3_class(err, "st_http_error")
  expect_equal(err$status, 401L)
  expect_false(grepl(secret, paste(capture.output(str(err)), collapse = "\n"), fixed = TRUE))
  local_api(function(...) httr2::response(200L, body = charToRaw("not json")))
  expect_error(metric_call(), class = "st_schema_error")
  local_api(function(...) list(list(unexpected = 1)))
  expect_error(metric_call(), class = "st_schema_error")
  local_api(function(...) httr2::response(200L, body = raw()))
  expect_error(metric_call(), "Empty response")
  local_api(function(...) list())
  expect_equal(nrow(metric_call()), 0L)
})

test_that("GET retry policy is bounded and obeys transient classifications", {
  req <- sensortowerR:::.st_request("ios/apps", auth_token = "placeholder")
  expect_equal(req$policies$retry_max_tries, 3)
  expect_equal(req$policies$retry_max_wait, 60)
  expect_true(req$policies$retry_on_failure)
  transient <- req$policies$retry_is_transient
  for (status in c(429L, 500L, 502L, 503L, 504L)) expect_true(transient(httr2::response(status)))
  for (status in c(401L, 403L, 404L, 422L)) expect_false(transient(httr2::response(status)))
})

test_that("cache is opt-in, expires, scopes credentials and never stores failures", {
  st_cache_clear()
  withr::defer(st_cache_clear())
  calls <- local_api(function(...) sales_rows())
  metric_call()
  metric_call()
  expect_length(calls$requests, 2L)
  metric_call(cache = TRUE)
  metric_call(cache = TRUE)
  expect_length(calls$requests, 3L)
  metric_call(cache = TRUE, auth_token = "second-token")
  expect_length(calls$requests, 4L)
  expect_equal(nrow(st_cache_info()), 2L)
  expect_false(any(grepl("token", st_cache_info()$key)))
  cache <- sensortowerR:::.st_cache
  for (key in ls(cache)) {
    x <- get(key, cache)
    x$time <- x$time - 400
    assign(key, x, cache)
  }
  metric_call(cache = TRUE)
  expect_length(calls$requests, 5L)
  st_cache_clear()
  local_api(function(...) httr2::response(403L, body = charToRaw("[]")))
  expect_warning(metric_call(cache = TRUE, errors = "partial"), class = "st_partial_warning")
  expect_equal(nrow(st_cache_info()), 0L)
})

test_that("URLs round-trip without network and never expose credentials", {
  url <- st_build_url(list(regions = c("US", "JP"), name = "RPG & strategy"))
  parsed <- st_parse_url(url)
  expect_equal(parsed$value, c("US,JP", "RPG & strategy"))
  expect_error(st_build_url(list(auth_token = "secret")), "Credentials")
  expect_equal(nrow(st_parse_url("https://app.sensortower.com/top-charts?auth_token=secret")), 0L)
})

test_that("array query parameters are repeated while ID lists use commas", {
  req <- sensortowerR:::.st_request("app_tag/tags_for_apps", list(app_ids = c("1", "2"), `fields[]` = c("Game Genre", "Free")), "test-token")
  query <- httr2::url_parse(req$url)$query
  expect_equal(query$app_ids, "1,2")
  expect_equal(sum(names(query) == "fields[]"), 2L)
})

test_that("actual httr2 retries 429 and 503 but not authentication failures", {
  withr::local_envvar(SENSORTOWER_AUTH_TOKEN = "placeholder")
  count <- 0L
  testthat::local_mocked_bindings(req_perform1 = function(...) {
    count <<- count + 1L
    if (count <= 2L) {
      return(httr2::response(if (count == 1L) 429L else 503L, headers = list(`retry-after` = "0"), body = charToRaw("[]")))
    }
    httr2::response(200L, body = charToRaw("[]"))
  }, .package = "httr2")
  expect_equal(sensortowerR:::.st_get("ios/apps"), list())
  expect_equal(count, 3L)
  count <- 0L
  testthat::local_mocked_bindings(req_perform1 = function(...) {
    count <<- count + 1L
    httr2::response(401L, body = charToRaw("[]"))
  }, .package = "httr2")
  expect_error(sensortowerR:::.st_get("ios/apps"), class = "st_http_error")
  expect_equal(count, 1L)
})

test_that("filter definitions can be retrieved without creation", {
  local_api(function(path, q, req) list(custom_fields = list(list(name = "Free", global = TRUE, values = list("true")))))
  x <- st_filter_read(strrep("a", 24))
  expect_equal(x$name, "Free")
  expect_equal(x$filter_id, strrep("a", 24))
  expect_equal(st_filter(date_from = "2026-01-01", date_to = "2026-01-31")$fields[[1]]$values[[1]], "2026-01-01 to 2026-01-31")
})
