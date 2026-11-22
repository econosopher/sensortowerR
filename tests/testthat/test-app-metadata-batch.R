test_that("batch app metadata preserves country and regional mappings", {
  calls <- local_api(function(path, q, req) {
    expect_equal(q$country, "US")
    expect_equal(q$app_ids, "a,b")
    list(apps = list(list(app_id="a", name="Title", release_date="2020-01-01"), list(app_id="b", name="Other")))
  })
  x <- st_app_metadata(c("a", "b"), os="ios", country="US")
  expect_equal(x$app_id, c("a", "b"))
  expect_equal(x$release_date[1], "2020-01-01")
  expect_equal(length(calls$requests), 1)
})

test_that("unified metadata keeps store IDs and nested mappings across batches", {
  ids <- as.character(seq_len(101))
  calls <- local_api(function(path, q, req) {
    batch <- strsplit(q$app_ids, ",", fixed = TRUE)[[1]]
    list(apps = lapply(batch, function(id) {
      canonical <- switch(id, "1" = 1234567890, "2" = "com.example.shooter",
                          "3" = NULL, "00123")
      list(unified_app_id = id, canonical_app_id = canonical,
           itunes_apps = list(list(app_id = 123, country = "US")))
    }))
  })

  x <- st_app_metadata(ids, os = "unified")
  expect_identical(x$unified_app_id, ids)
  expect_identical(x$canonical_app_id[c(1, 2, 3, 101)],
                   c("1234567890", "com.example.shooter", NA_character_, "00123"))
  expect_equal(x$itunes_apps[[1]][[1]]$app_id, 123)
  expect_length(calls$requests, 2)
})

test_that("store metadata preserves mixed JSON values within and across pages", {
  ids <- as.character(seq_len(101))
  local_api(function(path, q, req) {
    batch <- strsplit(q$app_ids, ",", fixed = TRUE)[[1]]
    list(apps = lapply(batch, function(id) {
      app <- list(app_id = as.integer(id))
      fields <- switch(id,
        "1" = list(in_app_purchases = TRUE, offer = FALSE, rating = 4L,
                   featured = TRUE, categories = list("Games")),
        "2" = list(in_app_purchases = "unknown", rating = 4.5,
                   featured = 1L, categories = list(primary = "Games")),
        "3" = list(in_app_purchases = NULL, rating = NULL, categories = list()),
        "101" = list(in_app_purchases = FALSE, offer = "preview"),
        list()
      )
      c(app, fields)
    }))
  })

  x <- st_app_metadata(ids, os = "ios")
  expect_identical(x$app_id, ids)
  expect_identical(x$in_app_purchases[c(1, 2, 3, 101)], list(TRUE, "unknown", NULL, FALSE))
  expect_identical(x$offer[c(1, 2, 101)], list(FALSE, NULL, "preview"))
  expect_identical(x$featured[1:2], list(TRUE, 1L))
  expect_identical(x$rating[1:3], c(4, 4.5, NA_real_))
  expect_identical(x$categories[1:4], list(list("Games"), list(primary = "Games"), list(), NULL))
})
