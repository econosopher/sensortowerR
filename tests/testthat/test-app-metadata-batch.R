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
