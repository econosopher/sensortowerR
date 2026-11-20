test_that("unified breakdown retains platform components and missingness", {
  calls <- local_api(function(path, q, req) {
    if (grepl("usage/active_users", path)) return(list(list(app_id="a", country="WW", date="2026-01-05", iphone_users=4, ipad_users=2, android_users=7)))
    list(list(app_id="a", country="WW", date="2026-01-05", iphone_revenue=100, ipad_revenue=20,
              android_revenue=200, unified_revenue=320, iphone_units=5, ipad_units=NULL,
              android_units=10, unified_units=15))
  })
  x <- st_unified_breakdown("a", "2026-01-05", "2026-01-11", type="sales")
  expect_equal(x$iphone_revenue, 100)
  expect_true(is.na(x$ipad_units))
  expect_equal(x$unified_units, 15)
  u <- st_unified_breakdown("a", "2026-01-05", "2026-01-11", type="wau")
  expect_equal(u$iphone_users, 4)
  expect_equal(u$android_users, 7)
  expect_error(st_unified_breakdown("a", "2026-01-05", "2026-01-11", type="invalid"))
})
