test_that("st_active_users validates metric choices", {
  expect_error(
    st_active_users(
      os = "ios",
      app_list = "553834731",
      metrics = c("dau", "revenue"),
      countries = "US",
      auth_token = "token"
    ),
    "must be one or more of: dau, wau, mau"
  )
})

test_that("st_active_users delegates to st_batch_metrics and returns active metrics only", {
  testthat::local_mocked_bindings(
    resolve_auth_token = function(auth_token, env_var = "SENSORTOWER_AUTH_TOKEN", error_message = NULL) {
      "token"
    },
    st_batch_metrics = function(...) {
      tibble::tibble(
        original_id = c("app_b", "app_a", "app_a"),
        app_name = c("B", "A", "A"),
        app_id = c("2", "1", "1"),
        app_id_type = c("ios", "ios", "ios"),
        date = as.Date(c("2024-01-02", "2024-01-01", "2024-01-01")),
        country = c("US", "US", "US"),
        metric = c("wau", "revenue", "dau"),
        value = c(20, 999, 10)
      )
    },
    .package = "sensortowerR"
  )

  result <- st_active_users(
    os = "ios",
    app_list = c("1", "2"),
    metrics = c("dau", "wau"),
    countries = "US",
    auth_token = "token",
    verbose = FALSE
  )

  expect_equal(nrow(result), 2)
  expect_true(all(result$metric %in% c("dau", "wau")))
  expect_false(any(result$metric == "revenue"))
  expect_equal(result$original_id, c("app_a", "app_b"))
})
