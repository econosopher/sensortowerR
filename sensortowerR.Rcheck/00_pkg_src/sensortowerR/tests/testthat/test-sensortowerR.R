test_that("API calls retrieve data", {
  skip_on_cran()

  auth_token <- Sys.getenv("SENSORTOWER_AUTH_TOKEN")
  if (auth_token == "") {
    skip("Authentication token not found, skipping tests")
  }

  # Test st_app_info
  app_info <- st_app_info(
    term = "Pokemon",
    app_store = "unified",
    entity_type = "app",
    limit = 1,
    auth_token = auth_token
  )
  expect_s3_class(app_info, "tbl_df")
  expect_true(nrow(app_info) > 0)

  # Test st_publisher_apps
  publisher_apps <- st_publisher_apps(
    auth_token = auth_token,
    publisher_id = "560c48b48ac350643900b82d" # Supercell
  )
  expect_s3_class(publisher_apps, "tbl_df")
  expect_true(nrow(publisher_apps) > 0)

  # Test st_metrics
  metrics <- st_metrics(
    auth_token = auth_token,
    unified_app_id = "602c795c912b51622f233ffe", # Pokemon GO
    start_date = "2021-09-22",
    end_date = "2021-09-22"
  )
  expect_s3_class(metrics, "tbl_df")
  expect_true(nrow(metrics) > 0)

  # Test st_top_sales
  top_sales <- st_top_sales(
    os = "ios",
    comparison_attribute = "absolute",
    time_range = "month",
    measure = "units",
    date = "2023-10-01",
    category = 6000,
    regions = "US",
    limit = 5,
    auth_token = auth_token
  )
  expect_s3_class(top_sales, "tbl_df")
  expect_true(nrow(top_sales) > 0)

  # Test st_top_active_users
  top_active <- st_top_active_users(
    os = "android",
    comparison_attribute = "absolute",
    time_range = "quarter",
    measure = "MAU",
    date = "2023-10-01",
    regions = "WW",
    limit = 3,
    auth_token = auth_token
  )
  expect_s3_class(top_active, "tbl_df")
  expect_true(nrow(top_active) > 0)
}) 