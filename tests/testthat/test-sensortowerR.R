test_that("API calls retrieve data", {
  skip_on_cran()

  auth_token <- Sys.getenv("SENSORTOWER_AUTH_TOKEN")
  if (auth_token == "") {
    skip("Authentication token not found, skipping tests")
  }

  # st_apps() - search by name (replaces st_app_info)
  apps <- st_apps(
    query = "Pokemon",
    os = "unified",
    limit = 1,
    auth_token = auth_token
  )
  expect_s3_class(apps, "tbl_df")
  expect_true(nrow(apps) > 0)

  # st_publisher_apps
  publisher_apps <- st_publisher_apps(
    auth_token = auth_token,
    publisher_id = "560c48b48ac350643900b82d" # Supercell
  )
  expect_s3_class(publisher_apps, "tbl_df")
  expect_true(nrow(publisher_apps) > 0)

  # st_metrics (new v1.0.0 signature)
  metrics <- st_metrics(
    app_id      = "602c795c912b51622f233ffe", # Pokemon GO
    os          = "unified",
    countries   = "US",
    date_from   = "2021-09-22",
    date_to     = "2021-09-22",
    granularity = "daily",
    auth_token  = auth_token
  )
  expect_s3_class(metrics, "tbl_df")

  # st_rankings (replaces st_top_charts)
  top_ios <- st_rankings(
    entity     = "app",
    os         = "ios",
    category   = 6000,
    country    = "US",
    date       = "2023-10-01",
    limit      = 5,
    auth_token = auth_token
  )
  expect_s3_class(top_ios, "tbl_df")
})

test_that("clean_numeric_values removes special characters correctly", {
  # Create test data with special characters in numeric columns
  test_data <- tibble::tibble(
    downloads_180d_ww = c("1,234,567", "$2,500", "45%", "100.5", NA),
    revenue_30d_us = c("$1,000.50", "2,500%", "45", "", "N/A"),
    retention_7d_us = c("15.5%", "25%", "0", "45.2%", NA),
    aggregate_tags.test_metric = c("$100", "200%", "1,500", "", NA),
    app_name = c("Test App", "Another App", "Game", "Tool", "App"),
    category = c("Games", "Social", "Productivity", "Entertainment", "Utilities")
  )

  cleaned_data <- sensortowerR:::clean_numeric_values(test_data)

  expect_true(is.numeric(cleaned_data$downloads_180d_ww))
  expect_true(is.numeric(cleaned_data$revenue_30d_us))
  expect_true(is.numeric(cleaned_data$retention_7d_us))
  expect_true(is.numeric(cleaned_data$`aggregate_tags.test_metric`))

  expect_true(is.character(cleaned_data$app_name))
  expect_true(is.character(cleaned_data$category))

  expect_equal(cleaned_data$downloads_180d_ww[1], 1234567)
  expect_equal(cleaned_data$downloads_180d_ww[2], 2500)
  expect_equal(cleaned_data$downloads_180d_ww[3], 45)
  expect_equal(cleaned_data$revenue_30d_us[1], 1000.50)
  expect_equal(cleaned_data$retention_7d_us[1], 0.155)
  expect_equal(cleaned_data$retention_7d_us[2], 0.25)

  expect_true(is.na(cleaned_data$downloads_180d_ww[5]))
  expect_true(is.na(cleaned_data$retention_7d_us[5]))
})
