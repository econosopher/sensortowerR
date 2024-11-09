library(testthat)
library(sensortowerR)

test_that("fetch_sensor_tower_metrics handles invalid inputs", {
  expect_error(
    fetch_sensor_tower_metrics(
      auth_token = "",
      app_id = "123",
      start_date = "2024-01-01",
      end_date = "2024-01-31",
      app_name = "TestGame",
      grain = "daily"
    ),
    "Invalid auth_token"
  )
})
