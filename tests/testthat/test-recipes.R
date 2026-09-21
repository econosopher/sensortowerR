test_that("report recipes preserve missingness and avoid audience totals", {
  env <- new.env(parent = globalenv())
  source(system.file("recipes", "reports.R", package = "sensortowerR"), local = env)
  data <- tibble::tibble(app_id = "a", os = "unified", country = "US", date = as.Date(c("2025-01-01", "2026-01-01")), metric = "revenue", value = c(100, 150), unit = "USD", period = "month")
  out <- env$recipe_yoy(data)
  expect_equal(out$yoy_growth, c(NA_real_, 0.5))
  expect_error(env$recipe_yoy(dplyr::bind_rows(data, data)), "duplicate")
  data$value[1] <- NA_real_
  expect_true(is.na(env$recipe_portfolio(data)$value[1]))
  data$metric <- "mau"
  expect_error(env$recipe_portfolio(data), "audiences overlap")
})
