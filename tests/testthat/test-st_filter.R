test_that("st_filter() constructs an st_filter object from criteria", {
  skip_if(!nzchar(Sys.getenv("SENSORTOWER_AUTH_TOKEN")),
          "Auth token not available — skipping server-side filter creation")
  skip_on_cran()

  f <- st_filter(
    date_from = "2025-01-01",
    date_to   = "2025-06-30"
  )
  expect_s3_class(f, "st_filter")
  expect_true(is.character(as.character(f)))
})

test_that("st_filter() wraps a pre-existing filter_id without an API call", {
  f <- st_filter(filter_id = "5ba4585f539ce75b97db6bcb")
  expect_s3_class(f, "st_filter")
  expect_equal(as.character(f), "5ba4585f539ce75b97db6bcb")
})

test_that("print.st_filter produces output", {
  f <- st_filter(filter_id = "5ba4585f539ce75b97db6bcb")
  expect_output(print(f))
})

test_that("format.st_filter returns a character string", {
  f <- st_filter(filter_id = "5ba4585f539ce75b97db6bcb")
  expect_type(format(f), "character")
})
