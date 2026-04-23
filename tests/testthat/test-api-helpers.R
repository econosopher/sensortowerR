test_that("endpoint helpers build expected paths", {
  expect_equal(
    sensortowerR:::st_endpoint_path("usage_active_users", os = "ios"),
    "v1/ios/usage/active_users"
  )

  expect_equal(
    sensortowerR:::st_endpoint_path("facets_metrics"),
    "v1/facets/metrics"
  )

  expect_equal(
    sensortowerR:::st_endpoint_relative_path("search_entities", app_store = "unified"),
    "unified/search_entities"
  )

  expect_error(
    sensortowerR:::st_endpoint_path("usage_active_users"),
    "Missing required endpoint placeholder"
  )

  expect_error(
    sensortowerR:::st_endpoint_path("not_a_real_endpoint"),
    "Unknown endpoint key"
  )
})

test_that("resolve_auth_token supports direct and env-token resolution", {
  expect_equal(
    sensortowerR:::resolve_auth_token("  token_123  "),
    "token_123"
  )

  previous <- Sys.getenv("SENSORTOWER_AUTH_TOKEN", unset = NA_character_)
  on.exit({
    if (is.na(previous)) {
      Sys.unsetenv("SENSORTOWER_AUTH_TOKEN")
    } else {
      Sys.setenv(SENSORTOWER_AUTH_TOKEN = previous)
    }
  }, add = TRUE)

  Sys.setenv(SENSORTOWER_AUTH_TOKEN = "from_env")
  expect_equal(
    sensortowerR:::resolve_auth_token(NULL),
    "from_env"
  )

  Sys.setenv(SENSORTOWER_AUTH_TOKEN = "")
  expect_error(
    sensortowerR:::resolve_auth_token(NULL),
    "Authentication token not found"
  )
})

test_that("st_facets_metrics validates raw query fragments", {
  expect_error(
    st_facets_metrics(query = 1, auth_token = "token"),
    "`query` must be a character vector"
  )

  expect_error(
    st_facets_metrics(params = "nope", auth_token = "token"),
    "`params` must be a named list"
  )
})

test_that("st_retention_facets validates entity filters and breakdowns", {
  expect_error(
    st_retention_facets(
      start_date = "2025-01-01",
      end_date = "2025-01-31",
      auth_token = "token"
    ),
    "Exactly one of `app_ids` or `unified_app_ids` must be supplied"
  )

  expect_error(
    st_retention_facets(
      app_ids = "553834731",
      unified_app_ids = "5f16a8019f7b275235017614",
      start_date = "2025-01-01",
      end_date = "2025-01-31",
      auth_token = "token"
    ),
    "Exactly one of `app_ids` or `unified_app_ids` must be supplied"
  )

  expect_error(
    st_retention_facets(
      app_ids = "553834731",
      breakdown = c("date", "region"),
      start_date = "2025-01-01",
      end_date = "2025-01-31",
      auth_token = "token"
    ),
    "Unsupported `breakdown` field"
  )

  expect_error(
    st_retention_facets(
      app_ids = "553834731",
      breakdown = c("date", "app_id", "unified_app_id", "extra"),
      start_date = "2025-01-01",
      end_date = "2025-01-31",
      auth_token = "token"
    ),
    "Unsupported `breakdown` field"
  )

  expect_error(
    st_retention_facets(
      app_ids = "553834731",
      start_date = "2025-02-01",
      end_date = "2025-01-31",
      auth_token = "token"
    ),
    "`end_date` must be on or after `start_date`"
  )
})

test_that("st_ratings_facets validates required parameters", {
  expect_error(
    st_ratings_facets(
      start_date = "2024-01-01",
      end_date = "2024-01-07",
      auth_token = "token"
    ),
    "`app_ids` must contain at least one app ID"
  )

  expect_error(
    st_ratings_facets(
      app_ids = "553834731",
      breakdown = c("app_id", "date"),
      start_date = "2024-01-01",
      end_date = "2024-01-07",
      auth_token = "token"
    ),
    "`date_granularity` is required when `breakdown` includes `date`"
  )

  expect_error(
    st_ratings_facets(
      app_ids = "553834731",
      breakdown = c("app_id", "region"),
      start_date = "2024-01-01",
      end_date = "2024-01-07",
      date_granularity = "day",
      auth_token = "token"
    ),
    "Unsupported `breakdown` for ratings facets"
  )
})

test_that("st_reviews_by_rating_facets validates required parameters", {
  expect_error(
    st_reviews_by_rating_facets(
      start_date = "2024-01-01",
      end_date = "2024-01-07",
      auth_token = "token"
    ),
    "`app_id` must be a single non-empty app ID"
  )

  expect_error(
    st_reviews_by_rating_facets(
      app_id = "553834731",
      breakdown = c("date", "review_rating"),
      start_date = "2024-01-01",
      end_date = "2024-01-07",
      auth_token = "token"
    ),
    "`date_granularity` is required when `breakdown` includes `date`"
  )

  expect_error(
    st_reviews_by_rating_facets(
      app_id = "553834731",
      breakdown = c("region", "language"),
      start_date = "2024-01-01",
      end_date = "2024-01-07",
      date_granularity = "day",
      auth_token = "token"
    ),
    "Unsupported `breakdown` for reviews-by-rating facets"
  )
})
