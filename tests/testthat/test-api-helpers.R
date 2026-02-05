test_that("endpoint helpers build expected paths", {
  expect_equal(
    sensortowerR:::st_endpoint_path("usage_active_users", os = "ios"),
    "v1/ios/usage/active_users"
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
