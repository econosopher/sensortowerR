test_that("st_rankings rejects invalid entity", {
  expect_error(
    st_rankings(
      entity     = "bogus",
      os         = "ios",
      category   = 6000,
      country    = "US",
      auth_token = "dummy"
    )
  )
})

test_that("st_rankings rejects invalid os", {
  expect_error(
    st_rankings(
      entity     = "app",
      os         = "nope",
      category   = 6000,
      country    = "US",
      auth_token = "dummy"
    ),
    "must be one of"
  )
})

test_that("st_rankings accepts the three allowed entities", {
  # Each errors at the network layer, not the arg layer
  for (e in c("app", "publisher", "category")) {
    res <- tryCatch(
      st_rankings(
        entity     = e,
        os         = "ios",
        category   = 6000,
        country    = "US",
        date       = "2024-01-01",
        auth_token = "dummy"
      ),
      error = function(err) err
    )
    # The entity passed validation if we did NOT get an entity-arg error
    msg <- conditionMessage(res) %||% ""
    expect_false(grepl("'arg' should be one of", msg))
  }
})
