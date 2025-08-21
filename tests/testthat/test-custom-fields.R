test_that("st_custom_fields_values works correctly", {
  skip_if_no_auth()
  
  # Test basic retrieval
  fields <- st_custom_fields_values()
  expect_s3_class(fields, "tbl_df")
  expect_true(nrow(fields) > 0)
  expect_true("name" %in% names(fields))
  expect_true("global" %in% names(fields))
  expect_true("values" %in% names(fields))
  
  # Test search functionality
  game_fields <- st_custom_fields_values(term = "game")
  expect_true(nrow(game_fields) > 0)
  expect_true(all(grepl("game", game_fields$name, ignore.case = TRUE)))
  
  # Test that we get fewer results with search
  expect_true(nrow(game_fields) < nrow(fields))
})

test_that("st_custom_fields_filter creates filters correctly", {
  skip_if_no_auth()
  
  # Test creating a simple filter
  filter_id <- st_custom_fields_filter(
    custom_fields = list(
      list(
        name = "Game Sub-genre",
        values = list("Word"),
        global = TRUE,
        exclude = FALSE
      )
    )
  )
  
  expect_type(filter_id, "character")
  expect_match(filter_id, "^[a-f0-9]{24}$")  # MongoDB ObjectId format
  
  # Test that identical filters return same ID
  filter_id2 <- st_custom_fields_filter(
    custom_fields = list(
      list(
        name = "Game Sub-genre",
        values = list("Word"),
        global = TRUE,
        exclude = FALSE
      )
    )
  )
  
  expect_equal(filter_id, filter_id2)
  
  # Test validation errors
  expect_error(
    st_custom_fields_filter(custom_fields = list()),
    "at least one filter"
  )
  
  expect_error(
    st_custom_fields_filter(
      custom_fields = list(
        list(
          # Missing name
          values = list("Word")
        )
      )
    ),
    "name is required"
  )
})

test_that("st_custom_fields_filter_by_id retrieves filter details", {
  skip_if_no_auth()
  
  # Use a known filter ID
  filter_id <- "603697f4241bc16eb8570d37"  # Word games filter
  
  details <- st_custom_fields_filter_by_id(id = filter_id)
  
  expect_type(details, "list")
  expect_true("custom_fields" %in% names(details))
  expect_s3_class(details$custom_fields, "data.frame")
  expect_true("name" %in% names(details$custom_fields))
  expect_true("global" %in% names(details$custom_fields))
  expect_true("values" %in% names(details$custom_fields))
  expect_true("exclude" %in% names(details$custom_fields))
  
  # Verify the filter is for Word games
  expect_equal(details$custom_fields$name[1], "Game Sub-genre")
  expect_true("Word" %in% details$custom_fields$values[[1]])
  
  # Test invalid filter ID
  expect_error(
    st_custom_fields_filter_by_id(id = "invalid_id"),
    "Invalid|not found"
  )
})

test_that("st_create_simple_filter works correctly", {
  skip_if_no_auth()
  
  # Test creating a filter with single value
  filter_id <- st_create_simple_filter(
    field_name = "Game Sub-genre",
    field_values = "Word"
  )
  
  expect_type(filter_id, "character")
  expect_match(filter_id, "^[a-f0-9]{24}$")
  
  # Test creating a filter with multiple values
  filter_id2 <- st_create_simple_filter(
    field_name = "Game Genre",
    field_values = c("Puzzle", "Word")
  )
  
  expect_type(filter_id2, "character")
  expect_match(filter_id2, "^[a-f0-9]{24}$")
  
  # Test exclude functionality
  filter_id3 <- st_create_simple_filter(
    field_name = "Contains Ads",
    field_values = list(),
    exclude = TRUE
  )
  
  expect_type(filter_id3, "character")
})

test_that("st_discover_fields works correctly", {
  skip_if_no_auth()
  
  # Test basic discovery
  fields <- st_discover_fields()
  expect_s3_class(fields, "tbl_df")
  expect_true(nrow(fields) > 0)
  
  # Test search
  date_fields <- st_discover_fields("date")
  expect_true(nrow(date_fields) > 0)
  expect_true(all(grepl("date", date_fields$name, ignore.case = TRUE)))
  
  # Test show_values parameter
  fields_with_values <- st_discover_fields(show_values = TRUE)
  expect_true("values" %in% names(fields_with_values))
  
  fields_without_values <- st_discover_fields(show_values = FALSE)
  expect_true("value_count" %in% names(fields_without_values))
})

test_that("st_get_filtered_apps integrates correctly", {
  skip_if_no_auth()
  
  # Test with field name and values
  apps <- st_get_filtered_apps(
    field_name = "Game Sub-genre",
    field_values = "Word",
    measure = "DAU",
    regions = "US",
    limit = 5,
    enrich_response = FALSE
  )
  
  expect_s3_class(apps, "tbl_df")
  expect_true(nrow(apps) > 0)
  expect_true("app_id" %in% names(apps))
  expect_true("users_absolute" %in% names(apps))
  
  # Test with existing filter ID
  apps2 <- st_get_filtered_apps(
    filter_id = "603697f4241bc16eb8570d37",
    measure = "DAU",
    regions = "US",
    limit = 5,
    enrich_response = FALSE
  )
  
  expect_s3_class(apps2, "tbl_df")
  expect_true(nrow(apps2) > 0)
  
  # Test error when neither filter_id nor field_name provided
  expect_error(
    st_get_filtered_apps(measure = "DAU", regions = "US"),
    "Either provide filter_id or both field_name and field_values"
  )
})

test_that("st_analyze_filter provides correct analysis", {
  skip_if_no_auth()
  
  # Test analysis
  analysis <- st_analyze_filter(
    filter_id = "603697f4241bc16eb8570d37",
    measure = "DAU",
    regions = "US",
    top_n = 3
  )
  
  expect_type(analysis, "list")
  expect_equal(analysis$filter_id, "603697f4241bc16eb8570d37")
  expect_equal(analysis$measure, "DAU")
  expect_equal(analysis$regions, "US")
  expect_true("filter_criteria" %in% names(analysis))
  expect_true("total_apps" %in% names(analysis))
  expect_true("top_apps" %in% names(analysis))
  expect_true(nrow(analysis$top_apps) <= 3)
})

test_that("st_app_tag works with custom filters", {
  skip_if_no_auth()
  
  # Test fetching apps with custom filter
  result <- st_app_tag(
    app_id_type = "unified",
    custom_fields_filter_id = "603697f4241bc16eb8570d37"
  )
  
  expect_type(result, "list")
  expect_true("data" %in% names(result))
  
  # The data should contain app IDs
  if (!is.null(result$data) && length(result$data) > 0) {
    expect_true("unified_app_id" %in% names(result$data) || 
                "itunes_app_ids" %in% names(result$data))
  }
})

test_that("Custom fields handle edge cases", {
  skip_if_no_auth()
  
  # Test with boolean field
  filter_id <- st_custom_fields_filter(
    custom_fields = list(
      list(
        name = "Free",
        global = TRUE,
        true = TRUE
      )
    )
  )
  expect_type(filter_id, "character")
  
  # Test with empty values
  filter_id2 <- st_custom_fields_filter(
    custom_fields = list(
      list(
        name = "Has In-App Purchases",
        values = list(),
        global = TRUE
      )
    )
  )
  expect_type(filter_id2, "character")
  
  # Test combining multiple criteria
  filter_id3 <- st_custom_fields_filter(
    custom_fields = list(
      list(
        name = "Game Genre",
        values = list("Puzzle"),
        global = TRUE
      ),
      list(
        name = "Free",
        global = TRUE,
        true = TRUE
      )
    )
  )
  expect_type(filter_id3, "character")
})

test_that("Custom fields integration with st_top_charts", {
  skip_if_no_auth()
  
  # Create a custom filter
  filter_id <- st_create_simple_filter(
    field_name = "Game Sub-genre",
    field_values = "Word"
  )
  
  # Use it with st_top_charts
  apps <- st_top_charts(
    os = "unified",
    category = 0,
    custom_fields_filter_id = filter_id,
    custom_tags_mode = "include_unified_apps",
    measure = "DAU",
    regions = "US",
    limit = 5,
    enrich_response = FALSE
  )
  
  expect_s3_class(apps, "tbl_df")
  expect_true(nrow(apps) > 0)
  expect_true("app_id" %in% names(apps))
  
  # Test that category 0 is used with custom filter
  expect_message(
    st_top_charts(
      os = "unified",
      category = 7019,  # Should warn about using non-zero category
      custom_fields_filter_id = filter_id,
      custom_tags_mode = "include_unified_apps",
      measure = "DAU",
      regions = "US",
      limit = 5
    ),
    "category.*0.*custom filter"
  )
})