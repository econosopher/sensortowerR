# Verify that every defunct stub errors with the `defunctError` class.
# Using eval(call(...)) rather than do.call() because .Defunct() looks at
# sys.call() to extract the function name, and do.call rewrites that frame.

defunct_fns <- c(
  "st_sales_report",
  "st_unified_sales_report",
  "st_batch_metrics",
  "st_smart_metrics",
  "st_top_charts",
  "st_top_publishers",
  "st_category_rankings",
  "st_app_info",
  "st_app_lookup",
  "st_app_details",
  "st_filter_by_date",
  "st_filter_by_genre",
  "st_filter_by_monetization",
  "st_filter_by_publisher",
  "st_filter_by_sdk",
  "st_custom_fields_filter",
  "st_custom_fields_filter_by_id",
  "st_combine_filters",
  "st_create_simple_filter",
  "st_compare_filter_results",
  "st_generate_example_filter_ids"
)

for (fn in defunct_fns) {
  test_that(sprintf("%s() is defunct", fn), {
    expr <- as.call(list(as.name(fn)))
    err <- tryCatch(
      eval(expr, envir = asNamespace("sensortowerR")),
      error = function(e) e
    )
    expect_true(inherits(err, "error"),
                info = sprintf("%s() should raise an error", fn))
    expect_true(inherits(err, "defunctError") ||
                  grepl("defunct", conditionMessage(err), ignore.case = TRUE),
                info = sprintf("%s() error should be a defunctError", fn))
  })
}

test_that("every defunct stub is in the package NAMESPACE", {
  exports <- getNamespaceExports("sensortowerR")
  expect_true(all(defunct_fns %in% exports))
})
