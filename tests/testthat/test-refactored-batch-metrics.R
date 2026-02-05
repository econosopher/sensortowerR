test_that("normalize_app_list handles various inputs correctly", {
    # Test character vector
    res_vec <- sensortowerR:::normalize_app_list(c("123", "456"), "ios", "token", FALSE)
    expect_s3_class(res_vec, "data.frame")
    expect_equal(nrow(res_vec), 2)
    expect_equal(res_vec$app_id, c("123", "456"))

    # Test data frame
    input_df <- data.frame(app_id = c("123", "456"), stringsAsFactors = FALSE)
    res_df <- sensortowerR:::normalize_app_list(input_df, "ios", "token", FALSE)
    expect_equal(res_df$app_id, c("123", "456"))

    # Test list
    input_list <- list(list(app_id = "123"), list(app_id = "456"))
    res_list <- sensortowerR:::normalize_app_list(input_list, "ios", "token", FALSE)
    expect_equal(res_list$app_id, c("123", "456"))
})

test_that("group_apps_by_platform correctly categorizes apps", {
    apps_df <- tibble::tibble(
        app_id = c("1", "2", "3", "4"),
        ios_id = c("101", "102", NA, NA),
        android_id = c("201", NA, "203", NA),
        unified_id = c(NA, NA, NA, "404")
    )

    groups <- sensortowerR:::group_apps_by_platform(apps_df)

    expect_equal(nrow(groups$both), 1) # App 1
    expect_equal(groups$both$app_id, "1")

    expect_equal(nrow(groups$ios), 1) # App 2
    expect_equal(groups$ios$app_id, "2")

    expect_equal(nrow(groups$android), 1) # App 3
    expect_equal(groups$android$app_id, "3")

    expect_equal(nrow(groups$unified), 1) # App 4
    expect_equal(groups$unified$app_id, "4")
})

test_that("finalize_batch_results_internal handles empty and valid results", {
    # Test empty results
    expect_equal(nrow(sensortowerR:::finalize_batch_results_internal(list(), tibble::tibble())), 0)

    # Test valid results
    results_df <- tibble::tibble(
        entity_id = c("101_201", "102"),
        date = as.Date("2024-01-01"),
        country = "US",
        metric = "revenue",
        value = 100
    )

    apps_df <- tibble::tibble(
        app_id = c("1", "2"),
        ios_id = c("101", "102"),
        android_id = c("201", NA),
        unified_id = c(NA, NA)
    )

    final <- sensortowerR:::finalize_batch_results_internal(list(results_df), apps_df)

    expect_equal(nrow(final), 2)
    expect_true("app_id_type" %in% names(final))
    expect_equal(final$app_id_type[final$original_id == "1"], "unified_pair")
    expect_equal(final$app_id_type[final$original_id == "2"], "ios")
})

test_that("active user time period helpers map values correctly", {
    expect_equal(sensortowerR:::active_users_default_time_period("daily"), "day")
    expect_equal(sensortowerR:::active_users_default_time_period("weekly"), "week")
    expect_equal(sensortowerR:::active_users_default_time_period("monthly"), "month")
    expect_equal(sensortowerR:::active_users_default_time_period("quarterly"), "quarter")
    expect_equal(sensortowerR:::active_users_default_time_period("unknown"), "month")

    expect_equal(sensortowerR:::active_users_time_period_for_metric("dau", "month"), "day")
    expect_equal(sensortowerR:::active_users_time_period_for_metric("wau", "month"), "week")
    expect_equal(sensortowerR:::active_users_time_period_for_metric("mau", "week"), "month")
    expect_equal(sensortowerR:::active_users_time_period_for_metric("custom", "quarter"), "quarter")
})

test_that("build_active_users_request_plan creates batched platform-metric requests", {
    apps_df <- tibble::tibble(
        app_id = c("1", "2"),
        ios_id = c("101", "102"),
        android_id = c("com.a", "com.b")
    )

    plan_both <- sensortowerR:::build_active_users_request_plan(
        group = apps_df,
        group_name = "both",
        active_user_metrics = c("dau", "mau")
    )

    expect_equal(nrow(plan_both), 4)
    expect_equal(sort(unique(plan_both$platform)), c("android", "ios"))
    expect_true(all(sort(unique(plan_both$metric)) == c("dau", "mau")))

    ios_ids <- plan_both$app_ids[plan_both$platform == "ios"][[1]]
    android_ids <- plan_both$app_ids[plan_both$platform == "android"][[1]]
    expect_equal(sort(ios_ids), c("101", "102"))
    expect_equal(sort(android_ids), c("com.a", "com.b"))
})
