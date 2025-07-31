#!/usr/bin/env Rscript

# Test app ID validation functionality

library(sensortowerR)
source("R/app_id_validation.R")

# Test detect_app_id_type function
cat("Testing detect_app_id_type()...\n")

# iOS tests
stopifnot(detect_app_id_type("1234567890") == "ios")
stopifnot(detect_app_id_type("553834731") == "ios")

# Android tests
stopifnot(detect_app_id_type("com.example.app") == "android")
stopifnot(detect_app_id_type("com.playrix.homescapes") == "android")
stopifnot(detect_app_id_type("com.android.game.with.numbers123") == "android")

# Unified tests
stopifnot(detect_app_id_type("5ba4585f539ce75b97db6bcb") == "unified")
stopifnot(detect_app_id_type("67890abcdef1234567890abc") == "unified")

# Unknown tests
stopifnot(detect_app_id_type("not-valid-id") == "unknown")
stopifnot(detect_app_id_type("123abc") == "unknown")
stopifnot(detect_app_id_type("") == "unknown")
stopifnot(detect_app_id_type(NULL) == "unknown")

cat("✓ detect_app_id_type() tests passed\n\n")

# Test is_valid_app_id function
cat("Testing is_valid_app_id()...\n")

# iOS validation
stopifnot(is_valid_app_id("1234567890", "ios") == TRUE)
stopifnot(is_valid_app_id("com.example.app", "ios") == FALSE)
stopifnot(is_valid_app_id("5ba4585f539ce75b97db6bcb", "ios") == FALSE)

# Android validation
stopifnot(is_valid_app_id("com.example.app", "android") == TRUE)
stopifnot(is_valid_app_id("1234567890", "android") == FALSE)
stopifnot(is_valid_app_id("5ba4585f539ce75b97db6bcb", "android") == FALSE)

# Unified validation
stopifnot(is_valid_app_id("5ba4585f539ce75b97db6bcb", "unified") == TRUE)
stopifnot(is_valid_app_id("1234567890", "unified") == FALSE)
stopifnot(is_valid_app_id("com.example.app", "unified") == FALSE)

cat("✓ is_valid_app_id() tests passed\n\n")

# Test error message generation
cat("Testing create_app_id_error_message()...\n")

# Test iOS error
ios_error <- create_app_id_error_message("com.example.app", "ios")
stopifnot(grepl("Invalid app ID for ios platform", ios_error))
stopifnot(grepl("Detected format: android", ios_error))

# Test Android error
android_error <- create_app_id_error_message("1234567890", "android")
stopifnot(grepl("Invalid app ID for android platform", android_error))
stopifnot(grepl("Detected format: ios", android_error))

# Test unified error
unified_error <- create_app_id_error_message("5ba4585f539ce75b97db6bcb", "ios")
stopifnot(grepl("unified ID", unified_error))
stopifnot(grepl("st_app_lookup", unified_error))

cat("✓ create_app_id_error_message() tests passed\n\n")

# Test validate_and_transform_app_ids function
cat("Testing validate_and_transform_app_ids()...\n")

# Test with matching IDs (should pass through unchanged)
ios_ids <- c("1234567890", "9876543210")
result <- validate_and_transform_app_ids(ios_ids, "ios", lookup_mismatched = FALSE, verbose = FALSE)
stopifnot(identical(result, ios_ids))

android_ids <- c("com.example.app", "com.test.game")
result <- validate_and_transform_app_ids(android_ids, "android", lookup_mismatched = FALSE, verbose = FALSE)
stopifnot(identical(result, android_ids))

# Test error on mismatched IDs without lookup
tryCatch({
  validate_and_transform_app_ids("com.example.app", "ios", lookup_mismatched = FALSE, verbose = FALSE)
  stop("Should have thrown an error")
}, error = function(e) {
  stopifnot(grepl("Android app ID provided but iOS ID required", e$message))
})

# Test error on invalid format
tryCatch({
  validate_and_transform_app_ids("not-valid-id", "ios", lookup_mismatched = FALSE, verbose = FALSE)
  stop("Should have thrown an error")
}, error = function(e) {
  stopifnot(grepl("Invalid app ID format", e$message))
})

cat("✓ validate_and_transform_app_ids() tests passed\n\n")

cat("All app ID validation tests passed! ✓\n")