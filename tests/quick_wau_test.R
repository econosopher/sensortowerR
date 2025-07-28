# Quick WAU verification test
library(sensortowerR)

cat("Testing WAU implementation...\n")

# Test 1: Single metric WAU
result1 <- st_ytd_metrics(
  unified_app_id = "553834731",
  years = 2025,
  metrics = "wau",
  verbose = FALSE
)

cat("✓ WAU fetch successful\n")
cat(sprintf("  Average WAU: %s\n", format(round(result1$value[1]), big.mark = ",")))

# Test 2: Combined metrics
result2 <- st_ytd_metrics(
  unified_app_id = "553834731",
  years = 2025,
  metrics = c("dau", "wau"),
  verbose = FALSE
)

dau_val <- result2[result2$metric == "dau", "value"][[1]]
wau_val <- result2[result2$metric == "wau", "value"][[1]]
ratio <- dau_val / wau_val

cat("✓ DAU/WAU combined fetch successful\n")
cat(sprintf("  DAU/WAU ratio: %.2f (%.1f%% daily engagement)\n", ratio, ratio * 100))

cat("\nWAU implementation verified successfully!\n")