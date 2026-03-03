library(testthat)
library(SensorTowerR)

# Run cross-platform checks if requested
if (nzchar(Sys.getenv("RUN_CROSS_PLATFORM_CHECKS"))) {
  message("Running cross-platform checks...")
}

test_check("SensorTowerR") 
