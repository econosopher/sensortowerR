# sensortowerR Manual Test Scripts

This directory contains two comprehensive test scripts for validating the sensortowerR package functionality:

## test-all-api-functions.R
**Purpose**: Tests all major API functions to ensure they work correctly and return expected data.

**Functions tested**:
- st_metrics (iOS, Android, Unified)
- st_batch_metrics (revenue/downloads)
- st_yoy_metrics (year-over-year comparisons)
- st_top_charts (market rankings)
- st_top_publishers (publisher rankings)
- st_app_info (app search)
- st_app_lookup (app details)
- st_game_summary (category analysis)
- st_category_rankings (category charts)

**Output**: Generates a detailed markdown report with test results and timing information.

## test-active-users.R
**Purpose**: Comprehensive test suite focusing on active user metrics but also covering various functions with different configurations.

**Key test areas**:
1. **ID Type Testing**: iOS numeric, Android package, unified hex IDs
2. **Granularity Testing**: Daily, weekly, monthly data
3. **Country Testing**: Multiple country support (US, GB, JP, WW)
4. **Function Coverage**: 
   - st_metrics with various configurations
   - st_batch_metrics with active users (DAU, WAU, MAU)
   - st_yoy_metrics for year-over-year analysis
   - st_top_charts, st_app_info, st_game_summary
5. **Performance Testing**: Batch processing efficiency and rate limit warnings
6. **Data Validation**: Cross-platform comparisons and metric relationships

## Running the Tests

```bash
# Run all API function tests
Rscript test-all-api-functions.R

# Run comprehensive test suite
Rscript test-active-users.R
```

Both scripts are optimized to minimize API calls while ensuring comprehensive coverage of functionality.