# Debug st_metrics issue
library(sensortowerR)

# Test what st_sales_report returns
result <- st_sales_report(
  app_ids = "1195621598",
  os = "ios", 
  countries = "US",
  start_date = Sys.Date() - 7,
  end_date = Sys.Date() - 1,
  date_granularity = "daily"
)

cat("Columns returned by st_sales_report:\n")
print(names(result))

cat("\nFirst few rows:\n")
print(head(result))

# Check for revenue columns
rev_cols <- grep("revenue", names(result), value = TRUE)
cat("\nRevenue columns found:", paste(rev_cols, collapse = ", "), "\n")

dl_cols <- grep("download", names(result), value = TRUE)
cat("Download columns found:", paste(dl_cols, collapse = ", "), "\n")