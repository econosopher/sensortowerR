st_category_data <- tibble::tribble(
  ~platform, ~category_id, ~category_name,
  "ios", "6014", "Games",
  "ios", "6016", "Social Networking",
  "android", "GAME", "Games",
  "android", "SOCIAL", "Social"
)

# Games breakdown API response key for field interpretation
games_breakdown_key <- list(
  ios = list(
    aid = "App ID",
    cc = "Country Code", 
    d = "Date",
    iu = "iPhone Downloads",
    ir = "iPhone Revenue",
    au = "iPad Downloads",
    ar = "iPad Revenue"
  ),
  android = list(
    aid = "App ID",
    cc = "Country Code",
    d = "Date", 
    u = "Android Downloads",
    r = "Android Revenue"
  )
)

usethis::use_data(st_category_data, games_breakdown_key, internal = TRUE, overwrite = TRUE) 