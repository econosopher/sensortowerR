st_category_data <- tibble::tribble(
  ~platform, ~category_id, ~category_name,
  "ios", "6014", "Games",
  "ios", "6016", "Social Networking",
  "android", "GAME", "Games",
  "android", "SOCIAL", "Social"
)

usethis::use_data(st_category_data, internal = TRUE, overwrite = TRUE) 