#' Example Data for sensortowerR Package
#'
#' Creates example data structures that mimic the API responses for testing
#' and demonstration purposes without requiring actual API calls.
#'
#' @param type Character string specifying the type of example data to create.
#'   Options: "app_info", "rankings", "details", "metrics", "publisher_apps",
#'   "top_charts", "game_summary"
#' @param n Number of example records to generate (default: 5)
#'
#' @return A tibble with example data matching the structure of actual API responses
#' 
#' @examples
#' # Get example app search results
#' example_apps <- example_sensortower_data("app_info")
#' head(example_apps)
#'
#' # Get example ranking data
#' example_rankings <- example_sensortower_data("rankings", n = 10)
#' print(example_rankings)
#'
#' @importFrom stats runif
#' @keywords internal
#' @export
example_sensortower_data <- function(type = "app_info", n = 5) {
  
  type <- match.arg(type, c("app_info", "rankings", "details", "metrics", 
                           "publisher_apps", "top_charts", "game_summary"))
  
  # Sample app names and publishers for realistic data
  app_names <- c("Candy Crush Saga", "Clash Royale", "Pokemon GO", 
                 "Roblox", "Among Us", "Genshin Impact", "PUBG Mobile",
                 "Call of Duty Mobile", "Minecraft", "Fortnite")
  
  publishers <- c("King", "Supercell", "Niantic", "Roblox Corporation",
                 "InnerSloth", "miHoYo", "Tencent", "Activision",
                 "Mojang", "Epic Games")
  
  app_ids_ios <- c("553834731", "1053012308", "834731712", "431946152",
                   "1351168404", "1517712324", "1330667917", "1476862022",
                   "479516143", "1261357398")
  
  n <- min(n, length(app_names))
  
  switch(type,
    app_info = tibble::tibble(
      unified_app_id = paste0("ua_", seq_len(n)),
      unified_app_name = app_names[seq_len(n)],
      category_details = rep(list(tibble::tibble(
        platform = "ios",
        category_id = 6014,
        category_name = "Games"
      )), n)
    ),
    
    rankings = tibble::tibble(
      rank = seq_len(n),
      app_id = app_ids_ios[seq_len(n)],
      category = 6014,
      country = "US",
      date = Sys.Date(),
      chart_type = "topfreeapplications",
      os = "ios"
    ),
    
    details = tibble::tibble(
      app_id = app_ids_ios[seq_len(n)],
      app_name = app_names[seq_len(n)],
      publisher_name = publishers[seq_len(n)],
      rating = round(runif(n, 4.0, 5.0), 2),
      rating_count = sample(10000:1000000, n),
      price = rep(0, n),
      description = paste("Example description for", app_names[seq_len(n)]),
      os = "ios"
    ),
    
    metrics = tibble::tibble(
      unified_app_id = paste0("ua_", seq_len(n)),
      date = Sys.Date() - seq_len(n) + 1,
      country = "US",
      revenue = round(runif(n, 10000, 1000000), 0),
      downloads = round(runif(n, 1000, 100000), 0),
      active_users = round(runif(n, 5000, 500000), 0)
    ),
    
    publisher_apps = tibble::tibble(
      unified_app_id = paste0("ua_", seq_len(n)),
      unified_app_name = app_names[seq_len(n)],
      publisher_id = rep("pub_123456", n),
      publisher_name = publishers[1]
    ),
    
    top_charts = tibble::tibble(
      unified_app_id = paste0("ua_", seq_len(n)),
      unified_app_name = app_names[seq_len(n)],
      revenue_180d_ww = round(runif(n, 1e6, 1e8), 0),
      downloads_180d_ww = round(runif(n, 1e5, 1e7), 0),
      mau_month_ww = round(runif(n, 1e5, 1e7), 0),
      retention_1d_us = round(runif(n, 20, 60), 1),
      retention_7d_us = round(runif(n, 10, 40), 1)
    ),
    
    game_summary = tibble::tibble(
      Date = rep(Sys.Date() - 0:2, each = n/3)[seq_len(n)],
      Country = rep(c("US", "GB", "JP"), length.out = n),
      `iOS Revenue` = round(runif(n, 1e6, 1e8), 0),
      `iOS Downloads` = round(runif(n, 1e5, 1e7), 0),
      `Android Revenue` = round(runif(n, 5e5, 5e7), 0),
      `Android Downloads` = round(runif(n, 2e5, 2e7), 0)
    )
  )
}