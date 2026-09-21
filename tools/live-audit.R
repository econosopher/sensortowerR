# Opt-in, read-only contract checks. No filter creation or account changes.
if (!identical(Sys.getenv("SENSORTOWER_RUN_LIVE"), "true")) stop("Set SENSORTOWER_RUN_LIVE=true to run bounded live reads.")
pkgload::load_all(quiet = TRUE)
dir.create("audit", showWarnings = FALSE)
source("tools/compare-live.R")
rows <- list(); responses <- list(); request_count <- 0L
raw_perform <- sensortowerR:::.st_perform
observer <- function(req) {
  request_count <<- request_count + 1L
  if (request_count > 40L) stop("Live audit request budget exceeded.")
  u <- httr2::url_parse(req$url)
  if (!is.null(req$method) && req$method != "GET") stop("Live audit allows GET only.")
  # Keep a compact successful response for independent parser comparisons.
  resp <- raw_perform(req)
  if (httr2::resp_status(resp) < 300L) {
    q <- u$query; q$auth_token <- NULL
    body <- if (length(resp$body)) rawToChar(resp$body) else ""
    token <- Sys.getenv("SENSORTOWER_AUTH_TOKEN")
    if (nzchar(token)) body <- gsub(token, "[REDACTED]", body, fixed = TRUE)
    responses[[length(responses) + 1L]] <<- list(path = u$path, query = q, body = body)
  }
  resp
}
run <- function(family, fun, compare = NULL) {
  start <- Sys.time(); before <- request_count; response_start <- length(responses)
  result <- tryCatch({
    x <- fun()
    if (!is.data.frame(x)) stop("Expected a tibble result")
    if (nrow(x)) {
      if (!is.null(compare)) compare(x) else compare_live(family,x,responses[seq.int(response_start+1L,length(responses))])
    }
    list(status = if (nrow(x)) "live_verified" else "live_empty", detail = if (nrow(x)) "Request, parser and independent raw comparison passed" else "Successful empty response; values not verified", rows = nrow(x), value = x)
  }, error = function(e) {
    while (!is.null(e$parent)) e <- e$parent
    list(status = if (!is.null(e$status) && e$status %in% c(401L,403L,404L)) "unavailable" else "failed", detail = conditionMessage(e), rows = 0L, value = NULL)
  })
  rows[[length(rows) + 1L]] <<- data.frame(family = family, status = result$status, rows = result$rows, requests = request_count - before,
    detail = result$detail, checked_at = format(start, tz = "UTC", usetz = TRUE))
  cat(family, result$status, result$rows, "rows\n")
  utils::write.csv(do.call(rbind, rows), "audit/live-audit.csv", row.names = FALSE)
  jsonlite::write_json(responses, "audit/live-responses.json", auto_unbox = TRUE, pretty = TRUE)
  result$value
}
if (!nzchar(Sys.getenv("SENSORTOWER_AUTH_TOKEN"))) {
  families <- c("discovery", "metadata", "publisher_discovery", "publisher_apps", "sales", "active_users", "rankings", "charts", "market",
    "retention_facets", "retention_legacy", "demographics", "sessions", "ratings", "reviews", "tags", "fields")
  utils::write.csv(data.frame(family = families, status = "unavailable", rows = 0L, requests = 0L, detail = "No configured token"), "audit/live-audit.csv", row.names = FALSE)
  quit(status = 0)
}
compare_sales <- function(x) {
  raw <- jsonlite::fromJSON(responses[[length(responses)]]$body)
  platform <- unique(x$os)
  if(length(platform)!=1L) stop("Unexpected mixed-platform receipt")
  ids <- if(platform=="unified") raw$app_id else raw$aid
  country <- if(platform=="unified") raw$country else if(platform=="ios") raw$cc else raw$c
  date <- as.Date(if(platform=="unified") raw$date else raw$d)
  for (i in seq_len(nrow(x))) {
    j <- which(as.character(ids)==x$app_id[i] & country==x$country[i] & date==x$date[i])
    if(length(j)!=1L) stop("Cannot match raw sales observation")
    revenue <- if(platform=="unified") raw$unified_revenue[j] else if(platform=="ios") raw$ir[j]+raw$ar[j] else raw$r[j]
    downloads <- if(platform=="unified") raw$unified_units[j] else if(platform=="ios") raw$iu[j]+raw$au[j] else raw$u[j]
    expected <- if(x$metric[i]=="revenue") revenue/100 else downloads
    if(!isTRUE(all.equal(x$value[i],as.double(expected)))) stop("Raw/normalized sales mismatch")
  }
}

invisible(testthat::with_mocked_bindings({
  apps <- run("discovery", function() st_apps("Clash of Clans", limit = 3))
  selected <- if (!is.null(apps) && nrow(apps)) apps[1L,c("app_id","os")] else tibble::tibble(app_id="55c5025102ac64f9c0001f96",os="unified")
  meta <- run("metadata", function() st_app(selected))
  pubs <- run("publisher_discovery", function() st_publishers("Supercell", limit=1))
  publisher <- if (!is.null(pubs) && nrow(pubs)) pubs[1L,c("publisher_id","os")] else tibble::tibble(publisher_id="560c48b48ac350643900b82d",os="unified")
  run("publisher_apps", function() st_publisher_apps(publisher,limit=3))
  run("sales", function() st_metrics(selected,"2026-08-01","2026-08-02",countries="US"),compare_sales)
  run("active_users", function() st_metrics(selected,"2026-08-01","2026-08-31",metrics=c("dau","wau","mau"),countries="US"))
  run("rankings", function() st_rankings("downloads","ios","2026-08-01","2026-08-31",countries="US",category="6014",limit=2))
  run("charts", function() st_charts("ios","6014","topfreeapplications","2026-08-01",countries="US",limit=2))
  run("market", function() st_market_metrics("7001","ios","2026-08-01","2026-08-02",countries="US",granularity="daily"))
  run("retention_facets", function() st_retention(selected,"2026-08-01","2026-08-31",countries="US"))
  store <- tibble::tibble(app_id="529479190",os="ios")
  run("retention_legacy", function() st_retention(store,"2026-04-01","2026-06-30",countries="US",method="legacy"))
  run("demographics", function() st_demographics(store,"2026-04-01","2026-06-30",countries="US"))
  run("sessions", function() st_sessions(store,"2026-08-01","2026-08-31",countries="US"))
  run("ratings", function() st_ratings(store,"2026-08-01","2026-08-02",countries="US"))
  run("reviews", function() st_reviews(store,"2026-08-01","2026-08-02",countries="US"))
  run("tags", function() st_app_tags(store, fields = "Game Genre"))
  run("fields", function() st_fields("Game Genre"))
  run("sales_ios", function() st_metrics(store,"2026-08-01","2026-08-02",countries="US"),compare_sales)
  run("sales_android", function() st_metrics("com.supercell.clashofclans","2026-08-01","2026-08-02",os="android",countries="US"),compare_sales)
  run("sales_batch", function() st_metrics(c(selected$app_id,"55c5022602ac64f9c0001f7c"),"2026-08-01","2026-08-02",os="unified",countries="US"),compare_sales)
  run("metadata_unified_to_ios", function() st_app(selected,target_os="ios"))
  run("publisher_apps_ios", function() st_publisher_apps("488106216",os="ios",limit=2))
  run("metadata_ios", function() st_app(store))
  run("publisher_rankings", function() st_rankings("revenue","ios","2026-08-01","2026-08-31",entity="publisher",countries="US",limit=2))
  run("active_rankings", function() st_rankings("mau","ios","2026-08-01","2026-08-31",countries="US",limit=2))
}, .st_perform = observer, .package = "sensortowerR"))
