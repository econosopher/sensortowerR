# Independent arithmetic checks against captured response fields. No package
# normalization helpers are used here; raw responses stay local and ignored.
compare_live <- function(family, out, receipts) {
  parsed <- lapply(receipts, function(x) jsonlite::fromJSON(x$body, flatten = TRUE))
  raw <- parsed[[length(parsed)]]
  equal <- function(a,b) if (!isTRUE(all.equal(unname(as.double(a)), unname(as.double(b)), tolerance=1e-10))) stop("Independent raw/value comparison failed for ", family)
  if (family == "metadata_unified_to_ios") {
    ids <- unlist(lapply(raw$apps$itunes_apps,function(x)as.character(x$app_id)))
    stopifnot(setequal(out$app_id,ids)); return(invisible(TRUE))
  }
  if (family == "active_users") {
    for (i in seq_len(nrow(out))) {
      period <- c(dau="day",wau="week",mau="month")[[out$metric[i]]]
      k <- which(vapply(receipts,function(x) identical(x$query$time_period,period),logical(1)))
      r <- parsed[[k]]
      j <- which(as.character(r$app_id)==out$app_id[i] & r$country==out$country[i] & as.Date(r$date)==out$date[i])
      if(length(j)!=1L) stop("Ambiguous audience row")
      equal(out$value[i],r$android_users[j]+r$iphone_users[j]+r$ipad_users[j])
    }
  } else if (family %in% c("rankings","publisher_rankings","active_rankings")) {
    field <- if(family=="active_rankings") "users_absolute" else if(family=="publisher_rankings") "revenue_absolute" else "units_absolute"
    divisor <- if(family=="publisher_rankings") 100 else 1
    key <- if(family=="publisher_rankings") "publisher_id" else "app_id"
    if(!key %in% names(raw)) key <- paste0("unified_",key)
    ids <- if(family=="publisher_rankings") out$publisher_id else out$app_id
    equal(out$value,raw[[field]][match(ids,as.character(raw[[key]]))]/divisor)
  } else if (family=="charts") {
    stopifnot(identical(as.character(out$app_id),as.character(raw$ranking[seq_len(nrow(out))])))
  } else if (family=="market") {
    for(i in seq_len(nrow(out))) {
      j <- which(as.character(raw$ca)==out$category_id[i] & raw$cc==out$country[i] & as.Date(raw$d)==out$date[i])
      if(length(j)!=1L) stop("Ambiguous market row")
      equal(out$value[i],if(out$metric[i]=="revenue") (raw$ir[j]+raw$ar[j])/100 else raw$iu[j]+raw$au[j])
    }
  } else if (family %in% c("retention_facets","ratings","reviews","demographics","sessions")) {
    data <- if(family=="demographics") raw$app_data else if(family=="sessions") raw$apps$timeseries[[1]] else raw$data
    for(i in seq_len(nrow(out))) {
      j <- if("date" %in% names(data)) which(as.Date(data$date)==out$date[i]) else seq_len(nrow(data))
      if("review_rating" %in% names(out)) j <- j[data$review_rating[j]==out$review_rating[i]]
      if(length(j)!=1L || !out$metric[i] %in% names(data)) stop("Cannot match specialist value for ",family)
      equal(out$value[i],data[[out$metric[i]]][j])
    }
  } else if(family %in% c("discovery","metadata","publisher_discovery","publisher_apps","publisher_apps_ios","metadata_ios","tags","fields")) {
    data <- if(is.data.frame(raw)) raw else if(!is.null(raw$apps)) raw$apps else if(!is.null(raw$data)) raw$data else raw$custom_fields
    key <- if(family=="fields") "name" else if(family=="publisher_discovery") "publisher_id" else "app_id"
    candidates <- intersect(c(key,paste0("unified_",key),if(key=="publisher_id") "app_id"),names(data))
    if(!length(candidates) || any(!as.character(out[[key]]) %in% as.character(data[[candidates[1]]]))) stop("Raw identifier comparison failed for ",family)
  } else stop("Missing independent comparison for ",family)
  invisible(TRUE)
}
