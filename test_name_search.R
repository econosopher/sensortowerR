library(sensortowerR)

# Test searching by different name variants
cat("Testing name-based search for unified IDs:\n\n")

# Test 1: Dragon Ball Z with different cases
names1 <- c("Dragon Ball Z Dokkan Battle", "DRAGON BALL Z DOKKAN BATTLE")
cat("Test 1: Dragon Ball Z variants\n")
for (name in names1) {
  cat("  Searching for:", name, "\n")
  result <- st_app_info(
    term = name,
    app_store = "unified",
    limit = 1,
    return_all_fields = TRUE
  )
  if (!is.null(result) && nrow(result) > 0) {
    cat("    Found unified ID:", result$app_id[1], "\n")
  }
}

# Test 2: Honkai Star Rail (if it exists in different languages)
cat("\n\nTest 2: Honkai Star Rail\n")
names2 <- c("Honkai: Star Rail", "崩壊：スターレイル", "崩坏：星穹铁道")
for (name in names2) {
  cat("  Searching for:", name, "\n")
  result <- tryCatch({
    st_app_info(
      term = name,
      app_store = "unified",
      limit = 1,
      return_all_fields = TRUE
    )
  }, error = function(e) NULL)
  
  if (!is.null(result) && nrow(result) > 0) {
    cat("    Found unified ID:", result$app_id[1], "\n")
    cat("    App name returned:", result$name[1], "\n")
  } else {
    cat("    No results\n")
  }
}

# Test 3: Check if searching by exact platform ID works
cat("\n\nTest 3: Platform ID search\n")
platform_ids <- list(
  ios = "943599237",
  android = "com.bandainamcogames.dbzdokkanww"
)

for (type in names(platform_ids)) {
  id <- platform_ids[[type]]
  cat("  Searching for", type, "ID:", id, "\n")
  
  # Try searching in unified
  result <- tryCatch({
    st_app_info(
      term = id,
      app_store = "unified",
      limit = 10,
      return_all_fields = TRUE
    )
  }, error = function(e) NULL)
  
  if (!is.null(result) && nrow(result) > 0) {
    # Check if any result contains our platform ID
    for (i in 1:nrow(result)) {
      found <- FALSE
      if (type == "ios" && "ios_apps" %in% names(result)) {
        ios_apps <- result$ios_apps[[i]]
        if (!is.null(ios_apps) && is.data.frame(ios_apps) && id %in% ios_apps$app_id) {
          found <- TRUE
        }
      } else if (type == "android" && "android_apps" %in% names(result)) {
        android_apps <- result$android_apps[[i]]
        if (!is.null(android_apps) && is.data.frame(android_apps) && id %in% android_apps$app_id) {
          found <- TRUE
        }
      }
      
      if (found) {
        cat("    FOUND in result", i, "!\n")
        cat("    Unified ID:", result$app_id[i], "\n")
        cat("    App name:", result$name[i], "\n")
        break
      }
    }
  } else {
    cat("    No results or error\n")
  }
}