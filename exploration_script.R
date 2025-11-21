# Exploration Script: API Responses & Unification (Genshin Impact)

devtools::load_all(".")
library(dplyr)
library(jsonlite)

# Helper to print structure safely
print_structure <- function(data, name) {
    message(sprintf("\n--- Structure of %s ---", name))
    if (is.data.frame(data)) {
        message(sprintf("Dimensions: %d rows x %d cols", nrow(data), ncol(data)))
        message("Columns: ", paste(names(data), collapse = ", "))
        if ("entities" %in% names(data)) {
            message("Contains 'entities' column (nested)")
            message(
                "Example metrics: ",
                paste(grep("(revenue|download|dau|mau|wau)", names(data),
                    value = TRUE
                )[seq_len(min(5, length(grep("(revenue|download|dau|mau|wau)",
                    names(data),
                    value = TRUE
                ))))], collapse = ", ")
            )
        }
        if (any(grepl("custom_tags", names(data)))) {
            message("Contains 'custom_tags' columns")
        }
    } else {
        str(data, max.level = 1)
    }
}

# 1. Find Genshin Impact IDs
message("1. Searching for Genshin Impact...")
search_res <- st_app_info("Genshin Impact", limit = 5)
message("Search Result Columns: ", paste(names(search_res), collapse = ", "))

# Pick the first result
target_app <- search_res[1, ]
unified_id <- target_app$unified_app_id
unified_name <- if ("unified_app_name" %in% names(target_app)) target_app$unified_app_name else "Unknown"

# Try to get platform IDs if available, otherwise set to NULL
ios_id <- if ("ios_app_id" %in% names(target_app)) target_app$ios_app_id else NULL
android_id <- if ("android_app_id" %in% names(target_app)) target_app$android_app_id else NULL

message(sprintf("\nTarget: %s (Unified: %s)", unified_name, unified_id))

# If we don't have platform IDs, try to resolve them
if (is.null(ios_id) || is.null(android_id)) {
    message("Platform IDs not found in search. Attempting resolution...")
    mapping <- tryCatch(
        {
            st_get_unified_mapping(unified_id, os = "unified")
        },
        error = function(e) NULL
    )

    if (!is.null(mapping)) {
        ios_id <- mapping$ios_app_id[1]
        android_id <- mapping$android_app_id[1]
        message(sprintf("Resolved -> iOS: %s, Android: %s", ios_id, android_id))
    }
}

# 2. Fetch Sales Data (Revenue/Downloads) - Unified with Custom Processor
message("\n2. Fetching Sales Data (Unified) with Custom Processor...")

# Custom processor to inspect raw tags
inspect_tags_processor <- function(resp, ...) {
    body_raw <- httr2::resp_body_raw(resp)
    body_text <- rawToChar(body_raw)
    data <- jsonlite::fromJSON(body_text, flatten = FALSE)

    message("Debug: inspect_tags_processor called.")
    message("Debug: data class: ", class(data))
    message("Debug: data names: ", paste(names(data), collapse = ", "))

    if (is.list(data) && "entities" %in% names(data)) {
        message("--- Raw Entities Inspection ---")
        entities <- data$entities
        if (is.list(entities)) {
            # It might be a list of lists or a data frame depending on jsonlite parsing
            if (is.data.frame(entities)) {
                tag_cols <- grep("custom_tags", names(entities), value = TRUE)
                if (length(tag_cols) > 0) {
                    message("Found ", length(tag_cols), " custom_tags columns in entities:")
                    print(head(tag_cols, 10))
                    print(head(entities[, tag_cols[1:min(length(tag_cols), 3)]], 2))
                } else {
                    message("No custom_tags columns found in entities dataframe.")
                }
            } else {
                message("Entities is a list. Inspecting first item...")
                first_entity <- entities[[1]]
                if ("custom_tags" %in% names(first_entity)) {
                    message("Found custom_tags in first entity:")
                    print(names(first_entity$custom_tags))
                }
            }
        }
    }

    # Return standard processing to continue script
    sensortowerR:::process_response(resp, enrich_response = TRUE)
}

# We need to construct the request manually to use the custom processor
# This mimics st_metrics but allows us to inject the processor
sales_data <- tryCatch(
    {
        params <- list(
            app_ids = unified_id,
            countries = "US",
            date_granularity = "monthly",
            start_date = "2024-01-01",
            end_date = "2024-01-31"
        )

        sensortowerR:::fetch_data_core(
            endpoint = "unified/sales_report_estimates",
            params = params,
            processor = inspect_tags_processor,
            auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN")
        )
    },
    error = function(e) {
        message("Error fetching sales: ", e$message)
        NULL
    }
)

if (!is.null(sales_data)) {
    print_structure(sales_data, "Sales Data (Processed)")
}

# 3. Fetch Smart Metrics (Unification)
message("\n3. Fetching Smart Metrics (Unification)...")
# Use the resolved IDs from step 1
ids_to_fetch <- c(ios_id, android_id)
ids_to_fetch <- ids_to_fetch[!is.null(ids_to_fetch)]

if (length(ids_to_fetch) > 0) {
    smart_data <- tryCatch(
        {
            st_smart_metrics(
                app_ids = ids_to_fetch,
                metrics = c("revenue", "downloads", "dau", "mau"),
                start_date = "2024-01-01",
                end_date = "2024-01-31",
                countries = "US",
                granularity = "monthly",
                parallel = FALSE,
                verbose = TRUE
            )
        },
        error = function(e) {
            message("Error fetching smart metrics: ", e$message)
            NULL
        }
    )

    if (!is.null(smart_data)) {
        print_structure(smart_data, "Smart Metrics Result")
        print(head(smart_data))

        # Check if we have both sales and usage metrics for the same entity
        if ("metric" %in% names(smart_data)) {
            metrics_found <- unique(smart_data$metric)
            message("Metrics found: ", paste(metrics_found, collapse = ", "))
        }
    }
} else {
    message("No platform IDs available for smart metrics test.")
}

# 3b. Debug: Fetch DAU directly for iOS
message("\n3b. Debug: Fetching DAU directly for iOS...")
if (!is.null(ios_id)) {
    dau_data <- tryCatch(
        {
            params <- list(
                app_ids = ios_id,
                countries = "US",
                start_date = "2024-01-01",
                end_date = "2024-01-05", # Shorten range for daily to avoid huge output
                time_period = "day"
            )
            # Try fetching monthly active users first as it's less data
            sensortowerR:::fetch_data_core(
                endpoint = "ios/usage/active_users",
                params = params,
                auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN")
            )
        },
        error = function(e) {
            message("Error fetching DAU: ", e$message)
            NULL
        }
    )

    if (!is.null(dau_data)) {
        print_structure(dau_data, "Direct DAU Result")
        print(head(dau_data))
    } else {
        message("No DAU data returned for iOS.")
    }
}

# 4. Deep Dive into 'entities' structure (Mocking a raw response inspection)
# We'll look at how utils.R processes the response by looking at the 'sales_data' again
# which went through process_response.
# If we want to see the raw 'entities' before unnesting, we'd need to debug process_response.
# For now, let's look at what columns survived unnesting in sales_data.

if (!is.null(sales_data)) {
    message("\n4. Inspecting Unnested Columns in Sales Data...")
    # Look for columns that might have come from 'entities'
    entity_cols <- grep("^entities", names(sales_data), value = TRUE)
    if (length(entity_cols) > 0) {
        message("Columns from 'entities': ", paste(head(entity_cols, 10), collapse = ", "))
    } else {
        message("No 'entities.*' columns found (likely cleaned up or fully flattened).")
    }

    # Look for app name variations
    name_cols <- grep("name", names(sales_data), ignore.case = TRUE, value = TRUE)
    message("Name columns: ", paste(name_cols, collapse = ", "))
    if (length(name_cols) > 0) {
        print(unique(sales_data[, name_cols]))
    }
}
