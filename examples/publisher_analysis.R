# Publisher Revenue Analysis with Spider Chart and GT Table
# Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  devtools,
  dplyr,
  tidyr,
  ggplot2,
  gt,
  gtExtras,
  scales,
  lubridate,
  fmsb,
  gridExtra,
  stringr,
  webshot2,
  httr,
  jsonlite,
  tibble,
  purrr
)

# Load the sensortowerR package
devtools::load_all()

# Set API key
auth_token <- "ST0_SRc8L4bf_XHVd6VQUHbvLgQ"

# Function to fetch top publishers by revenue using the new native function
fetch_top_publishers <- function(limit = 10, auth_token = auth_token) {
  # Use the new st_top_publishers function
  publisher_data <- st_top_publishers(
    measure = "revenue",
    os = "unified",
    category = 6014,  # Games category
    time_range = "month",
    comparison_attribute = "absolute",
    date = Sys.Date() - 180,  # For 180-day data
    end_date = Sys.Date() - 1,
    # country defaults to "WW" for worldwide data
    limit = limit,
    include_apps = TRUE,
    auth_token = auth_token
  )
  
  # If API call fails or returns no data, stop with error
  if (is.null(publisher_data) || nrow(publisher_data) == 0) {
    stop("Failed to fetch publisher data from Sensor Tower API. Please check your API token and connection.")
  }
  
  # Ensure we have the column names we expect
  if ("revenue_usd" %in% names(publisher_data)) {
    publisher_data <- publisher_data %>%
      rename(revenue_180d_ww = revenue_usd)
  } else if ("revenue_absolute" %in% names(publisher_data)) {
    publisher_data <- publisher_data %>%
      mutate(revenue_180d_ww = revenue_absolute / 100)
  }
  
  return(publisher_data)
}

# Function to generate category revenue breakdown for publishers
generate_category_breakdown <- function(publisher_data) {
  # Extract publisher IDs
  publisher_ids <- publisher_data$publisher_id
  
  # Use the new st_publisher_category_breakdown function
  category_data <- st_publisher_category_breakdown(
    publisher_ids = publisher_ids,
    time_range = "month",
    date = Sys.Date() - 180,
    os = "unified",
    auth_token = auth_token
  )
  
  # Map category IDs to names
  category_names <- c(
    "6014" = "Games",
    "7001" = "Action",
    "7002" = "Adventure", 
    "7003" = "Arcade",
    "7004" = "Board",
    "7005" = "Card",
    "7006" = "Casino",
    "7014" = "RPG",
    "7015" = "Simulation",
    "7017" = "Strategy",
    "7019" = "Puzzle"
  )
  
  # Process the data
  if (!is.null(category_data) && nrow(category_data) > 0) {
    category_data <- category_data %>%
      mutate(
        category = ifelse(category_id %in% names(category_names), 
                         category_names[category_id], 
                         category_id),
        percentage = category_percentage,
        revenue_amount = revenue_usd
      ) %>%
      select(publisher_name, category, percentage, revenue_amount)
  } else {
    stop("Failed to fetch category breakdown data from Sensor Tower API.")
  }
  
  return(category_data)
}

# Function to create spider chart
create_spider_chart <- function(category_data, top_n = 5) {
  # Prepare data for spider chart
  spider_data <- category_data %>%
    filter(publisher_name %in% unique(publisher_name)[1:top_n]) %>%
    select(publisher_name, category, percentage) %>%
    pivot_wider(names_from = category, values_from = percentage, values_fill = 0)
  
  # Create the plot using ggplot2 with polar coordinates
  p <- category_data %>%
    filter(publisher_name %in% unique(publisher_name)[1:top_n]) %>%
    ggplot(aes(x = category, y = percentage, group = publisher_name, 
               color = publisher_name, fill = publisher_name)) +
    geom_polygon(alpha = 0.2, linewidth = 1) +
    geom_point(size = 3) +
    coord_polar() +
    scale_y_continuous(limits = c(0, 100)) +
    labs(
      title = "Publisher Revenue Distribution by Game Category",
      subtitle = "Percentage of revenue derived from each category",
      x = "",
      y = "Revenue %",
      color = "Publisher",
      fill = "Publisher"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 12, hjust = 0),
      plot.title.position = "panel",
      plot.subtitle.position = "panel",
      axis.text.x = element_text(size = 10, face = "bold"),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major = element_line(color = "gray80"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    ) +
    scale_color_brewer(palette = "Set2") +
    scale_fill_brewer(palette = "Set2")
  
  return(p)
}

# Function to create GT table with revenue metrics
create_revenue_gt_table <- function(publisher_data, category_data) {
  # Use actual data from API response
  table_data <- publisher_data %>%
    mutate(
      # Calculate market share (as percentage of total)
      market_share = (revenue_180d_ww / sum(revenue_180d_ww)) * 100,
      # Use rank if available, otherwise calculate
      rank = if ("rank" %in% names(.)) rank else row_number()
    )
  
  # Select columns based on what's available in the data
  available_cols <- c("rank", "publisher_name", "revenue_180d_ww", "market_share")
  
  # Add optional columns if they exist
  if ("revenue_delta" %in% names(publisher_data)) {
    table_data <- table_data %>%
      mutate(growth_180d = (revenue_delta / (revenue_180d_ww - revenue_delta)) * 100)
    available_cols <- c(available_cols, "growth_180d")
  }
  
  # Add app count if available
  if ("apps" %in% names(publisher_data)) {
    table_data <- table_data %>%
      mutate(app_count = map_int(apps, ~if(is.data.frame(.x)) nrow(.x) else 0))
    available_cols <- c(available_cols, "app_count")
  }
  
  table_data <- table_data %>%
    select(all_of(available_cols))
  
  # Create GT table
  gt_table <- table_data %>%
    gt() %>%
    
    # Apply FiveThirtyEight theme
    gt_theme_538() %>%
    
    # Header
    tab_header(
      title = "Top Mobile Game Publishers by Revenue",
      subtitle = paste("Worldwide revenue metrics as of", format(Sys.Date(), "%B %Y"))
    ) %>%
    
    # Dynamic column labels based on available columns
    cols_label(
      rank = "",
      publisher_name = "Publisher",
      revenue_180d_ww = "180-Day Revenue",
      market_share = "Market Share"
    )
  
  # Add optional column labels if they exist
  if ("growth_180d" %in% names(table_data)) {
    gt_table <- gt_table %>%
      cols_label(growth_180d = "180d Growth %")
  }
  
  if ("app_count" %in% names(table_data)) {
    gt_table <- gt_table %>%
      cols_label(app_count = "App Count")
  }
  
  # Continue building the table
  gt_table <- gt_table %>%
    
    # Format revenue columns
    fmt_currency(
      columns = revenue_180d_ww,
      currency = "USD",
      decimals = 0,
      suffixing = TRUE
    ) %>%
    
    # Format market share
    fmt_percent(
      columns = market_share,
      decimals = 1
    )
  
  # Format optional columns if they exist
  if ("growth_180d" %in% names(table_data)) {
    gt_table <- gt_table %>%
      fmt_percent(
        columns = growth_180d,
        decimals = 1,
        scale_values = FALSE
      )
  }
  
  if ("app_count" %in% names(table_data)) {
    gt_table <- gt_table %>%
      fmt_number(
        columns = app_count,
        decimals = 0
      )
  }
  
  gt_table <- gt_table %>%
    
    # Add bar charts to revenue columns
    text_transform(
      locations = cells_body(columns = revenue_180d_ww),
      fn = function(x) {
        values <- as.numeric(gsub("[^0-9.]", "", x)) * 
                 ifelse(grepl("B", x), 1e9, ifelse(grepl("M", x), 1e6, 1))
        max_val <- max(values, na.rm = TRUE)
        
        purrr::map2_chr(x, values, function(label, val) {
          bar_width <- (val / max_val) * 100
          sprintf(
            '<div style="display: flex; align-items: center; gap: 8px;">
              <div style="flex: 0 0 80px; height: 18px; background: #E1E1E1; position: relative; border-radius: 3px;">
                <div style="height: 100%%; width: %s%%; background: #FF6600; border-radius: 3px;"></div>
              </div>
              <span style="font-size: 13px; color: #333; font-weight: 600;">%s</span>
            </div>',
            round(bar_width),
            label
          )
        })
      }
    )
  
  # Style growth columns with color if they exist
  if ("growth_180d" %in% names(table_data)) {
    gt_table <- gt_table %>%
      data_color(
        columns = growth_180d,
        method = "numeric",
        palette = c("#D32F2F", "#FFEB3B", "#4CAF50"),
        domain = c(-20, 0, 40),
        na_color = "#F5F5F5"
      )
  }
  
  # Add ranking medals
  gt_table <- gt_table %>%
    text_transform(
      locations = cells_body(columns = rank, rows = 1),
      fn = function(x) paste0(x, " 🥇")
    ) %>%
    text_transform(
      locations = cells_body(columns = rank, rows = 2),
      fn = function(x) paste0(x, " 🥈")
    ) %>%
    text_transform(
      locations = cells_body(columns = rank, rows = 3),
      fn = function(x) paste0(x, " 🥉")
    ) %>%
    
    # Style the rank column
    tab_style(
      style = list(
        cell_text(
          size = px(20),
          weight = "bold",
          color = "#666666"
        )
      ),
      locations = cells_body(columns = rank)
    ) %>%
    
    # Highlight top 3 publishers
    tab_style(
      style = list(
        cell_fill(color = "#F2F2F2"),
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        rows = 1:3,
        columns = publisher_name
      )
    ) %>%
    
    # Add source note
    tab_source_note(
      source_note = md("**Source:** Sensor Tower Store Intelligence | **Note:** Revenue figures are estimates")
    ) %>%
    
    # Table options
    tab_options(
      data_row.padding = px(4),
      row.striping.include_table_body = FALSE
    )
  
  return(gt_table)
}

# Main execution
message("Fetching top publishers...")
top_publishers <- fetch_top_publishers(limit = 10, auth_token = auth_token)

message("Generating category breakdown...")
category_breakdown <- generate_category_breakdown(top_publishers)

message("Creating spider chart...")
spider_chart <- create_spider_chart(category_breakdown, top_n = 5)

message("Creating GT table...")
revenue_table <- create_revenue_gt_table(top_publishers, category_breakdown)

# Create output directory if it doesn't exist
output_dir <- "inst/images"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save spider chart
ggsave(
  filename = file.path(output_dir, "publisher_spider_chart_api.png"),
  plot = spider_chart,
  width = 10,
  height = 8,
  dpi = 300
)

# Save GT table
revenue_table %>%
  gtsave(
    filename = file.path(output_dir, "publisher_revenue_table_api.png"),
    vwidth = 1400,
    vheight = 600
  )

# Display messages
cat("\nAnalysis complete!\n")
cat("Spider chart saved to:", file.path(output_dir, "publisher_spider_chart_api.png"), "\n")
cat("Revenue table saved to:", file.path(output_dir, "publisher_revenue_table_api.png"), "\n")

# Optional: Display the GT table in the console
print(revenue_table)