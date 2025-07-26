#!/usr/bin/env Rscript

# Monitor rhub GitHub Actions workflow

monitor_rhub_run <- function(run_id = NULL) {
  if (is.null(run_id)) {
    # Get latest run
    runs <- system2("gh", 
                    args = c("run", "list", "--workflow=rhub.yaml", 
                             "--repo", "econosopher/sensortowerR", 
                             "--limit", "1", "--json", "databaseId,status"),
                    stdout = TRUE)
    run_info <- jsonlite::fromJSON(runs)
    if (nrow(run_info) == 0) {
      stop("No rhub runs found")
    }
    run_id <- run_info$databaseId[1]
  }
  
  cat("Monitoring rhub run:", run_id, "\n")
  cat("View on GitHub: https://github.com/econosopher/sensortowerR/actions/runs/", run_id, "\n\n", sep = "")
  
  # Monitor until complete
  while (TRUE) {
    status <- system2("gh", 
                      args = c("run", "view", run_id, 
                               "--repo", "econosopher/sensortowerR", 
                               "--json", "status,conclusion,jobs"),
                      stdout = TRUE)
    
    run_data <- jsonlite::fromJSON(status)
    
    # Display job status
    cat("\033[2J\033[H") # Clear screen
    cat("=== R-hub Cross-Platform Check Status ===\n")
    cat("Run ID:", run_id, "\n")
    cat("Status:", run_data$status, "\n")
    if (!is.null(run_data$conclusion)) {
      cat("Conclusion:", run_data$conclusion, "\n")
    }
    cat("\nJobs:\n")
    
    jobs <- run_data$jobs
    for (i in seq_len(nrow(jobs))) {
      job <- jobs[i, ]
      icon <- if (job$status == "completed") {
        if (job$conclusion == "success") "✓" else "✗"
      } else if (job$status == "in_progress") {
        "⋯"
      } else {
        "○"
      }
      cat(sprintf("  %s %s (%s)\n", icon, job$name, job$status))
    }
    
    # Check if all done
    if (run_data$status == "completed") {
      cat("\nWorkflow completed with conclusion:", run_data$conclusion, "\n")
      
      # Show summary
      if (run_data$conclusion == "success") {
        cat("\n✅ All platform checks passed!\n")
      } else {
        cat("\n❌ Some checks failed. Review the logs at:\n")
        cat("   https://github.com/econosopher/sensortowerR/actions/runs/", run_id, "\n", sep = "")
      }
      break
    }
    
    Sys.sleep(10) # Check every 10 seconds
  }
}

# If run directly
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  run_id <- if (length(args) > 0) args[1] else NULL
  monitor_rhub_run(run_id)
}