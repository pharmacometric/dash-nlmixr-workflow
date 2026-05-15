#!/usr/bin/env Rscript

# Command line interface for the nlmixr workflow pipeline
# Usage: Rscript main_cli.R <config_path> <output_dir> [data_path]

# Determine the directory of the current script to safely source relative files
initial_options <- commandArgs(trailingOnly = FALSE)
file_arg_name <- "^--file="
script_name <- sub(file_arg_name, "", initial_options[grep(file_arg_name, initial_options)])
script_dir <- if (length(script_name) > 0) dirname(normalizePath(script_name[1])) else "."

# Source the main pipeline logic (chdir = TRUE ensures clean execution context)
suppressMessages(source(file.path(script_dir, "main.R"), chdir = TRUE))

# Parse trailing arguments
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 2) {
  cat("Error: Missing required arguments.\n\n")
  cat("Usage: Rscript main_cli.R <config_path> <output_dir> [data_path]\n")
  cat("  config_path : Path to the JSON configuration file\n")
  cat("  output_dir  : Directory path passed in to be used as work area for outputting files\n")
  cat("  data_path   : (Optional) Path to the CSV dataset. If omitted, uses path from JSON config.\n")
  quit(status = 1)
}

config_path <- args[1]
output_dir <- args[2]
data_path <- if (length(args) >= 3) args[3] else NULL

cat("=========================================================\n")
cat("Starting CLI nlmixr workflow...\n")
cat("=========================================================\n")

# Run pipeline
fit <- run_pipeline(
  config_path = config_path,
  data_path = data_path,
  output_dir = output_dir
)

if (!is.null(fit)) {
  cat("\nCLI execution completed successfully.\n")
} else {
  cat("\nCLI execution encountered an error. Please check the generated log file.\n")
  quit(status = 1)
}
