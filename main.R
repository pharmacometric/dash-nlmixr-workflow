# Main execution script for the nlmixr workflow pipeline

# Autodetect script location to ensure relative paths work reliably
get_pkg_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_match <- grep("^--file=", cmd_args)
  if (length(file_match) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", cmd_args[file_match[1]]))))
  } else {
    tryCatch({
      for (i in seq_along(sys.frames())) {
        if (exists("ofile", envir = sys.frames()[[i]])) {
          return(dirname(normalizePath(get("ofile", envir = sys.frames()[[i]]))))
        }
      }
    }, error = function(e) {})
  }
  return(".")
}

workflow_dir <- get_pkg_dir()

source(file.path(workflow_dir, "R", "parse_config.R"))
source(file.path(workflow_dir, "R", "build_model.R"))
source(file.path(workflow_dir, "R", "run_estimation.R"))
source(file.path(workflow_dir, "R", "generate_reports.R"))

#' Run the full nlmixr pipeline from a JSON configuration
#'
#' @param config_path Path to the JSON configuration file translated from a NONMEM control stream.
#' @param data_path Path to the analysis dataset (CSV).
#' @param output_dir Directory to save pipeline artifacts and reports.
#' @return The fitted nlmixr model object.
run_pipeline <- function(config_path, data_path = NULL, output_dir = "outputs") {
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Setup verbose logging
  start_time <- Sys.time()
  rand_str <- paste(sample(c(letters, 0:9), 6, replace = TRUE), collapse = "")
  log_filename <- sprintf("run_%s_%s.log", format(start_time, "%Y%m%d_%H%M%S"), rand_str)
  log_path <- file.path(output_dir, log_filename)
  
  log_con <- file(log_path, open = "wt")
  sink(log_con, split = TRUE)
  sink(log_con, type = "message")
  
  cat("=========================================================\n")
  cat("             NLMIXR WORKFLOW EXECUTION LOG               \n")
  cat("=========================================================\n")
  cat("Start Time: ", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Config Path:", config_path, "\n")
  cat("Data Path:  ", if (is.null(data_path)) "Will be resolved from config" else data_path, "\n")
  cat("Output Dir: ", output_dir, "\n")
  cat("=========================================================\n\n")
  
  fit <- NULL
  
  tryCatch({
    # 1. Parse JSON Config
    message("Step 1: Parsing JSON configuration...")
    config <- parse_json_config(config_path)
    
    if (is.null(data_path)) {
      data_path <- config$data$file
      if (is.null(data_path)) {
        stop("Data path not provided as argument and not found in configuration.")
      }
      message("Resolved data path from configuration: ", data_path)
    } else {
      config$data$file <- data_path
      message("Using explicitly provided data path: ", data_path)
    }
    
    # 2. Build nlmixr model UI
    message("Step 2: Building nlmixr model from configuration...")
    model_ui <- build_nlmixr_model(config)
    
    # 3. Load Dataset
    message("Step 3: Loading dataset...")
    if (!file.exists(data_path)) {
      stop("Dataset not found at path: ", data_path)
    }
    dataset <- read.csv(data_path)
    
    # 4. Run Estimation
    message("Step 4: Running nlmixr estimation...")
    fit <- run_nlmixr_estimation(model_ui, dataset, config$estimation)
    
    # 5. Generate Reports
    message("Step 5: Generating diagnostic reports...")
    generate_gof_plots(fit, output_dir, config)
    generate_parameter_table(fit, output_dir)
    generate_predictions_table(fit, output_dir)
    
    message("\nPipeline complete. Artifacts saved to '", output_dir, "'.")
    message("Execution Log: ", log_path)
    
  }, error = function(e) {
    message("\n[ERROR] Pipeline aborted due to an error: ", e$message)
    generate_failed_summary(output_dir, e$message)
  }, finally = {
    end_time <- Sys.time()
    exec_time <- difftime(end_time, start_time)
    
    cat("\n=========================================================\n")
    cat("End Time:   ", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
    cat("Elapsed:    ", round(as.numeric(exec_time), 2), " ", attr(exec_time, "units"), "\n")
    cat("=========================================================\n")
    
    tryCatch(sink(type = "message"), error=function(e){})
    while (sink.number() > 0) {
      tryCatch(sink(), error=function(e){})
    }
    tryCatch(close(log_con), error=function(e){})
  })
  
  return(fit)
}
