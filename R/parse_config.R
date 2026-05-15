# Uses jsonlite to parse translated NONMEM artifacts
library(jsonlite)

#' Parse and validate the JSON configuration
#' 
#' @param file_path Path to the config file
#' @return A validated R list containing configuration blocks
parse_json_config <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("Configuration file not found: ", file_path)
  }
  
  # Parse JSON string into R lists
  config <- fromJSON(file_path, simplifyVector = FALSE)
  
  # Validate minimal expected structure required for nlmixr execution
  required_keys <- c("model", "data", "parameters", "estimation")
  missing_keys <- setdiff(required_keys, names(config))
  
  if (length(missing_keys) > 0) {
    stop("Invalid JSON structure. Missing required translation blocks: ", 
         paste(missing_keys, collapse = ", "))
  }
  
  if (is.null(config$parameters$theta) || is.null(config$parameters$omega)) {
    warning("Config appears to be missing standard parameter definitions (theta, omega).")
  }
  
  return(config)
}
