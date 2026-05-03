# MA223 Exam R Package
# Utility Functions

#' Get repository root from .env file
#' @return Character string with repo root path
get_repo_root <- function() {
  env_path <- file.path(getwd(), ".env")
  if (!file.exists(env_path)) {
    stop(".env file not found")
  }

  lines <- readLines(env_path, warn = FALSE)
  line <- grep("REPO_ROOT", lines, value = TRUE)
  gsub('REPO_ROOT=|"', '', line)
}

#' Get output directory
#' @return Character string with output path
get_output_dir <- function() {
  "./output"
}

#' List available experiments
#' @param base_path Base path to experiments directory
#' @return Character vector of experiment IDs
list_experiments <- function(base_path = get_repo_root()) {
  exp_path <- file.path(base_path, "results/experiments/ActivationComparison")

  if (!dir.exists(exp_path)) {
    stop("Experiments directory not found: ", exp_path)
  }

  dirs <- list.dirs(exp_path, full.names = FALSE, recursive = FALSE)
  dirs[grepl("^exp_", dirs)]
}

#' Load JSON file safely
#' @param path Path to JSON file
#' @return Parsed JSON as list
load_json <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  jsonlite::fromJSON(path)
}

#' Load CSV file safely
#' @param path Path to CSV file
#' @return Data frame or NULL
load_csv <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  read.csv(path, stringsAsFactors = FALSE)
}