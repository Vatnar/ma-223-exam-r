# MA223 Exam R Package
# Utility Functions

get_repo_root <- function() {
  "."
}

#' Get output directory
#' @return Character string with output path
get_output_dir <- function() {
  "./output"
}

#' List available experiments
#' @param base_path Base path to experiments directory
#' @param group Group name
#' @return Character vector of experiment IDs
list_experiments <- function(base_path = get_repo_root(), group = "ActivationComparison") {
  exp_path <- file.path(base_path, "results/experiments", group)

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