get_repo_root <- function() {
  "."
}

#' Get output directory
#' @return Character string with output path
get_output_dir <- function() {
  "./output"
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