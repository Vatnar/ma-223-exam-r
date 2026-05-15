# MA223 Exam R Package
# Setup and Initialization Module

#' Install required packages if not already installed
#' @return Invisible NULL
install_libraries <- function() {
  # Core packages required by the analysis pipeline
  # tidyverse includes: ggplot2, dplyr, tidyr, purrr, readr, stringr, forcats, lubridate, tibble
  required_packages <- c(
    "conflicted",   # Namespace conflict resolution
    "tidyverse",    # Data manipulation and visualization (ggplot2, dplyr, readr, etc.)
    "jsonlite",     # JSON file parsing
    "cowplot"       # Grid layout for multiple ggplot plots
  )
  
  for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      message(paste("Be adviced that installing packages take a while, since it might compile it from source. \n Installing package:", pkg))
      install.packages(pkg, dependencies = TRUE, quiet = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
  
  invisible(NULL)
}

#' Initialize workspace - load libraries and source all R files
#' @param theme Theme name ("poster" or "report")
#' @return List with configuration settings
initialize_workspace <- function(theme = "poster") {
  # --- INSTALL/LOAD LIBRARIES ---
  install_libraries()
  
  library(conflicted)
  library(tidyverse)  # Loads dplyr, ggplot2, purrr, etc.
  library(jsonlite)
  
  # --- NAMESPACE CONFLICT RESOLUTION ---
  # Explicitly resolve function name conflicts between packages
  conflicts_prefer(dplyr::filter)      # Use dplyr's filter, not stats::filter
  conflicts_prefer(dplyr::lag)         # Use dplyr's lag, not stats::lag  
  conflicts_prefer(jsonlite::flatten)  # Use jsonlite's flatten, not purrr::flatten
  
  # --- SOURCE CORE PACKAGE FILES ---
  source("R/utils.R")
  source("R/data.R")
  source("R/summarize.R")
  source("R/inference.R")
  source("R/tests.R")
  source("R/visualize.R")
  source("R/uncertainty.R")
  
  # --- SET THEME ---
  set_plot_theme(theme)
  
  message("Package loaded successfully.\n")
  
  if (theme == "poster") {
    message("Using poster theme (vivid colors)\n")
  } else {
    message("Using report theme (clean for LaTeX)\n")
  }
  
  invisible(list(theme = theme))
}

#' Setup output directory
#' @return Path to output directory
setup_output <- function() {
  output_dir <- get_output_dir()
  dir.create(output_dir, showWarnings = FALSE)
  output_dir
}

#' Load all experiments from all groups
#' @return Named list of experiment groups
load_all_experiment_groups <- function() {
  root <- get_repo_root()
  
  ExpGroups <- c("ActivationComparison", "InterpolateLearningRate", "Linear3",
                 "LinearRepetition", "LinearRepetitionManyEpoch")
  
  experiments <- sapply(ExpGroups, function(group) {
    load_all_experiments(group = group)
  }, simplify = FALSE)
  
  experiments
}
