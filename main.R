#!/usr/bin/env Rscript

USE_POSTER_THEME <- TRUE

message("=== MA223 Exam R Package ===\n")

source("R/setup.R")


source("analyses/analysis_ci.R")
source("analyses/analysis_mcnemar.R")
source("analyses/analysis_hypothesis.R")
source("analyses/analysis_uncertainty.R")
source("analyses/analysis_visualization.R")

# Initialize
theme_name <- if (USE_POSTER_THEME) "poster" else "report"
config <- initialize_workspace(theme = theme_name)
root <- get_repo_root()
output_dir <- setup_output()

message("Root: ", root, "\n")

# List available groups
groups <- list_experiment_groups(root)
message("Available experiment groups: ", paste(groups, collapse = ", "), "\n")

# Load all experiments
experiments <- load_all_experiment_groups()

message("\n--- Credibility interval (symmetric Beta posterior) ---")
exps_list <- c('exp_026', 'exp_027', 'exp_028', 'exp_029', 'exp_030')
run_ci_analysis(experiments$ActivationComparison, exps_list, output_dir, theme_name)

message("\n--- Uncertainty Analysis ---")
exp_path <- file.path(root, "results", "experiments", "ActivationComparison", "exp_027")
run_uncertainty_analysis(exp_path, output_dir, theme_name)

message("\n--- Creating Visualizations ---")
run_visualization_suite(experiments, output_dir, theme_name)


message("\n---Hypothesis Tests ---")

message("\n--- t-test: GeLU vs. Linear activation ---")
run_hypothesis_tests(experiments, "LinearRepetitionManyEpoch", c("exp_001", "exp_002"), output_dir)

message("\n--- McNemar's test ---")
run_mcnemar_analysis(
  experiments$ActivationComparison[["exp_026"]],
  experiments$ActivationComparison[["exp_027"]],
  output_dir
)

message("\n=== Analysis Complete ===")
message("Output files in: ", output_dir, "\n")
