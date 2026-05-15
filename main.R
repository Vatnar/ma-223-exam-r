#!/usr/bin/env Rscript
# MA223 Exam R Package - Main Entry Point
# -----------------------------------------
# Description: Loads all package functions and runs demo analysis

# === CONFIGURATION ===
# Set theme: TRUE for poster (vivid colors), FALSE for report (clean LaTeX plots)
USE_POSTER_THEME <- TRUE

# Use this flag in plots
if (USE_POSTER_THEME) {
  theme_name <- "poster"
} else {
  theme_name <- "report"
}

message("=== MA223 Exam R Package ===\n")
message("Theme: ", if (USE_POSTER_THEME)
  "poster"
  else
    "report", "\n")

# --- LOAD LIBRARIES ---
library(jsonlite)
library(ggplot2)

# --- SOURCE PACKAGE FILES ---
source("R/utils.R")
source("R/data.R")
source("R/summarize.R")
source("R/inference.R")
source("R/tests.R")
source("R/compare.R")
source("R/visualize.R")
source("R/bootstrap.R")
source("R/uncertainty.R")

message("Package loaded successfully.\n")

# --- SET THEME ---
if (USE_POSTER_THEME) {
  message("Using poster theme (vivid colors)\n")
} else {
  message("Using report theme (clean for LaTeX)\n")
}

# --- ENSURE OUTPUT DIR ---
output_dir <- get_output_dir()
dir.create(output_dir, showWarnings = FALSE)

# =========================================
# DEMO ANALYSIS
# =========================================

message("=== MA223 Exam R Package Demo ===\n")

root <- get_repo_root()
message("Root: ", root, "\n")

# List available experiment groups
groups <- list_experiment_groups(root)
message("Available experiment groups: ",
        paste(groups, collapse = ", "),
        "\n")

# Load all experiments from ActivationComparison (default)
exp_ids <- valid_experiments(root, "ActivationComparison")
message("\nLoading: ",
        length(exp_ids),
        "experiments from ActivationComparison\n")




# --- 1. SUMMARY TABLE ---
#message("\n--- 1. Experiment Summary ---\n")
#summary_df <- summarize_experiments(exps)
#print(summary_df[, c("exp_id", "activation", "accuracy_test", "accuracy_valid", "runtime_seconds")])


message("\n--- Credibility interval (symmetric Beta posterior) ---")
ActivationComparisonExps <- load_experiments(exp_ids, root, "ActivationComparison")

exps_list <- c('exp_026', 'exp_027', 'exp_028', 'exp_029', 'exp_030')
named_exps <- ActivationComparisonExps[exps_list]
names(named_exps) <- exps_list
set_plot_theme(theme_name)
plot_binomial_with_ci(named_exps,
                      file = file.path(get_output_dir(), "binomial_with_ci.pdf"))


exps_list <- c('exp_026', 'exp_027', 'exp_028', 'exp_029', 'exp_030')
named_exps <- ActivationComparisonExps[exps_list]
names(named_exps) <- exps_list
set_plot_theme(theme_name)
plot_binomial_with_ci(named_exps,
                      file = file.path(get_output_dir(), "binomial_with_ci.pdf"))

# Header and Binomial (symmetric) CI table
cat(sprintf("  %-12s %-12s %12s %15s\n", "exp", "activation", "estimate", "95% CI"))
for (nm in exps_list) {
  exp <- ActivationComparisonExps[[nm]]
  
  
  n <- exp$summary$metrics$test$num_samples
  k <- exp$summary$metrics$test$num_correct
  
  
  ci <- symmetric_ci(k, n, 0.95)
  cat(sprintf(
    "  %-12s %-12s %12.4f [%10.4f, %10.4f]\n",
    nm,
    as.character(exp$summary$hyperparameters$activation),
    ci$estimate,
    ci$lower,
    ci$upper
  ))
  cat(sprintf(
    "  %-12s %-12s %12.4f [%10.4f, %10.4f]\n",
    nm,
    as.character(exp$summary$hyperparameters$activation),
    ci$estimate,
    ci$lower,
    ci$upper
  ))
}

message("\n---  Test Accuracy with Wilson confidence interval ---")
cat(sprintf("  %-12s %-12s %12s %15s\n", "exp", "activation", "estimate", "95% CI"))
for (nm in exps_list) {
  exp <- ActivationComparisonExps[[nm]]
  metrics <- exp$summary$metrics
  n_correct <- metrics$test$num_correct
  n_total <- metrics$test$num_samples
  
  ci <- prop.test(n_correct, n_total, conf.level = 0.95)
  cat(sprintf(
    "  %-12s %-12s %12.4f [%10.4f, %10.4f]\n",
    nm,
    as.character(exp$summary$hyperparameters$activation),
    as.numeric(ci$estimate),
    as.numeric(ci$conf.int[1]),
    as.numeric(ci$conf.int[2])
  ))
  ci <- prop.test(n_correct, n_total, conf.level = 0.95)
  cat(sprintf(
    "  %-12s %-12s %12.4f [%10.4f, %10.4f]\n",
    nm,
    as.character(exp$summary$hyperparameters$activation),
    as.numeric(ci$estimate),
    as.numeric(ci$conf.int[1]),
    as.numeric(ci$conf.int[2])
  ))
}

# --- 8. UNCERTAINTY ANALYSIS ---
message("\n--- 8. Prediktiv usikkerhet ---\n")

exp_path <- file.path(root, "results", "experiments", "ActivationComparison", "exp_027")
if (dir.exists(exp_path)) {
  results <- analyze_experiment_uncertainty(exp_path)

  cat("Summary:\n")
  cat("  Samples:", results$summary$n_samples, "\n")
  cat("  Overall accuracy:", round(results$summary$overall_accuracy, 4), "\n")
  cat("  Mean confidence:", round(results$summary$mean_confidence, 4), "\n")
  cat("  Median confidence:", round(results$summary$median_confidence, 4), "\n")

  cat("\nAccuracy by confidence level:\n")
  print(results$by_confidence)

  cat("\nCorrelation test (confidence vs correct):\n")
  cat("  r =", round(results$correlation$estimate, 4), "\n")
  cat("  p-value =", format(results$correlation$p.value, scientific = TRUE), "\n")

  cat("\nCalibration:\n")
  print(results$calibration)

  # Plot confidence histogram
  test_results <- read_csv(file.path(exp_path, "test_results.csv"), show_col_types = FALSE)
  p1 <- plot_confidence_histogram(test_results, "Confidence Distribution (exp_027)")
  ggsave(file.path(output_dir, "confidence_histogram.pdf"), p1, width = 8, height = 4)

  # Plot accuracy by confidence
  p2 <- plot_confidence_accuracy(results$by_confidence, "Accuracy by Confidence Level")
  ggsave(file.path(output_dir, "confidence_accuracy.pdf"), p2, width = 6, height = 4)

  message("\nPlots saved to output/\n")
}




# --- 5. PLOTS ---
message("\n--- 5. Creating Plots ---\n")

ExpGroups = c("ActivationComparison", "InterpolateLearningRate", "Linear3",
              "LinearRepetition", "LinearRepetitionManyEpoch")

Experiments <- sapply(ExpGroups, function(group) load_all_experiments(group = group))
# Experiments <- unlist(Experiments, recursive = FALSE)


ExpGroup = "ActivationComparison"
ExpId = "exp_002"
exp <- Experiments[[ExpGroup]][[ExpId]]
path <- file.path(output_dir, sprintf("%s-%s-matrix.pdf", ExpGroup, ExpId))

plot_confusion_matrix(
  exp$confusion_matrix,
  file = path,
  max_classes = 50
)

path <- file.path(output_dir, sprintf("%s-%s-reliability.pdf", ExpGroup, ExpId))
plot_reliability_diagram(exp$test_results, file = path)

plot_hyperparam_vs_metric(
  unlist(Experiments, recursive = FALSE),
  "learning_rate",
  "accuracy_test",
  file = file.path(output_dir, "all_acc_vs_rel.pdf"),
  draw_labels = FALSE
)

# === HYPOTHESES TESTS === #

#View(Experiments[["LinearRepetitionManyEpoch"]])

ExpGroup <- Experiments[["LinearRepetitionManyEpoch"]]
ExpPrefixes <- c("exp_001", "exp_002")

exp_001_data <- ExpGroup[grep("^exp_001", names(ExpGroup))]
exp_002_data <- ExpGroup[grep("^exp_002", names(ExpGroup))]


gelu_acc <- sapply(exp_001_data, function(rep) {
  rep$summary$metrics$test$accuracy
})
linear_acc <- sapply(exp_002_data, function(rep) {
  rep$summary$metrics$test$accuracy
})

# acc_001 is GeLU and acc_002 is linear (activations)
message("\n--- Hypothesis t-test GeLU vs. Linear activation ---")
print(t.test(linear_acc, gelu_acc, paired = TRUE, alternative = "less"))

# --- mcnemar --- 


ExpA <- Experiments[["ActivationComparison"]][["exp_026"]]$test_results$correct
ExpB <- Experiments[["ActivationComparison"]][["exp_027"]]$test_results$correct

table_mcnemar <- table(Model_A = ExpA, Model_B = ExpB)
print(table_mcnemar)
mcnemar_result <- mcnemar.test(table_mcnemar)
print(mcnemar_result)
c <- table_mcnemar[2]
b <- table_mcnemar[3]
print((b-c)^2/(b+c))

View(Experiments)

# 


#View(exp_002_data)

stop()

# === FORPROSJEKT SPECIFIC PLOTS ===

message("\n--- 5b. Forprosjekt Visualizations ---\n")

# 3D Binomial surface (using image/contour instead of persp for readability)
plot_binom_surface(n = 758,
                   file = file.path(output_dir, "binom_surface.pdf"))
message("  Created: output/binom_surface.pdf (binomial surface)\n")

stop()

# Confusion matrix for best model
cm_exp <- exps$exp_048$confusion_matrix
plot_confusion_matrix(
  cm_exp,
  file = file.path(output_dir, "confusion_matrix_exp048.pdf"),
  max_classes = 50
)
message("  Created: output/confusion_matrix_exp048.pdf\n")

# Scatter: Learning rate vs Accuracy
plot_hyperparam_vs_metric(
  exps,
  "learning_rate",
  "accuracy_test",
  file = file.path(output_dir, "scatter_lr_vs_accuracy.pdf")
)
message("  Created: output/scatter_lr_vs_accuracy.pdf\n")

# Scatter: Dropout vs Accuracy
plot_hyperparam_vs_metric(
  exps,
  "dropout",
  "accuracy_test",
  file = file.path(output_dir, "scatter_dropout_vs_accuracy.pdf")
)
message("  Created: output/scatter_dropout_vs_accuracy.pdf\n")

# --- 6. BOOTSTRAP ANALYSIS ---
message("\n--- 6. Bootstrap Analysis ---\n")

# Bootstrap CI for exp_048
boot_result <- bootstrap_accuracy_ci(exps$exp_048$test_results, n_bootstrap = 500)
cat(
  "  exp_048 bootstrap CI:",
  round(boot_result$ci_lower, 4),
  "-",
  round(boot_result$ci_upper, 4),
  "\n"
)

# Bootstrap comparison
boot_cmp <- bootstrap_comparison(exps$exp_048$test_results,
                                 exps$exp_050$test_results,
                                 n_bootstrap = 500)
cat("  Difference CI:",
    round(boot_cmp$ci_lower, 4),
    "-",
    round(boot_cmp$ci_upper, 4),
    "\n")

# --- 7. ENTROPY & CALIBRATION ---
message("\n--- 7. Entropy & Calibration ---\n")

# Calibration error for exp_048
ece <- calibration_error(exps$exp_048$test_results)
cat("  exp_048 Expected Calibration Error:", round(ece, 4), "\n")

# Entropy analysis
entropies <- calculate_entropy(exps$exp_048$test_results$confidence)
cat("  exp_048 mean entropy:", round(mean(entropies, na.rm = TRUE), 4), "\n")

# Reliability diagram
plot_reliability_diagram(exps$exp_048$test_results,
                         file = file.path(output_dir, "demo_reliability.pdf"))
message("  Created: output/demo_reliability.pdf\n")

message("=== Demo Complete ===\n")
message("Output files in: ", output_dir, "\n")

