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
message("Theme: ", if (USE_POSTER_THEME) "poster" else "report", "\n")

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

# List available experiments
all_exps <- list_experiments(root)
message("Available experiments: ", length(all_exps), "\n")

# Load example experiments
exp_ids <- valid_experiments(root)
message("\nLoading all: ", length(exp_ids), "experiments\n")
exps <- load_experiments(exp_ids)

# --- 1. SUMMARY TABLE ---
message("\n--- 1. Experiment Summary ---\n")
summary_df <- summarize_experiments(exps)
print(summary_df[, c("exp_id", "activation", "accuracy_test", "accuracy_valid", "runtime_seconds")])

# --- 2. ACCURACY WITH CI ---
message("\n--- 2. Test Accuracy with 95% CI (Wilson) ---\n")
for (nm in names(exps)) {
  exp <- exps[[nm]]
  metrics <- exp$summary$metrics
  n_correct <- metrics$test$num_correct
  n_total <- metrics$test$num_samples

  ci <- accuracy_ci_wilson(n_correct, n_total)
  cat(sprintf("  %s: %.1f%% [%0.1f%%, %0.1f%%]\n",
             nm, ci$estimate * 100, ci$lower * 100, ci$upper * 100))
}

# --- 3. COMPARE EXPERIMENTS ---
message("\n--- 3. Pairwise Comparison ---\n")
pairwise <- pairwise_comparison(exps)
if (!is.null(pairwise)) {
  print(pairwise[, c("exp1", "exp2", "diff", "p_value_mcnemar")])
}

# --- 4. STATS BY GROUP ---
message("\n--- 4. Stats by Activation Function ---\n")
grouped <- group_by_activation(exps)
grouped_df <- stats_by_group(grouped)
print(grouped_df)

# --- 5. PLOTS ---
message("\n--- 5. Creating Plots ---\n")

plot_training_curves(exps$exp_048, file = file.path(output_dir, "demo_training_curves.pdf"))
message("  Created: output/demo_training_curves.pdf\n")

plot_accuracy_comparison(exps, file = file.path(output_dir, "demo_accuracy_comparison.pdf"))
message("  Created: output/demo_accuracy_comparison.pdf\n")

plot_validation_curves(exps, file = file.path(output_dir, "demo_validation_curves.pdf"))
message("  Created: output/demo_validation_curves.pdf\n")

# === FORPROSJEKT SPECIFIC PLOTS ===

message("\n--- 5b. Forprosjekt Visualizations ---\n")

# 3D Binomial surface (using image/contour instead of persp for readability)
plot_binom_surface(n = 758, file = file.path(output_dir, "binom_surface.pdf"))
message("  Created: output/binom_surface.pdf (binomial surface)\n")

# Confusion matrix for best model
cm_exp <- exps$exp_048$confusion_matrix
plot_confusion_matrix(cm_exp, file = file.path(output_dir, "confusion_matrix_exp048.pdf"),
                    max_classes = 50)
message("  Created: output/confusion_matrix_exp048.pdf\n")

# Scatter: Learning rate vs Accuracy
plot_hyperparam_vs_metric(exps, "learning_rate", "accuracy_test",
                        file = file.path(output_dir, "scatter_lr_vs_accuracy.pdf"))
message("  Created: output/scatter_lr_vs_accuracy.pdf\n")

# Scatter: Dropout vs Accuracy  
plot_hyperparam_vs_metric(exps, "dropout", "accuracy_test",
                        file = file.path(output_dir, "scatter_dropout_vs_accuracy.pdf"))
message("  Created: output/scatter_dropout_vs_accuracy.pdf\n")

# --- 6. BOOTSTRAP ANALYSIS ---
message("\n--- 6. Bootstrap Analysis ---\n")

# Bootstrap CI for exp_048
boot_result <- bootstrap_accuracy_ci(exps$exp_048$test_results, n_bootstrap = 500)
cat("  exp_048 bootstrap CI:", round(boot_result$ci_lower, 4), "-", round(boot_result$ci_upper, 4), "\n")

# Bootstrap comparison
boot_cmp <- bootstrap_comparison(exps$exp_048$test_results,
                                   exps$exp_050$test_results, n_bootstrap = 500)
cat("  Difference CI:", round(boot_cmp$ci_lower, 4), "-", round(boot_cmp$ci_upper, 4), "\n")

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