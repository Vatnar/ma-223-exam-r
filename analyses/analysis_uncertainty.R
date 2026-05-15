# MA223 Exam R Package
#' Run uncertainty analysis for a single experiment
#' @param exp_path Path to experiment directory
#' @param output_dir Output directory for plots
#' @param theme_name Theme name for styling
#' @return Invisible NULL
run_uncertainty_analysis <- function(exp_path, output_dir, theme_name = "poster") {
  if (!dir.exists(exp_path)) {
    warning("Experiment path does not exist: ", exp_path)
    return(invisible(NULL))
  }
  
  results <- analyze_experiment_uncertainty(exp_path)
  
  if (is.null(results)) {
    return(invisible(NULL))
  }
  
  # Print summary
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
  
  test_results <- read.csv(file.path(exp_path, "test_results.csv"), stringsAsFactors = FALSE)
  
  # confidence histogram
  p1 <- plot_confidence_histogram(test_results, "Confidence Distribution", theme_name)
  ggsave(file.path(output_dir, "confidence_histogram.pdf"), p1, width = 8, height = 4)
  
  # Plot accuracy by confidence
  p2 <- plot_confidence_accuracy(results$by_confidence, "Accuracy by Confidence Level", theme_name)
  ggsave(file.path(output_dir, "confidence_accuracy.pdf"), p2, width = 6, height = 4)
  
  message("\nUncertainty plots saved to output/\n")
  
  invisible(NULL)
}
