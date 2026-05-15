analyze_confidence <- function(test_results, n_bins = 4) {
  test_results <- test_results %>%
    mutate(
      confidence_bin = cut(confidence, breaks = n_bins, labels = FALSE)
    )

  results <- test_results %>%
    group_by(confidence_bin) %>%
    summarise(
      n = n(),
      n_correct = sum(correct),
      accuracy = mean(correct),
      mean_conf = mean(confidence),
      .groups = "drop"
    ) %>%
    mutate(
      se = sqrt(accuracy * (1 - accuracy) / n),
      ci_lower = accuracy - 1.96 * se,
      ci_upper = accuracy + 1.96 * se
    )

  return(results)
}

plot_confidence_histogram <- function(test_results, title = "Confidence Distribution", theme_name = "poster") {
  # Get theme colors
  theme_colors <- get_theme_colors(theme_name)
  
  p <- ggplot(test_results, aes(x = confidence, fill = factor(correct, levels = c(0, 1)))) +
    geom_histogram(alpha = 0.7, bins = 30, position = "identity") +
    labs(
      title = title,
      x = "Confidence (model certainty)",
      y = "Count",
      fill = "Prediction"
    ) +
    theme_minimal() +
    scale_fill_manual(
      values = c("0" = theme_colors$error, "1" = theme_colors$success),
      labels = c("Wrong", "Right")
    ) +
    theme(
      legend.position = "top",
      plot.title = element_text(hjust = 0.5)
    )

  return(p)
}

plot_confidence_accuracy <- function(confidence_results, title = "Accuracy by Confidence Level", theme_name = "poster") {
  # Get theme colors
  theme_colors <- get_theme_colors(theme_name)
  
  confidence_results <- confidence_results %>%
    mutate(
      bin_label = case_when(
        confidence_bin == 1 ~ "0.00-0.25",
        confidence_bin == 2 ~ "0.25-0.50",
        confidence_bin == 3 ~ "0.50-0.75",
        confidence_bin == 4 ~ "0.75-1.00",
        TRUE ~ as.character(confidence_bin)
      )
    )

  p <- ggplot(confidence_results, aes(x = factor(bin_label, levels = c("0.00-0.25", "0.25-0.50", "0.50-0.75", "0.75-1.00")), y = accuracy)) +
    geom_bar(stat = "identity", fill = theme_colors$primary, alpha = 0.8) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = theme_colors$accent) +
    geom_text(aes(label = sprintf("%.1f%%", accuracy * 100)), vjust = -0.5, color = theme_colors$accent, size = 3.5) +
    labs(
      title = title,
      x = "Confidence Interval (model certainty)",
      y = "Accuracy"
    ) +
    ylim(0, 1.1) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      plot.title = element_text(hjust = 0.5)
    )

  return(p)
}

correlation_test <- function(test_results) {
  cor_test <- cor.test(test_results$confidence, test_results$correct, method = "pearson")
  return(cor_test)
}

calibration_analysis <- function(test_results) {
  calibration <- test_results %>%
    mutate(
      bin = cut(confidence, breaks = seq(0, 1, 0.1), include.lowest = TRUE)
    ) %>%
    group_by(bin) %>%
    summarise(
      n = n(),
      avg_confidence = mean(confidence),
      accuracy = mean(correct),
      .groups = "drop"
    ) %>%
    mutate(
      calibration_error = abs(avg_confidence - accuracy)
    )

  return(calibration)
}

analyze_experiment_uncertainty <- function(exp_path) {
  
  test_file <- file.path(exp_path, "test_results.csv")

  if (!file.exists(test_file)) {
    warning(paste("No test_results.csv found in", exp_path))
    return(NULL)
  }
  
  # rust burn ML module already assigns confidence to it tests
  # so we can just extract from the data
  test_results <- read_csv(test_file, show_col_types = FALSE)

  results <- list(
    summary = list(
      n_samples = nrow(test_results),
      overall_accuracy = mean(test_results$correct),
      mean_confidence = mean(test_results$confidence),
      median_confidence = median(test_results$confidence)
    ),
    by_confidence = analyze_confidence(test_results),
    correlation = correlation_test(test_results), # pearson correlation test
    calibration = calibration_analysis(test_results) # adjust for average accuracy
  )

  return(results)
}