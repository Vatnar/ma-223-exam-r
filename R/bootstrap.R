# MA223 Exam R Package
# Bootstrap and Entropy Functions

#' Bootstrap confidence interval for accuracy
#' @param test_results Test results data frame
#' @param n_bootstrap Number of bootstrap samples
#' @param conf Confidence level
#' @return List with point estimate and CI
bootstrap_accuracy_ci <- function(test_results, n_bootstrap = 1000, conf = 0.95) {
  if (is.null(test_results)) {
    return(NULL)
  }

  n_samples <- nrow(test_results)
  correct <- test_results$correct

  boot_accuracies <- sapply(1:n_bootstrap, function(i) {
    sample_idx <- sample(n_samples, replace = TRUE)
    mean(correct[sample_idx])
  })

  point_est <- mean(correct)
  alpha <- 1 - conf
  ci_lower <- quantile(boot_accuracies, alpha / 2)
  ci_upper <- quantile(boot_accuracies, 1 - alpha / 2)

  list(
    estimate = point_est,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    conf_level = conf,
    n_bootstrap = n_bootstrap,
    boot_dist = boot_accuracies
  )
}

#' Bootstrap comparison of two models
#' @param test_results1 Test results from model 1
#' @param test_results2 Test results from model 2
#' @param n_bootstrap Number of bootstrap samples
#' @return List with diff estimate and CI
bootstrap_comparison <- function(test_results1, test_results2, n_bootstrap = 1000) {
  if (is.null(test_results1) || is.null(test_results2)) {
    return(NULL)
  }

  n1 <- nrow(test_results1)
  n2 <- nrow(test_results2)

  if (n1 != n2) {
    stop("Test results must have same length")
  }

  correct1 <- test_results1$correct
  correct2 <- test_results2$correct
  diff_correct <- correct1 - correct2

  boot_diffs <- sapply(1:n_bootstrap, function(i) {
    sample_idx <- sample(n1, replace = TRUE)
    mean(diff_correct[sample_idx])
  })

  point_diff <- mean(diff_correct)
  ci_lower <- quantile(boot_diffs, 0.025)
  ci_upper <- quantile(boot_diffs, 0.975)

  list(
    difference = point_diff,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    n_bootstrap = n_bootstrap,
    boot_dist = boot_diffs
  )
}

#' Calculate entropy of prediction probabilities
#' @param confidence Confidence score(s) 0-1
#' @return Entropy value(s)
calculate_entropy <- function(confidence) {
  if (length(confidence) == 1) {
    if (confidence <= 0 || confidence >= 1) {
      return(0)
    }
    p <- confidence
    -p * log2(p) - (1 - p) * log2(1 - p)
  } else {
    sapply(confidence, function(p) {
      if (p <= 0 || p >= 1) {
        return(0)
      }
      -p * log2(p) - (1 - p) * log2(1 - p)
    })
  }
}

#' Calculate calibration metrics
#' @param test_results Test results with confidence scores
#' @param n_bins Number of bins for reliability diagram
#' @return Data frame with calibration data
calibration_data <- function(test_results, n_bins = 10) {
  if (is.null(test_results) || !"confidence" %in% names(test_results)) {
    return(NULL)
  }

  bin_edges <- seq(0, 1, length.out = n_bins + 1)

  results <- lapply(1:n_bins, function(i) {
    mask <- test_results$confidence >= bin_edges[i] & test_results$confidence < bin_edges[i + 1]
    if (i == n_bins) {
      mask <- test_results$confidence >= bin_edges[i] & test_results$confidence <= bin_edges[i + 1]
    }

    n_samples <- sum(mask)
    if (n_samples == 0) {
      return(NULL)
    }

    avg_confidence <- mean(test_results$confidence[mask])
    actual_accuracy <- mean(test_results$correct[mask])

    data.frame(
      bin = i,
      confidence_low = bin_edges[i],
      confidence_high = bin_edges[i + 1],
      avg_confidence = avg_confidence,
      actual_accuracy = actual_accuracy,
      n_samples = n_samples,
      stringsAsFactors = FALSE
    )
  })

  results <- Filter(function(x) !is.null(x), results)
  do.call(rbind, results)
}

#' Expected Calibration Error (ECE)
#' @param test_results Test results with confidence scores
#' @param n_bins Number of bins
#' @return ECE value
calibration_error <- function(test_results, n_bins = 10) {
  cal_data <- calibration_data(test_results, n_bins)

  if (is.null(cal_data)) {
    return(NULL)
  }

  sum(cal_data$n_samples / sum(cal_data$n_samples) *
      abs(cal_data$avg_confidence - cal_data$actual_accuracy))
}

#' Reliability diagram data
#' @param test_results Test results with confidence scores
#' @param n_bins Number of bins
#' @return Data frame for plotting
reliability_diagram_data <- function(test_results, n_bins = 10) {
  cal_data <- calibration_data(test_results, n_bins)

  if (is.null(cal_data)) {
    return(NULL)
  }

  cal_data$bin_center <- (cal_data$confidence_low + cal_data$confidence_high) / 2

  cal_data
}