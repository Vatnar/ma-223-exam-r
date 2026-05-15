
#' Symmetric Credibility Interval (Beta Posterior)
#' @param x Number of successes
#' @param n Total trials
#' @param conf Confidence level (default 0.95)
#' @param a0 Prior alpha (default 0.5)
#' @param b0 Prior beta (default 0.5)
#' @return List with estimate, lower, upper
symmetric_ci <- function(x,
                         n,
                         conf = 0.95,
                         a0 = 0.5,
                         b0 = 0.5) {
  if (n == 0) {
    return(list(
      estimate = NA,
      lower = NA,
      upper = NA
    ))
    
  }
  
  a1 <- a0 + x
  b1 <- b0 + (n - x)
  
  alpha <- 1 - conf
  
  estimate <- a1 / (a1 + b1)
  lower <- qbeta(alpha / 2, a1, b1)
  upper <- qbeta(1 - alpha / 2, a1, b1)
  
  list(estimate = estimate,
       lower = lower,
       upper = upper)
}

#' Compare two proportions
#' @param n1 Number correct in experiment 1
#' @param n2 Number correct in experiment 2
#' @param N Total samples (same for both)
#' @return List with test statistic, p-value, CI for difference
compare_proportions <- function(n1, n2, N) {
  p1 <- n1 / N
  p2 <- n2 / N
  diff <- p1 - p2
  
  p_pooled <- (n1 + n2) / (2 * N)
  se_diff <- sqrt(p_pooled * (1 - p_pooled) * (2 / N))
  
  z_stat <- diff / se_diff
  p_value <- 2 * pnorm(-abs(z_stat))
  
  z <- qnorm(0.975)
  ci_lower <- diff - z * se_diff
  ci_upper <- diff + z * se_diff
  
  list(
    p1 = p1,
    p2 = p2,
    difference = diff,
    z_statistic = z_stat,
    p_value = p_value,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  )
}

#' Wilson Score Interval for binomial proportion
#' @param n_success Number of successes
#' @param n_total Total number of trials
#' @param conf Confidence level (default 0.95)
#' @return List with estimate, lower, and upper bounds
wilson_ci <- function(n_success, n_total, conf = 0.95) {
  if (n_total == 0) {
    return(list(estimate = NA, lower = NA, upper = NA))
  }
  
  p_hat <- n_success / n_total
  alpha <- 1 - conf
  z <- qnorm(1 - alpha / 2)
  
  denominator <- 1 + z^2 / n_total
  centre <- (p_hat + z^2 / (2 * n_total)) / denominator
  half_width <- z * sqrt((p_hat * (1 - p_hat) + z^2 / (4 * n_total)) / n_total) / denominator
  
  list(
    estimate = p_hat,
    lower = max(0, centre - half_width),
    upper = min(1, centre + half_width)
  )
}

#' Compute Expected Calibration Error (ECE)
#' @param test_results Data frame with confidence and correct columns
#' @param n_bins Number of bins for calibration
#' @return ECE value
calibration_error <- function(test_results, n_bins = 10) {
  if (is.null(test_results) || !"confidence" %in% names(test_results)) {
    return(NULL)
  }
  
  bin_edges <- seq(0, 1, length.out = n_bins + 1)
  ece <- 0
  total_samples <- nrow(test_results)
  
  for (i in 1:n_bins) {
    if (i == n_bins) {
      mask <- test_results$confidence >= bin_edges[i] & test_results$confidence <= bin_edges[i + 1]
    } else {
      mask <- test_results$confidence >= bin_edges[i] & test_results$confidence < bin_edges[i + 1]
    }
    
    n_samples <- sum(mask)
    if (n_samples == 0) next
    
    avg_confidence <- mean(test_results$confidence[mask])
    actual_accuracy <- mean(test_results$correct[mask])
    
    ece <- ece + (n_samples / total_samples) * abs(avg_confidence - actual_accuracy)
  }
  
  ece
}

#' Prepare data for reliability diagram
#' @param test_results Data frame with confidence and correct columns
#' @param n_bins Number of bins
#' @return Data frame with bin_center and actual_accuracy
reliability_diagram_data <- function(test_results, n_bins = 10) {
  if (is.null(test_results) || !"confidence" %in% names(test_results)) {
    return(NULL)
  }
  
  bin_edges <- seq(0, 1, length.out = n_bins + 1)
  
  results <- lapply(1:n_bins, function(i) {
    if (i == n_bins) {
      mask <- test_results$confidence >= bin_edges[i] & test_results$confidence <= bin_edges[i + 1]
    } else {
      mask <- test_results$confidence >= bin_edges[i] & test_results$confidence < bin_edges[i + 1]
    }
    
    n_samples <- sum(mask)
    if (n_samples == 0) {
      return(NULL)
    }
    
    avg_confidence <- mean(test_results$confidence[mask])
    actual_accuracy <- mean(test_results$correct[mask])
    
    data.frame(
      bin_center = (bin_edges[i] + bin_edges[i + 1]) / 2,
      actual_accuracy = actual_accuracy,
      avg_confidence = avg_confidence,
      n_samples = n_samples,
      stringsAsFactors = FALSE
    )
  })
  
  results <- Filter(function(x) !is.null(x), results)
  if (length(results) == 0) {
    return(NULL)
  }
  
  do.call(rbind, results)
}