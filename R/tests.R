# MA223 Exam R Package
# Hypothesis Test Functions

#' McNemar's test for paired nominal data
#' @param test_results1 Test results from experiment 1
#' @param test_results2 Test results from experiment 2
#' @param method Method for continuity correction ("continuity", "exact", "asymptotic")
#' @return List with chi-squared, p-value, OR, b, c
mcnemar_test <- function(test_results1, test_results2, method = "continuity") {
  if (is.null(test_results1) || is.null(test_results2)) {
    stop("Test results cannot be NULL")
  }

  if (nrow(test_results1) != nrow(test_results2)) {
    stop("Test results must have same number of samples")
  }

  true_labels1 <- test_results1$true_label
  true_labels2 <- test_results2$true_label

  pred_labels1 <- test_results1$predicted_label
  pred_labels2 <- test_results2$predicted_label

  correct1 <- pred_labels1 == true_labels1
  correct2 <- pred_labels2 == true_labels2

  b <- sum(correct1 & !correct2)
  c <- sum(!correct1 & correct2)
  a <- sum(correct1 & correct2)
  d <- sum(!correct1 & !correct2)

  if (method == "exact" || min(b, c) < 25) {
    total <- b + c
    k <- min(b, c)

    p_value <- 2 * pbinom(k, total, 0.5)
    p_value_exact <- pbinom(min(b, c), total, 0.5) +
                  dbinom(0:min(b, c), total, 0.5) * 2

    chi2 <- NA

  } else if (method == "continuity") {
    chi2 <- (abs(b - c) - 1)^2 / (b + c)
    p_value <- 1 - pchisq(chi2, df = 1)

  } else {
    chi2 <- (b - c)^2 / (b + c)
    p_value <- 1 - pchisq(chi2, df = 1)
  }

  OR <- if (c > 0) b / c else Inf

  list(
    method = method,
    chi2 = chi2,
    p_value = p_value,
    OR = OR,
    b = b,
    c = c,
    a = a,
    d = d,
    table = matrix(c(a, b, c, d), nrow = 2, ncol = 2,
                  dimnames = list(c("Model2_Correct", "Model2_Wrong"),
                                c("Model1_Correct", "Model1_Wrong")))
  )
}

#' Binomial test for comparing hit rates
#' @param n1 Correct in model 1
#' @param n2 Correct in model 2
#' @param N Total test samples
#' @return List with p-value, OR
binom_test_compare <- function(n1, n2, N) {
  if (N == 0) {
    stop("Total samples N must be > 0")
  }

  p1 <- n1 / N
  p2 <- n2 / N
  diff <- n1 - n2

  p_pooled <- (n1 + n2) / (2 * N)

  expected <- p_pooled * N

  chi2 <- ((n1 - expected)^2 / expected) + ((n2 - expected)^2 / expected)
  p_value <- 1 - pchisq(chi2, df = 1)

  OR <- if (n2 > 0) n1 / n2 else Inf

  list(
    p1 = p1,
    p2 = p2,
    diff = diff,
    chi2 = chi2,
    p_value = p_value,
    OR = OR
  )
}

#' Chi-square test on confusion matrix
#' @param cm Confusion matrix (matrix or data frame)
#' @return List with chi-squared, p-value, df
chi2_test_cm <- function(cm) {
  if (is.null(cm)) {
    stop("Confusion matrix cannot be NULL")
  }

  if (is.data.frame(cm)) {
    cm <- as.matrix(cm)
  }

  if (nrow(cm) == 0 || ncol(cm) == 0) {
    stop("Confusion matrix has invalid dimensions")
  }

  row_totals <- rowSums(cm)
  col_totals <- colSums(cm)
  grand_total <- sum(cm)

  expected <- outer(row_totals, col_totals) / grand_total
  expected[expected == 0] <- 0.001

  chi2 <- sum((cm - expected)^2 / expected)
  df <- (nrow(cm) - 1) * (ncol(cm) - 1)
  p_value <- 1 - pchisq(chi2, df = df)

  list(
    chi2 = chi2,
    p_value = p_value,
    df = df
  )
}

#' Paired t-test for comparing accuracies across epochs
#' @param history1 History data frame from exp 1
#' @param history2 History data frame from exp 2
#' @return List with t-stat, p-value
paired_t_test_history <- function(history1, history2) {
  if (is.null(history1) || is.null(history2)) {
    stop("History data cannot be NULL")
  }

  epochs1 <- history1$epoch
  epochs2 <- history2$epoch

  common_epochs <- intersect(epochs1, epochs2)

  if (length(common_epochs) == 0) {
    stop("No common epochs")
  }

  acc1 <- history1$accuracy[match(common_epochs, epochs1)]
  acc2 <- history2$accuracy[match(common_epochs, epochs2)]

  diffs <- acc1 - acc2
  t_stat <- mean(diffs) / (sd(diffs) / sqrt(length(diffs)))
  p_value <- 2 * pt(-abs(t_stat), df = length(diffs) - 1)

  list(
    t_stat = t_stat,
    p_value = p_value,
    df = length(diffs) - 1,
    mean_diff = mean(diffs)
  )
}

#' One-sample t-test for accuracy vs baseline
#' @param exp Experiment object
#' @param baseline Baseline accuracy to test against
#' @return List with t-stat, p-value
t_test_vs_baseline <- function(exp, baseline = 0.5) {
  if (is.null(exp) || is.null(exp$history_train)) {
    stop("Experiment or history cannot be NULL")
  }

  acc <- exp$history_train$accuracy
  n <- length(acc)

  t_stat <- (mean(acc) - baseline) / (sd(acc) / sqrt(n))
  p_value <- 2 * pt(-abs(t_stat), df = n - 1)

  list(
    t_stat = t_stat,
    p_value = p_value,
    df = n - 1,
    mean_acc = mean(acc)
  )
}