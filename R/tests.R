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
