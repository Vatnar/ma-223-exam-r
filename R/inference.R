# MA223 Exam R Package
# Inference Functions

#' Wilson Score Interval
#' @param x Number of successes
#' @param n Total trials
#' @param conf Confidence level
#' @return List(estimate, lower, upper)
wilson_ci <- function(x, n, conf = 0.95) {
  if (n == 0) {
    return(list(estimate = NA, lower = NA, upper = NA))
  }

  p_hat <- x / n

  if (p_hat == 0) {
    p_hat <- 1 / n
  }
  if (p_hat == 1) {
    p_hat <- 1 - 1 / n
  }

  z <- qnorm(1 - (1 - conf) / 2)

  center <- p_hat + z^2 / (2 * n)
  spread <- z * sqrt(p_hat * (1 - p_hat) / n + z^2 / (4 * n^2))
  denom <- 1 + z^2 / n

  estimate <- center / denom
  lower <- (center - spread) / denom
  upper <- (center + spread) / denom

  list(estimate = p_hat, lower = lower, upper = upper)
}

#' Wald (Standard) Confidence Interval
#' @param x Number of successes
#' @param n Total trials
#' @param conf Confidence level
#' @return List(estimate, lower, upper)
wald_ci <- function(x, n, conf = 0.95) {
  if (n == 0) {
    return(list(estimate = NA, lower = NA, upper = NA))
  }

  p_hat <- x / n
  z <- qnorm(1 - (1 - conf) / 2)

  se <- sqrt(p_hat * (1 - p_hat) / n)

  list(
    estimate = p_hat,
    lower = max(0, p_hat - z * se),
    upper = min(1, p_hat + z * se)
  )
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

#' Test for difference in proportions (chi-square approximation)
#' @param n1 Number correct in group 1
#' @param n2 Number correct in group 2
#' @param N Total samples per group
#' @return List with chi2, p-value
prop_test <- function(n1, n2, N) {
  p1 <- n1 / N
  p2 <- n2 / N
  p_pooled <- (n1 + n2) / (2 * N)

  expected1 <- p_pooled * N
  expected2 <- p_pooled * N

  chi2 <- ((n1 - expected1)^2 / expected1) + ((n2 - expected2)^2 / expected2)

  p_value <- 1 - pchisq(chi2, df = 1)

  list(
    p1 = p1,
    p2 = p2,
    chi2 = chi2,
    p_value = p_value
  )
}

#' Calculate odds ratio with CI
#' @param a Correct in both
#' @param b Correct in model 1 only
#' @param c Correct in model 2 only
#' @param d Wrong in both
#' @return List with OR, CI
odds_ratio_ci <- function(a, b, c, d) {
  if (any(c(a, b, c, d) == 0)) {
    a <- a + 0.5
    b <- b + 0.5
    c <- c + 0.5
    d <- d + 0.5
  }

  OR <- (a * d) / (b * c)

  log_OR <- log(OR)
  se_log_OR <- sqrt(1/a + 1/b + 1/c + 1/d)

  z <- qnorm(0.975)

  ci_lower <- exp(log_OR - z * se_log_OR)
  ci_upper <- exp(log_OR + z * se_log_OR)

  list(
    OR = OR,
    log_OR = log_OR,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  )
}