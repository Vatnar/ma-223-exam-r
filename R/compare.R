# MA223 Exam R Package
# Cross-Experiment Comparison Functions

#' Compare multiple experiments
#' @param exps Named list of experiments
#' @return Data frame with comparison table
compare_experiments <- function(exps) {
  if (is.null(exps) || length(exps) == 0) {
    return(NULL)
  }

  summary_df <- summarize_experiments(exps)

  if (is.null(summary_df)) {
    return(NULL)
  }

  summary_df
}

#' Find best experiment by metric
#' @param exps Named list of experiments
#' @param metric Metric name ("accuracy_test", "accuracy_valid", etc.)
#' @param higher_is_better TRUE for accuracy, FALSE for loss
#' @return Experiment ID of best
best_experiment <- function(exps, metric = "accuracy_test", higher_is_better = TRUE) {
  if (is.null(exps) || length(exps) == 0) {
    return(NULL)
  }

  summary_df <- summarize_experiments(exps)

  if (is.null(summary_df) || !(metric %in% names(summary_df))) {
    return(NULL)
  }

  values <- summary_df[[metric]]

  if (higher_is_better) {
    best_idx <- which.max(values)
  } else {
    best_idx <- which.min(values)
  }

  summary_df$exp_id[best_idx]
}

#' Get top N experiments by metric
#' @param exps Named list of experiments
#' @param metric Metric name
#' @param n Number of top experiments
#' @param higher_is_better TRUE for accuracy, FALSE for loss
#' @return Data frame with top N experiments
top_n_experiments <- function(exps, metric = "accuracy_test", n = 5,
                            higher_is_better = TRUE) {
  if (is.null(exps) || length(exps) == 0) {
    return(NULL)
  }

  summary_df <- summarize_experiments(exps)

  if (is.null(summary_df) || !(metric %in% names(summary_df))) {
    return(NULL)
  }

  values <- summary_df[[metric]]

  if (higher_is_better) {
    order_idx <- order(-values)
  } else {
    order_idx <- order(values)
  }

  n_to_show <- min(n, length(order_idx))
  summary_df[order_idx[1:n_to_show], , drop = FALSE]
}

#' Correlation between hyperparameters and performance
#' @param exps Named list of experiments
#' @return Correlation matrix
hyperparam_performance_corr <- function(exps) {
  if (is.null(exps) || length(exps) == 0) {
    return(NULL)
  }

  summary_df <- summarize_experiments(exps)

  if (is.null(summary_df)) {
    return(NULL)
  }

  hyperparams <- c("dropout", "learning_rate", "max_epochs")
  performance <- c("accuracy_test", "accuracy_valid", "accuracy_train", "loss_train")

  hyperparams <- hyperparams[hyperparams %in% names(summary_df)]
  performance <- performance[performance %in% names(summary_df)]

  vars_to_corr <- c(hyperparams, performance)

  if (length(vars_to_corr) < 2) {
    return(NULL)
  }

  cor_matrix <- cor(summary_df[, vars_to_corr, drop = FALSE],
                  use = "pairwise.complete.obs")

  cor_matrix
}

#' Pairwise comparisons table
#' @param exps Named list of experiments
#' @return Data frame with all pairwise comparisons
pairwise_comparison <- function(exps) {
  if (is.null(exps) || length(exps) < 2) {
    return(NULL)
  }

  exp_ids <- names(exps)
  n_exp <- length(exp_ids)

  results <- list()

  for (i in 1:(n_exp - 1)) {
    for (j in (i + 1):n_exp) {
      exp1 <- exps[[exp_ids[i]]]
      exp2 <- exps[[exp_ids[j]]]

      if (is.null(exp1$test_results) || is.null(exp2$test_results)) {
        next
      }

      mcnemar <- mcnemar_test(exp1$test_results, exp2$test_results)

      n_correct1 <- sum(exp1$test_results$correct)
      n_correct2 <- sum(exp2$test_results$correct)
      n_total <- nrow(exp1$test_results)

      prop_result <- compare_proportions(n_correct1, n_correct2, n_total)

      results[[length(results) + 1]] <- data.frame(
        exp1 = exp_ids[i],
        exp2 = exp_ids[j],
        acc1 = n_correct1 / n_total,
        acc2 = n_correct2 / n_total,
        diff = prop_result$difference,
        p_value_mcnemar = mcnemar$p_value,
        p_value_prop = prop_result$p_value,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(results) == 0) {
    return(NULL)
  }

  do.call(rbind, results)
}

#' Group experiments by activation function
#' @param exps Named list of experiments
#' @return List with activation function as key
group_by_activation <- function(exps) {
  if (is.null(exps) || length(exps) == 0) {
    return(NULL)
  }

  groups <- list()

  for (nm in names(exps)) {
    exp <- exps[[nm]]

    if (is.null(exp$config) || is.null(exp$config$activation)) {
      next
    }

    act <- exp$config$activation

    if (is.null(groups[[act]])) {
      groups[[act]] <- list()
    }

    groups[[act]][[nm]] <- exp
  }

  groups
}

#' Calculate summary statistics by group
#' @param grouped_exps List from group_by_activation
#' @return Data frame with statistics per group
stats_by_group <- function(grouped_exps) {
  if (is.null(grouped_exps) || length(grouped_exps) == 0) {
    return(NULL)
  }

  results <- lapply(names(grouped_exps), function(grp_name) {
    exps <- grouped_exps[[grp_name]]

    summary_df <- summarize_experiments(exps)

    data.frame(
      group = grp_name,
      n_experiments = nrow(summary_df),
      mean_accuracy = mean(summary_df$accuracy_test, na.rm = TRUE),
      sd_accuracy = sd(summary_df$accuracy_test, na.rm = TRUE),
      mean_accuracy_valid = mean(summary_df$accuracy_valid, na.rm = TRUE),
      sd_accuracy_valid = sd(summary_df$accuracy_valid, na.rm = TRUE),
      mean_runtime = mean(summary_df$runtime_seconds, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, results)
}