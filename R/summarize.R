# MA223 Exam R Package
# Summarization Functions

#' Summarize single experiment
#' @param exp Experiment object from load_experiment
#' @return Data frame with key metrics
summarize_experiment <- function(exp) {
  if (is.null(exp) || is.null(exp$summary)) {
    return(NULL)
    
  }
  
  config <- exp$config
  summary <- exp$summary
  metrics <- summary$metrics
  
  data.frame(
    exp_id = exp$exp_id,
    activation = if (!is.null(config$activation))
      config$activation
    else
      NA,
    dropout = if (!is.null(config$dropout))
      config$dropout
    else
      NA,
    learning_rate = if (!is.null(config$learning_rate))
      config$learning_rate
    else
      NA,
    max_epochs = if (!is.null(config$max_epochs))
      config$max_epochs
    else
      NA,
    accuracy_test = if (!is.null(metrics$test$accuracy))
      metrics$test$accuracy
    else
      NA,
    num_correct_test = if (!is.null(metrics$test$num_correct))
      metrics$test$num_correct
    else
      NA,
    num_samples_test = if (!is.null(metrics$test$num_samples))
      metrics$test$num_samples
    else
      NA,
    accuracy_valid = if (!is.null(metrics$valid$best_accuracy))
      metrics$valid$best_accuracy
    else
      NA,
    accuracy_train = if (!is.null(metrics$train$final_accuracy))
      metrics$train$final_accuracy
    else
      NA,
    loss_train = if (!is.null(metrics$train$final_loss))
      metrics$train$final_loss
    else
      NA,
    best_epoch = if (!is.null(metrics$valid$epoch))
      metrics$valid$epoch
    else
      NA,
    total_epochs = if (!is.null(summary$training$total_epochs))
      summary$training$total_epochs
    else
      NA,
    runtime_seconds = if (!is.null(summary$training$runtime_seconds))
      summary$training$runtime_seconds
    else
      NA,
    stringsAsFactors = FALSE
  )
}

#' Summarize multiple experiments
#' @param exps Named list of experiment objects
#' @return Data frame with all experiments
summarize_experiments <- function(exps) {
  if (is.null(exps) || length(exps) == 0) {
    return(NULL)
  }
  
  dfs <- lapply(exps, summarize_experiment)
  dfs <- Filter(function(x)
    ! is.null(x), dfs)
  
  if (length(dfs) == 0) {
    return(NULL)
  }
  
  do.call(rbind, dfs)
}

#' Wilson Score Interval for binomial proportion
#' Per-class accuracy from test results
#' @param test_results Test results data frame
#' @return Data frame with class, accuracy, n samples
per_class_accuracy <- function(test_results) {
  if (is.null(test_results) || nrow(test_results) == 0) {
    return(NULL)
  }
  
  true_labels <- test_results$true_label
  pred_labels <- test_results$predicted_label
  
  classes <- sort(unique(true_labels))
  
  sapply(classes, function(cls) {
    mask <- true_labels == cls
    n_samples <- sum(mask)
    n_correct <- sum(pred_labels[mask] == true_labels[mask])
    
    data.frame(
      class = cls,
      accuracy = n_correct / n_samples,
      n_samples = n_samples,
      stringsAsFactors = FALSE
    )
  }) |> do.call(rbind, args = _)
}

#' Get test accuracy with CI for experiment
#' @param exp Experiment object
#' @param conf Confidence level
#' @return Data frame with accuracy, CI
accuracy_with_ci <- function(exp, conf = 0.95) {
  if (is.null(exp) || is.null(exp$summary)) {
    return(NULL)
  }
  
  metrics <- exp$summary$metrics
  n_correct <- metrics$test$num_correct
  n_total <- metrics$test$num_samples
  
  wilson <- wilson_ci(n_correct, n_total, conf)
  
  data.frame(
    exp_id = exp$exp_id,
    accuracy = wilson$estimate,
    ci_lower = wilson$lower,
    ci_upper = wilson$upper,
    conf_level = conf,
    stringsAsFactors = FALSE
  )
}