# MA223 Exam R Package
# Visualization Functions

#' Plot theme settings
#' @param theme One of "report" (clean, black/white) or "poster" (vivid colors)
set_plot_theme <- function(theme = "report") {
  if (theme == "poster") {
    par(bg = "white", col.main = "black", col.lab = "black", col.axis = "black")
    par(mar = c(5, 5, 4, 2), mgp = c(3, 1, 0))
    options(device = "pdf")
  } else {
    par(bg = "white", col.main = "black", col.lab = "black", col.axis = "black")
    par(mar = c(5, 4, 4, 2), mgp = c(3, 1, 0))
  }
  invisible(NULL)
}

# Colors for different themes
get_theme_colors <- function(theme = "report") {
  if (theme == "poster") {
    list(
      primary = "#E63946",
      secondary = "#457B9D",
      accent = "#1D3557",
      background = "#F1FAEE",
      palette = c("#E63946", "#457B9D", "#1D3557", "#A8DADC", "#F4A261", "#2A9D8F")
    )
  } else {
    list(
      primary = "steelblue",
      secondary = "darkgray",
      accent = "black",
      background = "white",
      palette = c("steelblue", "darkgray", "black", "lightgray", "firebrick", "forestgreen")
    )
  }
}

plot_training_curves <- function(exp, file = NULL, width = 10, height = 4) {
  if (is.null(exp)) {
    warning("Experiment is NULL")
    return(NULL)
  }

  history_train <- exp$history_train
  history_valid <- exp$history_valid
  summary <- exp$summary

  has_train_data <- !is.null(history_train) &&
                   !all(is.na(history_train$accuracy)) &&
                   sum(history_train$accuracy, na.rm = TRUE) > 0

  has_valid_data <- !is.null(history_valid) &&
                   !all(is.na(history_valid$accuracy)) &&
                   sum(history_valid$accuracy, na.rm = TRUE) > 0

  if (!is.null(file)) {
    pdf(file, width = width, height = height)
  }

  par(mfrow = c(1, 3))

  test_acc <- summary$metrics$test$accuracy
  train_acc <- summary$metrics$train$final_accuracy
  valid_acc <- summary$metrics$valid$best_accuracy
  best_epoch <- summary$metrics$valid$epoch
  train_loss <- summary$metrics$train$final_loss
  valid_loss <- summary$metrics$valid$best_loss

  plot(1, test_acc, pch = 19, cex = 2, col = "blue",
       xlim = c(0, 2), ylim = c(0, 1),
       xlab = "", ylab = "Accuracy", main = "Final Accuracies")
  points(2, valid_acc, pch = 19, cex = 2, col = "red")
  points(1.5, train_acc, pch = 19, cex = 2, col = "green")
  axis(1, at = c(1, 1.5, 2), labels = c("Test", "Train", "Valid"))
  text(1, test_acc + 0.08, round(test_acc, 3), cex = 0.7)
  text(1.5, train_acc + 0.08, round(train_acc, 3), cex = 0.7)
  text(2, valid_acc + 0.08, round(valid_acc, 3), cex = 0.7)

  plot(1:25, rep(train_loss, 25), type = "l", col = "green",
       xlab = "Epoch", ylab = "Loss", main = "Training/Valid Loss",
       ylim = c(0, max(train_loss, valid_loss, na.rm = TRUE) * 1.2))
  lines(1:25, rep(valid_loss, 25), col = "red", lty = 2)
  legend("topright", c("Train", "Valid"),
         col = c("green", "red"), lty = c(1, 2), cex = 0.6)

  plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
  text(1, 1, paste0(
    "Exp: ", exp$exp_id, "\n\n",
    "Activation: ", exp$config$activation, "\n",
    "Learning Rate: ", round(exp$config$learning_rate, 5), "\n",
    "Dropout: ", exp$config$dropout, "\n",
    "Max Epochs: ", exp$config$max_epochs, "\n\n",
    "Test Acc: ", round(test_acc, 3), "\n",
    "Valid Acc: ", round(valid_acc, 3), " (epoch ", best_epoch, ")\n",
    "Train Acc: ", round(train_acc, 3), "\n",
    "Train Loss: ", round(train_loss, 4)
  ), cex = 0.9)

  par(mfrow = c(1, 1))

  if (!is.null(file)) {
    dev.off()
  }

  invisible(NULL)
}

plot_confusion_matrix <- function(cm, file = NULL, max_classes = 50,
                              width = 8, height = 8) {
  if (is.null(cm)) {
    warning("Confusion matrix is NULL")
    return(NULL)
  }

  cm_mat <- get_confusion_matrix(cm, n_classes = max_classes)

  if (is.null(cm_mat)) {
    warning("Could not convert confusion matrix")
    return(NULL)
  }

  if (!is.null(file)) {
    pdf(file, width = width, height = height)
  }

  cols <- colorRampPalette(c("white", "navy"))(100)

  image(1:nrow(cm_mat), 1:ncol(cm_mat), log10(cm_mat + 1),
        col = cols, xlab = "Predicted", ylab = "True",
        main = "Confusion Matrix (log10 scale)")

  abline(v = seq(0.5, max_classes + 0.5, by = 5), col = "gray", lty = 1)
  abline(h = seq(0.5, max_classes + 0.5, by = 5), col = "gray", lty = 1)

  if (!is.null(file)) {
    dev.off()
  }

  invisible(NULL)
}

plot_accuracy_comparison <- function(exps, metric = "accuracy_test", file = NULL,
                            width = 10, height = 5) {
  if (is.null(exps) || length(exps) == 0) {
    warning("No experiments to plot")
    return(NULL)
  }

  summary_df <- summarize_experiments(exps)

  if (is.null(summary_df) || !(metric %in% names(summary_df))) {
    warning("Metric not found in summary")
    return(NULL)
  }

  test_n <- summary_df$num_samples_test[1]

  acc_with_ci <- lapply(exps, function(exp) {
    if (is.null(exp$summary)) {
      return(NULL)
    }
    metrics <- exp$summary$metrics
    n_correct <- metrics$test$num_correct
    n_total <- metrics$test$num_samples

    wilson_ci(n_correct, n_total)
  })

  acc_with_ci <- Filter(function(x) !is.null(x), acc_with_ci)

  estimates <- sapply(acc_with_ci, function(x) x$estimate)
  lowers <- sapply(acc_with_ci, function(x) x$lower)
  uppers <- sapply(acc_with_ci, function(x) x$upper)

  yerr <- rbind(estimates - lowers, uppers - estimates)

  if (!is.null(file)) {
    pdf(file, width = width, height = height)
  }

  par(mar = c(8, 4, 4, 2))

  barplot(estimates, names.arg = names(exps),
         ylim = c(0, max(uppers) * 1.1),
         xlab = "Experiment", ylab = "Accuracy",
         main = paste("Test Accuracy Comparison (n =", test_n, ")"),
         col = "steelblue", las = 2)

  arrows(1:length(estimates), estimates,
         1:length(estimates), estimates + yerr[2, ],
         angle = 90, length = 0.05)
  arrows(1:length(estimates), estimates - yerr[1, ],
         1:length(estimates), estimates,
         angle = 90, length = 0.05)

  abline(h = 0, col = "gray")

  par(mar = c(5.1, 4.1, 4.1, 2.1))

  if (!is.null(file)) {
    dev.off()
  }

  invisible(NULL)
}

plot_binom_surface <- function(n = 758, p_val = 0.5, file = NULL,
                            width = 8, height = 6) {
  if (!is.null(file)) {
    pdf(file, width = width, height = height)
  }

  x_vals <- 0:n
  p_vals <- seq(0.05, 0.95, length.out = 25)

  z <- outer(x_vals, p_vals, function(k, prob) {
    dbinom(k, n, prob)
  })

  cols <- colorRampPalette(c("white", "lightyellow", "orange", "red"))(100)

  image(x_vals, p_vals, z, col = cols,
       xlab = "Number of successes (k)",
       ylab = "Probability (p)",
       main = paste("Binomial(n =", n, ", p) PMF"))

  contour(x_vals, p_vals, z, add = TRUE, col = "darkgray",
          levels = c(0.001, 0.01, 0.05, 0.1), labcex = 0.7)

  if (!is.null(file)) {
    dev.off()
  }

  invisible(NULL)
}

plot_hyperparam_vs_metric <- function(exps, param = "learning_rate",
                                  metric = "accuracy_test",
                                  file = NULL, width = 6, height = 4) {
  if (is.null(exps) || length(exps) == 0) {
    warning("No experiments to plot")
    return(NULL)
  }

  summary_df <- summarize_experiments(exps)

  if (is.null(summary_df) ||
      !(param %in% names(summary_df)) ||
      !(metric %in% names(summary_df))) {
    warning("Parameter or metric not found")
    return(NULL)
  }

  x_vals <- summary_df[[param]]
  y_vals <- summary_df[[metric]]

  if (!is.null(file)) {
    pdf(file, width = width, height = height)
  }

  plot(x_vals, y_vals, xlab = param, ylab = metric,
       main = paste(param, "vs", metric),
       pch = 19, col = "steelblue")

  if (length(x_vals) > 2) {
    loess_fit <- loess(y_vals ~ x_vals)
    x_order <- order(x_vals)
    lines(x_vals[x_order], fitted(loess_fit)[x_order],
          col = "red", lty = 2)
  }

  text(x_vals, y_vals, labels = summary_df$exp_id,
       pos = 3, cex = 0.6, offset = 0.5)

  if (!is.null(file)) {
    dev.off()
  }

  invisible(NULL)
}

plot_validation_curves <- function(exps, file = NULL, width = 8, height = 5) {
  if (is.null(exps) || length(exps) == 0) {
    warning("No experiments to plot")
    return(NULL)
  }

  if (!is.null(file)) {
    pdf(file, width = width, height = height)
  }

  summary_df <- summarize_experiments(exps)

  has_history <- sapply(exps, function(exp) {
    !is.null(exp$history_valid) &&
    !all(is.na(exp$history_valid$accuracy)) &&
    sum(exp$history_valid$accuracy, na.rm = TRUE) > 0
  })

  if (any(has_history)) {
    cols <- rainbow(length(exps))

    first_exp <- TRUE

    for (nm in names(exps)) {
      exp <- exps[[nm]]

      if (is.null(exp$history_valid)) {
        next
      }

      history <- exp$history_valid

      if (first_exp) {
        plot(history$epoch, history$accuracy,
             type = "l", col = cols[which(nm == names(exps))],
             xlab = "Epoch", ylab = "Validation Accuracy",
             main = "Validation Accuracy Curves",
             ylim = c(0, 1))
        first_exp <- FALSE
      } else {
        lines(history$epoch, history$accuracy,
             col = cols[which(nm == names(exps))])
      }
    }

    legend("bottomright", names(exps), col = cols, lty = 1, cex = 0.7)
  } else {
    barplot(summary_df$accuracy_valid,
           names.arg = summary_df$exp_id,
           ylim = c(0, 1),
           xlab = "Experiment", ylab = "Best Validation Accuracy",
           main = "Validation Accuracy (from summary)",
           col = "steelblue", las = 2)

    text(1:length(exps), summary_df$accuracy_valid + 0.03,
         round(summary_df$accuracy_valid, 3), cex = 0.7)
  }

  if (!is.null(file)) {
    dev.off()
  }

  invisible(NULL)
}

plot_reliability_diagram <- function(test_results, n_bins = 10, file = NULL,
                                  width = 6, height = 5) {
  if (is.null(test_results) || !"confidence" %in% names(test_results)) {
    warning("Test results must have confidence scores")
    return(NULL)
  }

  rel_data <- reliability_diagram_data(test_results, n_bins)

  if (is.null(rel_data)) {
    warning("Could not compute reliability data")
    return(NULL)
  }

  ece <- calibration_error(test_results, n_bins)

  if (!is.null(file)) {
    pdf(file, width = width, height = height)
  }

  plot(rel_data$bin_center, rel_data$actual_accuracy,
       pch = 19, col = "steelblue", cex = 1.5,
       xlab = "Confidence", ylab = "Actual Accuracy",
       main = paste("Reliability Diagram (ECE =", round(ece, 3), ")"),
       xlim = c(0, 1), ylim = c(0, 1))

  lines(rel_data$bin_center, rel_data$actual_accuracy, col = "steelblue", lty = 1)

  abline(0, 1, col = "gray", lty = 2)

  if (!is.null(file)) {
    dev.off()
  }

  invisible(NULL)
}

plot_bootstrap_dist <- function(boot_result, file = NULL,
                               width = 6, height = 4) {
  if (is.null(boot_result) || is.null(boot_result$boot_dist)) {
    warning("No bootstrap distribution available")
    return(NULL)
  }

  if (!is.null(file)) {
    pdf(file, width = width, height = height)
  }

  hist(boot_result$boot_dist,
       xlab = "Accuracy / Difference",
       main = paste("Bootstrap Distribution (n =", boot_result$n_bootstrap, ")"),
       col = "lightgray", border = "gray")

  abline(v = boot_result$estimate, col = "blue", lwd = 2)
  abline(v = boot_result$ci_lower, col = "red", lty = 2)
  abline(v = boot_result$ci_upper, col = "red", lty = 2)

  legend("topright", c("Estimate", "95% CI"),
         col = c("blue", "red"), lty = c(1, 2), cex = 0.7)

  if (!is.null(file)) {
    dev.off()
  }

  invisible(NULL)
}