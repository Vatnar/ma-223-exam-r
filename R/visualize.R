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

  n_rows <- nrow(cm_mat)
  n_cols <- ncol(cm_mat)

  image(1:n_rows, 1:n_cols, log10(cm_mat + 1),
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
                                  file = NULL, width = 6, height = 4, draw_labels=TRUE) {
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

  means <- tapply(y_vals, x_vals, mean)
  x_unique <- as.numeric(names(means))

  plot(x_unique, means, xlab = param, ylab = paste("Mean", metric),
       main = paste(param, "vs", metric),
       pch = 19, col = "steelblue", log = "x")

  if (length(x_unique) >= 3) {
    x_order <- order(x_unique)
    x_sorted <- x_unique[x_order]
    y_sorted <- means[x_order]
    x_log <- log2(x_sorted)
    k <- max(1, round(length(y_sorted) * 0.1))
    smoothed <- y_sorted
    for (i in 1:length(y_sorted)) {
      start <- max(1, i - k)
      end <- min(length(y_sorted), i + k)
      smoothed[i] <- mean(y_sorted[start:end])
    }
    lines(x_sorted, smoothed, col = "red", lwd = 2, lty = 2)
  } else if (length(x_unique) >= 2) {
    x_order <- order(x_unique)
    x_sorted <- x_unique[x_order]
    y_sorted <- means[x_order]
    lines(x_sorted, y_sorted, col = "red", lwd = 2, lty = 2)
  }
  
  if (draw_labels) {
    text(x_vals, y_vals, labels = summary_df$exp_id,
         pos = 3, cex = 0.6, offset = 0.5)
  }

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

plot_binomial_with_ci <- function(exps, file = NULL, width = 8, height = 5) {
  if (length(exps) == 0) {
    warning("No experiments provided")
    return(NULL)
  }
  
  # Derive activation names (fallback to existing names)
  act_names <- sapply(exps, function(e) {
    act <- NULL
    if (!is.null(e$summary$hyperparameters$activation)) act <- e$summary$hyperparameters$activation
    if (is.null(act) && !is.null(e$config$activation)) act <- e$config$activation
    if (is.null(act)) act <- NA
    as.character(act)
  })
  # Use activation names where available; ensure unique names for legend
  use_names <- ifelse(is.na(act_names) | act_names == "NA" | act_names == "", names(exps), act_names)
  use_names <- make.unique(use_names)
  names(exps) <- use_names
  
  if (!is.null(file)) {
    pdf(file, width = width, height = height)
  }
  
  k_vals <- sapply(exps, function(exp) exp$summary$metrics$test$num_correct)
  n_vals <- sapply(exps, function(exp) exp$summary$metrics$test$num_samples)
  p_hat <- k_vals / n_vals
  max_n <- max(n_vals)
  all_probs <- unlist(lapply(seq_along(n_vals), function(i) dbinom(0:n_vals[i], n_vals[i], p_hat[i])))
  max_prob <- max(all_probs)
  max_n <- min(max_n, 500)
  total_tests <- paste0("Total tests: ", n_vals[1])
  
  plot(NULL, xlim = c(0, max_n), ylim = c(0, max_prob * 1.2),
       xlab = "Number of successes (k)", ylab = "Probability",
       main = paste("Binomial Distributions with Symmetric Credible Intervals\n", total_tests))
  
  cols <- rainbow(length(exps))
  for (i in seq_along(exps)) {
    n <- n_vals[i]
    k <- k_vals[i]
    ci <- symmetric_ci(k, n, 0.95)
    p <- p_hat[i]
    
    k_all <- 0:n
    probs <- dbinom(k_all, n, p)
    
    mask <- probs > 1e-10
    k_clip <- k_all[mask]
    probs_clip <- probs[mask]
    
    x_poly <- c(k_clip, rev(k_clip))
    y_poly <- c(probs_clip, rep(0, length(probs_clip)))
    polygon(x_poly, y_poly,
            col = adjustcolor(cols[i], alpha.f = 0.3), border = NA)
    lines(k_clip, probs_clip, type = "l", col = cols[i], lwd = 2)
    abline(v = k, col = cols[i], lty = 2, lwd = 1.5)
    abline(v = ci$lower * n, col = cols[i], lty = 3)
    abline(v = ci$upper * n, col = cols[i], lty = 3)
  }
  
  # Legend shows activation function names (names(exps) were set above)
  legend("topright", legend = names(exps), col = cols, lty = 1, lwd = 2, cex = 0.8, bg = "white")
  
  if (!is.null(file)) {
    dev.off()
  }
  
  invisible(NULL)
}

#' Plot activation functions reference chart
#' @param output_dir Output directory
#' @param theme_name Theme name (for consistent styling)
#' @return Invisible NULL
plot_activation_functions <- function(output_dir, theme_name = "poster") {
  # Define activation functions
  relu <- function(x) pmax(0, x)
  sigmoid <- function(x) 1 / (1 + exp(-x))
  gelu <- function(x) x * pnorm(x)
  silu <- function(x) x * sigmoid(x)
  
  # Generate x values
  x <- seq(-5, 5, length.out = 500)
  
  # Create data frames for each function
  data_linear <- data.frame(x = x, y = x, function_name = "Linear")
  data_relu <- data.frame(x = x, y = relu(x), function_name = "ReLU")
  data_sigmoid <- data.frame(x = x, y = sigmoid(x), function_name = "Sigmoid")
  data_tanh <- data.frame(x = x, y = tanh(x), function_name = "Tanh")
  data_gelu <- data.frame(x = x, y = gelu(x), function_name = "GELU")
  data_silu <- data.frame(x = x, y = silu(x), function_name = "SiLU")
  
  # Color palette - rainbow colors
  colors <- c(
    "Linear" = "#E41A1C",   # Red
    "ReLU" = "#377EB8",     # Blue
    "Sigmoid" = "#4DAF4A",  # Green
    "Tanh" = "#984EA3",     # Purple
    "GELU" = "#FF7F00",     # Orange
    "SiLU" = "#F781BF"      # Pink
  )
  
  # Function to create individual plot
  create_activation_plot <- function(data, y_limits = NULL) {
    p <- ggplot(data, aes(x = x, y = y, color = function_name)) +
      geom_line(linewidth = 1.2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5) +
      scale_color_manual(values = colors) +
      labs(title = data$function_name[1],
           x = "x",
           y = "f(x)") +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.position = "none",
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95"),
        panel.border = element_rect(color = "gray70", fill = NA, linewidth = 0.5),
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 9)
      )
    
    if (!is.null(y_limits)) {
      p <- p + coord_cartesian(ylim = y_limits)
    }
    
    return(p)
  }
  
  # Create individual plots
  p1 <- create_activation_plot(data_linear)
  p2 <- create_activation_plot(data_relu)
  p3 <- create_activation_plot(data_sigmoid, y_limits = c(-0.2, 1.2))
  p4 <- create_activation_plot(data_tanh, y_limits = c(-1.2, 1.2))
  p5 <- create_activation_plot(data_gelu)
  p6 <- create_activation_plot(data_silu)
  
  # Create 2x3 grid using patchwork (part of tidyverse) or cowplot
  # Using cowplot for grid arrangement
  if (requireNamespace("cowplot", quietly = TRUE)) {
    combined_plot <- cowplot::plot_grid(
      p1, p2, p3, p4, p5, p6,
      nrow = 2, ncol = 3,
      labels = NULL
    )
  } else {
    # Fallback: save individual plots or use gridExtra if available
    # For now, just use ggsave on the first plot as a fallback
    combined_plot <- p1
  }
  
  # Save to output directory
  ggsave(
    file.path(output_dir, "activation_functions.pdf"),
    plot = combined_plot,
    width = 12,
    height = 8,
    dpi = 300
  )
  
  invisible(NULL)
}