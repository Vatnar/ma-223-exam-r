#' Run confidence interval analysis on selected experiments
#' @param experiments List of experiments from a group
#' @param exps_list Character vector of experiment IDs to analyze
#' @param output_dir Output directory for plots
#' @param theme_name Theme name for plotting
#' @return Invisible NULL
run_ci_analysis <- function(experiments, exps_list, output_dir, theme_name) {
  
  # get experiments to run get CI's
  named_exps <- experiments[exps_list]
  names(named_exps) <- exps_list
  
  # binomial plot visualization for report
  set_plot_theme(theme_name)
  plot_binomial_with_ci(named_exps,
                        file = file.path(output_dir, "binomial_with_ci.pdf"),
                        theme_name = theme_name)
  
  # Print symmetric Beta CI table
  cat(sprintf("  %-12s %-12s %12s %15s\n", "exp", "activation", "estimate", "95% CI"))
  for (nm in exps_list) {
    # Based on formula 15.3.1 from Nyberg 2025
    exp <- experiments[[nm]]
    
    n <- exp$summary$metrics$test$num_samples
    k <- exp$summary$metrics$test$num_correct
    
    ci <- symmetric_ci(k, n, 0.95)
    cat(sprintf(
      "  %-12s %-12s %12.4f [%10.4f, %10.4f]\n",
      nm,
      as.character(exp$summary$hyperparameters$activation),
      ci$estimate,
      ci$lower,
      ci$upper
    ))
  }
  
  message("\n--- Test Accuracy with Wilson confidence interval ---")
  cat(sprintf("  %-12s %-12s %12s %15s\n", "exp", "activation", "estimate", "95% CI"))
  for (nm in exps_list) {
    exp <- experiments[[nm]]
    metrics <- exp$summary$metrics
    n_correct <- metrics$test$num_correct
    n_total <- metrics$test$num_samples
    
    # Wilson CI is implemented in stats package, so we use that instead of implementing it from scratch
    ci <- prop.test(n_correct, n_total, conf.level = 0.95)
    cat(sprintf(
      "  %-12s %-12s %12.4f [%10.4f, %10.4f]\n",
      nm,
      as.character(exp$summary$hyperparameters$activation),
      as.numeric(ci$estimate),
      as.numeric(ci$conf.int[1]),
      as.numeric(ci$conf.int[2])
    ))
  }
  
  invisible(NULL)
}
