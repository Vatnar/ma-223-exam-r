# MA223 Exam R Package
# Visualization Suite Module

#' Run full visualization suite
#' @param experiments List of all experiment groups
#' @param output_dir Output directory for plots
#' @param theme_name Theme name for plotting
#' @return Invisible NULL
run_visualization_suite <- function(experiments, output_dir, theme_name) {
  set_plot_theme(theme_name)
  
  # --- Confusion Matrix for specific experiment ---
  ExpGroup <- "ActivationComparison"
  ExpId <- "exp_027"
  
  if (!is.null(experiments[[ExpGroup]]) && !is.null(experiments[[ExpGroup]][[ExpId]])) {
    exp <- experiments[[ExpGroup]][[ExpId]]
    path <- file.path(output_dir, sprintf("%s-%s-matrix.pdf", ExpGroup, ExpId))
    
    plot_confusion_matrix(
      exp$confusion_matrix,
      file = path,
      max_classes = 50,
      theme_name = theme_name
    )
  }
  
  # --- Reliability Diagram ---
  if (!is.null(experiments[[ExpGroup]]) && !is.null(experiments[[ExpGroup]][[ExpId]])) {
    exp <- experiments[[ExpGroup]][[ExpId]]
    path <- file.path(output_dir, sprintf("%s-%s-reliability.pdf", ExpGroup, ExpId))
    plot_reliability_diagram(exp$test_results, file = path, theme_name = theme_name)
  }
  
  # --- Hyperparameter vs Metric Plot ---
  # Flatten all experiments into a single list
  all_exps <- unlist(experiments, recursive = FALSE)
  
  plot_hyperparam_vs_metric(
    all_exps,
    "learning_rate",
    "accuracy_test",
    file = file.path(output_dir, "all_acc_vs_rel.pdf"),
    draw_labels = FALSE,
    theme_name = theme_name
  )
  
  # --- Activation Functions Reference ---
  plot_activation_functions(output_dir, theme_name)
  
  # --- Top 5 Models ---
  plot_top_n_models(all_exps, n = 5, output_dir, theme_name)
  
  # --- Best Model Training Curves ---
  best_result <- find_best_model(all_exps)
  if (!is.null(best_result$exp)) {
    plot_best_model_curves(best_result$exp, output_dir, theme_name)
    message("  Best model: ", best_result$name, 
            " (test acc: ", round(best_result$accuracy, 4), ")")
  }
  
  message("  Visualizations saved to output/\n")
  
  invisible(NULL)
}
