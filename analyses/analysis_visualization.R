#' Run full visualization suite
#' @param experiments List of all experiment groups
#' @param output_dir Output directory for plots
#' @param theme_name Theme name for plotting
#' @return Invisible NULL
run_visualization_suite <- function(experiments, output_dir, theme_name) {
  set_plot_theme(theme_name)
  
  # Confusion matrix
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
  
  # Reliability diagram
  if (!is.null(experiments[[ExpGroup]]) && !is.null(experiments[[ExpGroup]][[ExpId]])) {
    exp <- experiments[[ExpGroup]][[ExpId]]
    path <- file.path(output_dir, sprintf("%s-%s-reliability.pdf", ExpGroup, ExpId))
    plot_reliability_diagram(exp$test_results, file = path, theme_name = theme_name)
  }
  
  # Learning rate vs test accuracy
  all_exps <- unlist(experiments, recursive = FALSE)
  
  plot_hyperparam_vs_metric(
    all_exps,
    "learning_rate",
    "accuracy_test",
    file = file.path(output_dir, "all_acc_vs_rel.pdf"),
    draw_labels = FALSE,
    theme_name = theme_name
  )
  
  # Activation function chart
  plot_activation_functions(output_dir, theme_name)
  
  # draw top 5 model accuracy
  plot_top_n_models(all_exps, n = 5, output_dir, theme_name)
  
  # Training progress
  illustration_model <- experiments[["ActivationComparison"]][["exp_033"]]
  if (!is.null(illustration_model)) {
    plot_training_curves_illustration(illustration_model, output_dir, theme_name)
    message("  Training curves illustration: exp_033")
  }
  
  message("  Visualizations saved to output/\n")
  
  invisible(NULL)
}
