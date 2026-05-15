# MA223 Exam R Package
# Hypothesis Testing Suite Module

#' Run hypothesis t-test comparing two experiment groups
#' @param experiments List of all experiment groups
#' @param group_name Name of the group to analyze (e.g., "LinearRepetitionManyEpoch")
#' @param exp_prefixes Character vector of two prefixes (e.g., c("exp_001", "exp_002"))
#' @param output_dir Output directory (not used currently but kept for API consistency)
#' @return Invisible NULL
run_hypothesis_tests <- function(experiments, group_name, exp_prefixes, output_dir) {
  # Get the experiment group
  ExpGroup <- experiments[[group_name]]
  
  if (is.null(ExpGroup)) {
    warning("Group not found: ", group_name)
    return(invisible(NULL))
  }
  
  # Extract experiments matching prefixes
  exp_data_a <- ExpGroup[grep(paste0("^", exp_prefixes[1]), names(ExpGroup))]
  exp_data_b <- ExpGroup[grep(paste0("^", exp_prefixes[2]), names(ExpGroup))]
  
  # Extract accuracies
  acc_a <- sapply(exp_data_a, function(rep) {
    rep$summary$metrics$test$accuracy
  })
  
  acc_b <- sapply(exp_data_b, function(rep) {
    rep$summary$metrics$test$accuracy
  })
  
  # Run paired t-test
  result <- t.test(acc_b, acc_a, paired = TRUE, alternative = "less")
  print(result)
  
  invisible(NULL)
}
