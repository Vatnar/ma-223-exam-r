#' Run hypothesis t-test comparing two experiment groups
#' @param experiments List of all experiment groups
#' @param group_name Name of the group to analyze 
#' @param exp_prefixes Character vector of two prefixes
#' @param output_dir Output directory 
#' @return Invisible NULL
run_hypothesis_tests <- function(experiments, group_name, exp_prefixes, output_dir) {

  ExpGroup <- experiments[[group_name]]
  
  if (is.null(ExpGroup)) {
    warning("Group not found: ", group_name)
    return(invisible(NULL))
  }
  

  exp_data_a <- ExpGroup[grep(paste0("^", exp_prefixes[1]), names(ExpGroup))]
  exp_data_b <- ExpGroup[grep(paste0("^", exp_prefixes[2]), names(ExpGroup))]
  
  # extract accuracies
  acc_a <- sapply(exp_data_a, function(rep) {
    rep$summary$metrics$test$accuracy
  })
  
  acc_b <- sapply(exp_data_b, function(rep) {
    rep$summary$metrics$test$accuracy
  })
  
  # Run paired t-test, similar to 17.5.2 from Nyberg 2025
  result <- t.test(acc_b, acc_a, paired = TRUE, alternative = "less")
  print(result)
  
  invisible(NULL)
}
