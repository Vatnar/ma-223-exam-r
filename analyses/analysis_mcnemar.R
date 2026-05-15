# MA223 Exam R Package
# McNemar's Test Analysis Module

#' Run McNemar's test comparing two experiments
#' @param expA First experiment object
#' @param expB Second experiment object
#' @param output_dir Output directory (not used currently but kept for API consistency)
#' @return Invisible NULL
run_mcnemar_analysis <- function(expA, expB, output_dir) {
  # Extract correct/incorrect vectors
  ExpA_correct <- expA$test_results$correct
  ExpB_correct <- expB$test_results$correct
  
  # Create contingency table
  table_mcnemar <- table(Model_A = ExpA_correct, Model_B = ExpB_correct)
  print(table_mcnemar)
  
  # Run McNemar's test
  mcnemar_result <- mcnemar.test(table_mcnemar)
  print(mcnemar_result)
  
  # Manual chi-square calculation
  c <- table_mcnemar[2]
  b <- table_mcnemar[3]
  cat("\nManual chi-square calculation: (b-c)^2 / (b+c) =", (b - c)^2 / (b + c), "\n")
  
  invisible(NULL)
}
