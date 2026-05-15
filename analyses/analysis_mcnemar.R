#' Run McNemar's test comparing two experiments
#' @param expA First experiment object
#' @param expB Second experiment object
#' @param output_dir Output directory 
#' @return Invisible NULL
run_mcnemar_analysis <- function(expA, expB, output_dir) {

  
  ExpA_correct <- expA$test_results$correct
  ExpB_correct <- expB$test_results$correct
  
  table_mcnemar <- table(Model_A = ExpA_correct, Model_B = ExpB_correct)
  print(table_mcnemar)
  
  # Mcnemars chi squared test https://en.wikipedia.org/wiki/McNemar%27s_test
  mcnemar_result <- mcnemar.test(table_mcnemar)
  print(mcnemar_result)
  
  # chis square calculation
  c <- table_mcnemar[2]
  b <- table_mcnemar[3]
  cat("\nManual chi-square calculation: (b-c)^2 / (b+c) =", (b - c)^2 / (b + c), "\n")
  
  invisible(NULL)
}
