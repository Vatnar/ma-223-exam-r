#' List available experiment groups
#' @param base_path Base path to experiments
#' @return Character vector of group names
list_experiment_groups <- function(base_path = get_repo_root()) {
  exp_base <- file.path(base_path, "results/experiments")
  list.dirs(exp_base, full.names = FALSE, recursive = FALSE)
}

#' List available experiments in a group
#' @param base_path Base path to experiments
#' @param group Group name
#' @return Character vector of experiment IDs
list_experiments <- function(base_path = get_repo_root(), group = "ActivationComparison") {
  exp_path <- file.path(base_path, "results/experiments", group)

  if (!dir.exists(exp_path)) {
    stop("Experiments directory not found: ", exp_path)
  }

  dirs <- list.dirs(exp_path, full.names = FALSE, recursive = FALSE)
  dirs[grepl("^exp_", dirs)]
}

#' Load single experiment results
#' @param exp_id Experiment ID (e.g., "exp_050")
#' @param base_path Base path to experiments (default from get_repo_root)
#' @param group Group name (e.g., "InterpolateLearningRate", "Linear3", "ActivationComparison")
#' @return List with all experiment data
load_experiment <- function(exp_id, base_path = get_repo_root(), group = "ActivationComparison") {
  exp_path <- file.path(base_path, "results/experiments", group, exp_id)

  if (!dir.exists(exp_path)) {
    stop("Experiment not found: ", exp_path)
  }

  config <- load_json(file.path(exp_path, "config.json"))
  summary <- load_json(file.path(exp_path, "summary.json"))
  history_train <- load_csv(file.path(exp_path, "history_train.csv"))
  history_valid <- load_csv(file.path(exp_path, "history_valid.csv"))
  test_results <- load_csv(file.path(exp_path, "test_results.csv"))
  confusion_matrix <- load_csv(file.path(exp_path, "confusion_matrix.csv"))

  list(
    exp_id = exp_id,
    path = exp_path,
    config = config,
    summary = summary,
    history_train = history_train,
    history_valid = history_valid,
    test_results = test_results,
    confusion_matrix = confusion_matrix
  )
}

#' Load multiple experiments
#' @param exp_ids Character vector of experiment IDs
#' @param base_path Base path to experiments
#' @param group Group name
#' @return Named list of experiment objects
load_experiments <- function(exp_ids, base_path = get_repo_root(), group = "ActivationComparison") {
  exps <- lapply(exp_ids, function(exp_id) {
    tryCatch(
      load_experiment(exp_id, base_path, group),
      error = function(e) {
        warning("Failed to load ", exp_id, ": ", e$message)
        NULL
      }
    )
  })

  names(exps) <- exp_ids
  exps[!sapply(exps, is.null)]
}

#' Load all available experiments from a group
#' @param base_path Base path to experiments
#' @param group Group name
#' @return Named list of all experiment objects
load_all_experiments <- function(base_path = get_repo_root(), group = "ActivationComparison") {
  exp_ids <- list_experiments(base_path, group)
  load_experiments(exp_ids, base_path, group)
}

#' Get confusion matrix as matrix object
#' @param cm Confusion matrix data frame (from CSV)
#' @param n_classes Number of classes
#' @return Matrix object
get_confusion_matrix <- function(cm, n_classes = 50) {
  if (is.null(cm)) {
    return(NULL)
  }

  mat <- matrix(0, nrow = n_classes, ncol = n_classes)
  for (i in 1:nrow(cm)) {
    true_label <- as.integer(cm$true_label[i])
    pred_label <- as.integer(cm$predicted_label[i])
    count <- as.integer(cm$count[i])

    if (true_label > 0 && true_label <= n_classes &&
        pred_label > 0 && pred_label <= n_classes) {
      mat[true_label, pred_label] <- mat[true_label, pred_label] + count
    }
  }

  rownames(mat) <- as.character(1:n_classes)
  colnames(mat) <- as.character(1:n_classes)
  mat
}