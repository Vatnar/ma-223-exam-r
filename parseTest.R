library("rjson")
library(jsonlite)
library(here)

readRenviron(here(".env"))

ROOT <- Sys.getenv("REPO_ROOT")
OUTPUT <- Sys.getenv("OUTPUT")
dir.create(OUTPUT, showWarnings = FALSE, recursive = TRUE)

experiments <- file.path(ROOT, "results/experiments")
interpolateExp <- file.path(experiments, "InterpolateLearningRate")
summaryPath <- file.path(interpolateExp, "summary.json")

data <- fromJSON(summaryPath)
ids <- data$experiments$id


df <- data.frame(
  id = character(length(ids)),
  exp_index = seq_along(ids),
  prob = numeric(length(ids)),
  size = numeric(length(ids)),
  stringsAsFactors = FALSE
)


for (i in seq_along(ids)) {
  expPath <- file.path(interpolateExp, ids[i])
  metrics <- fromJSON(file.path(expPath, "summary.json"))$metrics
  df$id[i] <- ids[i]
  df$prob[i] <- metrics$test$accuracy
  df$size[i] <- metrics$test$num_samples
}


n <- unique(df$size)
if(length(n) > 1) stop("Experiments must have the same number of samples")
n <- n[1]

x <- 0:n
y <- df$exp_index

# Matrix Z must be: rows = length(x), cols = length(y)
z <- sapply(df$prob, function(p) dbinom(x, size = n, prob = p))


persp(x = x, y = y, z = z,
      theta = 45, phi = 30, expand = 0.8,
      col = "lightblue",    
      shade = 0.2,          
      border = NA,          
      ticktype = "detailed",
      xlab = "Successes", ylab = "Experiment", zlab = "Probability",
      main = "Binomial PMF across experiments")
