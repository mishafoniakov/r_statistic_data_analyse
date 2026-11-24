# Temporary verification file for the OpenCodeReview workflow.

compute_mean <- function(values) {
  total <- sum(values)
  total / length(values)
}

load_metrics <- function(path) {
  data <- read.csv(path)
  data$score * 100
}
