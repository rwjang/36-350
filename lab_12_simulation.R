generate_data <- function(n, p) {
  return (list(covariates = matrix(rnorm(n * p), nrow = n, ncol = p), samples = rnorm(n)))
}

model_select <- function(covariates, responses, cutoff) {
  p_cutoff <- which(summary(lm(responses ~ covariates))$coefficients[, 4] < cutoff)
  return(summary(lm(responses ~ covariates[, p_cutoff]))$coefficients[, 4])
}