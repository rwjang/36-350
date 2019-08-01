library(ggplot2)
library(gridExtra)

generate_data <- function(n, p) {
  return (list(covariates = matrix(rnorm(n * p), nrow = n, ncol = p), responses = rnorm(n)))
}

model_select <- function(covariates, responses, cutoff) {
  p_cutoff <- which(summary(lm(responses ~ covariates))$coefficients[-1, 4] < cutoff)
  if (length(p_cutoff) == 0) {
    return(vector())
  }
  else {
    return(summary(lm(responses ~ covariates[, p_cutoff]))$coefficients[-1, 4])
  }
}

run_simulation <- function(n_trials, n, p, cutoff) {
  sim_list <- vector("list", n_trials)
  for (i in 1:n_trials) {
    gd <- generate_data(n, p)
    sim_list[[i]] <- unname(model_select(gd$covariates, gd$responses, cutoff))
  }
  sim_df <- data.frame(x = unlist(sim_list))
  return(ggplot(data = sim_df, aes(x = x)) + geom_histogram())
}

sim_hist_list <- list()
i <- 1
for (nx in c(100, 1000, 10000)) {
  for (px in c(10, 20, 50)) {
    sim_hist_list[[i]] <- run_simulation(1000, nx, px, 0.05)
    i <- i + 1
  }
}
grid.arrange(grobs = sim_hist_list)