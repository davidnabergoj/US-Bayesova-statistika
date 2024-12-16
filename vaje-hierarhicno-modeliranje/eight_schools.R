library(cmdstanr)
library(bayesplot)
library(ggplot2)

# References:
# - https://www.tensorflow.org/probability/examples/Eight_Schools
# - https://mc-stan.org/users/documentation/case-studies/divergences_and_bias.html
# - https://github.com/stan-dev/posteriordb/blob/41b523f1de8d46df9fcf4c98a5bb97abf83d4700/posterior_database/models/stan/eight_schools_centered.stan
# - https://github.com/stan-dev/posteriordb/blob/41b523f1de8d46df9fcf4c98a5bb97abf83d4700/posterior_database/models/pymc3/eight_schools_noncentered.py

data <- list(
  J = 8,
  y = as.integer(c(28, 8, -3, 7, -1, 1, 18, 12)),
  sigma = as.integer(c(15, 10, 16, 11, 9, 11, 10, 18))
)

model_centered = cmdstan_model("eight_schools_centered.stan")
model_non_centered = cmdstan_model("eight_schools_non_centered.stan")

fit_centered = model_centered$sample(
  data = data,
  seed = 0,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 500,
  iter_sampling = 3000,
)
fit_centered$cmdstan_diagnose()

fit_non_centered = model_non_centered$sample(
  data = data,
  seed = 0,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 500,
  iter_sampling = 3000,
)
fit_non_centered$cmdstan_diagnose()


variables = c("mu", "tau", "theta")

fit_centered$summary(variables)
fit_non_centered$summary(variables)

# Observations:
# - ESS is higher for the non-centered model
# - rhat is lower for the non-centered model

# Look at the traceplots
bayesplot::mcmc_trace(fit_centered$draws(variables))
bayesplot::mcmc_trace(fit_non_centered$draws(variables))

bayesplot::mcmc_acf(fit_centered$draws(variables))
bayesplot::mcmc_acf(fit_non_centered$draws(variables))

bayesplot::mcmc_areas(fit_centered$draws(variables)) + ggplot2::xlim(-40, 40)
bayesplot::mcmc_areas(fit_non_centered$draws(variables)) + ggplot2::xlim(-40, 40)

bayesplot::mcmc_dens(fit_centered$draws(variables))
bayesplot::mcmc_dens(fit_non_centered$draws(variables))

# Probability that school 1 had better results than school 2?
mean(fit_non_centered$draws("theta[1]") > fit_non_centered$draws("theta[2]"))
