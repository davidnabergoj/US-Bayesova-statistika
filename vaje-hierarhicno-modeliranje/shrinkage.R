heights_slo <- c(1.77, 1.69, 1.70, 1.89, 1.88, 1.90, 1.83, 1.74)
heights_it <- c(1.77, 1.80, 1.71, 1.77, 1.76, 1.70, 1.77, 1.76, 1.88, 1.62, 1.77, 1.66)

heights <- c(heights_slo, heights_it)
country <- c(rep(0, length(heights_slo)), rep(1, length(heights_it)))

stan_data_wide <- list(
  N = length(heights),
  heights = heights,
  country = country,
  prior_sigma = 3.0
)

stan_data_narrow <- list(
  N = length(heights),
  heights = heights,
  country = country,
  prior_sigma = 0.01
)

library(cmdstanr)
model <- cmdstan_model("shrinkage.stan")
fit_wide_prior <- model$sample(stan_data_wide, seed = 0, parallel_chains = 4)
fit_narrow_prior <- model$sample(stan_data_narrow, seed = 0, parallel_chains = 4)

fit_wide_prior$summary()
fit_narrow_prior$summary()

# True means (in this example): SLO = 1.8, IT = 1.75