data(iris)

# Goal: see which attributes contribute most to a positive prediction

iris$setosa <- iris$Species == "setosa"

stan_data <- list(
  N = 150,
  features = scale(iris[,1:4]),  # 15% divergences if we do not standardize
  setosa = as.integer(iris$setosa)
)

library(cmdstanr)
model <- cmdstan_model("iris.stan")
fit <- model$sample(stan_data, seed = 0, chains = 4, parallel_chains = 4, iter_warmup = 500, iter_sampling = 3000)

fit$cmdstan_diagnose()
fit$summary()

library(bayesplot)
bayesplot::mcmc_areas(fit$draws("beta"))

# beta[2] is the most important setosa predictor!