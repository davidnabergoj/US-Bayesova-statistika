data {
  int<lower=0> N;
  int<lower=0> J;
  array[N] int<lower=1, upper=J> county_idx;
  vector[N] log_radon;
}
parameters {
  vector[J] alpha;
  real mu_alpha;
  real<lower=0> sigma_alpha;
  real<lower=0> sigma_y;
}
model {
  vector[N] mu;
  
  // priors
  sigma_y ~ normal(0, 1);
  sigma_alpha ~ normal(0, 1);
  mu_alpha ~ normal(0, 10);
  
  // likelihood
  alpha ~ normal(mu_alpha, sigma_alpha);
  for (n in 1 : N) {
    mu[n] = alpha[county_idx[n]];
    target += normal_lpdf(log_radon[n] | mu[n], sigma_y);
  }
}

