data {
  int<lower=0> N;
  real heights[N];
  int<lower=0, upper=1> country[N];
  real<lower=0> prior_sigma;
}

parameters {
  real mu;
  
  real mu_slo;
  real mu_it;
  real<lower=0> sigma_slo;
  real<lower=0> sigma_it;
}

model {
  mu ~ normal(1.78, 0.1);
  mu_slo ~ normal(mu, prior_sigma);
  mu_it ~ normal(mu, prior_sigma);
  sigma_slo ~ cauchy(0, 5);
  sigma_it ~ cauchy(0, 5);
  
  for (i in 1:N) {
    if (country[i] == 0) {
      heights[i] ~ normal(mu_slo, sigma_slo);
    } else {
      heights[i] ~ normal(mu_it, sigma_it);
    }
  }
}
