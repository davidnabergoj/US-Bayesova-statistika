data {
  int<lower=0> N; // dataset size
  int<lower=0> M; // number of features
  real y[N];  // median house value
  vector[M] features[N];
}

parameters {
  real alpha;
  vector[M] beta;
  real<lower=0> sigma;
}

model {
  alpha ~ normal(0, 5);
  beta ~ normal(0, 5);
  sigma ~ cauchy(0, 5);
  
  for (i in 1:N) {
    y[i] ~ normal(alpha + dot_product(features[i], beta), sigma);
  }
}
