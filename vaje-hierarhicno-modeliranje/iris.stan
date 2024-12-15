data {
  int<lower=0> N; // dataset size
  int<lower=0, upper=1> setosa[N];  // target
  vector[4] features[N];
}

parameters {
  real alpha;
  vector[4] beta;
}

model {
  alpha ~ normal(0, 5);
  beta ~ normal(0, 5);
  
  for (i in 1:N) {
    setosa[i] ~ bernoulli_logit(alpha + dot_product(features[i], beta));
  }
}
