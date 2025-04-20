data {
  int<lower=2> T;
  vector[T] y;
}

parameters {
  real phi;
  real<lower=0> sigma;
}

model {
  phi ~ normal(0, 0.5);
  sigma ~ normal(0, 1);

  for (t in 2:T)
    y[t] ~ normal(phi * y[t - 1], sigma);
}

generated quantities {
  vector[T] log_lik;
  log_lik[1] = 0;  // manually assign first element
  for (t in 2:T)
    log_lik[t] = normal_lpdf(y[t] | phi * y[t - 1], sigma);
}

