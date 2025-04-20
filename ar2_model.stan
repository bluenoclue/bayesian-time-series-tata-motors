data {
  int<lower=3> T;
  vector[T] y;
}

parameters {
  real phi1;
  real phi2;
  real<lower=0> sigma;
}

model {
  phi1 ~ normal(0, 0.5);
  phi2 ~ normal(0, 0.5);
  sigma ~ normal(0, 1);

  for (t in 3:T)
    y[t] ~ normal(phi1 * y[t - 1] + phi2 * y[t - 2], sigma);
}

generated quantities {
  vector[T] log_lik;
  log_lik[1] = 0;
  log_lik[2] = 0;
  for (t in 3:T)
    log_lik[t] = normal_lpdf(y[t] | phi1 * y[t - 1] + phi2 * y[t - 2], sigma);
}

