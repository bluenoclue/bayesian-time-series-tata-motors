data {
  int<lower=1> T;
  vector[T] y;
}
parameters {
  real phi;
  real<lower=0> sigma;
}
model {
  phi ~ normal(0, 1);  // prior 1 (we’ll change this for sensitivity test)
  sigma ~ normal(0, 1);
  for (t in 2:T)
    y[t] ~ normal(phi * y[t - 1], sigma);
}

