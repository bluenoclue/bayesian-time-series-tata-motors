# -------------------------------------------------------------
# Time Series Analysis of Tata Motors Stock Using Bayesian AR Models
# -------------------------------------------------------------

# Load required libraries
library(tidyverse)
library(rstan)
library(posterior)
library(bayesplot)
library(coda)
library(loo)

# Configured parallel settings for rstan
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# -------------------------------------------------------------
# Loaded and preprocessed stock data
# -------------------------------------------------------------

# Set working directory and read CSV
setwd("~/Downloads")
stock_data <- read_csv("TATAMOTORS.NS.csv")

# Converted relevant columns to numeric and sort chronologically
stock_data <- stock_data %>%
  mutate(
    Date = as.Date(Date),
    `Adj Close` = as.numeric(`Adj Close`),
    Close = as.numeric(Close),
    Open = as.numeric(Open),
    High = as.numeric(High),
    Low = as.numeric(Low),
    Volume = as.numeric(Volume)
  ) %>%
  arrange(Date)

# Calculated daily log returns
stock_data <- stock_data %>%
  mutate(log_return = log(`Adj Close` / lag(`Adj Close`))) %>%
  drop_na(log_return)

# Ploted histogram of log returns
ggplot(stock_data, aes(x = log_return)) +
  geom_histogram(bins = 60, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Log Returns", x = "Log Return", y = "Count")

# -------------------------------------------------------------
# Focus on recent data for autocorrelation analysis
# -------------------------------------------------------------

stock_data_recent <- stock_data %>%
  filter(Date >= as.Date("2020-01-01")) %>%
  arrange(Date)

# Plot ACF of recent log returns
acf(stock_data_recent$log_return, lag.max = 30,
    main = "ACF of Log Returns (2020–2024)")

# -------------------------------------------------------------
# Prepared data and fit AR(1) model in Stan
# -------------------------------------------------------------

y <- stock_data$log_return
stan_data <- list(
  T = length(y),
  y = y
)

fit_ar1 <- stan(
  file = "ar1_model.stan",
  data = stan_data,
  iter = 2000,
  warmup = 500,
  chains = 4,
  seed = 123
)

# Print results and check convergence
print(fit_ar1, digits = 3)
summary(fit_ar1)$summary[, "Rhat"]

# Posterior diagnostics
posterior_samples <- as_draws_df(fit_ar1)
mcmc_trace(posterior_samples, pars = c("phi", "sigma"))
mcmc_dens_overlay(posterior_samples, pars = c("phi", "sigma"))

# -------------------------------------------------------------
# Prior sensitivity analysis for AR(1) coefficient φ
# -------------------------------------------------------------

fit_ar1_sens <- stan(
  file = "ar1_model_wideprior.stan",
  data = stan_data,
  iter = 2000,
  warmup = 500,
  chains = 4,
  seed = 456
)

# Compared posterior under different priors
phi_default <- as.matrix(fit_ar1, pars = "phi")[, 1]
phi_wide    <- as.matrix(fit_ar1_sens, pars = "phi")[, 1]

df_phi <- tibble(
  phi = c(phi_default, phi_wide),
  prior = rep(c("N(0, 0.5)", "N(0, 1)"), each = length(phi_default))
)

ggplot(df_phi, aes(x = phi, fill = prior, color = prior)) +
  geom_density(alpha = 0.4, linewidth = 1) +
  labs(
    title = "Prior Sensitivity Analysis for φ",
    x = "φ (AR coefficient)", y = "Density"
  ) +
  theme_minimal()

# -------------------------------------------------------------
# Fitted AR(2) model for comparison
# -------------------------------------------------------------

stan_data_ar2 <- list(
  T = length(y),
  y = y
)

fit_ar2 <- stan(
  file = "ar2_model.stan",
  data = stan_data_ar2,
  iter = 2000,
  warmup = 500,
  chains = 4,
  seed = 789
)

# -------------------------------------------------------------
# Modeled comparison using LOO cross-validation
# -------------------------------------------------------------

log_lik_ar1 <- extract_log_lik(fit_ar1, merge_chains = FALSE)
log_lik_ar2 <- extract_log_lik(fit_ar2, merge_chains = FALSE)

loo_ar1 <- loo(log_lik_ar1)
loo_ar2 <- loo(log_lik_ar2)

# Compared model predictive performance
loo_compare(loo_ar1, loo_ar2)






