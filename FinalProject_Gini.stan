data {
  int<lower=0> N;                  // Number of observations
  int<lower=1> K;                  // Number of predictors excluding intercept
  int<lower=1> J_country;          // Number of countries
  int<lower=1> J_year;             // Number of years
  matrix[N, K] X;                  // Predictor matrix
  array[N] int<lower=1, upper=J_country> country;   // Country indicator
  array[N] int<lower=1, upper=J_year> year;         // Year indicator
  array[N] int<lower=0, upper=100> pct_contra; // Outcome variable
}

parameters {
  vector[K] beta;                  // Regression coefficients
  real<lower=0> sigma;             // Standard deviation of the residuals
  vector[J_country] country_offset; // Random intercepts for countries
  vector[J_year] year_offset;       // Random intercepts for years
}

model {
  // Prior distributions
  beta ~ normal(0, 5);             // Weakly informative prior for coefficients
  sigma ~ normal(0, 5);            // Weakly informative prior for residual standard deviation
  country_offset ~ normal(0, 1);   // Prior for country random effects
  year_offset ~ normal(0, 1);      // Prior for year random effects
  
  // Likelihood
  for (i in 1:N) {
    pct_contra[i] ~ normal(X[i] * beta + country_offset[country[i]] + year_offset[year[i]], sigma);
  }
}
generated quantities {
  // Compute posterior distributions or quantities of interest
  // For example, you can compute predictions or transformed parameters
  
  // Example: Compute posterior predictions for pct_contra
  vector[N] posterior_predictions;
  for (i in 1:N) {
    posterior_predictions[i] = normal_rng(X[i] * beta + country_offset[country[i]] + year_offset[year[i]], sigma);
  }
}
