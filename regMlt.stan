// The input data is a vector 'y' of length 'T'.
data {
  int T;             # length of data
  vector[T] ex_temp; # explanatory variable: Temperature
  vector[T] ex_humi; # explanatory variable: Humidity
  vector[T] y;       # observations
}

parameters {
  vector[T] alpha;       # estimation of level
  vector[T] beta_temp;   # estimation of time-vary coefficient for temperature
  vector[T] beta_humi;   # estimation of time-vary coefficient for humidity
  real<lower=0> s_alpha;      # standard deviation (sd) of level 
  real<lower=0> s_beta_temp;  # sd of time-vary coefficient for temperature
  real<lower=0> s_beta_humi;  # sd of time-vary coefficient for humidity
  real<lower=0> s_obs;        # sd of observations
}

transformed parameters {
  vector[T] mu;
  
  for(i in 1:T) {
    mu[i] = alpha[i] + beta_temp[i] * ex_temp[i] + beta_humi[i] * ex_humi[i];
  }
}

model {
  for(i in 2:T) {
    alpha[i]     ~ normal(alpha[i-1], s_alpha);
    beta_temp[i] ~ normal( beta_temp[i-1], s_beta_temp);
    beta_humi[i] ~ normal( beta_humi[i-1], s_beta_humi);
  }
  
  for(i in 1:T) {
    y[i] ~ normal(mu[i], s_obs);
  }
}
