data {
  int T;        // Observation Length
  vector[T] y;  // Observations
}

parameters {
  vector[T] mu;       // Level Estimation
  vector[T] gamma;    // Seasonal Estimation
  real<lower=0> s_z;  // SD for Level Variation
  real<lower=0> s_v;  // SD for Observations
  real<lower=0> s_s;  // SD for Seasonality
}

transformed parameters {
  vector[T] alpha;        // State Estimation of all sum: level + seasonal
  
  for(i in 1:T) {
    alpha[i] = mu[i] + gamma[i];
  }
}

model {
  // Level Factor
  for(i in 3:T) {
    mu[i] ~ normal(2 * mu[i-1] - mu[i-2], s_z); // Prior dist
  }
  
  // Seasonal Factor
  for(i in 7:T){
    gamma[i] ~ normal(-sum(gamma[(i-6):(i-1)]), s_s);
  }
  
  // Return Obs values from Obs. equation
  for(i in 1:T) {
    y[i] ~ normal(alpha[i], s_v);
  }
}