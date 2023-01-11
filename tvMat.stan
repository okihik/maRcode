data {
  int<lower=1>  N; // Number of observations
  int<lower=1> nX; // Number of Explanatory Variables including intercept
  matrix[N,nX]  X; // Design matrix including ones in first column
  vector[N]     y; // Response variable
}

parameters {
  vector[nX] B[N];          // N x nX coefficient vectors
  vector<lower=0> [nX] s_B; // s.d. for coefficients
  real  <lower=0>      s_y; // s.d. for observation
}

transformed parameters {
  vector[N] mu; // estimations of state each observed time
  
  for (i in 1:N)
    mu[i] = X[i]*B[i]; // dot-product of X and coefficients
}

model {
  // State transitioned as per the state equation
  for (i in 2:N)
    B[i] ~ normal(B[i-1], s_B);

  // Observations are obtained per the given observation equation
  for (i in 1:N)
    y[i] ~ normal(mu[i], s_y);
}

