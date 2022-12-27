// The input data is a vector 'y' of length 'T'.
data {
  int T;        # length of data
  vector[T] ex; # explanatory variable
  vector[T] y;  # observations
}

parameters {
  vector[T] alpha;   # estimation of level
  vector[T] beta;    # estimation of time-vary coefficient
  real<lower=0> s_u; # standard deviation of level 
  real<lower=0> s_v; # standard deviation of time-vary coefficient
  real<lower=0> s_w; # standard deciation of observation
}

transformed parameters {
  vector[T] mu;
  
  for(i in 1:T) {
    mu[i] = alpha[i] + beta[i] * ex[i];
  }
}

model {
  for(i in 2:T) {
    alpha[i] ~ normal(alpha[i-1], s_u);
     beta[i] ~ normal( beta[i-1], s_v);
  }
  
  for(i in 1:T) {
    y[i] ~ normal(mu[i], s_w);
  }

}
