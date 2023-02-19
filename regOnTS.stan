data{
  int T;       // Number of observations
  vector[T] x; // explanatory variable
  vector[T] y; // response variable
}

parameters{
  vector[T] a;       // intercepts
  vector[T] b;       // coefficient
  real<lower=0> s_a; // s.d. for intercept
  real<lower=0> s_b; // s.d. for coefficient
  real<lower=0> s_mu;// s.d. for estimated state
}

transformed parameters {
  vector[T] mu;   // estimations of state each observed time
  
  for(i in 1:T) { // mu = a + b * x for each time
    mu[i] = a[i] + b[i] * x[i];
  }
}

model {
  for(i in 2:T) {  // State transitioned as per the state equation
    a[i] ~ normal(a[i-1], s_a); // a_i = a_{i-1} + N(0,s_a)
    b[i] ~ normal(b[i-1], s_b); // b_i = b_{i-1} + N(0,s_b)
  }
  // Observations are obtained per the given observation equation
  for(i in 1:T){
    y[i] ~ normal(mu[i], s_mu);
  }
}