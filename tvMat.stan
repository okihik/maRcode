data {
  int<lower=1>  N;
  int<lower=1> nX;
  matrix[N,nX]  X;
  vector[N]     y;
}

parameters {
  real B[nX,N];
  vector<lower=0> [nX] s_B;
  real<lower=0> s_y;
}

transformed parameters{
  vector[N] mu;
  
  for (i in 1:N){
    for (j in 1:nX){
      mu[i] += X[i,j]*B[j,i];
    }
  }
}

model {
  for (i in 2:N){
    for (j in 1:nX) {
      B[j,i] ~ normal(B[j,i-1],s_B[j]);
    }
  }
  for (i in 1:N){
    y[i] ~ normal(mu[i],s_y);
  }
}

