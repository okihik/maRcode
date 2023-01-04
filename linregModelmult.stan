
data { 
int<lower=1> n;   // total number of observations 
vector[n] Y;      // response variable 
int<lower=1> nX;  // number of effects 
matrix[n, nX] X;   // model matrix 
} 
transformed data { 
matrix[n, nX - 1] Xc;  // centered version of X 
vector[nX - 1] means_X;  // column means of X before centering 

for (i in 2:nX) { 
means_X[i - 1] = mean(X[, i]); 
Xc[, i - 1] = X[, i] - means_X[i - 1]; 
}  
} 
parameters { 
vector[nX-1] beta;  // population-level effects 
real cbeta0;  // center-scale intercept 
real<lower=0> sigma;  // residual SD 
} 
transformed parameters { 
} 
model { 
vector[n] mu; 
mu = Xc * beta + cbeta0; 
// prior specifications 
beta ~ normal(0, 100); 
cbeta0 ~ normal(0, 100); 
sigma ~ cauchy(0, 5); 
// likelihood contribution 
Y ~ normal(mu, sigma); 
} 
generated quantities {
real beta0;  // population-level intercept 
vector[n] log_lik;
beta0 = cbeta0 - dot_product(means_X, beta);
for (i in 1:n) {
log_lik[i] = normal_lpdf(Y[i] | Xc[i] * beta + cbeta0, sigma);
} 
}


