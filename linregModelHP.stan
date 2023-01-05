
data {
int < lower =0 > n; // number of observations
int < lower =0 > nX; // number of predictors
vector [ n] Y; // outputs
matrix [n ,nX] X; // inputs
real < lower =0 > scale_icept ; // prior std for the intercept
real < lower =0 > scale_global ; // scale for the half -t prior for tau
real < lower =1 > nu_global ; // degrees of freedom for the half -t priors for tau
real < lower =1 > nu_local ; // degrees of freedom for the half - t priors for lambdas
real < lower =0 > slab_scale ; // slab scale for the regularized horseshoe
real < lower =0 > slab_df ; // slab degrees of freedom for the regularized horseshoe
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
real logsigma ;
real cbeta0 ;
vector [ nX-1] z;
real < lower =0 > tau ; // global shrinkage parameter
vector < lower =0 >[ nX-1] lambda ; // local shrinkage parameter
real < lower =0 > caux ;
}
transformed parameters {
real < lower =0 > sigma ; // noise std
vector < lower =0 >[ nX-1] lambda_tilde ; // truncated local shrinkage parameter
real < lower =0 > c; // slab scale
vector [ nX-1] beta ; // regression coefficients
vector [ n] mu; // latent function values
sigma = exp ( logsigma );
c = slab_scale * sqrt ( caux );
lambda_tilde = sqrt ( c ^2 * square ( lambda ) ./ (c ^2 + tau ^2* square ( lambda )) );
beta = z .* lambda_tilde * tau ;
mu = cbeta0 + Xc* beta ;
}
model {
// half -t priors for lambdas and tau , and inverse - gamma for c ^2
z ~ normal (0 , 1);
lambda ~ student_t ( nu_local , 0, 1);
tau ~ student_t ( nu_global , 0 , scale_global * sigma );
caux ~ inv_gamma (0.5* slab_df , 0.5* slab_df );
cbeta0 ~ normal (0 , scale_icept );
Y ~ normal (mu , sigma );
}
generated quantities { 
real beta0;  // population-level intercept 
vector[n] log_lik;
beta0 = cbeta0 - dot_product(means_X, beta);
for (i in 1:n) {
log_lik[i] = normal_lpdf(Y[i] | Xc[i] * beta + cbeta0, sigma);
}
}

