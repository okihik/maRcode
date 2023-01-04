set.seed(123)
n = 100
intercept = 5
temp = runif(n)
nitro = runif(n) + 0.8 * temp
int.eff = 2
temp.eff <- 0.85
nitro.eff <- 0.5
res = rnorm(n, 0, 1)
coef <- c(int.eff, temp.eff, nitro.eff, int.eff)
mm <- model.matrix(~temp * nitro)

y <- t(coef %*% t(mm)) + res
data <- data.frame(y, 
                   x1 = temp, 
                   x2 = nitro, 
                   cx1 = scale(temp,scale = F), 
                   cx2 = scale(nitro, scale = F)
                   )
  head(data)

# Centering matrix
data <- within(data, {
  cx1 <- as.numeric(scale(x1, scale = FALSE))
  cx2 <- as.numeric(scale(x2, scale = FALSE))
  })
head(data)

mean.x1 = mean(data$x1)
mean.x2 = mean(data$x2)


# Stan for Additive Model
modelString = "
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

"
## write the model to a stan file 
writeLines(modelString, con = "linregModeladd.stan")
writeLines(modelString, con = "linregModelmult.stan")

# Data list
X = model.matrix(~cx1 + cx2, data = data)
data.list <- with(data, list(Y = y, X = X, nX = ncol(X), n = nrow(data)))

params <- c("beta","beta0", "cbeta0", "sigma", "log_lik")
nChains = 2
burnInSteps = 1000
thinSteps = 1
numSavedSteps = 3000  #across all chains
nIter = ceiling(burnInSteps + (numSavedSteps * thinSteps)/nChains)
nIter

# Library
library(rstan)

data.rstan.add <- stan(data = data.list, 
                       file = "linregModeladd.stan", 
                       chains = nChains, 
                       pars = params,
                       iter = nIter, 
                       warmup = burnInSteps, 
                       thin = thinSteps, 
                       save_dso = TRUE)

data.rstan.add

# Multiplicative model
X = model.matrix(~cx1 * cx2, data = data)
data.list <- with(data, list(Y = y, X = X, nX = ncol(X), n = nrow(data)))

data.rstan.mult <- stan(data = data.list, 
                        file = "linregModelmult.stan", 
                        chains = nChains, 
                        pars = params,
                        iter = nIter, 
                        warmup = burnInSteps, 
                        thin = thinSteps, 
                        save_dso = TRUE)
data.rstan.mult

# Diagnostic
library(mcmcplots)
s = as.array(data.rstan.mult)
mcmc <- do.call(mcmc.list, plyr:::alply(s[, , -(length(s[1, 1, ]))], 2, as.mcmc))
denplot(mcmc, parms = c("beta0","beta","cbeta0","sigma"))

traplot(mcmc, parms = c("beta0","beta","cbeta0","sigma"))


#Raftery diagnostic
raftery.diag(mcmc)

#Autocorrelation diagnostic
stan_ac(data.rstan.mult, pars = c("beta","beta0"))

stan_ac(data.rstan.mult, pars = c("beta","beta0"))

stan_ess(data.rstan.mult, pars = c("beta","beta0"))


# Model validation
# Residuals
library(ggplot2)
library(dplyr)
mcmc = as.data.frame(data.rstan.mult) %>% 
  dplyr:::select(beta0, starts_with("beta"),sigma) %>% 
  as.matrix

# generate a model matrix
newdata = data
Xmat = model.matrix(~cx1 * cx2, newdata)
## get median parameter estimates
coefs = apply(mcmc[, 1:4], 2, median)
fit = as.vector(coefs %*% t(Xmat))
resid = data$y - fit
ggplot() + 
  geom_point(data = NULL, aes(y = resid, x = fit))

# Residuals against predictors
library(tidyr)
mcmc = as.data.frame(data.rstan.mult) %>% 
  dplyr:::select(beta0, starts_with("beta"),sigma) %>% 
  as.matrix
# generate a model matrix
newdata = newdata
Xmat = model.matrix(~cx1 * cx2, newdata)
## get median parameter estimates
coefs = apply(mcmc[, 1:4], 2, median)
fit = as.vector(coefs %*% t(Xmat))
resid = data$y - fit
newdata = data %>% cbind(fit, resid)
newdata.melt = newdata %>% gather(key = Pred, value = Value, cx1:cx2)

ggplot(newdata.melt) + 
  geom_point(aes(y = resid, x = Value)) + 
  facet_wrap(~Pred)

# studentised residuals
mcmc = as.data.frame(data.rstan.mult) %>% 
  dplyr:::select(beta0, starts_with("beta"),sigma) %>% 
  as.matrix

# generate a model matrix
newdata = data
Xmat = model.matrix(~cx1 * cx2, newdata)

## get median parameter estimates
coefs = apply(mcmc[, 1:4], 2, median)
fit = as.vector(coefs %*% t(Xmat))
resid = data$y - fit
sresid = resid/sd(resid)

ggplot() + geom_point(data = NULL, aes(y = sresid, x = fit))

# Density of posterior distribution
mcmc = as.data.frame(data.rstan.mult) %>% 
  dplyr:::select(beta0,starts_with("beta"), sigma) %>% 
  as.matrix
# generate a model matrix
Xmat = model.matrix(~cx1 * cx2, data)
## get median parameter estimates
coefs = mcmc[, 1:4]
fit = coefs %*% t(Xmat)
## draw samples from this model
yRep = sapply(1:nrow(mcmc), 
              function(i) rnorm(nrow(data), 
                                fit[i,], 
                                mcmc[i, "sigma"]))

ggplot() + 
  geom_density(data = NULL, 
               aes(x = as.vector(yRep),fill = "Model"), alpha = 0.5) + 
  geom_density(data = data, aes(x = y, fill = "Obs"), alpha = 0.5)

# the posteriors of each parameter
library(bayesplot)
thm = list(theme(text=element_text(size=rel(5))))
bayesplot_theme_set(theme_default() + theme(text=element_text(family="Arial")))

mcmc_intervals(as.matrix(data.rstan.mult), regex_pars = "beta|sigma") + thm

mcmc_areas(as.matrix(data.rstan.mult), regex_pars = "beta|sigma")

# Parameter estimates
mcmcpvalue <- function(samp) {
## elementary version that creates an empirical p-value for the
## hypothesis that the columns of samp have mean zero versus a general
## multivariate distribution with elliptical contours.

## differences from the mean standardized by the observed
## variance-covariance factor

## Note, I put in the bit for single terms
  if (length(dim(samp)) == 0) {
    std <- backsolve(chol(var(samp)), 
                     cbind(0, t(samp)) - mean(samp),
                     transpose = TRUE)
    sqdist <- colSums(std * std)
    sum(sqdist[-1] > sqdist[1])/length(samp)
  } 
  else {
      std <- backsolve(chol(var(samp)), 
                       cbind(0, t(samp)) - colMeans(samp),
                       transpose = TRUE)
      sqdist <- colSums(std * std)
      sum(sqdist[-1] > sqdist[1])/nrow(samp)
  }
}


print(data.rstan.add, pars = c("beta0", "beta", "sigma"))

# OR
library(broom)
library(broom.mixed)
tidyMCMC(data.rstan.add, 
         conf.int = TRUE,
         conf.method = "HPDinterval",
         pars = c("beta0", "beta", "sigma"))


# Conclusions

# When is held constant, a one unit increase in is associated with a cx2cx1 2.83 change in y. 
# That is, y increases at a rate of 2.83 per unit increase in when standardised for .cx1cx2

# When is held constant, a one unit increase in is associated with a cx1cx2 1.58 change in y. 
# That is, y increases at a rate of 1.58 per unit increase in when standardised for .cx2cx1

# Note, as this is an additive model, 
# the rates associated with are assumed to be constant throughtout the range of and vice versa. 
# The cx1cx2 95% confidence interval for each partial slope does not overlap with 0,
# implying a significant effects of and on cx1cx2 y. 
# While workers attempt to become comfortable with a new statistical framework, 
# it is only natural that they like to evaluate and 
# comprehend new structures and output alongside more familiar concepts. 
# One way to facilitate this is via Bayesian p-values that are somewhat analogous to the frequentist 
# p-values for investigating the hypothesis that a parameter is equal to zero.

























