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
# the rates associated with are assumed to be constant throughout the range of and vice versa. 
# The cx1cx2 95% confidence interval for each partial slope does not overlap with 0,
# implying a significant effects of and on cx1cx2 y. 
# While workers attempt to become comfortable with a new statistical framework, 
# it is only natural that they like to evaluate and 
# comprehend new structures and output alongside more familiar concepts. 
# One way to facilitate this is via Bayesian p-values that are somewhat analogous to the frequentist 
# p-values for investigating the hypothesis that a parameter is equal to zero.


## since values are less than zero
mcmcpvalue(as.matrix(data.rstan.mult)[, "beta[1]"])

mcmcpvalue(as.matrix(data.rstan.mult)[, "beta[2]"])

mcmcpvalue(as.matrix(data.rstan.mult)[, "beta[3]"])



## since values are less than zero
library(loo)
(full = loo(extract_log_lik(data.rstan.mult)))

(reduced = loo(extract_log_lik(data.rstan.add)))

par(mfrow = 1:2, mar = c(5, 3.8, 1, 0) + 0.1, las = 3)
plot(full, label_points = TRUE)
plot(reduced, label_points = TRUE)


# Graphical summaries
mcmc = as.matrix(data.rstan.add)
## Calculate the fitted values
newdata = rbind(data.frame(cx1 = seq(min(data$cx1, na.rm = TRUE), 
                                     max(data$cx1,na.rm = TRUE), len = 100), 
                           cx2 = 0, Pred = 1), 
                data.frame(cx1 = 0,
                           cx2 = seq(min(data$cx2, na.rm = TRUE), 
                                     max(data$cx2, na.rm = TRUE), len = 100), 
                           Pred = 2))

Xmat = model.matrix(~cx1 + cx2, newdata)
coefs = mcmc[, c("beta0", "beta[1]", "beta[2]")]
fit = coefs %*% t(Xmat)
newdata = newdata %>% mutate(x1 = cx1 + mean.x1, x2 = cx2 + mean.x2) %>%
  cbind(tidyMCMC(fit, conf.int = TRUE, conf.method = "HPDinterval")) %>%
  mutate(x = dplyr:::recode(Pred, x1, x2))

ggplot(newdata, aes(y = estimate, x = x)) +
  geom_line() + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              fill = "blue", alpha = 0.3) + 
  scale_y_continuous("Y") +
  scale_x_continuous("X") + theme_classic() + facet_wrap(~Pred)

## Calculate partial residuals fitted values
fdata = rdata = rbind(data.frame(cx1 = data$cx1, cx2 = 0, Pred = 1), 
                      data.frame(cx1 = 0,cx2 = data$cx2, Pred = 2))

fMat = rMat = model.matrix(~cx1 + cx2, fdata)
fit = as.vector(apply(coefs, 2, median) %*% t(fMat))
resid = as.vector(data$y - apply(coefs, 2, median) %*% t(rMat))
rdata = rdata %>% mutate(partial.resid = resid + fit) %>%
  mutate(x1 = cx1 + mean.x1, 
         x2 = cx2 + mean.x2) %>% 
  mutate(x = dplyr:::recode(Pred, x1, x2))

  ggplot(newdata, aes(y = estimate, x = x)) + 
    geom_point(data = rdata, aes(y = partial.resid),color = "gray") + 
    geom_line() + geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                              fill = "blue", alpha = 0.3) + 
    scale_y_continuous("Y") + theme_classic() +
    facet_wrap(~Pred, strip.position = "bottom", 
                   labeller = label_bquote("x" *.(Pred))) + 
    theme(axis.title.x = element_blank(), 
          strip.background = element_blank(),
          strip.placement = "outside")


library(gridExtra)
mcmc = as.matrix(data.rstan.add)
## Calculate the fitted values
newdata = data.frame(cx1 = seq(min(data$cx1, na.rm = TRUE), 
                               max(data$cx1, na.rm = TRUE), len = 100), cx2 = 0)

Xmat = model.matrix(~cx1 + cx2, newdata)
coefs = mcmc[, c("beta0", "beta[1]", "beta[2]")]
fit = coefs %*% t(Xmat)

newdata = newdata %>% 
  mutate(x1 = cx1 + mean.x1, x2 = cx2 + mean.x2) %>%
  cbind(tidyMCMC(fit, conf.int = TRUE, conf.method = "HPDinterval"))

## Now the partial residuals
fdata = rdata = data.frame(cx1 = data$cx1, cx2 = 0)
fMat = rMat = model.matrix(~cx1 + cx2, fdata)
fit = as.vector(apply(coefs, 2, median) %*% t(fMat))
resid = as.vector(data$y - apply(coefs, 2, median) %*% t(rMat))

rdata = rdata %>% 
  mutate(partial.resid = resid + fit) %>% 
  mutate(x1 = cx1 + mean.x1, x2 = cx2 + mean.x2)

g1 <- ggplot(newdata, aes(y = estimate, x = x1)) + 
  geom_point(data = rdata, aes(y = partial.resid), color = "grey") +
  geom_line() + geom_ribbon(aes(ymin = conf.low,
                                ymax = conf.high), fill = "blue", alpha = 0.3) + 
  scale_y_continuous("Y") + 
  scale_x_continuous("X1") + 
  theme_classic()

newdata = data.frame(cx2 = seq(min(data$cx2, na.rm = TRUE), 
                               max(data$cx2, na.rm = TRUE), len = 100), cx1 = 0)

Xmat = model.matrix(~cx1 + cx2, newdata)
coefs = mcmc[, c("beta0", "beta[1]", "beta[2]")]
fit = coefs %*% t(Xmat)

newdata = newdata %>%
  mutate(x1 = cx1 + mean.x1, x2 = cx2 + mean.x2) %>%
  cbind(tidyMCMC(fit, conf.int = TRUE, conf.method = "HPDinterval"))

## Now the partial residuals
fdata = rdata = data.frame(cx1 = 0, cx2 = data$cx2)
fMat = rMat = model.matrix(~cx1 + cx2, fdata)
fit = as.vector(apply(coefs, 2, median) %*% t(fMat))
resid = as.vector(data$y - apply(coefs, 2, median) %*% t(rMat))
rdata = rdata %>% mutate(partial.resid = resid + fit) %>% 
  mutate(x1 = cx1 + mean.x1, x2 = cx2 + mean.x2)

g2 <- ggplot(newdata, aes(y = estimate, x = x2)) + 
  geom_point(data = rdata, aes(y = partial.resid), color = "grey") +
  geom_line() + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              fill = "blue", alpha = 0.3) + 
  scale_y_continuous("Y") +
  scale_x_continuous("X2") + 
  theme_classic()

grid.arrange(g1, g2, ncol = 2)


# For Multiplicative
library(fields)
mcmc = as.matrix(data.rstan.mult)
## Calculate the fitted values
newdata = expand.grid(cx1 = seq(min(data$cx1, na.rm = TRUE), 
                                max(data$cx1, na.rm = TRUE), len = 100), 
                      cx2 = mean(data$cx2) + sd(data$cx2) %*% -2:2)

Xmat = model.matrix(~cx1 * cx2, newdata)
coefs = mcmc[, c("beta0", "beta[1]", "beta[2]", "beta[3]")]
fit = coefs %*% t(Xmat)

newdata = newdata %>% 
  mutate(x1 = cx1 + mean.x1, x2 = cx2 + mean.x2) %>%
  cbind(tidyMCMC(fit, conf.int = TRUE, conf.method = "HPDinterval")) %>%
  mutate(x2 = factor(x2, labels = paste("X2:~", c(-2, -1, 0, 1, 2), "*sigma")))

## Partial residuals
fdata = rdata = expand.grid(cx1 = data$cx1, 
                            cx2 = mean(data$cx2) + sd(data$cx2) * -2:2)

fMat = rMat = model.matrix(~cx1 * cx2, fdata)
fit = as.vector(apply(coefs, 2, median) %*% t(fMat))
resid = as.vector(data$y - apply(coefs, 2, median) %*% t(rMat))
rdata = rdata %>% mutate(partial.resid = resid + fit) %>% 
  mutate(x1 = cx1 + mean.x1, x2 = cx2 + mean.x2)

## Partition the partial residuals such that each x1 trend only includes
## x2 data that is within that range in the observed data
findNearest = function(x, y) {
  ff = fields:::rdist(x, y)
  apply(ff, 1, function(x) which(x == min(x)))
  }

fn = findNearest(x = data[, c("x1", "x2")], y = rdata[, c("x1", "x2")])

rdata = rdata[fn, ] %>% 
  mutate(x2 = factor(x2, labels = paste("X2:~", c(-2, -1, 0, 1, 2), "*sigma")))

ggplot(newdata, aes(y = estimate, x = x1)) + 
  geom_line() + 
  geom_blank(aes(y = 9)) +
  geom_point(data = rdata, aes(y = partial.resid), color = "grey") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "blue",alpha = 0.3) + 
  scale_y_continuous("Y") + 
  scale_x_continuous("X1") +
  facet_wrap(~x2, labeller = label_parsed, nrow = 1, scales = "free_y") +
  theme_classic() + 
  theme(strip.background = element_blank())


# Effect sizes
mcmc = as.matrix(data.rstan.mult)
newdata = expand.grid(cx1 = c(min(data$cx1), 
                              max(data$cx1)), 
                      cx2 = (-2:2) *sd(data$cx1))
Xmat = model.matrix(~cx1 * cx2, newdata)
coefs = mcmc[, c("beta0", "beta[1]", "beta[2]", "beta[3]")]
fit = coefs %*% t(Xmat)
s1 = seq(1, 9, b = 2)
s2 = seq(2, 10, b = 2)
## Raw effect size
(RES = tidyMCMC(as.mcmc(fit[, s2] - fit[, s1]), 
                conf.int = TRUE, 
                conf.method = "HPDinterval"))

## Cohen's D
cohenD = (fit[, s2] - fit[, s1])/sqrt(mcmc[, "sigma"])
(cohenDES = tidyMCMC(as.mcmc(cohenD), 
                     conf.int = TRUE, 
                     conf.method = "HPDinterval"))

# Percentage change (relative to Group A)
ESp = 100 * (fit[, s2] - fit[, s1])/fit[, s1]
(PES = tidyMCMC(as.mcmc(ESp), conf.int = TRUE, conf.method = "HPDinterval"))

# Probability that the effect is greater than 50% (an increase of >50%)
(p50 = apply(ESp, 2, function(x) sum(x > 50)/length(x)))

## fractional change
(FES = tidyMCMC(as.mcmc(fit[, s2]/fit[, s1]), 
                conf.int = TRUE, conf.method = "HPDinterval"))


mcmc <- as.matrix(data.rstan.mult)
Xmat = model.matrix(~cx1 * cx2, data)
coefs = mcmc[, c("beta0", "beta[1]", "beta[2]", "beta[3]")]
fit = coefs %*% t(Xmat)
resid = sweep(fit, 2, data$y, "-")
var_f = apply(fit, 1, var)
var_e = apply(resid, 1, var)
R2 = var_f/(var_f + var_e)
tidyMCMC(as.mcmc(R2), conf.int = TRUE, conf.method = "HPDinterval")

# for comparison with frequentest
summary(lm(y ~ cx1 * cx2, data))

modelStringHP = "
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
"
## write the model to a stan file 
writeLines(modelStringHP, con = "linregModelHP.stan")

X = model.matrix(~cx1 + cx2, data = data)
data.list <- with(data, list(Y = y, X = X, nX = ncol(X), 
                             n = nrow(data),
                             scale_icept = 100, 
                             scale_global = 1, 
                             nu_global = 1, 
                             nu_local = 1, 
                             slab_scale = 2,
                             slab_df = 4))

data.rstan.sparsity <- stan(data = data.list, 
                            file = "linregModelHP.stan", 
                            pars = params,
                            chains = nChains, 
                            iter = nIter, 
                            warmup = burnInSteps, 
                            thin = thinSteps, 
                            save_dso = TRUE)

tidyMCMC(data.rstan.sparsity, 
         pars = c("beta[1]", "beta[2]"), 
         conf.int = TRUE,
         conf.type = "HPDinterval", 
         rhat = TRUE, ess = TRUE)

mcmc_areas(as.matrix(data.rstan.sparsity), regex_par = "beta")
