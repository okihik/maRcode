## One simple sample True and Spurious Regression --------------------------------
# Checking p-values of regression coefficients by simulation
##--------------------------------------------------------------------------------
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(rstan)

## No relationship --------------------------------------------------------------
# set rand 
set.seed(1)
m = 0  # mean
v = 10 # variance
Nsample <- 400 # sample size

# simulate two sets of data
x_sim   <- rnorm(Nsample, mean = m, sd = sqrt(v))
y_sim   <- rnorm(Nsample, mean = m, sd = sqrt(v))
simData <- data.frame(x = as.numeric(x_sim),
                      y = as.numeric(y_sim))

# visualization
autoplot(ts(simData[,c(1,2)])) # Time series
#ggplot(simData, aes(x=x,y=y)) + geom_point() # 2D scatter plot
scatter <-ggscatter(simData,x="x",y="y", 
                    color = "black", shape = 16, size = 2,
                    add = "reg.line",
                    add.params = list(color = "blue", fill = "lightgray"),
                    conf.int = TRUE,cor.coef = TRUE) +
  ggtitle("No association")

# Linear Regression Analysis
mod <- lm(y ~., data = simData) # fitted linear model 
summary(mod)

## Spurious Regression ----------------------------------------------------------
# generate random-walk data
# Data that adds a random value at the current point
# to the value at the previous point in time.
x_sim_rw   <- cumsum(rnorm(Nsample)) 
y_sim_rw   <- cumsum(rnorm(Nsample))
simData_rw <- data.frame(x_rw = x_sim_rw,
                         y_rw = y_sim_rw)

# ggplot(simData_rw, aes(x=x_rw)) + geom_histogram()
# ggplot(simData_rw, aes(x=y_rw)) + geom_histogram()
autoplot(ts(simData_rw[,c(1:2)])) # Time series
ggplot(simData_rw, aes(x=x_rw,y=y_rw)) + geom_point() # 2D scatter plot
sc_rw <- ggscatter(simData_rw,x="x_rw",y="y_rw", 
                   color = "black", shape = 16, size = 2,
                   add = "reg.line",
                   add.params = list(color = "blue", fill = "lightgray"),
                   conf.int = TRUE,cor.coef = TRUE) +
  ggtitle("No association?")

mod <- lm(y_rw ~ x_rw, data = simData_rw)
summary(mod)
# 
# plot(mod)

grid.arrange(scatter,sc_rw,ncol=2)

# ## Spurious? Regression -------------------------------------------------------
# # generate random-walk data
# # Data that adds a random value at the current point
# # to the value at the previous point in time.
# x_sim_rw   <- cumsum(rnorm(Nsample)) 
# y_mod_rw   <- 3 + 0.5 * x_sim_rw + rnorm(Nsample, mean = m, sd=v)
# simData_mod_rw <- data.frame(x_rw = x_sim_rw,
#                          y_rw = y_mod_rw)
# 
# #ggplot(simData_rw, aes(x=x_rw)) + geom_histogram()
# #ggplot(simData_rw, aes(x=y_rw)) + geom_histogram()
# autoplot(ts(simData_mod_rw[,c(1,2)])) # Time series ggplot2
# ggplot(simData_rw, aes(x=x_rw,y=y_rw)) + geom_point() # 2D scatter plot
# 
# mod <- lm(y_rw ~ x_rw, data = simData_rw)
# summary(mod)

# 100 simulations (correct p-value) ---------------------------------------------
# Checking p-values of regression coefficients by simulation
# correct test
#--------------------------------------------------------------------------------
Nsim <- 100 # Number of simulation runs
pValues     <- numeric(Nsim) # Set vectors
pValuesRW   <- numeric(Nsim) # Set vectors for random walk
pValueARIMA <- numeric(Nsim) # Set vectors for ARIMA sim

# Nsim times simulation data 
for(i in 1:Nsim){ 
  # No random walk process
  y <- rnorm(Nsample, sd = sqrt(v))
  x <- rnorm(Nsample, sd = sqrt(v))
  
  # Random walk simulation data
  y_rw <- cumsum(rnorm(n=Nsample))
  x_rw <- cumsum(rnorm(n=Nsample))
  
  # ARIMA simulation data
  x_arima <- arima.sim(list(order=c(2,1,1), # ARIMA(2,1,1)
                            ar=c(0.2,-0.1),
                            ma=-0.1), n=Nsample) 
  y_arima <- arima.sim(list(order=c(0,1,1), # ARIMA(0,1,1)
                            ma=0.2), n=Nsample) 
  
  
  mod       <- lm(y ~ x) # linear regression analysis
  mod_rw    <- lm(y_rw ~ x_rw) # linear regression analysis for rw
  mod_arima <- lm(y_arima ~ x_arima) # linear regression analysis for arima
  
  # Save p-value
  pValues[i]     <- summary(mod)$coefficients[2,4]
  pValuesRW[i]   <- summary(mod_rw)$coefficients[2,4]
  pValueARIMA[i] <- summary(mod_arima)$coefficients[2,4]
}

# Combine data 
simPresult <- data.frame(pValues = c(pValues, pValuesRW, pValueARIMA),
                         simPattern = rep(c("Regression", 
                                            "Regression of RW",
                                            "Regression of ARIMA"), 
                                          each = Nsim))

# Histograms
histPlot <- 
  ggplot(simPresult, aes(x = pValues, fill = simPattern)) + 
  geom_histogram(alpha = 0.5, position = "identity", binwidth  = 0.1)+
  ggtitle("100 simulations and p-value")
plot(histPlot)




# Bayesian Modelling on RW  -----------------------------------------------------
date <- seq(as.POSIXct("2022-1-1"), 
            length.out = Nsample, by = "day")
date <- as.POSIXct(date)
simData_rw <- cbind(date, simData_rw)
head(simData_rw, n = 3)

# Create data list; mandatory for stan process
data_list <- list(
  T  = nrow(simData_rw),
  x  = simData_rw$x_rw,
  y  = simData_rw$y_rw
)

# stan process: takes long time
regOnTS_stan <- stan(
  file = "regOnTS.stan",
  data = data_list,
  seed = 1,
  iter = 2000,
  warmup = 1000,
  thin = 6
)

print(regOnTS_stan, 
      par = c("s_mu", "s_a","s_b","b[400]"),
      probs = c(0.025, 0.5, 0.975))

mcmc_sample <- rstan::extract(regOnTS_stan)
source("plotSSM.R")
p_all <- plotSSM(mcmc_sample = mcmc_sample, 
                 time_vec = simData_rw$date, 
                 obs_vec  = simData_rw$y_rw,
                 state_name = "mu", 
                 graph_title = "Estimate y", 
                 y_label = "Y") 

p_mu <- plotSSM(mcmc_sample = mcmc_sample, 
                time_vec = simData_rw$date, 
                obs_vec  = simData_rw$y_rw,
                state_name = "a", 
                graph_title = "Estimate Trend Intercept a", 
                y_label = "a") 

p_b <- plotSSM(mcmc_sample = mcmc_sample, 
               time_vec = simData_rw$date,
               state_name = "b", 
               graph_title = "Estimate Trend Coefficient", 
               y_label = "coef") 

grid.arrange(p_all, p_mu, p_b)
autoplot(ts(simData_rw[,c(2,3)]))

# ---------------------------------------
mcmc_rhat(rhat(regOnTS_stan))
check_hmc_diagnostics(regOnTS_stan)

# mcmc combo not work
# mcmc_combo(extract(regOnTS_stan, permuted=FALSE), pars = c("s_mu"))

# mcmc_acf_bar(mcmc_sample, 
#              pars = c("s_mu", "s_a", "s_b"))
# mcmc_trace(mcmc_sample, 
#            pars = c("s_alpha", "s_beta_temp", "s_beta_humi", "s_obs", "lp__"))

# Bayesian Modelling on ARIMA ---------------------------------------------------
date <- seq(as.POSIXct("2022-1-1"), 
            length.out = Nsample, by = "day")
date <- as.POSIXct(date)
x_arima <- arima.sim(list(order=c(2,1,1), # ARIMA(2,1,1)
                          ar=c(0.2,-0.1),
                          ma=-0.1), n=Nsample) 
y_arima <- arima.sim(list(order=c(0,1,1), # ARIMA(0,1,1)
                          ma=0.2), n=Nsample) 
simData_arima <- cbind(date, 
                       as.data.frame(x_arima[1:Nsample]),
                       as.data.frame(y_arima[1:Nsample]))
head(simData_arima, n = 3)

# Create data list; mandatory for stan process
data_list <- list(
  T  = nrow(simData_arima),
  x  = simData_arima$x_arima,
  y  = simData_arima$y_arima
)

# stan process: takes long time
regOnTS_stan <- stan(
  file = "regOnTS.stan",
  data = data_list,
  seed = 1,
  iter = 2000,
  warmup = 1000,
  thin = 6
)

print(regOnTS_stan, 
      par = c("s_mu", "s_a","s_b","b[400]"),
      probs = c(0.025, 0.5, 0.975))

mcmc_sample <- rstan::extract(regOnTS_stan)
source("plotSSM.R")
p_all <- plotSSM(mcmc_sample = mcmc_sample, 
                 time_vec = simData_arima$date, 
                 obs_vec  = simData_arima$y_arima[1:Nsample],
                 state_name = "mu", 
                 graph_title = "Estimate y", 
                 y_label = "Y") 

p_mu <- plotSSM(mcmc_sample = mcmc_sample, 
                time_vec = simData_arima$date, 
                obs_vec  = simData_arima$y_arima[1:Nsample],
                state_name = "a", 
                graph_title = "Estimate Trend Intercept a", 
                y_label = "a") 

p_b <- plotSSM(mcmc_sample = mcmc_sample, 
               time_vec = simData_arima$date,
               state_name = "b", 
               graph_title = "Estimate Trend Coefficient", 
               y_label = "coef") 

grid.arrange(p_all, p_mu, p_b)
autoplot(ts(simData_rw[,c(2,3)]))






# Diff Regression Analysis ------------------------------------------------------
# Take a difference time series data 
# 
# -------------------------------------------------------------------------------

diff_y <- diff(y_rw)
diff_x <- diff(x_rw)

diff_data <- data.frame(diff_y = diff_y,
                        diff_x = diff_x)
diff_rw <- lm(diff_y ~ diff_x, data = diff_data)
summary(diff_rw)
# Looks good

# Unit Root Test ----------------------------------------------------------------
# Augmented Dickey–Fuller Test
# Null Hypo: Unit Root Process
# Alt. Hypo: Not Unit Root Process
#--------------------------------------------------------------------------------
# install.packages("tseries")
library(tseries)

adf.test(y)
adf.test(x)
adf.test(y_rw)
adf.test(x_rw)


# Regression analysis on difference series --------------------------------------
# Get rid of Non-stationarity 
# -------------------------------------------------------------------------------
diff_x_sim_rw <- diff(x_sim_rw)
diff_y_sim_rw <- diff(y_sim_rw)

simData_rw_diff <- data.frame(diff_x_rw = diff_x_sim_rw,
                              diff_y_rw = diff_y_sim_rw)
autoplot(ts(simData_rw_diff[,c(1,2)]))
mod <- lm(diff_y_rw ~ diff_x_rw, data = simData_rw_diff)
summary(mod)

# N times simulations
pValuesDiff <- numeric(Nsim)

for(i in 1:Nsim){
  y <- cumsum(rnorm(n=Nsample))
  x <- cumsum(rnorm(n=Nsample))
  
  # Run Regression Analysis
  mod <- lm(y ~ x)
  
  # take the difference series
  yDiff <- diff(y)
  xDiff <- diff(x)
  
  # Run Regression series on Diff series
  modDiff <- lm(yDiff ~ xDiff)
  pValuesDiff[i] <- summary(modDiff)$coefficients[2,4]
}

# Merge Data
simResult <- data.frame(
  pValues = c(pValues, pValuesRW, pValuesDiff),
  simPattern = rep(c("Correct", "Spurious", "Diff"), each = Nsim)
)

# Histograms 
histPlot <- 
  ggplot(simResult, aes(x = pValues, fill = simPattern)) + 
  geom_histogram(alpha = 0.5,position = "identity",binwidth  = 0.1)
plot(histPlot)



# cointegration -----------------------------------------------------------------
# cointegration test：Engle-Granger method
#--------------------------------------------------------------------------------

# install.packages("urca")
library(urca)

# Matrix Data format
dataMat <- matrix(nrow = Nsample, ncol = 2)
dataMat[,1] <- y_rw
dataMat[,2] <- x_rw

# Phillips-Ouliaris Cointegration Test
summary(ca.po(dataMat, demean = "none"))

# Generate cointegrated data and run a cointegration test ---------------
library("reshape2")
rw <- cumsum(rnorm(n=Nsample))
x2 <- 0.6 * rw + rnorm(n=Nsample)
y2 <- 0.4 * rw + rnorm(n=Nsample)

# Unit Root test
adf.test(y2)
adf.test(x2)

# Visualize the generated data
df <- data.frame(id = 1:Nsample,
                 y2 = y2,
                 x2 = x2,
                 sum = x2 - (0.6/0.4)*y2)
df <- melt(df, id = "id")


linePlot <- 
  ggplot(df,aes(x = id, y = value, group = variable, colour = variable)) + 
  geom_line()
plot(linePlot)

# The graph still looks like a random walk
# However, if you multiply by a certain coefficient and then sum, it does not look like a random walk

# cointegration test
dataMat2 <- matrix(nrow = Nsample, ncol = 2)
dataMat2[,1] <- y2
dataMat2[,2] <- x2
summary(ca.po(dataMat2, demean = "none"))


# Differences and then regressions on cointegrated data can hide the relationship.
y2Diff <- diff(y2)
x2Diff <- diff(x2)

modLmDiffDame <- lm(y2Diff ~ x2Diff)
summary(modLmDiffDame)


#--------------------------------------------------------------------------------
# Check spurious regression from simulated data
# In case of stationary
#--------------------------------------------------------------------------------

Nsim <- 200
Nsample <- 400

pValuesAutoCol <- numeric(Nsim)
set.seed(1)

AIClm <- numeric(Nsim)
AICnull <- numeric(Nsim)

for(i in 1:Nsim){
  # Autocorrelated simulation data
  y <- arima.sim(n = Nsample,sd = sqrt(10),
                 model = list(order = c(1,0,0), ar = c(0.8)))
  
  x <- arima.sim(n = Nsample,sd = sqrt(10),
                 model = list(order = c(1,0,0), ar = c(0.8)))
  
  # Run regression model
  mod <- lm(y ~ x)
  
  # save p value
  pValuesAutoCol[i] <- summary(mod)$coefficients[2,4]
  
  # AIC
  AIClm[i]   <- AIC(mod)
  AICnull[i] <- AIC(lm(y ~ 1))
}


# Combine data
simResult <- data.frame(pValues = c(pValues, pValuesAutoCol),
                        simPattern = rep(c("Correct", "AutoCorr"), 
                                         each = Nsim))

# Histogram
histPlot <- 
  ggplot(simResult, aes(x = pValues,fill = simPattern)) + 
  geom_histogram(alpha = 0.5,position = "identity", binwidth  = 0.1)
plot(histPlot)


# Compare AIC
sum(AIClm < AICnull) / Nsim
# Model selection by AIC is not good.

# Durbin-Watson test ------------------------------------------------------------
# Test of autocorrelation of residuals
#--------------------------------------------------------------------------------

set.seed(2)

# 自己相関のあるシミュレーションデータ
y <- arima.sim(n = Nsample,sd = sqrt(10),
               model = list(order = c(1,0,0), ar = c(0.8)))

x <- arima.sim(n = Nsample,sd = sqrt(10),
               model = list(order = c(1,0,0), ar = c(0.8)))

autoCorData <- data.frame(y = y, x = x)

# Usual regression analysis
lmModel <- lm(y ~ x, data = autoCorData)

# Durbin-Watson test for autocorrelated serial

# install.packages("lmtest")
library(lmtest)

dwtest(lmModel)


# Perform Generalized Least Squares ---------------------------------------------
# 
#--------------------------------------------------------------------------------

# install.packages("nlme")
library(nlme)

glsModel_0 <- gls(y ~ x, correlation =  NULL         , data = autoCorData)
glsModel_1 <- gls(y ~ x, correlation = corARMA(p = 1), data = autoCorData)
glsModel_2 <- gls(y ~ x, correlation = corARMA(p = 2), data = autoCorData)

AIC(glsModel_0,glsModel_1,glsModel_2)
# AR(1) has the smallest AIC.

# likelihood ratio test can also be performed. 
anova(glsModel_0, glsModel_1)
anova(glsModel_1, glsModel_2)
anova(glsModel_0, glsModel_2)
# Here, too, AR(1) was chosen

# Estimation Results
summary(glsModel_1)



# Generalized Least Squares Simulation ------------------------------------------
# 
#--------------------------------------------------------------------------------

pValuesGls <- numeric(Nsim)
set.seed(1)

for(i in 1:Nsim){
  # Autocorrelated simulation data
  y <- arima.sim(n = Nsample,
                 model = list(order = c(1,0,0), ar = c(0.8)),
                 sd = sqrt(10))
  
  x <- arima.sim(n = Nsample,
                 model = list(order = c(1,0,0), ar = c(0.8)),
                 sd = sqrt(10))
  
  # Run Regression analysis
  mod <- gls(y ~ x, correlation = corARMA(p = 1))
  
  # save p value
  pValuesGls[i] <- summary(mod)$tTable[2,4]
}


# Merge data
simResult <- data.frame(pValues = c(pValues, pValuesAutoCol, pValuesGls),
                        simPattern = rep(c("Correct", "Autocorr", "GLS"), 
                                         each = Nsim))

# Histogram
histPlot <- 
  ggplot(simResult, aes(x = pValues,fill = simPattern)) + 
  geom_histogram(alpha = 0.5,position = "identity",binwidth  = 0.1)
plot(histPlot)


