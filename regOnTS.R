# One simple sample True and Spurious Regression --------------------------------
# Checking p-values of regression coefficients by simulation
#--------------------------------------------------------------------------------

# correct test ------------------------------------------------------------------
# set rand 
set.seed(1)
m = 0  # mean
v = 10 # variance
Nsample <- 400 # sample size

x_sim <- rnorm(Nsample,mean = m,sd = sqrt(v))
y_sim <- rnorm(Nsample,mean = m,sd = sqrt(v))

simData <- data.frame(x = x_sim,y=y_sim)
autoplot(ts(simData[,c(1,2)]))
mod <- lm(y ~ x, data = simData)
summary(mod)

# Purious Regression ------------------------------------------------------------
x_sim_rw <- cumsum(rnorm(Nsample))
y_sim_rw <- cumsum(rnorm(Nsample))



simData_rw <- data.frame(x_rw = x_sim_rw,y_rw=y_sim_rw)
ggplot(simData_rw, aes(x=x_rw)) + geom_histogram()
ggplot(simData_rw, aes(x=y_rw)) + geom_histogram()
autoplot(ts(simData_rw[,c(1,2)]))
mod <- lm(y_rw ~ x_rw, data = simData_rw)
summary(mod)
plot(mod)


# correct p-value ---------------------------------------------------------------
# Checking p-values of regression coefficients by simulation
# correct test
#--------------------------------------------------------------------------------

# Number of simulation runs
Nsim <- 200

# Set vectors
# x_sim <- y_sim <- matrix(, nrow = Nsample, ncol = Nsim)
pValues <- numeric(Nsim)

# Simulation data without autocorrelation
for(i in 1:Nsim){
  y <- rnorm(Nsample, sd = sqrt(v))
  x <- rnorm(Nsample, sd = sqrt(v))
  
  # Run a linear regression analysis
  mod <- lm(y ~ x)
  
  # Save x and y, p-value
  #x_sim[,i] <- x
  #y_sim[,i] <- y
  pValues[i] <- summary(mod)$coefficients[2,4]
}

# unit root (random walk) process -----------------------------------------------
# Simulation to check for apparent regression
# For unit root process
#--------------------------------------------------------------------------------
# Set vectors for random walk
# x_sim_rw <- y_sim_rw <- matrix(, nrow = Nsample, ncol = Nsim)
pValuesRW <- numeric(Nsim)

# Random walk simulation data
for(i in 1:Nsim){
  y_rw <- cumsum(rnorm(n=Nsample))
  x_rw <- cumsum(rnorm(n=Nsample))
  
  # Run a linear regression analysis
  mod_rw <- lm(y_rw ~ x_rw)
  
  # Save p-value
  # x_sim_rw[,i] <- x_rw
  # y_sim_rw[,i] <- y_rw
  pValuesRW[i] <- summary(mod_rw)$coefficients[2,4]
}

# Combine data 
# simData <- data.frame(sim = c(x_sim, y_sim),
#                       sim_rw = c(x_sim_rw,y_sim_rw),
#                       simPattern = rep(c("normal", "RW"),each = Nsim))
simPresult <- data.frame(pValues = c(pValues, pValuesRW),
                        simPattern = rep(c("Correct Regression", 
                                           "Spurious Regression"), 
                                         each = Nsim))

# Histograms
histPlot <- 
  ggplot(simPresult, aes(x = pValues, fill = simPattern)) + 
  geom_histogram(alpha = 0.5, position = "identity", binwidth  = 0.1)
plot(histPlot)


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


