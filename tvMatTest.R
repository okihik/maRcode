library(rstan)
library(brms)
library(bayesplot)
library(ggfortify)
library(gridExtra)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
source("plotSSM_mat.R", encoding="utf-8")


sales_df_2 <- read.csv("5-4-1-sales-ts-2.csv")
sales_df_2$date <- as.POSIXct(sales_df_2$date)
head(sales_df_2, n = 3)


data_list <- list(
  N  = nrow(sales_df_2),
  nX = 2,
  X  = matrix(c(rep(1,nrow(sales_df_2)),
              sales_df_2$publicity), 
              nrow = nrow(sales_df_2),
              ncol = 2),
  y  = sales_df_2$sales
)

tvMat_stan <- stan(
  file = "tvMat.stan",
  data = data_list,
  seed = 1,
  iter = 8000,
  warmup = 2000,
  thin = 6
)

print(tvMat_stan, 
      par = c("s_y", "s_B","B[100,1]","B[100,2]"),
      probs = c(0.025, 0.5, 0.975))
plot(tvMat_stan)

# Check Convergence of MCMC
mcmc_rhat(rhat(tvMat_stan))
check_hmc_diagnostics(tvMat_stan)

# Check Trace
mcmc_sample <- rstan::extract(tvMat_stan, permuted = FALSE)
mcmc_acf_bar(mcmc_sample, pars = c("s_y", "s_B[1]","s_B[2]","B[100,1]", "lp__"))
mcmc_trace(mcmc_sample, pars = c("s_y", "s_B[1]","s_B[2]","B[100,1]", "lp__"))

options(max.print=100000)
print(tvMat_stan, probs = c(0.025, 0.5, 0.975))

mcmc_sample1 <- rstan::extract(tvMat_stan)

p_all <- plotSSM(mcmc_sample = mcmc_sample1, 
                 time_vec = sales_df_2$date, 
                 obs_vec  = sales_df_2$sales,
                 state_name <- mcmc_sample1$mu, 
                 graph_title = "Estimated Result: State", 
                 y_label = "sales") 
p_all

p_intercept <- plotSSM(mcmc_sample = mcmc_sample1, 
                time_vec = sales_df_2$date, 
                obs_vec  = sales_df_2$sales,
                state_name <- mcmc_sample1$B[,,1], 
                graph_title = "Estimated Result: Effect of No Publicity", 
                y_label = "sales") 
p_intercept

mcmcBeta1 <- mcmc_sample1$B[,,2]
p_beta1 <- plotSSM(mcmc_sample = mcmc_sample1, 
               time_vec   = sales_df_2$date,
               state_name = mcmcBeta1, 
               graph_title = "Estimated Result:Effect of Publicity", 
               y_label = "coef") 
p_beta1

grid.arrange(p_all, p_intercept, p_beta1)

# for loop test --------------
# N  = nrow(sales_df_2)
# nX = 2
# X  = matrix(c(rep(1,nrow(sales_df_2)),
#               sales_df_2$publicity), 
#             nrow = nrow(sales_df_2),
#             ncol = 2)
# y  = sales_df_2$sales
# beta = solve(t(X)%*%X)%*%t(X)%*%y
# 
# mu <- numeric(N)
# B  <- matrix(0,nrow = nX,ncol = N)
# 
# for (i in 1:N){
#   for (j in 1:nX){
#     mu[i] = mu[i] + X[i,j]*B[j,i];
#   }
# }

