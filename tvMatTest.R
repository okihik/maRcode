library(rstan)
library(brms)
library(bayesplot)
library(ggfortify)
library(gridExtra)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
source("plotSSM.R", encoding="utf-8")


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
      par = c("s_y", "B[2,100]"),
      probs = c(0.025, 0.5, 0.975))
