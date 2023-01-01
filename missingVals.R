library(googlesheets4) # google sheets
library(lubridate)

# Retrieve data from Google sheet
# between starting and Nov. 30
sheet <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1K7VRu80v6qy1qhHgJqL9MrtPOsLaZbtBiMPRwuxgf9c/edit#gid=0",
  col_names = TRUE,
  col_types = "Ddddiiiididdddiiiid",
  range = "Sheet1!A1:S57"
)

# Number of observations
obs_days <- sum(!is.na(sheet$fl))

# Data Frame
df <- data.frame(
  date   = as.Date(sheet$date[1:obs_days]),
  foodLossKg = as.numeric(na.omit(sheet$fl)),
  allWasteKg = as.numeric(na.omit(sheet$slfw)),
  liquidWasteKg = as.numeric(na.omit(sheet$lfw)),
  solidWasteKg  = as.numeric(na.omit(sheet$slfw-sheet$lfw)), # solid = all - liquid in kg
  regularOrders = as.numeric(na.omit(sheet$orders)),
  miniOrders    = as.numeric(na.omit(sheet$miniOrders)),
  takeouts = as.numeric(na.omit(sheet$takeouts)),
  customer = as.numeric(na.omit(sheet$customers)),
  liquors  = as.numeric(na.omit(sheet$liquors)),
  sales    = as.numeric(na.omit(sheet$sales)),
  tempC    = as.numeric(na.omit(sheet$hrTM)),
  humidityPercent = as.numeric(na.omit(sheet$hrHM)),
  precipMM   = as.numeric(na.omit(sheet$precip))
)

df$date

week_day <- wday(df$date, label = TRUE) # 5 is starting at Friday
week_day <- factor(week_day, 
                   levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), 
                   ordered = TRUE)

dt2<-cbind(df,week_day)

GM_fl <- mean(dt2$foodLossKg)
GM_all <- mean(dt2$allWasteKg)
GM_l <- mean(dt2$liquidWasteKg)
GM_s <- mean(dt2$solidWasteKg)
GM_reg <- mean(dt2$regularOrders)
GM_mini <- mean(dt2$miniOrders)
GM_take <- mean(dt2$takeouts)
GM_cus <- mean(dt2$customer)
GM_liq <- mean(dt2$liquors)

wk_fl <- tapply(dt2$foodLossKg,dt2$week_day,mean)
wk_all <- tapply(dt2$allWasteKg,dt2$week_day,mean)
wk_l <- tapply(dt2$liquidWasteKg,dt2$week_day,mean)
wk_s <- tapply(dt2$solidWasteKg,dt2$week_day,mean)
wk_reg <- tapply(dt2$regularOrders,dt2$week_day,mean)
wk_mini <- tapply(dt2$miniOrders,dt2$week_day,mean)
wk_take <- tapply(dt2$takeouts,dt2$week_day,mean)
wk_cus <- tapply(dt2$customer,dt2$week_day,mean)
wk_liq <- tapply(dt2$liquors,dt2$week_day,mean)


wk_fl_sig <- tapply(dt2$foodLossKg,dt2$week_day,sd)
wk_all_sig <- tapply(dt2$allWasteKg,dt2$week_day,sd)
wk_l_sig <- tapply(dt2$liquidWasteKg,dt2$week_day,sd)
wk_s_sig <- tapply(dt2$solidWasteKg,dt2$week_day,sd)
wk_reg_sig <- tapply(dt2$regularOrders,dt2$week_day,sd)
wk_mini_sig <- tapply(dt2$miniOrders,dt2$week_day,sd)
wk_take_sig <- tapply(dt2$takeouts,dt2$week_day,sd)
wk_cus_sig <- tapply(dt2$customer,dt2$week_day,sd)
wk_liq_sig <- tapply(dt2$liquors,dt2$week_day,sd)

set.seed(1234)
est_fl <- est_all <- est_l <- est_s <- est_reg <- est_mini <- est_take <- est_cus <- est_liq <- numeric(0)

for (i in dt2$week_day) {
  est_fl[i] <- round(rnorm(1,mean = wk_fl[i],sd=wk_fl_sig[i]),2)
  est_all[i] <- round(rnorm(1,mean = wk_all[i],sd=wk_all_sig[i]),2)
  est_l[i] <- round(rnorm(1,mean = wk_l[i],sd=wk_l_sig[i]),2)
  est_s[i] <- round(rnorm(1,mean = wk_s[i],sd=wk_s_sig[i]),2)
  est_reg[i] <- round(rnorm(1,mean = wk_reg[i],sd=wk_reg_sig[i]),2)
  est_mini[i] <- round(rnorm(1,mean = wk_mini[i],sd=wk_mini_sig[i]),2)
  est_take[i] <- round(rnorm(1,mean = wk_take[i],sd=wk_take_sig[i]),2)
  est_cus[i] <- round(rnorm(1,mean = wk_cus[i],sd=wk_cus_sig[i]),2)
  est_liq[i] <- round(rnorm(1,mean = wk_liq[i],sd=wk_liq_sig[i]),2)
}
data.frame(est_fl =est_fl,
           est_all= est_all,
           est_l=est_l,
           est_s=est_s,
           est_reg=est_reg,
           est_mini=est_mini,
           est_take=est_take,
           est_cus=est_cus,
           est_liq=est_liq)
barplot(wk_all)
barplot(est_all)
