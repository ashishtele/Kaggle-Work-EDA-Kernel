
# Loading the 
load_lb <- function()
{
  suppressPackageStartupMessages(library(doMC))
  registerDoMC(cores = 8)
  suppressPackageStartupMessages(library(readxl))
  suppressPackageStartupMessages(library(tidyr))
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(caret))
  suppressPackageStartupMessages(library(rpart))
  suppressPackageStartupMessages(library(tree))
  suppressPackageStartupMessages(library(MASS))
  suppressPackageStartupMessages(require(xgboost))
  require(forecast)
  library(lubridate)
}

load_lb()

# Import the data file

df <- fread("E:\\Study\\R Projects\\Common files\\Apple_data.csv",
            sep = ",")
head(df)
tail(df)
glimpse(df)

# Formating the columns and keeping only required columns for analysis
library(lubridate)

df %>% 
  mutate(date = as.Date(date)) %>% 
  dplyr::select(date, close) %>% 
  mutate(Year = year(date),
         Month = month(date),
         Day = day(date))-> df1
glimpse(df1)

ggplot(df1[df1$Year > 2017,],aes(x = date, y = close, color = date)) +
  geom_line(show.legend = FALSE) +
  labs(title = "Closing stock price trend - Apple")

## Naive forecasting - Trend data

# Most recently observed value, benchmarking

Apl_naive <- naive(df1[df1$Year > 2017,"close"], h = 10)
summary(Apl_naive)  
autoplot(Apl_naive) +
  labs(x = "Data Points", y = NULL)

# Confidence interval looks wide 

## Seasonal naive approach

rain_sn <- snaive(df.ts, 24)
summary(rain_sn)

autoplot(rain_sn)+
  labs(x = "year", y = NULL)

## Checking residuals - 2017 onwards

checkresiduals(Apl_naive)
## first plot: residuals show no trend -> white noise
## bottom left: One lag exceeds the threshold
## Residuals are almost normally distributed
## Lj test: p > 0.05 -> fail to reject the null hypothesis that is is purely due to noise

checkresiduals(rain_sn)
## first plot: residuals show trend
## bottom left: One lag exceeds the threshold
## Residuals are almost normally distributed, but with high kurtosis
## Lj test: p < 0.05 -> reject the null hypothesis that is is purely due to noise


# Training and test sets


ap_ts <- ts(df1[df1$Year > 2017,"close"])

ap_train <- subset(ap_ts, end = 85)
ap_test <- subset(ap_ts, start = 86, end = length(ap_ts))

naive_ap <- naive(ap_train, h = 5)
mean_ap <- meanf(ap_train, h =5)
accuracy(naive_ap, ap_test)
accuracy(mean_ap, ap_test)

err <- tsCV(ap_ts,
            forecastfunction = naive,
            h = 1)                                                # 1-step ahead
mean(err^2, na.rm = TRUE)


# for seasonal features

rain_tr <- window(df.ts, end = c(2013,12))
rain_te <- window(df.ts, start = c(2014,1))

r_sn <- snaive(rain_tr, h = length(rain_te))
r_mean <- meanf(rain_tr, h = length(rain_te))
accuracy(r_sn, rain_te)
accuracy(r_mean, rain_te)

err <- tsCV(df.ts,
            forecastfunction = snaive,
            h = 1)
mean(err^2, na.rm = TRUE)  
library(zoo)

df1 %>% 
  filter(Year > 2010) %>% 
  select(date, close) %>% 
  mutate(ma00 = rollmean(close, k = 13, fill = NA),
         ma01 = rollmean(close, k = 25, fill = NA),
         ma02 = rollmean(close, k = 49, fill = NA),
         ma03 = rollmean(close, k = 97, fill = NA)) -> df_ap
head(df_ap)

df_ap %>% 
  gather(metric, value, close:ma03) %>% 
  ggplot(aes(date, value, color = metric))+
  geom_line()

df_ap %>% 
  gather(metric, value, ma00:ma03) %>% 
  group_by(metric) %>% 
  summarise(MSE = mean((close - value)^2, na.rm = TRUE),
            MAPE = mean(abs((close - value)/close),na.rm = TRUE))


# trailing moving average

df1 %>% 
  filter(Year > 2010) %>% 
  dplyr::select(date, close) %>% 
  mutate(ma_trail = rollmean(close, k = 12, fill = NA, align = "right")) %>% 
  gather(metric, value, -date) %>% 
  ggplot(aes(date, value, color = metric)) +
  geom_line()

# Simple expotential smoothing

ses_ap <- ses(ap_train, alpha = 0.2, h = 5)
autoplot(ses_ap)

ap_diff <- diff(ap_train)
autoplot(ap_diff)

ses_ap_diff <- ses(ap_diff, alpha = 0.2, h = 5)
autoplot(ses_ap_diff)

# Holt's method

holt_app <- holt(ap_train, h = 5)
autoplot(holt_app)
holt_app$model

accuracy(holt_app, ap_test)[2,5]
# tuning the 'beta'value

beta <- seq(0.0001, 0.1, by = 0.001)
RMSE <- NA
for (i in seq_along(beta)){
  fit <- holt(ap_train, beta = beta[i], h = 5)
  RMSE[i] <- accuracy(fit, ap_test)[2,2]
}


beta.fit <- data.frame(beta, RMSE)
beta.min <- filter(beta.fit, RMSE == min(RMSE))
beta.fit %>% 
  ggplot(aes(beta, RMSE))+
  geom_line() +
  geom_point(data = beta.min, aes(beta, RMSE), size = 2, color="blue")
  
# refit the model

holt_app <- holt(ap_train, h = 5, beta = beta.min[1,1])
autoplot(holt_app)
accuracy(holt_app, ap_test)   # MAPE increased


# Holt-Winters seasonal method

autoplot(decompose(df.ts))



