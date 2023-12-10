setwd("Users/seoeun/UCSB/PSTAT197/vignette-tsforecasting")
library(tidyverse)
library(fda)
library(lubridate)
library(forecast)

gasoline <- read_csv("data/Weekly_California_All_Grades_All_Formulations_Retail_Gasoline_Prices.csv")
gasoline <- gasoline[-c(1:4),]

newcol <- strsplit(gasoline$`Weekly California All Grades All Formulations Retail Gasoline Prices`, ",")
gasoline$Date <- sapply(newcol, function(x) x[1])
gasoline$Price <- sapply(newcol, function(x) x[2])
gasoline <- na.omit(gasoline) 

gasoline <- gasoline %>% select(Date, Price)
gasoline$Date <- as.Date(gasoline$Date, "%m/%d/%Y")
gasoline$Price <- as.numeric(gasoline$Price)
gasoline <- gasoline %>% arrange(Date)
gasoline

############ EDA #################### 
dim(gasoline) ## 1227 obs. and 2 variables (Date, Price)

summary(gasoline$Price)
## Min: 1.146 | Q1: 2.514 | Median: 3.184 | Q3: 3.859 | Max: 6.364

gasoline %>% filter(Price==min(Price) | Price==max(Price))
## From May 2000 to November 2023, the lowest price was 1.15 on Dec 31st, 2001, and the highst price
## was 6.36 on Jun 13, 2002.

## Plot
x <- ts(gasoline$Price, start=c(2000,5), end=c(2023,11), frequency=52)
plot.ts(x, type="l", main="Weekly California Retail Gasoline Prices", ylab="Price",
        col="blue") 
grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 2)

## Autocorrelation Function
acf(x, main="ACF")
pacf(x, main="PACF")

###################################
xreg <- gasoline %>% pull(Date) %>%
  fda::fourier(nbasis=3, period=52)

y <- pull(gasoline, Price)
reg_df <- bind_cols(price=y, xreg)

fit <- lm(price ~ . -1, data=reg_df)
fit_df <- broom::augment(fit) %>%
  bind_cols(date=gasoline$Date)

## Residual Series Plot
fit_df %>%
  ggplot(aes(x=date, y=.resid)) + geom_path() ## it is stationary

## ACF & PACF of Residuals
acf(fit_df$.resid, main="ACF")
pacf(fit_df$.resid, main="PACF") ## spike at lag 2

## Fit Error Model
fit_resid <- Arima(fit_df$.resid, order=c(2,0,0), 
                   include.mean=F, method="ML")
resid_acf_fitted <- ARMAacf(ar=coef(fit_resid), lag.max=25)
acf(fit_df$.resid, main="ACF")
lines(resid_acf_fitted, col="red")

######## Regression with AR Errors
## Training/Testing
train80 <- floor(nrow(xreg)*0.8)
x_train <- xreg[1:train80,]
y_train <- y[1:train80]
x_test <- xreg[(train80+1):nrow(xreg),]
y_test <- y[(train80+1):nrow(xreg)]

## Fit the Model
fit_full <- Arima(y_train, order=c(2,0,0), 
                  xreg=x_train,include.mean=F, method="ML")

broom::tidy(fit_full) %>% knitr::kable()

## Forecasting
preds <- forecast(fit_full, h=nrow(x_test), xreg=x_test)
preds %>% as_tibble() %>% head()

## Plot the forecasts
fig_forecast <- gasoline %>%
  dplyr::select(Date, Price) %>%
  bind_cols(pred = c(fit_full$fitted, preds$mean),
            status = c(rep('obs', nrow(x_train)),
                       rep('pred', nrow(x_test)))) %>%
  ggplot(aes(x=Date)) +
  geom_path(aes(y=Price, linetype=status)) +
  geom_path(aes(y=pred), color="blue", alpha=0.5)
fig_forecast

ci_df <- gasoline %>%
  slice_tail(n=nrow(x_test)) %>%
  dplyr::select(Date) %>%
  bind_cols(lwr=preds$lower[,2],
            upr=preds$upper[,2])

fig_forecast + geom_ribbon(aes(x=Date, ymin=lwr, ymax=upr),
                           alpha=0.3, fill="blue", data=ci_df)











