library(tidyverse)
library(lubridate)
library(forecast)
library(dplyr)
library(fda)
library(broom)
library(qpcR)


# Read and prepare the data
data <- read.csv("~/Desktop/PSTAT 197/vignette-tsforecasting/data/Weekly_California_All_Grades_All_Formulations_Retail_Gasoline_Prices.csv", skip = 5, header = TRUE)
data <- data[, 1:2]
colnames(data) <- c("Date", "Price")
data <- data %>%
  mutate(Date = mdy(Date)) %>%
  arrange(Date)


#EDA
# Convert to a ts object (assuming the data starts from 22-05-2000 and is weekly)
start_date <- as.Date("2000-05-22")
end_date <- as.Date("2023-11-27")
weeks <- as.numeric(difftime(end_date, start_date, units = "weeks"))
gas_price_ts <- ts(data$Price, start = c(2000, 22), end = c(2023, 47), frequency = 52)

# Plot the time series
ts.plot(gas_price_ts, main = "Weekly California Gasoline Prices (2000 - 2023)",
        ylab = "Price per Gallon", xlab = "Year")

# Calculate the mean of the plot
mean_val <- mean(gas_price_ts)

# Add a horizontal line at the mean value
abline(h = mean_val, col = "blue", lwd = 1)

# Add trend to data plot
fit <- lm(gas_price_ts ~ time(gas_price_ts))
abline(fit, col = "red", lwd = 1)

# Assuming 'gas_price_ts' is your time series object
x = length(gas_price_ts)
split_point <- round(x * 0.8)  # Adjust the split point as needed

# Partition data into training and test dataset
train_data <- gas_price_ts[1:split_point]
test_data <- gas_price_ts[(split_point + 1):x]

# Plot histogram of training data
hist(train_data, col = "lightblue", main = "Histogram of Training Data",
     xlab = "Price per Gallon")

# Plot ACF of training data
acf(train_data, lag.max = 60, main = "ACF of the Training Data")

# Log transformation
logtrain <- log(train_data)

# Histogram comparison
op <- par(mfrow = c(1, 2))
hist(train_data, col = "lightblue", xlab = "", main = "Histogram; Train Data")
hist(logtrain, col = "lightblue", xlab = "", main = "Histogram; Log Transformed Train Data")

# Reset par to original settings
par(op)

# Plot the original train data
ts.plot(train_data, main = "Plot of Training Set",
        ylab = "Price per Gallon", xlab = "Time")

# Plot the logtrain data
ts.plot(logtrain, main = "Plot of Log Transformed Training Set",
        ylab = "Log(Price per Gallon)", xlab = "Time")

var(train_data) # train data
var(logtrain) 

diff1<- diff(logtrain, lag=1)


ts.plot(diff1,
        main = "Plot of log transformed, differenced at lag 1 training Set",
        ylab = "diff1", xlab = "Time")
fit <- lm(diff1 ~ as.numeric(1:length(diff1))); abline(fit, col="red")
abline(h=mean(diff1), col="blue")
acf(diff1, lag.max = 40,
    main = "ACF: log Transformed, differenced at lag 1", cex.main = 0.8)
pacf(diff1, lag.max = 40,
     main = "ACF: log Transformed, differenced at lag 1", cex.main = 0.8)


# Define the range for p, d, and q
p_max <- 5  # maximum value of p
d_max <- 2  # maximum value of d (usually 0, 1, or 2)
q_max <- 5  # maximum value of q

# Initialize a dataframe to store the results
results <- data.frame(p = integer(), d = integer(), q = integer(), AICc = numeric())

# Loop over different combinations of p, d, and q
for (p in 0:p_max) {
  for (d in 0:d_max) {
    for (q in 0:q_max) {
      # Fit the ARIMA model
      model <- tryCatch(Arima(logtrain, order = c(p, d, q), method = "ML"), 
                        error = function(e) return(NULL))
      # If the model is successfully fitted, record the AICc value
      if (!is.null(model)) {
        results <- rbind(results, data.frame(p = p, d = d, q = q, AIC = AIC(model)))
      }
    }
  }
}

# Order the results by AICc and get the top 3
top_models <- results[order(results$AIC), ][1:3, ]

# Print the top 3 models
print(top_models)



model1 <- arima(logtrain, order=c(4,1,3), method="ML")
model2 <- arima(logtrain, order=c(4,1,4), method="ML")
model3 <- arima(logtrain, order=c(5,1,3), method="ML")


# Diagnostics Checking for Model (1)
res1 <- residuals(model1)
hist(res1,density=20,breaks=20, col="blue", xlab="",
     prob=TRUE, main= "Residuals of Model(1)")
m <- mean(res1)
std <- sqrt(var(res1))
curve( dnorm(x,m,std), add=TRUE )
plot.ts(res1)
fitt <- lm(res1 ~ as.numeric(1:length(res1))); abline(fitt, col="red")
abline(h=mean(res1), col="blue")
qqnorm(res1,main= "Normal Q-Q Plot for Model(1)")
qqline(res1,col="blue")
m # sample mean
# ACF and PACF of the residuals for model(1)
op <- par(mfrow = c(1,2))
acf(res1, lag.max=40)
pacf(res1, lag.max=40)
# Diagnostics for model(1)
shapiro.test(res1)
Box.test(res1, lag = 12, type = c("Box-Pierce"), fitdf = 2)
Box.test(res1, lag = 12, type = c("Ljung-Box"), fitdf = 2)
Box.test(res1^2, lag = 12, type = c("Ljung-Box"), fitdf = 0)
# Diagnostics for model(1)
ar(res1, aic = TRUE, order.max = NULL, method = c("yule-walker"))

par(op)
fit.2 <- arima(logtrain, order=c(4,1,3), method="ML")
forecast(fit.2)
# produce graph with 12 forecasts on transformed data
pred.tr <- predict(fit.2, n.ahead = 11)
U.tr= pred.tr$pred + 2*pred.tr$se
L.tr= pred.tr$pred - 2*pred.tr$se
ts.plot(logtrain, xlim=c(1,length(logtrain)+11),
        ylim = c(min(logtrain),max(U.tr))
        , main = "graph with 12 forecasts on transformed data")
lines(U.tr, col="blue", lty="dashed")
lines(L.tr, col="blue", lty="dashed")
points((length(logtrain)+1):(length(logtrain)+11), pred.tr$pred, col="red")
# produce graph with forecasts on original data:
pred.orig <- exp(pred.tr$pred)
U= exp(U.tr)
L= exp(L.tr)
ts.plot(train_data, xlim=c(1,length(train_data)+11),
        ylim = c(min(train_data),max(U))
        , main = "graph with 12 forecasts on original data")
lines(U, col="blue", lty="dashed")
lines(L, col="blue", lty="dashed")
points((length(train_data)+1):(length(train_data)+11), pred.orig, col="red")
# produce graph with 12 forecasts on original data and true values
ts.plot(test_data, xlim = c(100,length(train_data)+11),
        ylim = c(min(train_data),max(U)), col="red",
        main = "graph with 12 forecasts and true values")
lines(U, col="blue", lty="dashed")
lines(L, col="blue", lty="dashed")
points((length(train_data)+1):(length(train_data)+11), pred.orig[1:11], col="black")


# produce graph with 100 forecasts
pred.tra <- predict(fit.2, n.ahead = 100)
U.tr= pred.tra$pred + 2*pred.tra$se
L.tr= pred.tra$pred - 2*pred.tra$se
ts.plot(logtrain, xlim=c(100,length(logtrain)+100),
        ylim = c(min(logtrain),max(U.tr)), main = "graph with 100 forecasts")
lines(U.tr, col="blue", lty="dashed")
lines(L.tr, col="blue", lty="dashed")
points((length(logtrain)+1):(length(logtrain)+100), pred.tra$pred, col="red")









# Split into training and testing sets
split_point <- round(nrow(data) * 0.8)
train_data <- data[1:split_point, ]
test_data <- data[(split_point + 1):nrow(data), ]

# Following the forecasting activity during section

# Create a time series object from the training data
# Assuming the data starts from 05/22/2000 and is weekly
start_date <- as.Date("2000-05-22")
train_data <- train_data %>% mutate(Week = as.numeric(difftime(Date, start_date, units = "weeks")))
data_ts <- ts(train_data$Price, frequency = 52, start = c(2000, 22))

# Fit an ARIMA model to the time series data
fit_arima <- auto.arima(data_ts)

# Forecast using the fitted ARIMA model
forecasts <- forecast(fit_arima, h = nrow(test_data))

# Prepare the data for plotting
last_train_date <- max(train_data$Date)
forecast_dates <- seq(last_train_date, by = "week", length.out = nrow(test_data))
forecast_df <- data.frame(Date = c(train_data$Date, forecast_dates),
                          Price = c(train_data$Price, rep(NA, nrow(test_data))),
                          Fitted = c(fitted(fit_arima), forecasts$mean))

# Plot the actual data and the forecasts
ggplot(forecast_df, aes(x = Date)) +
  geom_line(aes(y = Price), colour = "black") +
  geom_line(aes(y = Fitted), colour = "blue", linetype = "dotted") +
  labs(title = "Gasoline Price Forecast", y = "Price", x = "Date") +
  theme_minimal()




# keras neural network
library(keras)