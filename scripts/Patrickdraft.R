library(tidyverse)
library(lubridate)
library(forecast)
library(dplyr)
library(fda)
library(broom)

# Read and prepare the data
data <- read.csv("~/Desktop/PSTAT 197/vignette-tsforecasting/data/Weekly_California_All_Grades_All_Formulations_Retail_Gasoline_Prices.csv", skip = 5, header = TRUE)
data <- data[, 1:2]
colnames(data) <- c("Date", "Price")
data <- data %>%
  mutate(Date = mdy(Date)) %>%
  arrange(Date)

# Split into training and testing sets
split_point <- round(nrow(data) * 0.8)
train_data <- data[1:split_point, ]
test_data <- data[(split_point + 1):nrow(data), ]

# Convert Date to numeric for regression
train_data <- train_data %>%
  mutate(DateNumeric = as.numeric(Date))

# Fit linear model
fit <- lm(Price ~ DateNumeric, data = train_data)

# Augment with residuals and convert Date back to Date class for plotting
resid_df <- augment(fit, train_data) %>%
  mutate(Date = as.Date(DateNumeric, origin = "1970-01-01"))

# Plot the residual series
ggplot(resid_df, aes(x = Date, y = .resid)) +
  geom_line()


# Compute and plot ACF and PACF for residuals
acf(resid_df$.resid, lag.max = 20, main = "ACF of Residuals")
pacf(resid_df$.resid, lag.max = 20, main = "PACF of Residuals")

# Fit an ARIMA model using auto.arima()
fit_arima <- auto.arima(data_ts, seasonal = FALSE)

# Create a future time index for forecasting
future_time <- seq_along(test_data$Price) + length(data_ts)

# Forecast using the fitted ARIMA model
forecasts <- forecast(fit_arima, h = length(future_time))

# Plot the actual data and the forecasts
forecast_df <- data.frame(Date = as.Date(c(time(data_ts), future_time), origin = "2000-01-01"),
                          Price = c(train_data$Price, rep(NA, length(future_time))),
                          Fitted = c(fitted(fit), forecasts$mean))

ggplot(forecast_df, aes(x = Date)) +
  geom_line(aes(y = Price), colour = "black") +
  geom_line(aes(y = Fitted), colour = "blue") +
  labs(title = "Gasoline Price Forecast", y = "Price", x = "Date") +
  theme_minimal()
