library(tidyverse)
library(lubridate)
library(forecast)
library(fda)
library(ggplot2)
library(keras)
library(xts)

oil <- read_csv('data/weekly_data.csv')
#oil <- oil[, 1:2]
colnames(oil) <- c("Date", "Price")
oil <- oil %>%
  mutate(Date = mdy(Date)) %>%
  arrange(Date)

oil_price <- oil$Price
  
oil_ts <- ts(data = oil_price, start = c(2000, 6), end = c(2023, 12), frequency = 52)

plot(oil_ts)

# Data preparation
window_size <- 10
train_size <- floor(length(oil_ts) * 0.8)  # 80% for training

train_data <- oil_ts[1:train_size]
test_data <- oil_ts[(train_size + 1):length(oil_ts)]

train_dates <- time(oil_ts)[1:train_size]
test_dates <- time(oil_ts)[(train_size + 1 + window_size):length(oil_ts)]

# Function to create input sequences and corresponding labels
create_sequences <- function(data, window_size) {
  sequences <- matrix(0, nrow = length(data) - window_size, ncol = window_size)
  labels <- numeric(length(data) - window_size)
  
  for (i in 1:(length(data) - window_size)) {
    sequences[i, ] <- data[i:(i + window_size - 1)]
    labels[i] <- data[i + window_size]
  }
  
  list(sequences = array(sequences, dim = c(length(data) - window_size, window_size, 1)), labels = labels)
}

# Create training sequences and labels
train_sequences <- create_sequences(train_data, window_size)
train_sequences_array <- train_sequences$sequences
train_labels <- train_sequences$labels

# Build a simple LSTM model
model <- keras_model_sequential() %>%
  layer_lstm(units = 50, input_shape = c(window_size, 1)) %>%
  layer_dense(units = 1)

# Compile the model
model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(),
  metrics = c('mean_absolute_error')
)

# Train the model
history <- model %>% fit(
  x = train_sequences_array,
  y = train_labels,
  epochs = 50,
  batch_size = 16,
  validation_split = 0.2
)

plot(history)

# Evaluate the model on the test data
test_sequences <- create_sequences(test_data, window_size)
test_sequences_matrix <- test_sequences$sequences
test_labels <- test_sequences$labels

results <- model %>% evaluate(test_sequences_matrix, test_labels)

# Make predictions
predictions <- model %>% predict(test_sequences_matrix)

# Plot predictions against actual values
plot(test_dates, test_labels, type = 'l', col = 'blue', ylab = 'Gasoline Prices', xlab = 'Year')
lines(test_dates, predictions, col = 'red')
legend('topleft', legend = c('Actual', 'Predicted'), col = c('blue', 'red'), lty = 1)
