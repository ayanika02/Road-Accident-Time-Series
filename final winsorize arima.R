library(DescTools)
library(forecast)

data <- data.frame(
  year = 2014:2022,
  Jan = c(41954, 42661, 41749, 39824, 41780, 41130, 39527, 40305, 37040),
  Feb = c(39899, 40661, 40765, 36742, 38238, 37280, 39055, 37339, 36617),
  Mar = c(42524, 42842, 42843, 40394, 40640, 39706, 31967, 39491, 40356),
  Apr = c(39867, 42432, 42010, 38966, 40841, 37777, 7855, 31240, 39033),
  May = c(45404, 46247, 43368, 42799, 42730, 41490, 19924, 22530, 43307),
  Jun = c(42448, 42065, 39489, 39339, 39176, 39868, 27442, 29142, 39432),
  Jul = c(38916, 39694, 37881, 36380, 36991, 36190, 27264, 32627, 36677),
  Aug = c(39213, 39126, 37729, 36294, 35845, 34096, 29319, 33781, 36451),
  Sep = c(37360, 39761, 36929, 36093, 35387, 35209, 31042, 33269, 34804),
  Oct = c(39791, 42089, 39952, 38527, 38238, 35398, 34918, 36534, 38650),
  Nov = c(40040, 41018, 38505, 39701, 38417, 36936, 38089, 37581, 38991),
  Dec = c(41984, 42827, 39432, 39793, 38737, 37071, 39736, 38773, 39954)
)

# Reshape the data to a long format
data_long <- melt(data, id.vars = "year", variable.name = "month", value.name = "value")

# Convert the month names to their respective month numbers
data_long$month_num <- match(data_long$month, month.abb)

data_long <- data_long %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-"), format = "%Y-%b-%d")) %>%
  arrange(date)

# Split data into train (up to end of 2021) and test (from start of 2022)
train_data <- data_long %>%
  filter(date < as.Date("2022-01-01"))
test_data <- data_long %>%
  filter(date >= as.Date("2022-01-01") & date <= as.Date("2022-12-31"))

# Function to apply Winsorization
winsorize_data <- function(data, lower_quantile = 0.05, upper_quantile = 0.95) {
  data %>%
    mutate(
      value = Winsorize(value, probs = c(lower_quantile, upper_quantile), na.rm = TRUE)
    )
}

# Apply Winsorization to train data
train_data_winsorized <- winsorize_data(train_data)

# Convert to time series
data_ts <- ts(train_data_winsorized$value, start = c(min(train_data_winsorized$year), min(train_data_winsorized$month_num)), frequency = 12)

# Fit ARIMA model
adf.test(data_ts)
kpss.test(data_ts)
pp.test(data_ts)
fit_arima <- auto.arima(data_ts)
summary(fit_arima)
checkresiduals(fit_arima)

# Forecast for the year 2022
forecasted_data <- forecast(fit_arima, h = 12)# Forecast for 12 months
plot(forecasted_data)

# Prepare forecast dates
forecast_dates <- seq.Date(from = as.Date("2022-01-01"), by = "month", length.out = 12)

# Combine original, forecasted, and actual test data
forecast_df <- data.frame(date = forecast_dates, Number_of_Accidents = forecasted_data$mean)

# Combine training data, forecasted data, and actual test data
plot_data <- train_data_winsorized %>%
  select(date, value) %>%
  rename(Number_of_Accidents = value) %>%
  bind_rows(forecast_df) %>%
  bind_rows(test_data %>% select(date, value) %>%
              rename(Number_of_Accidents = value))

library(ggplot2)

# Plot the time series data, forecast, and actuals for 2022
ggplot() +
  geom_line(data = train_data, aes(x = date, y = value), color = "blue") +  # Training data
  geom_line(data = forecast_df, aes(x = date, y = Number_of_Accidents), color = "red") +  # Forecasted data
  geom_line(data = test_data, aes(x = date, y = value), color = "green") +  # Actual data for 2022
  labs(title = "Number of Accidents Over Time with Forecasts and Actuals for 2022", x = "Date", y = "Number of Accidents") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

# Step 1: Extract the forecasted and actual values for 2022
predicted_values <- forecasted_data$mean
actual2022 <- test_data$value

predicted2022 <- window(predicted_values, start=c(2022,1))
rmse2022 <- sqrt(mean((predicted2022 - actual2022)^2))
mae2022 <- mean(abs(predicted2022-actual2022))
mape2022 <- mean(abs((actual2022 - predicted2022) / actual2022)) * 100
cat(mae2022, mape2022, rmse2022)

#winsorized plot
ggplot() +
  geom_line(data = train_data_winsorized, aes(x = date, y = value), color = "blue") +  # Training data
  geom_line(data = train_data, aes(x=date, y=value), color = "red")  

# Step 5: Combine the predicted and actual values into a single data frame for comparison
comparison_df <- data.frame(
  Date = forecast_dates,
  Predicted = predicted_values,
  Actual = actual_values
)

# Step 6: Print the comparison data frame
print(comparison_df)

####################################################################3
#For differenced data

# The last observed value in the original series
last_observed_value <- tail(data_ts, 1)

# Assuming forecasted_data1$mean is your forecasted data
forecasted_data <- data.frame(
  year = rep(2020:2022, each = 12),
  month = rep(month.abb, times = 3),
  value = as.vector(t(forecasted_data1$mean))
)

# Add month number
forecasted_data$month_num <- rep(1:12, times = 3)

# Create date column
forecasted_data$date <- as.Date(paste(forecasted_data$year, forecasted_data$month_num, "01", sep = "-"), format = "%Y-%m-%d")

# View the data frame
print(forecasted_data)

integrated_values <- numeric(length(forecasted_data$value))
integrated_values[1] <- last_observed_value + forecasted_data$value[1]

# Iterate over the forecasted values to integrate them
for (i in 2:length(forecasted_data$value)) {
  integrated_values[i] <- integrated_values[i - 1] + forecasted_data$value[i]
}

# Add the integrated values as a new column in the forecasted_data data frame
forecasted_data$integrated_accidents <- integrated_values

# View the updated data frame
print(forecasted_data)

ggplot() +
  geom_line(data = train_data_winsorized, aes(x = date, y = value), color = "blue") +  # Training data
  geom_line(data = forecasted_data, aes(x = date, y = integrated_accidents), color = "red") +  # Forecasted data
  geom_line(data = test_data, aes(x = date, y = value), color = "green") +  # Actual data for 2022
  labs(title = "Number of Accidents Over Time with Forecasts and Actuals for 2022", x = "Date", y = "Number of Accidents") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

predicted_values <- forecasted_data$integrated_accidents
actual_values <- test_data$value

predicted2022 <- predicted_values[25:36]
actual2022 <- actual_values[25:36]
rmse2022 <- sqrt(mean((predicted2022 - actual2022)^2))
mae2022 <- mean(abs(predicted2022-actual2022))
cat("RMSE for 2022:", rmse2022, "\n")
cat("MAE for 2022:", mae2022, "\n")
