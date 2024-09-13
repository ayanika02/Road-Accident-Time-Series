# Load necessary libraries
library(tidyverse)
library(zoo)
library(forecast)
library(reshape2)

# Manually creating the dataset from the image

data_ts <- data.frame(
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
data_long <- melt(data_ts, id.vars = "year", variable.name = "month", value.name = "value")

# Convert the month names to their respective month numbers
data_long$month_num <- match(data_long$month, month.abb)

# Check if there are any NAs in month_num
if(any(is.na(data_long$month_num))) {
  stop("There are NA values in month_num. Check the month conversion.")
}
# Convert to a date format
data_long <- data_long %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-"), format = "%Y-%b-%d")) %>%
  arrange(date)

if(any(is.na(data_long$date))) {
  stop("There are NA values in date. Check the date conversion.")
}

# View the prepared data
head(data_long)

# Load the necessary library for winsorizing
#install.packages("DescTools")
library(DescTools)

# Function to winsorize the data
winsorize_data <- function(data, lower_quantile, upper_quantile) {
  data$value <- Winsorize(data$value, probs = c(lower_quantile, upper_quantile))
  return(data)
}
autoplot(train_ts)
# Winsorize the data with the desired quantiles (e.g., 5th and 95th percentiles)
lower_quantile <- 0.05
upper_quantile <- 0.95
plot(data)

train_data <- data_long %>%
  filter(date < as.Date("2022-01-01"))
test_data <- data_long %>%
  filter(date >= as.Date("2022-01-01"))

train_data_winsorized <- winsorize_data(train_data, lower_quantile, upper_quantile)

data_ts <- ts(train_data_winsorized$value, start = c(min(train_data_winsorized$year), min(train_data_winsorized$month_num)), frequency = 12)
 adf.test(data_ts)
 kpss.test(data_ts)
 pp.test(data_ts)
# Fit ARIMA model
fit_arima <- auto.arima(data_ts)
summary(fit_arima)

# Forecast for the year 2022
forecasted_data <- forecast(fit_arima, h = 12)  # Forecast for 12 months

# Prepare forecast dates
forecast_dates <- seq.Date(from = as.Date("2022-01-01"), by = "month", length.out = 12)

# Combine original, forecasted, and actual test data
plot_data <- train_data_winsorized %>%
  select(date, value) %>%
  rename(Number_of_Accidents = value) %>%
  bind_rows(data.frame(date = forecast_dates, Number_of_Accidents = forecasted_data$mean)) %>%
  bind_rows(test_data %>% select(date, value) %>% rename(Number_of_Accidents = value))

library(ggplot2)

# Plot the time series data, forecast, and actuals for 2022
ggplot(plot_data, aes(x = date, y = Number_of_Accidents)) +
  geom_line(data = plot_data %>% filter(date <= as.Date("2021-12-31")), color = "blue") + # Training data
  geom_line(data = plot_data %>% filter(date >= as.Date("2022-01-01")), aes(y = Number_of_Accidents), color = "red") + # Forecasted data
  geom_point(data = test_data %>% select(date, value) %>% rename(Number_of_Accidents = value), aes(x = date, y = Number_of_Accidents), color = "green", size = 1.5) + # Actual data for 2022
  labs(title = "Number of Accidents Over Time with Forecasts and Actuals for 2022", x = "Date", y = "Number of Accidents") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")


# Combine train and test data back together for analysis
data_winsorized <- bind_rows(train_data_winsorized, test_data)

# Convert to time series
data_ts <- ts(data_winsorized$value, start = c(min(data_winsorized$year), min(data_winsorized$month_num)), frequency = 12)

# Fit ARIMA model
fit_arima <- auto.arima(data_ts)
summary(fit_arima)

# Check residuals
checkresiduals(fit_arima)

# Forecast future values
forecasted_data <- forecast(fit_arima, h = 24)  # Forecast for next 24 months

# Prepare forecast dates
forecast_dates <- seq.Date(from = max(data_winsorized$date) + 1, by = "month", length.out = 24)

# Combine original and forecasted data
plot_data <- data_winsorized %>%
  select(date, value) %>%
  rename(Number_of_Accidents = value) %>%
  bind_rows(data.frame(date = forecast_dates, Number_of_Accidents = forecasted_data$mean))

ggplot(plot_data, aes(x = date, y = Number_of_Accidents)) +
  geom_line(color = "blue") +
  geom_point(data = plot_data %>% filter(date %in% forecast_dates), color = "red", size = 1.5) +
  labs(title = "Number of Accidents Over Time with Forecasts", x = "Date", y = "Number of Accidents") +
  theme_minimal()

# Plot the results to visualize the effect of winsorizing
ggplot(data_long, aes(x = date, y = value)) +
  geom_line(color = "blue") +
  labs(title = "Number of Accidents After Winsorizing", y = "Number of Accidents", x = "Date")

fit_arima <- auto.arima(ts(train_data_winsorized$value, start = c(min(train_data_winsorized$year), min(train_data_winsorized$month_num)), frequency = 12))
summary(fit_arima)

# Forecast including the year 2022
forecasted_data <- forecast(fit_arima, h = 24)  # Forecast for next 24 months

# Prepare forecast dates
forecast_dates <- seq.Date(from = max(train_data_winsorized$date) + 1, by = "month", length.out = 24)

# Combine original and forecasted data
plot_data <- train_data_winsorized %>%
  select(date, value) %>%
  rename(Number_of_Accidents = value) %>%
  bind_rows(data.frame(date = forecast_dates, Number_of_Accidents = forecasted_data$mean))

# Bind test data for actual comparison
plot_data <- plot_data %>%
  bind_rows(test_data %>% select(date, value) %>% rename(Number_of_Accidents = value))

ggplot(plot_data, aes(x = date, y = Number_of_Accidents)) +
  geom_line(color = "blue") +
  geom_point(data = plot_data %>% filter(date %in% forecast_dates), color = "red", size = 1.5) +
  geom_point(data = plot_data %>% filter(date >= as.Date("2022-01-01") & date <= as.Date("2022-12-31")), aes(x = date, y = Number_of_Accidents), color = "green", size = 1.5) +
  labs(title = "Number of Accidents Over Time with Forecasts and Actuals for 2022", x = "Date", y = "Number of Accidents") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")






str(data_long)

data_ts <- ts(data_long$value, start = c(min(data_long$year), min(data_long$month_num, na.rm = TRUE)), frequency = 12)

# Plot the time series
plot(data_ts, main = "Time Series of Number of Accidents", ylab = "Number of Accidents", xlab = "Time")

decomposed_ts <- decompose(data_ts)
plot(decomposed_ts)

fit_arima <- auto.arima(data_ts)
summary(fit_arima)
autoplot(fit_arima) + labs(title = "ARIMA Model Diagnostics")
#checkresiduals(fit_arima)

# Forecast the next 24 months (2 years)
forecast_horizon <- 24
forecasts <- forecast(fit_arima, h = forecast_horizon)
autoplot(forecasts) +
  labs(title = "ARIMA Forecast of Number of Accidents", y = "Number of Accidents", x = "Time") +
  theme_minimal()

# Extract forecasted values
forecasted_values <- as.vector(forecasts$mean)

# Generate forecast dates
forecast_dates <- seq.Date(from = max(data_long$date) + months(1), by = "month", length.out = forecast_horizon)

# Combine original data with forecasted values
plot_data <- data_long %>%
  select(date, value) %>%
  rename(Number_of_Accidents = value) %>%
  bind_rows(data.frame(date = forecast_dates, Number_of_Accidents = forecasted_values))

# Plot original data and forecasts
ggplot(plot_data, aes(x = date, y = Number_of_Accidents)) +
  geom_line(color = "blue") +
  geom_line(data = filter(plot_data, date > max(data_long$date)), color = "red") +
  labs(title = "Time Series with ARIMA Forecast", y = "Number of Accidents", x = "Date") +
  theme_minimal()

# Check residuals
checkresiduals(fit_arima)

data_long <- train_data[order(train_data$year, train_data$Month), ]
start_year <- data_long$X[1]
data_ts <- ts(data_long$Value, start = c(start_year, 1), frequency = 12)
data_ts
adf.test(data_ts)
