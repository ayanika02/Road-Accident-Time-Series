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
# Load necessary libraries
library(tidyverse)
library(forecast)

# Convert data to long format
data_long <- data_ts %>%
  pivot_longer(cols = -year, names_to = "month", values_to = "value") %>%
  arrange(year, month)

# Create a time series object
ts_data <- ts(data_long$value, start = c(2014, 1), frequency = 12)

# Perform FFT
fft_result <- fft(ts_data)

# Calculate the power spectrum
n <- length(ts_data)
nyquist <- floor(n/2 + 1)
power_spectrum <- (abs(fft_result[1:nyquist])/n)^2
frequencies <- (0:(nyquist-1)) / n * 12  # Multiply by 12 to get cycles per year

# Plot the power spectrum
plot(frequencies, power_spectrum, type = "l",
     xlab = "Frequency (cycles per year)", ylab = "Power", main = "Power Spectrum")

# Identify dominant frequencies
n_top <- 5  # Number of top frequencies to identify
top_indices <- order(power_spectrum, decreasing = TRUE)[1:n_top]
top_frequencies <- frequencies[top_indices]
top_periods <- 12 / top_frequencies  # Convert to periods in months

print("Top periods (in months):")
print(top_periods[-1])  # Exclude the first one which is often the mean

# Use FFT for forecasting
n_forecast <- 12  # Forecast for 1 year

# Pad the FFT result with zeros
padded_fft <- c(fft_result, rep(0, n_forecast))

# Inverse FFT to get the forecast
forecast_values <- Re(fft(padded_fft, inverse = TRUE) / length(padded_fft))

# Create a time series object for the forecast
forecast_ts <- ts(forecast_values[(length(ts_data)+1):(length(ts_data)+n_forecast)],
                  start = c(2023, 1), frequency = 12)

# Plot original data and forecast
plot(ts_data, main = "Original Data and FFT Forecast", 
     ylim = range(c(ts_data, forecast_ts), na.rm = TRUE),
     xlim = c(2014, 2024))
lines(forecast_ts, col = "red")
#legend("bottomright", legend = c("Original", "Forecast"), col = c("black", "red"), lty = 1)

# Print forecasted values
print("Forecasted values for 2023:")
print(forecast_ts)

# Measure accuracy
# Use the last 12 months of actual data for comparison
actual_last_year <- tail(ts_data, 12)
forecast_last_year <- ts(forecast_values[(length(ts_data)-11):length(ts_data)],
                         start = end(ts_data) - c(1,0), frequency = 12)

# Calculate accuracy measures
mae <- mean(abs(actual_last_year - forecast_last_year))
rmse <- sqrt(mean((actual_last_year - forecast_last_year)^2))
mape <- mean(abs((actual_last_year - forecast_last_year) / actual_last_year)) * 100

print(paste("Mean Absolute Error (MAE):", round(mae, 2)))
print(paste("Root Mean Square Error (RMSE):", round(rmse, 2)))
print(paste("Mean Absolute Percentage Error (MAPE):", round(mape, 2), "%"))

