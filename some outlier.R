library(reshape2)
library(fpp3)
library(zoo)
library(forecast)
library(feasts)
library(stats)
library(tseries)
library(lmtest)
library(qqplotr)

# Step 1: Read the CSV file
data <- read.csv("C:/Users/Diya/OneDrive/Documents/accidents.csv", header = TRUE, sep=",")
data <- data[1:ncol(data)-1]

# Step 3: Melt the data into a long format
data_long <- melt(data, id.vars = "X", variable.name = "Month", value.name = "Value")

# Convert the 'Month' to a factor with levels in correct order
data_long$Month <- factor(data_long$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

# Step 4: Sort the data by Year and Month
data_long <- data_long[order(data_long$X, data_long$Month), ]

start_year <- data_long$X[1]
data_ts <- ts(data_long$Value, start = c(start_year, 1), frequency = 12)
data_ts

outliers <- tsoutliers(data_ts,lambda=NULL)
outliers
outlier_positions <- outliers$index
outlier_positions
data1 <- data_ts
#data1[76] = data_ts[76-12]
data1[76] <- as.integer((data_ts[64] + data_ts[52] + data_ts[40] + data_ts[28] + data_ts[16] + data_ts[4]) / 6)
data1
#data1[89] = data_ts[89-24]
#data1[89] = data_ts[89-12]
data1[89] <- as.integer((data_ts[77] + data_ts[65] + data_ts[53] + data_ts[41] + data_ts[29] + data_ts[17] + data_ts[5]) / 7)
data1

autoplot(data1) + 
  ggtitle("Time Series Data") + 
  xlab("Year") + 
  ylab("Values")

train_ts <- window(data1, end=c(2019,12))
test_ts <- window(data1, start=c(2020,1))
train_ts
adf.test(train_ts)  #stationary
kpss.test(train_ts)  #non stationary
pp.test(train_ts)  #stationary
adf.test(diff(train_ts))
test_ts
fit <- auto.arima(diff(train_ts))
summary(fit)
checkresiduals(fit)
plot(forecast(fit,12))


# Forecast for 12 months ahead (2022)
forecast_values <- forecast(fit, h=12)
print(forecast_values)

# Plot the forecasted values against the actual values
autoplot(forecast_values) + 
  autolayer(test_ts, series="Test Data") +
  ggtitle("Forecast vs Actuals") + 
  xlab("Year") + 
  ylab("Values")

# Calculate accuracy of the forecast
accuracy(forecast_values, test_ts)

# Decompose the time series
decomp <- stl(ts(data_long$Value, frequency=12), s.window="periodic")

# Adjust outliers in the residual component
df$adjusted_value <- decomp$time.series[, "trend"] + decomp$time.series[, "seasonal"]


outliers <- tsoutliers(data_ts,lambda=NULL)
outliers
outlier_positions <- outliers$index
outlier_positions
data1[76] <- 23696
data1[89] <- 33628
plot(decompose(data1))
adf.test(data1)
arima1<-auto.arima(data1)
arima1
checkresiduals(arima1)
plot(forecast(arima1,12))