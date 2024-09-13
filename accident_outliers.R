data1 <- data_ts
length(data1)
summary(data1)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#7855   36662   39044   37892   40456   46247 
 bench <- 36662 - 1.5*IQR(data1)
bench <- as.integer(bench) 
data1[data_ts > bench] #non outliers
data1[data_ts < bench] #outiers
#7 outliers
data1 <- tsclean(data1)
adf.test(data1)
adf.test(diff(data1))
data1 <- diff(diff(data1))
summary(data1)
auto_model1 <- auto.arima(data1)
summary(auto_model1)
checkresiduals(auto_model1)
plot(forecast(auto_model1))

train_ts <- window(data1, end=c(2021,12))
test_ts <- window(data1, start=c(2022,1))
auto_model2 <- auto.arima(train_ts)
summary(auto_model2)
checkresiduals(auto_model2)
forecast_values <- forecast(auto_model2, h=12)
print(forecast_values)

# Plot the forecasted values against the actual values
autoplot(forecast_values) + 
  autolayer(test_ts, series="Test Data") +
  ggtitle("Forecast vs Actuals") + 
  xlab("Year") + 
  ylab("Values")

# Calculate accuracy of the forecast
accuracy(forecast_values, test_ts)

adf.test(data1)
plot(decompose(data1))
#SEASONALITY IS GETTING REMOVED

