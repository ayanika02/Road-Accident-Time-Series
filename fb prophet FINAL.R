library(reshape2)
library(fpp3)
library(zoo)
library(forecast)
library(feasts)
library(stats)
library(tseries)
library(lmtest)
library(qqplotr)
library(prophet)
library(pracma)  #for hampel

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
data_long <- melt(data_ts, id.vars = "year", variable.name = "month", value.name = "y")
# Convert the month names to their respective month numbers
data_long$month_num <- match(data_long$month, month.abb)
data_long
data_long <- data_long %>%
  mutate(ds = as.Date(paste(year, month, "01", sep = "-"), format = "%Y-%b-%d")) %>%
  arrange(ds)
data_long

outliers <- c(7855, 19924, 27442, 27264 ,29319, 22530, 29142)
out_ind <- c(76, 77, 78, 79, 80, 89, 90)

df <-data_long
for (i in out_ind){
  #df$y[i] = data_long$y[60 + (i%%12)]
  df$y[i] = data_long$y[i-12]
}

data1 <- df
for (i in out_ind) {
  k<-i;
  sum = 0;
  count = 0;
  # Loop to sum all previous months' values
  while (i-12>0) {
    sum = sum + df$y[i-12];
    cat(df$y[i-12])
    count = count + 1;
    i=i-12;
    print(i)
  }
  # Assign the average of previous months to data1[i]
  data1$y[k] <- round(sum / count);
}
df <- data1

#MEDIAN
data1<-data_long
list <- data_long$y
median <- median(list)
for (i in out_ind){
  data1$y[i] = median
}
df <- data1

#12 months MA
data1 <- data_long
for (i in out_ind){
  ma <- 0
  for (j in 1:12){
    ma <- ma + data_long$y[i-j]
  }
  data1$y[i] <- round(ma/12)
}
df <- data1

df$y[76] <- 23636
df$y[89] <- 33529
ds <- df$ds
y <- df$y
df <- data.frame(ds,y)
df
train <- subset(df, ds < as.Date("2022-01-01"))
test <- subset(df, ds >= as.Date("2022-01-01"))

###############################################################
winsorize_data <- function(data, lower_quantile = 0.01, upper_quantile = 0.99) {
  data %>%
    mutate(
      values = Winsorize(y, probs = c(lower_quantile, upper_quantile), na.rm = TRUE)
    )
}
df <- data_long
ds <- df$ds
y <- df$y
df <- data.frame(ds,y)
df
train <- subset(df, ds < as.Date("2022-01-01"))
test <- subset(df, ds >= as.Date("2022-01-01"))
train_data_winsorized <- winsorize_data(train)
y<-train_data_winsorized$values
ds<-train_data_winsorized$ds
df <- data.frame(ds, y) 
df

# df1 <- ts(df$y, start=c(2014,1), frequency=12)
# filtered_series <- ts(hampel(df1, k=6)$y, frequency = 12)
# time_series_df <- data.frame(Time = as.numeric(time(df1)), 
#                              Value = as.numeric(df1),
#                              Type = "Original")
# filtered_series_df <- data.frame(Time = as.numeric(time(filtered_series)), 
#                                  Value = as.numeric(filtered_series),
#                                  Type = "Filtered")
# combined_df <- rbind(time_series_df, filtered_series_df)
# ggplot(combined_df, aes(x = Time, y = Value, color = Type)) +
#   geom_line() +
#   labs(title = "Time Series Data: Original vs Hampel Filtered", x = "Time", y = "Value") +
#   theme_minimal() +
#   theme(legend.position = "bottom")

m <- prophet(n.changepoints = 1)
m <- prophet(changepoint.range = 1)
#m <- fit.prophet(m, df)
m <- fit.prophet(m,train)
future <- make_future_dataframe(m, periods = 24, freq = 'month')
forecast <- predict(m, future)

dyplot.prophet(m,forecast)
plot(m, forecast)
prophet_plot_components(m, forecast)
pred2022 <- forecast$yhat[97:108]
pred2022
actual2022 <- data_long$y[97:108]
plot(actual2022,pred2022)
abline(lm(pred2022~actual2022),col='red') #line of best fit linear model
summary(lm(pred2022~actual2022))
mae <- mean(abs(actual2022 - pred2022))
mape <- mean(abs((actual2022 - pred2022) / actual2022)) * 100
rmse <- sqrt(mean((actual2022 - pred2022)^2))
cat(rmse, mape, mae)

final <- forecast$yhat[97:120]
final_ts <- ts(final, start=c(2022,1), frequency=12)
final_ts <- round(final_ts)
data_ts <- ts(data_long$y, start=c(2014,1), frequency = 12)
lower <- forecast$yhat_lower[97:120]
upper <- forecast$yhat_upper[97:120]
lower_ts <- ts(lower, start=c(2022,1), frequency=12)
upper_ts <- ts(upper, start=c(2022,1), frequency=12)

# Extend the time range to cover both the original data and the forecasted data
extended_time <- ts(c(data_ts, final_ts), start = start(data_ts), frequency = frequency(data_ts))
# Plot the extended time series (this will include both original and forecasted data)
plot(extended_time, type = "n", xlab = "Time", ylab = "Values",
     main = "Original vs Forecasted Values", cex.main=1.5, cex.lab=1.5, cex.axis=1.3)
# Add confidence interval
polygon(c(time(lower_ts), rev(time(upper_ts))),
        c(lower_ts, rev(upper_ts)),
        col = rgb(1, 0, 0, 0.2), border = NA)
lines(data_ts, col = "blue", lwd = 1.5)
# Add the forecasted data with lines and dots
lines(final_ts, col = "red", lwd = 1.5, type = "o", pch = 20, cex=0.5)
#lines(test, red)
legend("bottomleft", legend = c("Actual", "Forecast", "80% Confidence Interval"),
       col = c("blue", "red", rgb(1, 0, 0, 0.2)), lwd = c(2, 2, 10),
       cex = 0.6)

df <- column_to_rownames(df, var = "ds")
lam = BoxCox.lambda(df$y)
df$y = BoxCox(df$y, lam)
#df.m <- melt(df, measure.vars=c("value", "y"))

inverse_forecast <- forecast
inverse_forecast <- column_to_rownames(inverse_forecast, var = "ds")
inverse_forecast$yhat_untransformed = InvBoxCox(forecast$yhat, lam)
inverse_forecast$yhat_untransformed
plot(inverse_forecast$yhat_untransformed)

pred<-inverse_forecast$yhat_untransformed[97:108] #108 as last 10 values are predicted
actual<-df$value[97:108]
plot(actual,pred)
abline(lm(pred~actual),col='red') #line of best fit linear model
summary(lm(pred~actual))
mae <- mean(abs(actual - pred))
mape <- mean(abs((actual - pred) / actual)) * 100
rmse <- sqrt(mean((actual - pred)^2))
cat(mae, mape, rmse)

plot(actual, pred, main = "Actual vs Predicted", xlab = "Actual Values", ylab = "Predicted Values", pch = 19, col = "blue")

# Add line of best fit
abline(lm(pred ~ actual), col = "red", lwd = 2)

# Add identity line (perfect prediction)
abline(0, 1, col = "green", lwd = 2, lty = 2)

pred<-c(forecast$yhat[1:72],forecast$yhat[97:108]) #731 as last 10 values are predicted
actual<-c(df$y[1:72], df$y[97:108])
plot(actual,pred)
abline(lm(pred~actual),col='red') #line of best fit linear model
summary(lm(pred~actual))
mae <- mean(abs(actual - pred))
mape <- mean(abs((actual - pred) / actual)) * 100
rmse <- sqrt(mean((actual - pred)^2))
cat(mae, mape, rmse)
