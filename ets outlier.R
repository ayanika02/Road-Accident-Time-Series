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


data <- read.csv("C:/Users/Diya/OneDrive/Documents/accidents.csv", header = TRUE, sep=",")

data <- data[1:ncol(data)-1]
#data <- as.data.frame(data)
# Step 3: Melt the data into a long format
data_long <- melt(data, id.vars = "X", variable.name = "Month", value.name = "Value")

# Convert the 'Month' to a factor with levels in correct order
data_long$Month <- factor(data_long$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

# Step 4: Sort the data by Year and Month
data_long <- data_long[order(data_long$X, data_long$Month), ]

start_year <- data_long$X[1]
data_ts <- ts(data_long$Value, start = c(start_year, 1), end= c(2021,12), frequency = 12)
data_ts


box <- boxplot(data_ts,horizontal=T)
outliers <- box$out
outliers
out_ind <- which(data_ts %in% c(outliers))
out_ind

data1 <-round(tsclean(data_ts))
data1

#MEDIAN
data1<-data_ts
list <- data_long$Value
median <- median(list)
for (i in out_ind){
  data1[i] = median
}

#REPLACE WITH 2019 AND PREV MONTH
for (i in out_ind){
  data1[i] <- data_ts[60 + (i%%12)]
  #data1[i] <- data_ts[i-12]
}

#REPLACE WITH AVERAGE OF THAT MONTH
data1 <- data_ts # Initialize data1 to be the same as data_ts
for (i in out_ind) {
  k<-i;
  sum = 0;
  count = 0;
  # Loop to sum all previous months' values
  while (i-12>0) {
    sum = sum + data_ts[i-12];
    cat(data_ts[i-12])
    count = count + 1;
    i=i-12;
    print(i)
  }
  # Assign the average of previous months to data1[i]
  data1[k] <- round(sum / count);
}

#REPLACE WITH 12 MONTHS MA
data1 <- data_ts
for (i in out_ind){
  ma <- 0
  for (j in 1:12){
    ma <- ma + data_ts[i-j]
  }
  data1[i] <- round(ma/12)
}
data1
data_ts

ets <- ets(data_ts)
summary(ets)
checkresiduals(ets)

forecast <- forecast(ets, h=12)
fore2022 <- round(forecast$mean[1:12])
test_ts <- ts(data_long$Value[97:108], start = c(2022, 1), end = c(2022,12), frequency = 12)
fore <- ts(fore2022, start=c(2022,1), frequency=12)
#plot(forecast)

mae <- mean(abs(test_ts - fore2022))
mape <- mean(abs((test_ts - fore2022) / test_ts)) * 100
rmse <- sqrt(mean((test_ts - fore2022)^2))

cat(mae, mape, rmse)

extended_time <- ts(c(data_ts, test_ts), start = start(data_ts), frequency = 12)
# Plot the extended time series (this will include both original and forecasted data)
plot(extended_time, type = "n", xlab = "Time", ylab = "Values",
     main = "Original vs Forecasted Values", cex.main=1.5, cex.lab=1.5, cex.axis=1.3)
# Add confidence interval
lines(data_ts, col = "blue", lwd = 1.5)
# Add the forecasted data with lines and dots
lines(test_ts, col = "red", lwd = 1.5, type = "o", pch = 20, cex=0.5)
lines(fore, col = "green", lwd = 1.5, type = "o", pch = 20, cex=0.5)
#legend("topleft", legend = c("Actual", "Forecast", "80% Confidence Interval"),
#col = c("blue", "red", rgb(1, 0, 0, 0.2)), lwd = c(2, 2, 10),
#cex = 0.8)
