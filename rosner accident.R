install.packages('EnvStats')
library(EnvStats)

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

test <- rosnerTest(data_long$Value, k = 7)
test
rosner_out <- c(76,77,89)
data1 <- data_ts
for (i in rosner_out){
      data1[i] <- data_ts[60 + (i%%12)]
}
data1
adf.test(data1)
kpss.test(data1)
pp.test(data1)

decompose <- decompose(data1)
plot(decompose)
arima <- auto.arima(data1, seasonal = TRUE, stationary= FALSE, trace=TRUE)
summary(arima)
checkresiduals(arima)
forecast <- forecast(arima, h=12)
fore2022 <- forecast$mean
test_ts <- ts(data_long$Value, start = c(2022, 1), end = c(2022,12), frequency = 12)
plot(forecast)

mae <- mean(abs(test_ts - fore2022))
mape <- mean(abs((test_ts - fore2022) / test_ts)) * 100
rmse <- sqrt(mean((test_ts - fore2022)^2))

cat(mae, mape, rmse)

ets <- ets(data1)
summary(ets)
forecast <- forecast(ets, h=12)
plot(forecast)
checkresiduals(ets)