data <- read.csv("C:/Users/Diya/OneDrive/Documents/killed.csv", header = TRUE, sep=",")
data <- data[1:ncol(data)-1]
# Load necessary library
library(reshape2)

# Step 3: Melt the data into a long format
data_long <- melt(data, id.vars = "X", variable.name = "Month", value.name = "Value")

# Convert the 'Month' to a factor with levels in correct order
data_long$Month <- factor(data_long$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
data_long

# Step 4: Sort the data by Year and Month
data_long <- data_long[order(data_long$X, data_long$Month), ]
data_long

# Step 5: Create a time series object with a frequency of 12
start_year <- data_long$X[1]
data_ts <- ts(data_long$Value, start = c(start_year, 1), frequency = 12)

#par(mar=c(5, 4, 4, 8) + 0.1)
par(cex.lab=1, cex.axis=1, cex.main=1)

# Step 6: Verify the time series object
library(ggplot2)
library(tseries)
library(forecast)
print(data_ts)
class(data_ts)
autoplot(data_ts)
years <- unique(floor(time(data_ts)))
seasonplot(data_ts,col=unique(rainbow(9)), year.labels=FALSE,season.labels=TRUE,main="",
           xlab = NULL, ylab = "Road Accidents", lwd=1.4)
legend("bottomright", legend=years, col=unique(rainbow(9)),lty=1, cex=0.35,xpd=TRUE,lwd=3)
data_log <- log(data_ts)
plot(data_log)


acf(data_ts)
acf(data_log)
pacf(data_ts)
pacf(data_log)
plot(decompose(data_ts))
plot(decompose(data_log))

library(tseries)
adf.test(data_ts)
#P = 0.024
pp.test(data_ts)
#p-value = 0.01
kpss.test(data_ts)
#p-value = 0.1 (printed value greated than 1)
adf.test(data_ts, k=0)
#with k=0 (df test), dickey-fuller value is smaller
adf.test(data_log)
#Dickey-Fuller = -3.6548, Lag order = 4, p-value = 0.03157
pp.test(data_log)
#Dickey-Fuller Z(alpha) = -51.968, Truncation lag parameter = 4, p-value = 0.01
kpss.test(data_log)
#KPSS Level = 0.67503, Truncation lag parameter = 4, p-value = 0.01582

data_ts <- tsclean(data_ts)
auto_model <- auto.arima(data_ts)
summary(auto_model)
data_log <- tsclean(data_log)
par(mar=(c(1, 1, 1, 1)))
tsdiag(auto_model)
auto_model_log <- auto.arima(data_log)
#what do the coefficient values means?
summary(auto_model_log)
library(lmtest)
coeftest(auto_model)
ts.plot(data_ts)
fitted(auto_model)
auto_model_fit <- data_ts - resid(auto_model)
points(auto_model_fit,type='l',col=10,lwd=1.5)
predicted_arima111 <- forecast(auto_model,n=12)
forecast(predicted_arima111)
plot(forecast(auto_model))
#fitted(auto_model)

#probably wont be needed
shapiro.test(data_ts) #10^-11 range
shapiro.test(data_log)  #10^-16 range
var(data_ts) #26229586
var(data_log) #.038...

#install.packages("tsoutliers")
par(mar=(c(5.1, 4.1, 4.1, 2.1)))
library(tsoutliers)
outliers <- tsoutliers(data_ts,lambda=NULL)
outlier_positions <- outliers$index
outlier_values <- data_ts[outlier_positions]
print(outliers)
summary(outliers)
ts.plot(data_ts, main="Original data with outliers")
points(outliers$index,data_ts[outliers$index],col='red',pch = 20)
outliers1<-tso(data_ts)
outliers1
plot(outliers1)
summary(outliers1)
ts.plot(data_ts, col='black', lwd=2)
points(outlier_positions, outlier_values, col="red", pch=19, cex=10)
#tsclean() removes outliers identified by tsoutliers()

#BoxCox transforms your target variable so that your data closely resembles a normal distribution
no_outlier_boxcox <-  tsclean(data_ts, lambda = "auto")
plot(data_ts, col='black', lwd=2)
lines(no_outlier_boxcox, col = "orange", lwd=2)
title(main = "Box Cox applied")


boxplot(data_ts,horizontal=T)
boxplot(data_ts)

par(mar=c(1,1,1,1))
tsdiag(auto_model)

model1 <- arma(data_ts,order=c(1,1))
summary(model1)
model2 <- arma(data_log,order=c(1,0))
summary(model2)

exp_smooth <- arima(data_ts, order= c(0,1,1))
summary(exp_smooth)
randomwalk <- arima(data_ts, order=c(0,1,0))
summary(randomwalk)

library(prophet)
data_df <- data.frame(
  #time = time(data_ts),
  year = as.numeric(floor(time(data_ts))),
  month = as.numeric(cycle(data_ts)),
  value = as.numeric(data_ts)
)
data_df
typeof(data_ts)
class(data_df)

data_df$year <- data_df$year + ifelse(data_df$month == 1 & c(0, diff(data_df$month)) == -11, 1, 0)

# Create a date column in the format 'YYYY-MM'
data_df <- data_df %>%
  mutate(ds = as.Date(paste(year, month, "01", sep = "-"), format = "%Y-%m-%d")) %>%
  select(ds, y = value)
data_df
plot(data_df)

bench <- 11528 - 1.5*IQR(data_df$y)
bench 
data_df$y[data_df$y < bench] <- bench
data_df

m<- prophet(yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)
m<-fit.prophet(m,data_df)
future<-make_future_dataframe(m,periods=12,freq="month")
forecast<-predict(m,future)
plot(m,forecast)
prophet_plot_components(m, forecast)

#model performance
pred<-forecast$yhat[1:108] #108 as last 12 values are predicted
actual<-data_df[,2]
plot(actual,pred)
abline(lm(pred~actual),col='red') #line of best fit linear model
summary(lm(pred~actual))

m <- prophet(n.changepoints = 108,
             weekly.seasonality = TRUE,daily.seasonality = TRUE,yearly.seasonality = TRUE)
m<-add_country_holidays(m,country_name = "IN")
m<- fit.prophet(m,data_df)
future<-make_future_dataframe(m,periods=12,freq="month")
forecast<-predict(m,future)
plot(m,forecast)
pred<-forecast$yhat[1:108] #108 as last 12 values are predicted
actual<-data_df[,2]
plot(actual,pred)
abline(lm(pred~actual),col='red') #line of best fit linear model
summary(lm(pred~actual))
