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
#data <- as.data.frame(data)
# Step 3: Melt the data into a long format
data_long <- melt(data, id.vars = "X", variable.name = "Month", value.name = "Value")

# Convert the 'Month' to a factor with levels in correct order
data_long$Month <- factor(data_long$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

# Step 4: Sort the data by Year and Month
data_long <- data_long[order(data_long$X, data_long$Month), ]

start_year <- data_long$X[1]
data_ts <- ts(data_long$Value, start = c(start_year, 1), end=c(2021,12) ,frequency = 12)
data_ts

boxplot(data_long$Value~data_long$Month, data = data_long,
        ylab = "Monthly number of road accidents", 
        xlab = "Months (2014-2022)",drop = TRUE)


ets(data_ts)  #ETS(A,N,A)
#  AIC     AICc      BIC 
#2278.299 2283.517 2318.531 

fit_ses <- ses(data_ts)
accuracy(fit_ses)
fit_holt <- HoltWinters(data_ts, beta = TRUE, gamma = FALSE)  
#accuracy(fit_holt) why does this give error?
#if beta set to false, it will perfrom exponential smoothing
#gamma used for seasonal component. if set to false, a non-seasonal model is fitted

fit_damped <- ets(data_ts, model = "AAN", damped = TRUE)
accuracy(fit_damped)

# Generate forecasts
fc_ses <- forecast(fit_ses)
fc_holt <- forecast(fit_holt, h = 12)
fc_damped <- forecast(fit_damped, h = 12)

# Accuracy measures
accuracy_ses <- accuracy(fc_ses)
accuracy_holt <- accuracy(fc_holt)
accuracy_damped <- accuracy(fc_damped)

fitted_ses <- fc_ses$fitted
fitted_holt <- fc_holt$fitted
fitted_damped <- fc_damped$fitted
forecasts_ses <- fc_ses$mean
forecasts_holt <- fc_holt$mean
forecasts_damped <- fc_damped$mean

print(accuracy_ses)
print(accuracy_holt)
print(accuracy_damped)
#what does acf descibe here?

autoplot(data_ts) +
  autolayer(fc_ses$fitted, series = "SES", PI= FALSE) +  #just original plot + ses
  autolayer(fitted(fit_holt), series = "Holt", PI = FALSE) +  # Extract fitted values directly
  #gave just original plot, level. trend, xhat
  autolayer(fc_damped$fitted, series = "Damped", PI = FALSE) +  #just original + damped
  labs(title = "Model Comparison",
       y = "Value",
       x = "Time") +
  guides(colour = guide_legend(title = "Models"))

plot(decompose(data_ts))
data_ts
acf(data_ts, 60)
pacf(data_ts)   
adf.test(data_ts)   #no unit root found
arima1 <- auto.arima(data_ts)
arima1
summary(arima1)

data1 <- diff(data_ts)
plot(decompose(data1))
acf(data1)
pacf(data1)
adf.test(data1)  #no unit root found
arima2 <- auto.arima(data1)
arima2

data_ts1 <-tsclean(data_ts)
plot(decompose(data_ts1))
acf(data_ts1)
arima3 <- auto.arima(data_ts1)
summary(arima3)
plot(forecast(arima3))
arima3[['x']]
arima3[['fitted']]
checkresiduals(arima3)
adf.test(data_ts1)
data_ts1 <- diff(data_ts1)
adf.test(data_ts1)  #now stationary
arima3 <- auto.arima(data_ts1)
arima3
summary(arima3)
plot(forecast(arima3))
checkresiduals(arima3)

outliers <- tsoutliers(data_ts,lambda=NULL)
outliers
outlier_positions <- outliers$index
outlier_values <- data_ts[outlier_positions]
print(outlier_values)
print(outliers)
data_ts[outliers$index] <- data_ts[outliers$replacements]  #why did the indices become null?????
data_ts[76] = 23696.24
data_ts[89] = 33628.12
data_ts
summary(outliers)

autoplot(tsclean(data_ts), series="clean", color='red', lwd=0.9) +
  autolayer(data_ts, series="original", color='gray', lwd=1) +
  geom_point(data = tsoutliers(data_ts) %>% as.data.frame(),
             aes(x=index, y=replacements), col='black')

arima4 <- auto.arima(data_ts, d=1, D=1)
checkresiduals(arima4)
arima4
arima2 <- auto.arima(data1, D=1)
arima2
checkresiduals(arima2)

no_outlier_boxcox <-  tsclean(data_ts, lambda = NULL)
plot(no_outlier_boxcox)
adf.test(no_outlier_boxcox)
adf.test(diff(no_outlier_boxcox))  #not stationary initially, stationary after diff by 1
kpss.test(diff(no_outlier_boxcox))
arima5 <- auto.arima(no_outlier_boxcox)
checkresiduals(arima5)

DecompTseries<-decompose(data_ts)
ts.stl<-stl(data_ts,"periodic")  # decompose the TS
ts.sa<-seasadj(ts.stl) #de-seasonalize
plot(ts.sa)
acf(DecompTseries$seasonal)
autoplot(data_ts, col="blue", xlab = "Year", ylab = "Frequency")
ndiffs(data_ts)  #1

RoadAccident_seasdiff <- diff(data_ts, differences=1)  # seasonal differencing
plot(RoadAccident_seasdiff, type="l", main="Seasonally Differenced",col="blue",xlab="Year")  # still not stationary!
autoplot(RoadAccident_seasdiff)
### test for stationary using Difference series
kpss.test(RoadAccident_seasdiff)

ACFSEA<- acf(RoadAccident_seasdiff) # ACF plot
PACFSEA<- pacf(RoadAccident_seasdiff)  # PACF plot
stationaryTS <- diff(RoadAccident_seasdiff, differences= 1)
plot(stationaryTS, type="l", main="Differenced and Stationary",col="red")  # appears to be stationary
pacf(stationaryTS) 

SARIMA1<-Arima(data_ts, order = c(0,1,3), 
               seasonal = list(order = c(1,0,2), period = 12), 
               include.mean = TRUE,include.drift = FALSE)
# Coefficient test
coeftest(SARIMA1)
## Goodness of fit
summary(SARIMA1) # accuracy test
SARIMA1

#install.packages("qqplotr")
tsdisplay(residuals(arima1))
tsdiag(arima1)
?Box.test
library(qqplotr)
qqplot(arima1$residual) # Informal test of normality
lillie.test(SARIMA4$resid) # Formal test of normality
pacf(SARIMA4$resid,col="red")
acf(SARIMA4$resid,col="green")

p <- predict(arima1, n.ahead=24)
plot(p)

march <- mean(c(data_ts[3],data_ts[15],data_ts[27],
           data_ts[39],data_ts[51], data_ts[63]))
march
april <- mean(c(data_ts[4],data_ts[16],data_ts[28],
                data_ts[40],data_ts[52], data_ts[64], data_ts[76]))
april

data_ts[76] = april
plot(data_ts)
arima7 <- auto.arima(data_ts)
arima7
checkresiduals(arima7)
