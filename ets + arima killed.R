library(reshape2)
library(fpp3)
library(zoo)
library(forecast)
library(feasts)
library(stats)
library(tseries)
#install.packages("AICcmodavg")
library(AICcmodavg)

# Step 1: Read the CSV file
data <- read.csv("C:/Users/Diya/OneDrive/Documents/killed.csv", header = TRUE, sep=",")
data <- data[1:ncol(data)-1]

# Step 3: Melt the data into a long format
data_long <- melt(data, id.vars = "X", variable.name = "Month", value.name = "Value")

# Convert the 'Month' to a factor with levels in correct order
data_long$Month <- factor(data_long$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

# Step 4: Sort the data by Year and Month
data_long <- data_long[order(data_long$X, data_long$Month), ]

boxplot(data_long$Value~data_long$Month, data = data_long,
        ylab = "Monthly number of road accidents", 
        xlab = "Months (2014-2022)",drop = TRUE)

# Step 5: Create a time series object with a frequency of 12
start_year <- data_long$X[1]
data_ts <- ts(data_long$Value, start = c(start_year, 1), frequency = 12)

ets(data_ts)  #ETS(A,N,A)
#  AIC     AICc      BIC 
#2045.049 2050.266 2085.281 

#additive errors
a_ses <- ets(data_ts, model = "ANN")
a_holts_linear <- ets(data_ts, model = "AAN")
a_additive_damped <- ets(data_ts, model = "AAN", damped=TRUE)
a_add_holtwinter <- ets(data_ts, model = "AAA")

#multiplicative errors
m_ses <- ets(data_ts, model = "MNN")
m_holts_linear <- ets(data_ts, model = "MAN")
m_additive_damped <- ets(data_ts, model = "MAN", damped=TRUE)
m_add_holtwinter <- ets(data_ts, model = "MAA")
m_mult_holtwinter <- ets(data_ts, model = "MAM", damped=FALSE)
m_damped_mult_holtwinter <- ets(data_ts, model = "MAM", damped=TRUE)

model_names <- c("a_ses", "a_holts_linear", "a_additive_damped", "a_add_holtwinter",
                 "m_ses", "m_holts_linear", "m_additive_damped", "m_add_holtwinter",
                 "m_mult_holtwinter", "m_damped_mult_holtwinter")

models <- list(a_ses, a_holts_linear, a_additive_damped, a_add_holtwinter,
               m_ses, m_holts_linear, m_additive_damped, m_add_holtwinter,
               m_mult_holtwinter, m_damped_mult_holtwinter)

aic_values <- sapply(models, AIC)
bic_values <- sapply(models, BIC)

ljung_box_p_values <- sapply(models, function(model) {
  residuals <- residuals(model)
  Box.test(residuals, lag = log(length(residuals)))$p.value
})
                   
# Calculate AICc values
aicc_values <- sapply(models, function(model) {
  aic <- AIC(model)
  k <- length(coef(model))
  n <- length(model$x)
  aic + (2 * k * (k + 1)) / (n - k - 1)
})

# Create a data frame
results <- data.frame(
  Model = model_names,
  AIC = aic_values,
  BIC = bic_values,
  AICc = aicc_values,
  Ljung_Box = ljung_box_p_values
)

# Display the results
print(results)

default <- ets(data_ts)
default
plot(forecast(default, h=24))
checkresiduals(default)

plot(forecast(m_damped_mult_holtwinter))
checkresiduals(m_damped_mult_holtwinter)

fit.arima <- auto.arima(data_ts, trace=TRUE)
checkresiduals(fit.arima)
plot(forecast(fit.arima))
kpss.test(data_ts)
data1 <- diff(data_ts)
adf.test(data1)
kpss.test(data1)  #fails stationarity
data2 <- diff(data1)
kpss.test(data2)
plot(data1)

#outliers1<-tso(data_ts)
#outliers <- tsoutliers(data_ts,lambda=NULL)
#outlier_positions <- outliers$index
#outlier_values <- data_ts[outlier_positions]

plot(data_ts)
kpss.test(data_ts)
adf.test(data_ts)

no_outlier_boxcox <-  tsclean(data_ts, lambda = NULL)
plot(no_outlier_boxcox)
adf.test(no_outlier_boxcox)  #stationary
kpss.test(no_outlier_boxcox)
arima2 <- auto.arima(no_outlier_boxcox, ic = c("aicc", "aic", "bic"),
                     test = c("kpss", "adf", "pp"), trace=TRUE)
checkresiduals(arima2)
#satisfies

arima3 <- auto.arima(data_ts, ic = c("aicc", "aic", "bic"),
                     test = c("kpss", "adf", "pp"), trace=TRUE)
checkresiduals(arima3)
