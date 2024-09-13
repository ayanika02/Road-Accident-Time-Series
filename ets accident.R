library(reshape2)
library(fpp3)
library(zoo)
library(forecast)
library(feasts)
library(stats)
library(tseries)
install.packages("AICcmodavg")
library(AICcmodavg)

# Step 1: Read the CSV file
data <- read.csv("C:/Users/Diya/OneDrive/Documents/accidents.csv", header = TRUE, sep=",")
data <- data[1:ncol(data)-1]

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
data_ts

ets(data_ts)  #ETS(A,N,A)
#  AIC     AICc      BIC 
#2278.299 2283.517 2318.531 

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
  AICc = aicc_values
)

# Display the results
print(results)

a_add_holtwinter
checkresiduals(a_add_holtwinter)

default <- ets(data_ts)
default
plot(forecast(default, h=24))
checkresiduals(default)

plot(forecast(m_damped_mult_holtwinter))
checkresiduals(m_damped_mult_holtwinter)

