library(zoo)

data <- read.csv("C:/Users/Diya/OneDrive/Documents/accidents.csv", sep=",", row.names = NULL)
data1 <- read.zoo(data)
class(data1)
data_ts <- as.ts(data1)
data_ts
class(data_ts)
plot(data_ts)


year<- data$X
data <- data[1:ncol(data)-1]
data <- data[,-1]

data
data_matrix <- as.matrix(data)
data2 <- ts(data,frequency=12, start = c(2014,1))
data2
start_year <- year[1]
data_ts <- ts(data_matrix, start = c(start_year, 1), frequency = 12)
data_ts
decompose(data_ts)
