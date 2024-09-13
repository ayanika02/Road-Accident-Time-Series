library(ggplot2)

# Convert the time series object to a data frame
data_df <- data.frame(Time = as.Date(time(data_ts)), Number_of_Accidents = as.numeric(data_ts))

# Plotting the data
ggplot(data_df, aes(x = Time, y = Number_of_Accidents)) +
  geom_line() +  # Add the line
  geom_point() + # Add points at each data value
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Show every year on x-axis
  labs(y = "Number of Accidents", x = "Year") +  # Rename y-axis and label x-axis
  theme_minimal()  # Use a minimal theme

data_ts