# Load the necessary libraries
library(ggplot2)
library(forecast)
library(zoo)
library(dplyr)

# Create the data frame
data <- data.frame(
  Date = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01", "2023-04-01", "2023-05-01")),
  Sales = c(100, 120, 150, 130, 140)
)

# Convert the data to a time series object
ts_data <- ts(data$Sales, start = c(2023, 1), frequency = 12)

# 1. Line plot of Sales over time
ggplot(data, aes(x = Date, y = Sales)) +
  geom_line(color = "blue") +
  labs(title = "Sales Over Time", x = "Date", y = "Sales")

# 2. Bar plot showing monthly average Sales
data$Month <- format(data$Date, "%Y-%m")
monthly_avg_sales <- data %>%
  group_by(Month) %>%
  summarise(Average_Sales = mean(Sales))

ggplot(monthly_avg_sales, aes(x = Month, y = Average_Sales)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Monthly Average Sales", x = "Month", y = "Average Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Example: Decomposing a time series with STL

# Generate example time series data (replace with your actual data)
ts_data <- ts(rnorm(100), start = c(2010, 1), frequency = 12)  # Monthly data example

# Check plot to inspect the data
plot(ts_data)

# Decompose using STL
decomposed <- stl(ts_data, s.window = "periodic")

# Plot decomposed components
plot(decomposed)

ts_data <- ts(rnorm(100), start = c(2010, 1), frequency = 12)  # Monthly data example

# Lag plot function
lag.plot <- function(x, lag = 1, col = "blue", main = NULL) {
  plot(x[-length(x)], x[-1], col = col, main = main,
       xlab = "Y(t)", ylab = paste("Y(t +", lag, ")"))
}

# Create lag plot
lag.plot(ts_data, lag = 1, col = "blue", main = "Lag Plot of Time Series")

# 5. Time series plot with a smoothed line of Sales
ggplot(data, aes(x = Date, y = Sales)) +
  geom_line(color = "blue") +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(title = "Time Series Plot with Smoothed Line of Sales", x = "Date", y = "Sales")
