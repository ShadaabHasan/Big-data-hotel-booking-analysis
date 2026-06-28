# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(forecast)

# Load the dataset
df <- read.csv("data/hotel_bookings.csv", stringsAsFactors = FALSE)

# Convert the arrival date columns into a single date column
df$arrival_date <- make_date(df$arrival_date_year, match(df$arrival_date_month, month.name), df$arrival_date_day_of_month)

# Aggregate data to get monthly bookings
df <- df %>%
  mutate(bookings = 1) %>%
  group_by(year = year(arrival_date), month = month(arrival_date)) %>%
  summarize(total_bookings = sum(bookings), .groups = "drop")

# Create a time series object for the total bookings
ts_data <- ts(df$total_bookings,
              start = c(df$year[1], df$month[1]),
              frequency = 12)
# Plot the time series
autoplot(ts_data) +
  ggtitle("Monthly Hotel Bookings Over Time") +
  xlab("Year") + ylab("Forecasted Bookings")

# Train a forecasting model (ARIMA)
model <- auto.arima(ts_data)

# Forecast for the next 6 months
forecast_result <- forecast(model, h = 6)

# Plot the forecast
autoplot(forecast_result) +
  ggtitle("6-Month Forecast of Hotel Bookings") +
  xlab("Year") + ylab("Forecasted Bookings")

# Print the forecasted bookings
print(forecast_result)

