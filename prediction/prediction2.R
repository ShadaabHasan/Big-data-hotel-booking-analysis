# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(forecast)

# Load the dataset
df <- read.csv("hotel_bookings.csv")

# Check the structure and column names
str(df)
print(colnames(df))

# Print a sample of the first few rows to inspect the dataset
print(head(df))

# Convert columns to numeric where necessary
df$arrival_date_year <- as.numeric(as.character(df$arrival_date_year))
df$arrival_date_month <- as.numeric(as.character(df$arrival_date_month))

# Create the arrival_date column using only valid numeric data
df <- df %>%
  filter(!is.na(arrival_date_year) & !is.na(arrival_date_month)) %>%
  mutate(arrival_date = as.Date(paste(arrival_date_year, arrival_date_month, "01", sep = "-")))

# Check if arrival_date was created successfully
if ("arrival_date" %in% colnames(df)) {
  print(head(df$arrival_date))  # Verify the first few dates
} else {
  stop("arrival_date column was not created successfully.")
}

# Aggregate the data to get monthly total bookings
df_monthly <- df %>%
  group_by(year = year(arrival_date), month = month(arrival_date)) %>%
  summarize(total_bookings = n(), .groups = 'drop')

# Check if df_monthly has data
if (nrow(df_monthly) == 0) {
  stop("No data available in df_monthly after filtering and aggregation.")
}

# Create a time series object for total bookings
ts_data <- ts(df_monthly$total_bookings, start = c(min(df_monthly$year), min(df_monthly$month)), frequency = 12)

# Predict the next 5 months of hotel bookings
model <- auto.arima(ts_data)
forecast_result <- forecast(model, h = 5)

# Plot the forecast for the next 5 months
autoplot(forecast_result) +
  ggtitle("5-Month Forecast of Hotel Bookings") +
  xlab("Year") + ylab("Forecasted Bookings")

# Check for future bookings by aggregating data for the next 5 months
future_bookings <- df %>%
  filter(arrival_date >= Sys.Date()) %>%
  group_by(room_type) %>%  # Replace with actual room type column name
  summarize(total_future_bookings = n(), .groups = 'drop') %>%
  arrange(desc(total_future_bookings))

# Check if future_bookings has data
if (nrow(future_bookings) == 0) {
  stop("No future bookings data available.")
}

# Print the most booked room types in descending order
cat("Room types booked in the next 5 months (most to least):\n")
print(future_bookings)

