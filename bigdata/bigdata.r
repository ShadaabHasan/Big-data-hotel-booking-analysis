library(tidyverse)
library(caret)
library(randomForest)
hotel_bookings <- read.csv("G:\\Programming\\BDA project\\bigdata\\8952620\\hotel_bookings.csv")
hotel_bookings %>% 
  select(-babies, -distribution_channel, -agent, -company, -required_car_parking_spaces, -total_of_special_requests)
hotel_bookings<-hotel_bookings%>%
  mutate(
    hotel=as.factor(hotel),      
    is_canceled=as.factor(is_canceled),
    meal=as.factor(meal),
    country=as.factor(country),
    market_segment=as.factor(market_segment),
    distribution_channel=as.factor(distribution_channel),
    is_repeated_guest=as.factor(is_repeated_guest),
    reserved_room_type=as.factor(reserved_room_type),
    assigned_room_type=as.factor(assigned_room_type),
    deposit_type=as.factor(deposit_type),
    customer_type=as.factor(customer_type),
    reservation_status=as.factor(reservation_status),
    agent=as.factor(agent),
    company=as.factor(company),
    arrival_date_day_of_month=as.factor(arrival_date_day_of_month),
    arrival_date_month=as.factor(arrival_date_month),
    arrival_date_year=as.factor(arrival_date_year)
    
  )
hotel_bookings <- hotel_bookings %>% 
  mutate(total_nights = stays_in_weekend_nights + stays_in_week_nights, total_cost = adr * total_nights)
summary(hotel_bookings$total_nights)
summary(hotel_bookings$total_cost)

ggplot(hotel_bookings, aes(x = total_nights, y = total_cost,shape=hotel,colour=is_canceled))+geom_point(shape=3)

ggplot(hotel_bookings, aes(x = total_nights, y = total_cost,shape=hotel,colour=is_canceled))+geom_point(shape=6)+facet_wrap(~market_segment)+scale_color_manual(values = c("#7eb0d5", "#b2e061"))
                                                                                                                                              
                                                                                                                                                           
ggplot(hotel_bookings, aes(x=arrival_date_year, fill=is_canceled))+geom_bar()+scale_fill_manual(values = c("#7eb0d5","#b2e061"))


ggplot(hotel_bookings, aes(x=assigned_room_type, fill=is_canceled))+geom_bar()+scale_fill_manual(values = c("#7eb0d5","#b2e061"))


ggplot(hotel_bookings, aes(x = market_segment, fill=is_canceled))+geom_bar()+scale_fill_manual(values = c("#7eb0d5","#b2e061"))

ggplot(hotel_bookings, aes(x = distribution_channel, fill =is_canceled))+geom_bar()+scale_fill_manual(values = c("#7eb0d5","#b2e061"))
ggplot(hotel_bookings, aes(x = reserved_room_type, fill = is_canceled))+geom_bar()+scale_fill_manual(values = c("#7eb0d5","#b2e061"))

ggplot(hotel_bookings, aes(x = customer_type, fill = is_canceled))+geom_bar()+scale_fill_manual(values = c("#7eb0d5","#b2e061"))

hotel_bookings %>%
  filter(days_in_waiting_list>2)%>%
  ggplot(aes(x=days_in_waiting_list,fill=is_canceled))+
  geom_histogram(binwidth = 10)+scale_fill_manual(values = c("#7eb0d5","#b2e061"))

cancelledlist <- hotel_bookings[hotel_bookings$Canceled==1,]
notcancelledlist <- hotel_bookings[hotel_bookings$Not_Canceled==0,]

mytable <- table(hotel_bookings$Canceled)


data_model <- hotel_bookings %>%
  select(is_canceled, lead_time, hotel, arrival_date_month, stays_in_week_nights, stays_in_weekend_nights, 
         adults, children, babies, meal, market_segment, distribution_channel, customer_type)

data_model$hotel <- as.factor(data_model$hotel)
data_model$customer_type <- as.factor(data_model$customer_type)
data_model$arrival_date_month <- as.factor(data_model$arrival_date_month)

set.seed(123)  # Ensure reproducibility
train_index <- createDataPartition(data_model$is_canceled, p = 0.7, list = FALSE)
train_data <- data_model[train_index, ]
test_data <- data_model[-train_index, ]


set.seed(123)
rf_model <- randomForest(is_canceled ~ ., data = train_data, ntree = 500, mtry = 3, importance = TRUE)

# Check for missing values in the dataset
colSums(is.na(hotel_bookings))

preprocess_params <- preProcess(hotel_bookings, method = 'medianImpute')
hotel_bookings <- predict(preprocess_params, hotel_bookings)

colSums(is.na(hotel_bookings))

hotel_bookings <- hotel_bookings %>%
  mutate(across(where(is.factor), ~ ifelse(is.na(.), as.character(stat::Mode(.)), .)))

set.seed(123)
train_index <- createDataPartition(hotel_bookings$is_canceled, p = 0.7, list = FALSE)
train_data <- hotel_bookings[train_index, ]
test_data <- hotel_bookings[-train_index, ]

set.seed(123)
rf_model <- randomForest(is_canceled ~ ., data = train_data, ntree = 500, mtry = 3, importance = TRUE)
















