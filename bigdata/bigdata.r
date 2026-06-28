library(tidyverse)
library(caret)
library(randomForest)
library(pROC)


hotel_bookings <- read.csv("/Users/shadaabhasan/Coding/Big-data-hotel-booking-analysis/bigdata/hotel_bookings.csv", stringsAsFactors = FALSE)
cat("Rows:", nrow(hotel_bookings), " | Market segments:", n_distinct(hotel_bookings$market_segment), "\n")

hotel_bookings$children[is.na(hotel_bookings$children)] <- 0


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

# Evaluation 
rf_pred <- predict(rf_model, test_data)
rf_prob <- predict(rf_model, test_data, type = "prob")[, 2]

confusionMatrix(rf_pred, test_data$is_canceled, positive = "1")
rf_auc <- auc(roc(response = test_data$is_canceled, predictor = rf_prob))
cat("Random Forest AUC:", round(rf_auc, 3), "\n")

varImpPlot(rf_model, n.var = 10)

# Logistic Regression baseline 
log_model <- glm(is_canceled ~ ., data = train_data, family = binomial)

log_prob <- predict(log_model, test_data, type = "response")
log_pred <- factor(ifelse(log_prob > 0.5, "1", "0"), levels = c("0", "1"))

confusionMatrix(log_pred, test_data$is_canceled, positive = "1")
log_auc <- auc(roc(response = test_data$is_canceled, predictor = log_prob))
cat("Logistic Regression AUC:", round(log_auc, 3), "\n")

# Comparison
cat("\nModel comparison (test set):\n")
cat("Random Forest      AUC:", round(rf_auc, 3), "\n")
cat("Logistic Regression AUC:", round(log_auc, 3), "\n")

# Threshold tuning (Random Forest) 
# rf_prob = P(cancel) on the test set, already computed
actual <- test_data$is_canceled   # factor with levels "0","1"

thresholds <- seq(0.05, 0.95, by = 0.05)

sweep <- map_dfr(thresholds, function(t) {
  pred <- factor(ifelse(rf_prob >= t, "1", "0"), levels = c("0", "1"))
  tp <- sum(pred == "1" & actual == "1")
  fp <- sum(pred == "1" & actual == "0")
  fn <- sum(pred == "0" & actual == "1")
  precision <- ifelse((tp + fp) == 0, NA, tp / (tp + fp))
  recall    <- tp / (tp + fn)
  f1        <- ifelse(is.na(precision) | (precision + recall) == 0, NA,
                      2 * precision * recall / (precision + recall))
  tibble(threshold = t, precision = precision, recall = recall, f1 = f1)
})

print(sweep)

# Threshold that maximises F1 (balanced precision/recall)
best_f1 <- sweep %>% filter(!is.na(f1)) %>% slice_max(f1, n = 1)
cat("\nBest-F1 threshold:", best_f1$threshold,
    "| precision:", round(best_f1$precision, 3),
    "| recall:", round(best_f1$recall, 3),
    "| F1:", round(best_f1$f1, 3), "\n")

# Precision–recall vs threshold plot
sweep %>%
  pivot_longer(c(precision, recall, f1), names_to = "metric", values_to = "value") %>%
  ggplot(aes(threshold, value, colour = metric)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = best_f1$threshold, linetype = "dashed") +
  labs(title = "Precision, Recall and F1 vs Classification Threshold",
       subtitle = "Random Forest — cancellation prediction",
       x = "Threshold", y = "Score") +
  theme_minimal()


# Partial dependence: how features drive cancellation risk 
library(pdp)

# Lead time — continuous, so we get a readable risk curve.
# which.class = "1" tells pdp to plot probability of cancellation.
pd_lead <- partial(rf_model,
                   pred.var = "lead_time",
                   which.class = "1",
                   prob = TRUE,
                   train = train_data)

plotPartial(pd_lead,
            main = "Partial Dependence: Lead Time vs Cancellation Risk",
            ylab = "Predicted P(cancel)", xlab = "Lead time (days)")

# Market segment — categorical, shows risk level per segment
pd_seg <- partial(rf_model,
                  pred.var = "market_segment",
                  which.class = "1",
                  prob = TRUE,
                  train = train_data)

plotPartial(pd_seg,
            main = "Partial Dependence: Market Segment vs Cancellation Risk",
            ylab = "Predicted P(cancel)")
