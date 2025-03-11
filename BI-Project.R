# installing packages
install.packages("lubridate")
install.packages(c("corrplot"))
install.packages(c("caret","tidymodels"))  
install.packages(c("randomForest"))


# import libraries
library(ggplot2)
library(lubridate)
library(zoo)
library(dplyr)
library(corrplot)
library(caret)
library(tidymodels) 
library(randomForest)

# setting to the current working directory
setwd("C:/Users/smith/OneDrive/Desktop/BI")

--------------------------------------------------------------------------------
# load the data
--------------------------------------------------------------------------------

# get the names of all csv files in the data
all_files <- list.files(pattern = "*.csv")
all_files

# read each csv file
data_list <- lapply(all_files, read.csv)

# merge the data
merged_data <- do.call("rbind", data_list)

# first 5 records
head(merged_data, 5)

# last 5 records
tail(merged_data, 5)

--------------------------------------------------------------------------------
# Data Processing and Validation
--------------------------------------------------------------------------------

# memory
data_size <- object.size(merged_data)
print(paste("Total memory occupied by data: ", format(data_size, units = "auto")))

# number of rows and columns
data_shape <- dim(merged_data)
print(paste("Number of rows:", data_shape[1]))
print(paste("Number of columns:", data_shape[2]))

# column names
column_names <- names(merged_data)
print("Column names of the data:")
print(column_names)

# internal structure of the data
str(merged_data)

# column data types
column_types <- sapply(merged_data, function(x) class(x))
print("Data Types of Each Column:")
print(column_types)

# na counts
nan_counts <- colSums(is.na(merged_data))
print("NaN count in each Column:")
print(nan_counts)

# drop columns having NaN
columns_to_drop <- c("CARRIER_DELAY", "WEATHER_DELAY", "NAS_DELAY", "SECURITY_DELAY", "LATE_AIRCRAFT_DELAY", "Unnamed..27",
                     "DEP_TIME", "DEP_DELAY", "TAXI_OUT", "WHEELS_OFF", "WHEELS_ON", "TAXI_IN", "ARR_TIME", "ARR_DELAY",
                     "ACTUAL_ELAPSED_TIME", "AIR_TIME")
clean_data <- merged_data[, !(names(merged_data) %in% columns_to_drop)]
nan_counts <- colSums(is.na(clean_data))
print("NaN count after dropping columns:")
print(nan_counts)

# boxplot for CRS_ELAPSED_TIME
boxplot(clean_data$CRS_ELAPSED_TIME, 
        main = "Boxplot of CRS_ELAPSED_TIME",
        ylab = "CRS_ELAPSED_TIME",
        col = "lightblue",
        border = "black",
        notch = TRUE)
# Add labels and title
title(main = "Boxplot of CRS_ELAPSED_TIME")
ylabel <- "CRS_ELAPSED_TIME"
xlabel <- "Flights"
axis_labels <- c(ylabel, xlabel)
axis(1, at = 1, labels = axis_labels[2], col.axis = "blue", las = 1)
axis(2, col.axis = "red")
grid()

# impute CRS_ELAPSED_TIME with median
clean_data$CRS_ELAPSED_TIME <- na.aggregate(clean_data$CRS_ELAPSED_TIME, FUN = median)

# count of nans
nan_counts <- colSums(is.na(clean_data))
print("NaN count after imputation:")
print(nan_counts)

# final memory size
data_size <- object.size(clean_data)
print(paste("Total memory occupied by data: ", format(data_size, units = "auto")))

# number of rows and columns
data_shape <- dim(clean_data)
print(paste("Number of rows:", data_shape[1]))
print(paste("Number of columns:", data_shape[2]))

# column names
column_names <- names(clean_data)
print("Column names of the data:")
print(column_names)

--------------------------------------------------------------------------------
# EDA and Feature Engineering
--------------------------------------------------------------------------------

# bar plot for the 'CANCELLED' variable - Target Variable
ggplot(clean_data, aes(x = factor(CANCELLED))) +
  geom_bar(fill = "skyblue", color = "darkblue") +
  labs(title = "Distribution of Flight Cancellations", x = "Cancelled", y = "Count") +
  theme_minimal()

--------------------------------------------------------------------------------
# FL_DATE
--------------------------------------------------------------------------------
# sort the data based on FL_DATE in ascending order
clean_data <- clean_data[order(clean_data$FL_DATE), ]
head(clean_data, 5)
tail(clean_data, 5)

# Features - extracting month from FL_DATE
clean_data$FL_Month <- month(clean_data$FL_DATE)
clean_data$FL_Month <- month.abb[clean_data$FL_Month]
head(clean_data, 5)

# EDA - Density plot of flight Date in months
clean_data$FL_Month <- factor(clean_data$FL_Month, levels = month.abb)
ggplot(clean_data, aes(x = FL_Month, fill = FL_Month)) +
  geom_density(alpha = 0.7) +
  labs(title = "Flight Date by Months", x = "Month", y = "Density") +
  theme_minimal()

# EDA - Flight cancellations by month
ggplot(clean_data, aes(x = FL_Month, fill = factor(CANCELLED))) +
  geom_density(alpha = 0.7) +
  labs(title = "Flight Cancellations by Month", x = "Month", y = "Density") +
  theme_minimal()

# Feature - Month cancellation rate
fl_month_cancellation_rate <- clean_data %>%
  group_by(FL_Month) %>%
  summarise(month_cancellation_rate = mean(CANCELLED == 1, na.rm = TRUE))
clean_data <- left_join(clean_data, fl_month_cancellation_rate, by = "FL_Month")

# EDA - Month Cancellation Rate by Cancellation Status
boxplot(month_cancellation_rate ~ CANCELLED, data = clean_data,
        main = "Month Cancellation Rate by Cancellation Status",
        xlab = "Cancelled",
        ylab = "Month Cancellation Rate",
        col = c("lightblue", "lightgreen"))

# Features - Weekend
clean_data$FL_DATE <- as.Date(clean_data$FL_DATE)
# 1 -> Weekend, 0 -> Weekday
clean_data$Weekend <- ifelse(weekdays(clean_data$FL_DATE) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

# EDA - Flight Cancellation by Weekend
ggplot(clean_data, aes(x = Weekend, fill = factor(CANCELLED))) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Flight Cancellation by Weekend",
       x = "Weekend", y = "Count") +
  theme_minimal()

# Feature - Weekend Cancellation Rate
fl_weekend_cancellation_rate <- clean_data %>%
  group_by(Weekend) %>%
  summarise(weekend_cancellation_rate = mean(CANCELLED == 1, na.rm = TRUE))
clean_data <- left_join(clean_data, fl_weekend_cancellation_rate, by = "Weekend")

# EDA - Weekend Cancellation Rate by Cancellation Status
boxplot(weekend_cancellation_rate ~ CANCELLED, data = clean_data,
        main = "Weekend Cancellation Rate by Cancellation Status",
        xlab = "Cancelled",
        ylab = "Weekend Cancellation Rate",
        col = c("lightblue", "lightgreen"))

# Features - Holidays
# 1 -> Holiday, 0 -> Not a Holiday
clean_data$Holiday <- ifelse(month(clean_data$FL_DATE) == 1 & day(clean_data$FL_DATE) == 1, "Holiday", "Not-Holiday")

# EDA - Flight Cancellation by Holiday
ggplot(clean_data, aes(x = Holiday, fill = factor(CANCELLED))) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Flight Cancellation by Holiday",
       x = "Holiday", y = "Count") +
  theme_minimal()

# Feature - Holiday Cancellation Rate
fl_holiday_cancellation_rate <- clean_data %>%
  group_by(Holiday) %>%
  summarise(holiday_cancellation_rate = mean(CANCELLED == 1, na.rm = TRUE))
clean_data <- left_join(clean_data, fl_holiday_cancellation_rate, by = "Holiday")

# EDA - Holiday Cancellation Rate by Cancellation Status
boxplot(holiday_cancellation_rate ~ CANCELLED, data = clean_data,
        main = "Holiday Cancellation Rate by Cancellation Status",
        xlab = "Cancelled",
        ylab = "Holiday Cancellation Rate",
        col = c("lightblue", "lightgreen"))

--------------------------------------------------------------------------------
# OP_CARRIER
--------------------------------------------------------------------------------

# EDA - Flights by OP_CARRIER
ggplot(clean_data, aes(x = factor(OP_CARRIER))) +
  geom_bar(fill = "skyblue", color = "darkblue") +
  labs(title = "Flights by OP_CARRIER", x = "OP_CARRIER", y = "Count") +
  theme_minimal()

# Feature - Carrier Cancellation Rate
op_carrier_cancellation_rate <- clean_data %>%
  group_by(OP_CARRIER) %>%
  summarise(carrier_cancellation_rate = mean(CANCELLED == 1, na.rm = TRUE))
clean_data <- left_join(clean_data, op_carrier_cancellation_rate, by = "OP_CARRIER")

# EDA - Carrier Cancellation Rate by Cancellation Status
boxplot(carrier_cancellation_rate ~ CANCELLED, data = clean_data,
        main = "Carrier Cancellation Rate by Cancellation Status",
        xlab = "Cancelled",
        ylab = "Carrier Cancellation Rate",
        col = c("lightblue", "lightgreen"))

# Feature - Flight Density by Carrier
flight_density_by_carrier <- clean_data %>%
  group_by(OP_CARRIER) %>%
  summarise(carrier_count = n())
clean_data <- left_join(clean_data, flight_density_by_carrier, by = "OP_CARRIER")

# EDA - Flight Cancellations by OP_CARRIER
ggplot(clean_data, aes(x = OP_CARRIER, fill = factor(CANCELLED))) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Flight Cancellations by Carrier", x = "Carrier", y = "Count") +
  theme_minimal()

--------------------------------------------------------------------------------
# ORIGN and DEST
--------------------------------------------------------------------------------

# Feature - Frequency of Origin and Dest
clean_data <- clean_data %>%
group_by(ORIGIN) %>%
mutate(ORIGIN_FREQUENCY = n()) %>%
ungroup() %>%
group_by(DEST) %>%
mutate(DEST_FREQUENCY = n())

# EDA - Top 10 Origin flights cancelled
cancelled_data <- clean_data[clean_data$CANCELLED == 1, ]
top_10_origin <- head(sort(table(cancelled_data$ORIGIN), decreasing = TRUE), 10)
barplot(top_10_origin, main = "Top 10 ORIGIN with CANCELLED = 1",
        xlab = "ORIGIN", ylab = "Count", col = "skyblue")
axis(side = 1, at = 1:10, labels = names(top_10_origin), las = 2)

# EDA - Top 10 Dest flights cancelled
cancelled_data <- clean_data[clean_data$CANCELLED == 1, ]
top_10_dest <- head(sort(table(cancelled_data$DEST), decreasing = TRUE), 10)
barplot(top_10_dest, main = "Top 10 DEST with CANCELLED = 1",
        xlab = "DEST", ylab = "Count", col = "skyblue")
#text(x = 1:10, y = top_10_origin + 1, labels = top_10_origin, col = "darkred")
axis(side = 1, at = 1:10, labels = names(top_10_dest), las = 2)

--------------------------------------------------------------------------------
# DIVERTED
--------------------------------------------------------------------------------

# EDA - Flight Cancellations by DIVERTED
ggplot(clean_data, aes(x = DIVERTED, fill = factor(CANCELLED))) +
geom_bar(position = "dodge", stat = "count") +
labs(title = "Flight Cancellations by Diversion", x = "Diversion", y = "Count") +
theme_minimal()  

# Feature - DIVERTED Cancellation Rate
fl_diverted_cancellation_rate <- clean_data %>%
group_by(DIVERTED) %>%
summarise(diverted_cancellation_rate = mean(CANCELLED == 1, na.rm = TRUE))
clean_data <- left_join(clean_data, fl_diverted_cancellation_rate, by = "DIVERTED")

# EDA - Diverted Cancellation Rate by Cancellation Status
boxplot(diverted_cancellation_rate ~ CANCELLED, data = clean_data,
        main = "Diverted Cancellation Rate by Cancellation Status",
        xlab = "Cancelled",
        ylab = "Diverted Cancellation Rate",
        col = c("lightblue", "lightgreen"))

--------------------------------------------------------------------------------
# DISTANCE
--------------------------------------------------------------------------------

# EDA - Distance Cancellation Rate by Cancellation Status
boxplot(DISTANCE ~ CANCELLED, data = clean_data,
        main = "Distance by Cancellation Status",
        xlab = "Cancelled",
        ylab = "Distance",
        col = c("lightblue", "lightgreen"))

--------------------------------------------------------------------------------
# Correlation
--------------------------------------------------------------------------------
  
columns_to_remove <- c("FL_DATE", "OP_CARRIER", "OP_CARRIER_FL_NUM",
                       "ORIGIN", "DEST", "CANCELLATION_CODE", "DIVERTED",
                       "FL_Month", "Weekend", "Holiday")
final_data <- clean_data[, !(names(clean_data) %in% columns_to_remove)]


# column names
column_names <- names(final_data)
print("Column names of the data after removing unnecessary:")
print(column_names)

# Calculate the correlation matrix
cor_matrix <- cor(final_data)
#corrplot(cor_matrix, method = "number", type = "full", tl.col="black")
corrplot(cor_matrix, method = "color", type = "full", tl.col = "black", addCoef.col = "black", 
         col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))(200),
         number.cex = 0.5)  

#Removing correlated features
columns_to_remove <- c("holiday_cancellation_rate","DISTANCE")
final_data <- final_data[, !(names(final_data) %in% columns_to_remove)]

--------------------------------------------------------------------------------
# Splitting into Train and Test Data -(80:20)
--------------------------------------------------------------------------------
set.seed(123)
train_indices=sample(seq_len(nrow(final_data)),size=0.8*nrow(final_data))
train_data=final_data[train_indices, ]
print(paste("Train data size: ",nrow(train_data)))
test_data=final_data[-train_indices, ]
print(paste("Test data size: ",nrow(test_data)))

--------------------------------------------------------------------------------
# Save and Load the Data
--------------------------------------------------------------------------------

# Save the data frame to a .RData file
saveRDS(train_data, file = "train_data.RData")
saveRDS(test_data, file = "test_data.RData")

# Load the data frame from the .RData file
train_data <- readRDS("train_data.RData")
test_data <- readRDS("test_data.RData")

--------------------------------------------------------------------------------
# Function - Performance Metrics
--------------------------------------------------------------------------------

evaluate_classification_model <- function(model_name, Y_true, Y_pred) {
  print(model_name)
  Y_true <- factor(Y_true, levels = levels(factor(Y_pred)))
  Y_pred <- factor(Y_pred, levels = levels(factor(Y_true)))
  # Confusion Matrix
  confusion_mat <- confusionMatrix(Y_pred, Y_true)
  # Accuracy
  accuracy <- confusion_mat$overall["Accuracy"]
  # Recall
  recall <- confusion_mat$byClass["Sensitivity"]
  # Print and return results
  cat("Model Name:", model_name, "\n")
  cat("Confusion Matrix:\n", confusion_mat$table, "\n")
  cat("Accuracy:", accuracy, "\n")
  cat("Recall:", recall, "\n")
  return(list(
    confusion_matrix = confusion_mat$table,
    accuracy = accuracy,
    recall = recall
  ))
}

--------------------------------------------------------------------------------
# KNN with hyperparameter tuning
--------------------------------------------------------------------------------

# Define the training control
ctrl <- trainControl(method = "cv", number = 5)
# Create a grid of hyperparameters to tune
knn_grid <- expand.grid(k = seq(5, 20, by = 5))
# Train the KNN model with hyperparameter tuning
knn_model <- train(CANCELLED ~ ., data = train_data, method = "knn",
                   trControl = ctrl, tuneGrid = knn_grid)
# Print the best hyperparameters
print(knn_model)
# plot
plot(knn_model)
# Make predictions on the test set
y_pred <- predict(knn_model, newdata = test_data)
y_true = test_data$CANCELLED
evaluate_classification_model("knn_model", y_true, y_pred)

--------------------------------------------------------------------------------
# Random Forest with hyperparameter tuning
--------------------------------------------------------------------------------
  
# Define the training control
ctrl <- trainControl(method = "cv", number = 5)
# Create a grid of hyperparameters to tune
rf_grid <- expand.grid(
  ntree = seq(100, 500, by = 100)  # Number of trees in the forest
)
# Train the Random Forest model with hyperparameter tuning
rf_model <- train(
  CANCELLED ~ ., 
  data = train_data, 
  method = "rf",
  trControl = ctrl, 
  tuneGrid = rf_grid
)
# Print the best hyperparameters
print(rf_model)
# Make predictions on the test set
y_pred <- predict(rf_model, newdata = test_data)
evaluate_classification_model("RandomForest_model", y_true, y_pred)