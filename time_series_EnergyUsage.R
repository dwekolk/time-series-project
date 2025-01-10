library(readr)
library(tidyverse)
library(fpp3)
library(forecast)
library(tsibble)
library(fable.prophet)
library(timeDate)
library(fable)
library(xgboost)
library(lubridate)

hrl_load_metered_test1 <- read_csv("hrl_load_metered - test1.csv")
hrl_load_metered <- read_csv("hrl_load_metered.csv")

# weekly addition 
hrl_load_metered_test2 <- read_csv("hrl_load_metered - test2.csv")
hrl_load_metered_test3 <- read_csv("hrl_load_metered - test3.csv")
hrl_load_metered_test4 <- read_csv("hrl_load_metered - test4.csv")
hrl_load_metered_test5 <- read_csv("hrl_load_metered - test5.csv")
hrl_load_metered_test6 <- read_csv("hrl_load_metered - test6.csv")

# turn to date
hrl_load_metered$date <-
  as.POSIXct(hrl_load_metered$datetime_beginning_ept,
             format = "%m/%d/%y %H:%M",
             tz = "America/New_York")
hrl_load_metered_test1$date <-
  as.POSIXct(hrl_load_metered_test1$datetime_beginning_ept,
             format = "%m/%d/%y %H:%M",
             tz = "America/New_York")
hrl_load_metered_test2$date <-
  as.POSIXct(hrl_load_metered_test2$datetime_beginning_ept,
             format = "%m/%d/%y %H:%M",
             tz = "America/New_York")
hrl_load_metered_test3$date <-
  as.POSIXct(hrl_load_metered_test3$datetime_beginning_ept,
             format = "%m/%d/%y %H:%M",
             tz = "America/New_York")
hrl_load_metered_test4$date <-
  as.POSIXct(hrl_load_metered_test4$datetime_beginning_ept,
             format = "%m/%d/%y %H:%M",
             tz = "America/New_York")
hrl_load_metered_test5$date <-
  as.POSIXct(hrl_load_metered_test5$datetime_beginning_ept,
             format="%m/%d/%Y %I:%M:%S %p",
             tz = "America/New_York")
hrl_load_metered_test6$date <-
  as.POSIXct(hrl_load_metered_test6$datetime_beginning_ept,
             format = "%m/%d/%y %H:%M",
             tz = "America/New_York")

# TURN INTO DATE TYPE 
train <- hrl_load_metered
# Roll up 
train <- bind_rows(train, hrl_load_metered_test1)
train <- bind_rows(train, hrl_load_metered_test2)
train <- bind_rows(train, hrl_load_metered_test3)
train <- bind_rows(train, hrl_load_metered_test4)
train <- bind_rows(train, hrl_load_metered_test5)

# MOST RECENT TEST
test <- hrl_load_metered_test6
# subset
train <- train %>%
  filter(date >= "2023-10-01 00:00:00" & date <= max(date))
# make tsibble 
# REMOVE DUPLICATE FALL DAYLIGHT SAVINGS DAYS 
train <- train %>% distinct(date, .keep_all = TRUE)
# MAKE A TSIBBLE
train <- as_tsibble(train, index = date)
# FILL IN MISSING SERIES DATE WITH NA VALUE PLACEHOLDER FOR THE REST 
train <- train %>%
  complete(date = seq(min(date), max(date), by = "1 hour"))
# LOCF FOR MW REST ARE STILL NA
train <- train %>%
  fill(mw, .direction = "down")
train <- as_tsibble(train, index = date)
test <- as_tsibble(test, index = date)
# temp
temp <- read_csv("tempAEP.csv")
# REMOVE DUPLICATE FALL DAYLIGHT SAVINGS DAYS 
temp <- temp %>% distinct(date, .keep_all = TRUE)
# MAKE A TSIBBLE
temp <- as_tsibble(temp, index = date)
# FILL IN MISSING SERIES DATE WITH NA VALUE PLACEHOLDER FOR THE REST 
temp <- temp %>%
  complete(date = seq(min(date), max(date), by = "1 hour"))
# LOCF FOR MW REST ARE STILL NA
temp <- temp %>%
  fill(average_value, .direction = "down")
temp <- as_tsibble(temp, index=date)
train <- train %>%
  left_join(temp, by = "date") 
test <- test %>%
  left_join(temp, by = "date")
# tsibble with train
train <- as_tsibble(train, index = date)
# test <- as_tsibble(test, index = date)
set.seed(12345)
# ESM
train_HWM <- train |> model(ETS(mw ~ error("M") + trend("N") + season("M")))
train_fc <- forecast(train_HWM,h=168)
# ACCURACY
accuracy(train_fc, test)

# SARIMA
ar200210 <- train %>%
  model(ARIMA(mw ~ average_value + pdq(2,0,0) + PDQ(2,1,0) + 0))
fcS <- forecast(ar200210,h=168)

accuracy(fcS$.mean,test$mw)

# neural network
model_nnet <- train %>%
  mutate(diff_mw = difference(mw, 24)) %>%
  model(
    hand = NNETAR(diff_mw ~ average_value + AR(p = 2, P = 2)),
    
    auto = NNETAR(diff_mw ~ average_value)  
  )
model_nnet_for <- forecast(model_nnet, h=168)
nnet_for_hand <- rep(NA, 168)
nnet_for_auto <- rep(NA, 168)

for(i in 1:168){
  nnet_for_hand[i] <- train$mw[length(train$mw) - 168 + i] + 
    model_nnet_for$.mean[i]
}
for(i in 1:168){
  nnet_for_auto[i] <- train$mw[length(train$mw) - 168 + i] + 
    model_nnet_for$.mean[i+168]
}
# accuracy 
NN_error_auto <- test$mw - nnet_for_auto
NN_MAE_auto <- mean(abs(NN_error_auto))
NN_MAPE_auto <- mean(abs(NN_error_auto)/abs(test$mw))*100
NN_error_hand <- test$mw - nnet_for_hand
NN_MAE_hand <- mean(abs(NN_error_hand))
NN_MAPE_hand <- mean(abs(NN_error_hand)/abs(test$mw))*100

model_nnet %>%
  select(auto) %>%
  report()
model_nnet %>%
  select(hand) %>%
  report()

# temp forecast 
forecastedtemp <- read_csv("forecasttemps.csv")


# prophet 
proph <- train %>%
  model(
    prophet(mw ~ average_value + growth("linear") + 
              season(period = 168, order = 15, type = "multiplicative")) 
  )
# creating a df with just temp and date 
# for proph to forecast mw on
ft <- as_tsibble(forecastedtemp, index=date)
prophF <- forecast(proph, ft)
accuracy(prophF$.mean, test$mw)


# tbats 
tbats_model <- tbats(train$mw, seasonal.periods = 168)
tbatsF <- forecast(tbats_model, h = 168)
testvals <- test$mw
# accuracy
accuracy(tbatsF,testvals)


# xgboost
# turn data to be all numeric 
train_df <- as.data.frame(train)
# one hot encoding the date to make each part a feature variable
trainxg <- train_df %>%
  mutate(
    average_value = as.numeric(as.character(average_value)),
    hour = as.integer(format(date, "%H")),
    day_of_week = as.integer(format(date, "%u")),
    week_of_year = as.integer(format(date, "%U")),
    month = as.integer(format(date, "%m"))
  ) %>%
  select(mw, average_value, hour, day_of_week, week_of_year, month) %>%
  drop_na()  
# turn into matrix
x_train <- data.matrix(trainxg %>% select(-mw)) 
y_train <- as.numeric(trainxg$mw) 
dtrain <- xgb.DMatrix(data = x_train, label = y_train)
# same for test
testxg <- test %>%
  mutate(
    average_value = as.numeric(average_value),  # Ensure it's numeric
    hour = as.integer(format(date, "%H")),
    day_of_week = as.integer(format(date, "%u")),
    week_of_year = as.integer(format(date, "%U")),
    month = as.integer(format(date, "%m"))
  ) %>%
  select(average_value, hour, day_of_week, week_of_year, month) %>%
  drop_na()
x_test <- data.matrix(testxg)

# specify model parameters
params <- list(
  objective = "reg:squarederror",
  eval_metric = "mape",             # evaluation metric
  eta = 0.2,                        # learning rate
  max_depth = 5,    # max depth of tree
  subsample = 0.8,  # sample percentage used for each tree
  colsample_bytree = 1.0 
)
# train model
model_xgb <- xgb.train(params = params, data = dtrain, nrounds = 100) # early stopping avoids overfitting
# forecast
predictions <- predict(model_xgb, newdata = xgb.DMatrix(data = x_test))

# forecasting in the future with xg boost thanks to rob mulla on youtube
# future dates
last_date <- max(train$date)  
future_dates <- seq(from = last_date + hours(1), by = "hour", length.out = 192)
# creating a future data frame with all features used
forecasted_temperatures <- forecastedtemp$average_value # grabbing the forecasted temps as vector to add to df
future_data <- data.frame(date = future_dates) %>%
  mutate(
    hour = as.integer(format(date, "%H")),
    day_of_week = as.integer(format(date, "%u")),
    week_of_year = as.integer(format(date, "%U")),
    month = as.integer(format(date, "%m")),
    average_value = forecasted_temperatures 
  )
# predict 
predictions_future <- numeric(192)
feature_names <- c("average_value", "hour", "day_of_week", "week_of_year", "month")
# predict
# this is going through and adding a prediction for each hour to predict_future
for (i in 1:192) {
  # this is just to make sure that the features included here match up with dtrain
  # which was used to train the model, if they don't match will produce error
  x_future <- future_data[i, feature_names, drop = FALSE]
  d_future <- xgb.DMatrix(data = data.matrix(x_future))
  predictions_future[i] <- predict(model_xgb, newdata = d_future)
}
# add dates and create data frame 
future_results <- data.frame(date = future_dates, predicted_mw = predictions_future)



# mape
predictions <- future_results$predicted_mw # insert whatever the predictions are here
actuals <- test$mw  # Actual values
mape <- mean(abs((actuals - predictions) / actuals)) * 100
# mae 
mae <- mean(abs(actuals - predictions))

# ensemble stuff

# make tbat nnet and xg able to be ensembled 
xgDF <- as.data.frame(future_results)
tbFDF <- as.data.frame(tbatsF$mean)
nnDF <- as.data.frame(nnet_for_hand)
colnames(xgDF)[colnames(xgDF) == "predicted_mw"] <- ".mean"
colnames(tbFDF)[colnames(tbFDF) == "x"] <- ".mean"
colnames(nnDF)[colnames(nnDF) == "nnet_for_hand"] <- ".mean"
# Combine data frames into a list and calculate row-wise averages
dfs <- list(prophF, xgDF) # insert forecasts that you want ensembled here
average_values <- rowMeans(sapply(dfs, `[[`, ".mean"), na.rm = TRUE)
# Create a new data frame with the averaged values
average_df <- data.frame(average_value = average_values)
combined_df <- data.frame(date = prophF$date, average_value = average_df$average_value)
# Convert to a tsibble
average_tsibble <- as_tsibble(combined_df, index = date)
# subset this tsibble to be from 10/25 - 10/31
orange9 <- average_tsibble %>%
  # i went back to 10/24 at 20 because it kept spitting out 
  # data starting at 10/25 4:00 something to do with formatting 
  filter(date >= "2024-10-24 20:00:00" & date <= max(date)) 

write.csv(orange9, "orange9_predictions.csv",row.names = FALSE)


# ensemble on validation visualization

ggplot(test, aes(x = date)) +
  geom_line(aes(y = mw, color = "Actual"), show.legend = TRUE, size = 1.0) +  # Actual
  geom_line(aes(y = average_tsibble$average_value, color = "Predicted"), show.legend = TRUE, size = 1.0) +  # Predicted
  labs(y = "Hourly Megawatt (MW)", x = "Date", title = "Actual versus Forecast Values for Model Ensemble", color = NULL) +
  scale_color_manual(values = c("Actual" = "black", "Predicted" = "#FF5733")) +  # Updated to orange-red
  # Increase the y-axis limit to avoid cutting off values
  ylim(2700, NA) +  # Set lower limit to 2900 or adjust as needed
  theme_minimal() +  # Minimal theme
  theme(panel.grid.major.x = element_blank(),  # Remove major vertical grid lines
        panel.grid.minor.x = element_blank(),  # Remove minor vertical grid lines
        panel.grid.major.y = element_line(color = "grey80"),  # Add major horizontal grid lines
        panel.grid.minor.y = element_line(color = "grey90"),  # Add minor horizontal grid lines
        legend.position = "right",            # Position the legend
        plot.title = element_text(hjust = 0.5, family = "Times New Roman", size = 17),  # Title
        axis.title.x = element_text(family = "Times New Roman", size = 14),  # X-axis title
        axis.title.y = element_text(family = "Times New Roman", size = 14),  # Y-axis title
        axis.text.x = element_text(family = "Times New Roman", size = 12),  # X-axis tick labels
        axis.text.y = element_text(family = "Times New Roman", size = 12),  # Y-axis tick labels
        legend.text = element_text(family = "Times New Roman", size = 12))  # Legend labels





