# Load libraries
library(tidyverse)
library(forecast)
library(randomForest)
library(ggplot2)
library(reshape2)
library(lubridate)
library(ggthemes)

# Load models
load("bin/model.RData")

# Get predictions for the 4 types of models
simple_LR_copper_pred <- predict(simple_LR_copper, test)
ARIMA_copper_pred <- as.numeric(forecast_copper$mean)
NN_copper_pred <- data.frame(nn_forecast_copper)
RF_copper_pred <- predict(RF_copper, newdata = test)
length(ARIMA_copper_pred)
length(test$copper_price)

# If they are not equal, you may need to adjust the forecast to match the length of the test data.
# For example, you may need to subset the forecast to match the length of the test data.
# Assuming `test` is a data frame containing your test data:

ARIMA_copper_pred_subset <- ARIMA_copper_pred[1:length(test$copper_price)]

# Now, calculate residuals using the subsetted forecast
residuals_ARIMA <- ARIMA_copper_pred_subset - test$copper_price
# Evaluate Simple Linear Regression
residuals_LR <- simple_LR_copper_pred - test$copper_price
MAE_LR <- mean(abs(residuals_LR))
MSE_LR <- mean(residuals_LR^2)
RMSE_LR <- sqrt(mean(residuals_LR^2))
R_squared_LR <- summary(simple_LR_copper)$r.squared

# Evaluate ARIMA Model
residuals_ARIMA <- ARIMA_copper_pred - test$copper_price
MAE_ARIMA <- mean(abs(residuals_ARIMA))
MSE_ARIMA <- mean(residuals_ARIMA^2)
RMSE_ARIMA <- sqrt(mean(residuals_ARIMA^2))
MAPE_ARIMA <- mean(abs(residuals_ARIMA / test$copper_price)) * 100

# Evaluate Random Forest Model
residuals_RF <- RF_copper_pred - test$copper_price
MAE_RF <- mean(abs(residuals_RF))
MSE_RF <- mean(residuals_RF^2)
RMSE_RF <- sqrt(mean(residuals_RF^2))

# Model Results Graph
all_results_copper <- data.frame(
      Simple_LR = predict(simple_LR_copper, test),
      ARIMA = ARIMA_copper_pred,
      Random_Forest = predict(RF_copper, test),
      Actual_Price = test$copper_price,
      Date = test$date
)

# Plot comparison graph for copper prices
copper_melt <- melt(all_results_copper, id = "Date")
comparison_plot <- ggplot(data = copper_melt, aes(x = Date, y = value, col = variable)) +
      geom_line() +
      ggtitle("Predictions for Copper") +
      scale_color_manual(
            name = "Models",
            breaks = c("Simple_LR", "ARIMA", "Random_Forest", "Actual_Price"),
            labels = c("Linear Regression", "ARIMA", "Random Forest", "Actual Price"),
            values = c("red", "blue", "green", "black")
      ) +
      geom_rect(
            data = data.frame(
                  xmin = as.Date("2024-02-01"),
                  xmax = as.Date("2024-03-04"),
                  ymin = -Inf,
                  ymax = Inf
            ),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "orange",
            alpha = 0.3
      ) +
      xlab("Year") +
      ylab("Price in USD") +
      theme_stata()

# Plot forecast models
forecast_plots <- list(
      ARIMA = plot(forecast_copper, main = "ARIMA Forecast for Copper"),
      NN_Forecast = plot(nn_forecast_copper, main = "NN Forecast for Copper")
)

# Save forecast results to CSV
write.csv(nn_forecast_copper, "results/nn_results_copper.csv", row.names = FALSE)
write.csv(all_results_copper, "results/all_results_copper.csv", row.names = FALSE)

# Print evaluation metrics
cat("Simple Linear Regression Metrics:\n")
cat("MAE:", MAE_LR, "\n")
cat("MSE:", MSE_LR, "\n")
cat("RMSE:", RMSE_LR, "\n")
cat("R-squared:", R_squared_LR, "\n\n")

cat("ARIMA Model Metrics:\n")
cat("MAE:", MAE_ARIMA, "\n")
cat("MSE:", MSE_ARIMA, "\n")
cat("RMSE:", RMSE_ARIMA, "\n")
cat("MAPE:", MAPE_ARIMA, "%\n\n")

cat("Random Forest Model Metrics:\n")
cat("MAE:", MAE_RF, "\n")
cat("MSE:", MSE_RF, "\n")
cat("RMSE:", RMSE_RF, "\n")

