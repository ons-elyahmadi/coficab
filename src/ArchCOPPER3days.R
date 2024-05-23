library(fGarch)
library(rugarch)
library(FinTS)
# Load necessary libraries
library(forecast)
library(tseries)
library(ggplot2)
# Lecture des données
data <- read.csv("data/cleanindata.csv", header = TRUE)
ArchTest(data[,3])
# Load the necessary library
library(fGarch)

 

# Print the summary of the model
print(summary(garch_model))

  
# Spécification du modèle ARCH
#arch_model <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(5, 9)),
#                         mean.model = list(armaOrder = c(3, 9), include.mean = TRUE),
#                         distribution.model = "norm")

# Estimation du modèle ARCH
#arch_fit <- ugarchfit(spec = arch_model, data = data[,3], xreg = data[, 2], D=1)

# Prévision de la volatilité pour le mois suivant (en supposant 20 jours ouvrables)
#volatility_forecast <- ugarchforecast(arch_fit, xreg = tail(data[, 2], 20), n.ahead = 18)

# Affichage de la prévision de la volatilité
#print(volatility_forecast)
# Read the data
#daActuel <- read.csv("data/dataActuel.csv", header = TRUE)
#print(daActuel)

#daActuel_values <- as.numeric(gsub(",", ".",daActuel$price))
#print(daActuel$price)
## Extract the forecasted series from volatility_forecast
#forecasted_series <- volatility_forecast@forecast$seriesFor

# Compute squared errors
#squared_errors <- (daActuel$price - forecasted_series)^2
 
# Calculate mean squared error
#MSE <- mean(squared_errors)

# Calculate RMSE
#RMSE <- sqrt(MSE)

# Print RMSE

#print(RMSE)
# Chargement des bibliothèques
library(fGarch)
library(rugarch)

# Lecture des données
data <- read.csv("data/cleanindata.csv", header = TRUE)
# Création de la liste de modèles
arch_model_01 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(0, 1)),
                            mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
                            distribution.model = "norm")

# Ordre GARCH (1, 0)
arch_model_10 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
                            mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
                            distribution.model = "norm")

# Ordre GARCH (1, 2)
arch_model_12 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 2)),
                            mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
                            distribution.model = "norm")

# Ordre GARCH (2, 2)
arch_model_22 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2, 2)),
                            mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
                            distribution.model = "norm")

# Ordre GARCH (2, 1)
arch_model_21 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2, 1)),
                            mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
                            distribution.model = "norm")

# Ordre GARCH (2, 0)
arch_model_20 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2, 0)),
                            mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
                            distribution.model = "norm")

# Ordre GARCH (0, 2)
arch_model_02 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(0, 2)),
                            mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
                            distribution.model = "norm")

modelA <- list(
        ugarchfit(spec = arch_model_01, data =  data[, 3], xreg = data[, 2]),
        ugarchfit(spec = arch_model_10, data =  data[, 3], xreg = data[, 2]),
        ugarchfit(spec = arch_model_12, data =  data[, 3], xreg = data[, 2]),
        ugarchfit(spec = arch_model_22, data =  data[, 3], xreg = data[, 2]),
        ugarchfit(spec = arch_model_21, data =  data[, 3], xreg = data[, 2]),
        ugarchfit(spec = arch_model_20, data =  data[, 3], xreg = data[, 2]),
        ugarchfit(spec = arch_model_02, data =  data[, 3], xreg = data[, 2])
      )
 
 
 
rmse <- sapply(modelA, function(model) {
      residuals <- residuals(model)
      n <- length(residuals)
      SSE <- sum(residuals^2)
      MSE <- SSE / n
      RMSE <- sqrt(MSE)
      return(RMSE)
})

# Create a data frame for RMSE
rmse_dfAR <- data.frame(Model = paste("Model", 1:7), RMSE = rmse)

# Plot RMSE
ggplot(rmse_dfAR, aes(x = Model, y = RMSE)) +
      geom_line() +
      geom_point() +
      geom_text(aes(label = round(RMSE, 2)), vjust = 0.5) +
      labs(title = "RMSE for ARCH Models",
           x = "ARCH Model",
           y = "RMSE") +
      theme_minimal()




 
 
# Compute R-squared for each model
r_squared <- sapply(modelA, function(model) {
      residuals <- residuals(model)
      SSE <- sum(residuals^2)
      SST <- sum((data[,3] - mean(data[,3]))^2)
      R_squared <- 1 - SSE / SST
      return(R_squared)
})
# Create a data frame for R-squared
r_squared_dfAR <- data.frame(Model = paste("Model", 1:7), R_squared = r_squared)

# Plot R-squared
ggplot(r_squared_dfA, aes(x = Model, y = R_squared)) +
      geom_line() +
      geom_point() +
      geom_text(aes(label = round(R_squared, 2)), vjust = 0.5) +
      labs(title = "R-squared for ARCH  Models",
           x = "ARCH  Model",
           y = "R-squared") +
      theme_minimal()
 
 
 
# Compute MAE for each model
mae <- sapply(modelA, function(model) {
      residuals <- residuals(model)
      n <- length(residuals)
      MAE <- sum(abs(residuals)) / n
      return(MAE)
})

# Create a data frame for MAE
mae_dfAR <- data.frame(Model = paste("Model", 1:7), MAE = mae)

# Plot MAE
ggplot(mae_dfAR, aes(x = Model, y = MAE)) +
      geom_line() +
      geom_point() +
      geom_text(aes(label = round(MAE, 2)), vjust = 0.5) +
      labs(title = "Mean Absolute Error (MAE) for ARCH Models",
           x = "ARCH  Model",
           y = "MAE") +
      theme_minimal()
# Compute MSE for each model
mse <- sapply(modelA, function(model) {
      residuals <- residuals(model)
      n <- length(residuals)
      SSE <- sum(residuals^2)
      MSE <- SSE / n
      return(MSE)
})

# Create a data frame for MSE
mse_dfAR <- data.frame(Model = paste("Model", 1:7), MSE = mse)

# Plot MSE
ggplot(mse_dfAR, aes(x = Model, y = MSE)) +
      geom_line() +
      geom_point() +
      geom_text(aes(label = round(MSE, 2)), vjust = 0.5) +
      labs(title = "Mean Squared Error (MSE) for ARCH  Models",
           x = "ARCH  Model",
           y = "MSE") +
      theme_minimal()


jarque_bera <- sapply(modelA, function(model) {
      residuals <- residuals(model)
      jb_test <- jarque.bera.test(residuals)
      return(jb_test$p.value)
})

# Create a data frame for Jarque-Bera test p-values
jb_dfAR <- data.frame(Model = paste("Model", 1:7), p_value = jarque_bera)

# Plot p-values of Jarque-Bera test
ggplot(jb_dfAR, aes(x = Model, y = p_value)) +
      geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
      geom_text(aes(label = round(p_value, 3)), vjust = -0.5) +
      labs(title = "Jarque-Bera Test p-values for ARCH Models",
           x = "ARCH Model",
           y = "p-value") +
      theme_minimal()


ljung_box <- sapply(modelA, function(model) {
      ljung_box_test <- Box.test(residuals(model), lag = 10, type = "Ljung-Box")
      return(ljung_box_test$p.value)
})

# Create a data frame for Ljung-Box test p-values
ljung_box_dfAR <- data.frame(Model = paste("Model", 1:7), p_value = ljung_box)

# Plot p-values of Ljung-Box test
ggplot(ljung_box_dfAR, aes(x = Model, y = p_value)) +
      geom_bar(stat = "identity", fill = "salmon", width = 0.5) +
      geom_text(aes(label = round(p_value, 3)), vjust = -0.5) +
      labs(title = "Ljung-Box Test p-values for ARCH Models",
           x = "ARCH Model",
           y = "p-value") +
      theme_minimal()


# Spécification du modèle ARCH
arch_model <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(0, 2)),
                         mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
                         distribution.model = "norm")

# Estimation du modèle ARCH
arch_fit <- ugarchfit(spec = arch_model, data =  data[, 3], xreg = data[, 2])
arch_fit
# Affichage du résumé du modèle ARCH
print(arch_fit)

# Prévision de la volatilité pour le mois suivant (en supposant 3 jours ouvrables)
volatility_forecast <- ugarchforecast(arch_fit, xreg = tail(data[, 2], 3), n.ahead = 3)

# Affichage de la prévision de la volatilité
print(volatility_forecast)
class(volatility_forecast)
# Extraction des valeurs de la prévision de la volatilité
forecast_values <- as.data.frame(volatility_forecast@forecast)

# Ajout des dates pour les prévisions (si nécessaire)
forecast_dates <- as.data.frame(index(data)[length(data)-2:length(data)] + 1:3)  # Supposant une séquence de 3 jours ouvrables
colnames(forecast_dates) <- "Date"
forecast_values <- cbind(forecast_dates, forecast_values)

# Sauvegarde des résultats dans un fichier CSV
write.csv(forecast_values, file = "data/forecast_resultsARCH3.csv", row.names = FALSE)

daforcasted <- read.csv("data/forecast_resultsARCH3.csv" , header = TRUE  )
head(daforcasted)
print(daforcasted[,7])
# Read the data
daActuel <- read.csv("data/3day.csv", header = TRUE)
print(daActuel)


daActuel_values <- as.numeric(gsub(",", ".",daActuel$price))

# Plot actual values
plot(daActuel_values, type = "l", col = "blue", xlab = "Time", ylab = "Price", main = "Actual Copper Prices VS forcasted value", ylim = c(7000, 10000))

# Add the forecasted values to the plot
lines(daforcasted[,7], col = "red")

# Add legend
legend("bottomright", legend = c("Actual", "Forecast"), col = c("blue", "red"), lty = 1, cex = 0.2)


library(ggplot2)

# Read the actual data


# Calculate the Mean Absolute Error (MAE) and Mean Squared Error (MSE)
mae <- mean(abs(daforcasted[,7] -  daActuel_values))
mse <- mean((daforcasted[,7] -  daActuel_values)^2)

# Print MAE and MSE
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Mean Squared Error (MSE):", mse, "\n")
# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((daforcasted[,7]-  daActuel_values)^2))
# Calculate Mean Absolute Percentage Error (MAPE)
mape <- mean(abs((daforcasted[,7] -  daActuel_values) / daActuel_values)) * 100
# Print RMSE, MAPE, and Theil's U Statistic
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", mape, "%\n")
plot(forecast, main ="ARIMA Forecast for Copper") 

