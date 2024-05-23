library(tseries)
library(ggplot2)
data <-  read.csv("data/cleanindata.csv", header = TRUE)

attach(data)
head(data)
plot(Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.)
plot(Official.Ask.LME)
#p-value < 0.05  stationary
adf.test(Official.Ask.LME ,alternative = "stationary")
adf.test(Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel. ,alternative = "stationary")
# Load necessary libraries
library(forecast)
ts_data <- ts(data[, 3])
# Use auto.arima to fit the model
fit <- auto.arima(ts_data, xreg = data[, 2], seasonal = FALSE)

# Forecast the next 5 days
forecast <- forecast(fit, xreg = tail(data[, 2], 20), h = 20)

# Print the forecast
print(forecast)
# Calculer les résidus du modèle
residuals <- resid(fit)

# Calculer les prévisions pour les données d'entraînement
training_forecast <- fitted(fit)

# Calculer les mesures d'évaluation
rmse <- sqrt(mean(residuals^2))
mae <- mean(abs(residuals))
correlation <- cor(data[, 3], training_forecast)

# Afficher les résultats
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
cat("Corrélation entre les observations et les prévisions:", correlation, "\n")
 
summary(fit)
 
# Convertir les données en série temporelle avec la fréquence appropriée
data_ts <- ts(data[, 3], start = c(2017, 1), frequency = 5)  # Supposant une fréquence hebdomadaire

# Séparer la variable exogène
exog_variable <- data[, 2]

# Utiliser auto.arima pour ajuster un modèle SARIMA avec la variable exogène
sarima_fit <- auto.arima(data_ts, xreg = exog_variable, seasonal = TRUE)

# Afficher le résumé du modèle
summary(sarima_fit)

# Prévisions pour les 5 prochains jours
forecast_sarima <- forecast(sarima_fit, xreg = tail(exog_variable, 1), h = 20)


# Afficher les prévisions
print(forecast_sarima)

 
